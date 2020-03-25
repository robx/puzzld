{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (forever)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Text
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import RIO
import qualified RIO.ByteString.Lazy as BL

type Key = Text

type Rooms = Map Key (MVar Room)

emptyRooms :: Rooms
emptyRooms = Map.empty

getRoom :: Key -> Rooms -> RIO SimpleApp (Rooms, MVar Room)
getRoom key rooms = case Map.lookup key rooms of
  Just room -> return (rooms, room)
  Nothing -> do
    logInfo $ "creating new room: " <> display key
    room <- newMVar emptyRoom
    return $ (Map.insert key room rooms, room)

type Room = [Connection]

emptyRoom :: Room
emptyRoom = []

addConnection :: Connection -> Room -> Room
addConnection conn = (conn :)

connections :: Room -> [Connection]
connections = id

-- | Broadcast a message to all listeneres, dropping those
-- that aren't open anymore.
broadcastMessage :: WebSocketsData a => a -> Room -> RIO SimpleApp Room
broadcastMessage msg room = do
  let conns = connections room
  catMaybes <$> mapM trySend conns
  where
    trySend conn =
      (liftIO (sendTextData conn msg) >> return (Just conn))
        `catch` ( \ConnectionClosed -> do
                    logInfo $ "dropping closed connection"
                    return Nothing
                )

type Application = Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> RIO SimpleApp Wai.ResponseReceived

run :: Warp.Port -> Application -> RIO SimpleApp ()
run port app = do
  env <- ask
  liftIO $ Warp.run port (app' env)
 where
  app' env req respond = runRIO env (app req respond)

app :: MVar Rooms -> Application
app rooms req respond = do
  case Wai.pathInfo req of
    [] ->
      liftIO $ respond $
        Wai.responseLBS
          status200
          [("Content-Type", "text/plain")]
          "Hello, Web!"
    ["game", key] -> do
      room <- modifyMVar rooms (getRoom key)
      env <- ask
      liftIO $ game env room req respond

game :: SimpleApp -> MVar Room -> Wai.Application
game env room = websocketsOr defaultConnectionOptions app backup
  where
    app :: ServerApp
    app pending_conn = do
      conn <- acceptRequest pending_conn
      modifyMVar_ room (pure . addConnection conn)
      runRIO env $ logInfo $ "accepted connection"
      forever (handleMessage conn)
    handleMessage conn = runRIO env $ do
      msg <- liftIO $ receiveDataMessage conn
      case msg of
        Text b _ -> do
          logInfo $ "received text message: " <> displayBytesUtf8 (BL.toStrict b)
          modifyMVar_ room $ broadcastMessage b
        Binary _ -> logInfo "ignoring binary message"
    backup _ respond = respond $ Wai.responseLBS status400 [] "not a websocket request"

main :: IO ()
main = runSimpleApp $ do
  logInfo $ "listening on port 8787"
  rooms <- newMVar emptyRooms
  env <- ask
  liftIO $ run 8787 (app rooms)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (forever)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Text
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import RIO
import Prelude (putStrLn)

type Key = Text

type Rooms = Map Key (MVar Room)

emptyRooms :: Rooms
emptyRooms = Map.empty

getRoom :: Key -> Rooms -> IO (Rooms, MVar Room)
getRoom key rooms = case Map.lookup key rooms of
  Just room -> return (rooms, room)
  Nothing -> do
    putStrLn $ "creating new room: " ++ show key
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
broadcastMessage :: WebSocketsData a => a -> Room -> IO Room
broadcastMessage msg room = do
  let conns = connections room
  catMaybes <$> mapM trySend conns
  where
    trySend :: Connection -> IO (Maybe Connection)
    trySend conn =
      (sendTextData conn msg >> return (Just conn))
        `catch` ( \ConnectionClosed -> do
                    putStrLn $ "dropping closed connection"
                    return Nothing
                )

app :: MVar Rooms -> Application
app rooms req respond = do
  case pathInfo req of
    [] ->
      respond $
        responseLBS
          status200
          [("Content-Type", "text/plain")]
          "Hello, Web!"
    ["game", key] -> do
      room <- modifyMVar rooms (getRoom key)
      game room req respond

game :: MVar Room -> Application
game room = websocketsOr defaultConnectionOptions app backup
  where
    app :: ServerApp
    app pending_conn = do
      conn <- acceptRequest pending_conn
      modifyMVar_ room (pure . addConnection conn)
      putStrLn $ "accepted connection"
      forever (handleMessage conn)
    handleMessage conn = do
      msg <- receiveDataMessage conn
      case msg of
        Text b _ -> do
          putStrLn $ "received text message: " ++ show b
          modifyMVar_ room $ broadcastMessage b
        Binary _ -> putStrLn "ignoring binary message"
    backup :: Application
    backup _ respond = respond $ responseLBS status400 [] "not a websocket request"

main :: IO ()
main = runSimpleApp $ do
  liftIO $ putStrLn $ "listening on port 8787"
  rooms <- newMVar emptyRooms
  liftIO $ run 8787 (app rooms)

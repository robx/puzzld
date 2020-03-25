{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Text
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WebSockets
import Network.WebSockets (WebSocketsData)
import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map
import Web (WebHandler, WebsocketHandler, run, websocketsOr)

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

type Room = [WebSockets.Connection]

emptyRoom :: Room
emptyRoom = []

addConnection :: WebSockets.Connection -> Room -> Room
addConnection conn = (conn :)

connections :: Room -> [WebSockets.Connection]
connections = id

-- | Broadcast a message to all listeners, dropping those
-- that aren't open anymore.
broadcastMessage :: WebSocketsData a => a -> Room -> RIO SimpleApp Room
broadcastMessage msg room = do
  let conns = connections room
  catMaybes <$> mapM trySend conns
  where
    trySend conn =
      (liftIO (WebSockets.sendTextData conn msg) >> return (Just conn))
        `catch` ( \WebSockets.ConnectionClosed -> do
                    logInfo $ "dropping closed connection"
                    return Nothing
                )

toplevel :: MVar Rooms -> WebHandler (RIO SimpleApp)
toplevel rooms req respond = do
  case Wai.pathInfo req of
    [] ->
      respond $
        Wai.responseLBS
          status200
          [("Content-Type", "text/plain")]
          "Hello, Web!"
    ["game", key] -> do
      room <- modifyMVar rooms (getRoom key)
      game room req respond
    _ -> respond $ Wai.responseLBS status404 [] "not found"

game :: MVar Room -> WebHandler (RIO SimpleApp)
game room = websocketsOr WebSockets.defaultConnectionOptions handleConn fallback
  where
    handleConn :: WebsocketHandler (RIO SimpleApp)
    handleConn pendingConn = do
      conn <- liftIO $ WebSockets.acceptRequest pendingConn
      modifyMVar_ room (pure . addConnection conn)
      logInfo $ "accepted connection"
      forever (handleMessage conn)
    handleMessage conn = do
      msg <- liftIO $ WebSockets.receiveDataMessage conn
      case msg of
        WebSockets.Text b _ -> do
          logInfo $ "received text message: " <> displayBytesUtf8 (BL.toStrict b)
          modifyMVar_ room $ broadcastMessage b
        WebSockets.Binary _ -> logInfo "ignoring binary message"
    fallback _ respond = respond $ Wai.responseLBS status400 [] "not a websocket request"

main :: IO ()
main = runSimpleApp $ do
  logInfo $ "listening on port 8787"
  rooms <- newMVar emptyRooms
  run 8787 (toplevel rooms)

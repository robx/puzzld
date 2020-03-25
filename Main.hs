{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WebSockets
import Network.WebSockets (WebSocketsData)
import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map
import Web (WebHandler, run, sourceAddress, websocketsOr)

type Key = Text

type Rooms = Map Key (MVar Room)

emptyRooms :: Rooms
emptyRooms = Map.empty

data App
  = App
      { appLogFunc :: !LogFunc,
        appLogContext :: [Utf8Builder],
        appRooms :: !(MVar Rooms)
      }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

info :: Utf8Builder -> RIO App ()
info msg = do
  logContext <- view $ to appLogContext
  logInfo $ mconcat (map (\c -> c <> ": ") logContext) <> msg

withContext :: [Utf8Builder] -> RIO App a -> RIO App a
withContext ctx = local (\app -> app {appLogContext = ctx})

runApp :: RIO App () -> IO ()
runApp inner = runSimpleApp $ do
  logFunc <- view logFuncL
  rooms <- newMVar emptyRooms
  let app = App {appLogFunc = logFunc, appLogContext = [], appRooms = rooms}
  runRIO app inner

getRoom :: Key -> RIO App (MVar Room)
getRoom key = do
  roomsM <- view $ to appRooms
  modifyMVar roomsM $ \rooms ->
    case Map.lookup key rooms of
      Just room -> return (rooms, room)
      Nothing -> do
        info $ "creating new room: " <> display key
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
broadcastMessage :: WebSocketsData a => a -> Room -> RIO App Room
broadcastMessage msg room = do
  let conns = connections room
  catMaybes <$> mapM trySend conns
  where
    trySend conn =
      (liftIO (WebSockets.sendTextData conn msg) >> return (Just conn))
        `catch` ( \e -> case e of
                    WebSockets.ConnectionClosed -> do
                      info $ "dropping closed connection"
                      return Nothing
                    _ -> throwIO e
                )

toplevel :: WebHandler (RIO App)
toplevel req respond = do
  case Wai.pathInfo req of
    [] ->
      respond $
        Wai.responseLBS
          status200
          [("Content-Type", "text/plain")]
          "Hello, Web!"
    ["game", key] -> do
      let ctx = ["room " <> display key, sourceAddress req]
      room <- getRoom key
      withContext ctx $ game room req respond
    _ -> respond $ Wai.responseLBS status404 [] "not found"

game :: MVar Room -> WebHandler (RIO App)
game room = websocketsOr WebSockets.defaultConnectionOptions handleConn fallback
  where
    handleConn pendingConn = do
      conn <- liftIO $ WebSockets.acceptRequest pendingConn
      modifyMVar_ room (pure . addConnection conn)
      info $ "accepted connection"
      forever (handleMessage conn)
    handleMessage conn = do
      msg <- liftIO $ WebSockets.receiveDataMessage conn
      case msg of
        WebSockets.Text b _ -> do
          info $ "received text message: " <> displayBytesUtf8 (BL.toStrict b)
          modifyMVar_ room $ broadcastMessage b
        WebSockets.Binary _ -> info "ignoring binary message"
    fallback _ respond = respond $ Wai.responseLBS status400 [] "not a websocket request"

main :: IO ()
main = runApp $ do
  info $ "listening on port 8787"
  run 8787 toplevel

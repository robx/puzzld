{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..))
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WebSockets
import Network.WebSockets (WebSocketsData)
import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map
import Web (WebHandler, receiveDataMessageOrClosed, run, sourceAddress, websocketsOr, withPingThread)

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

type ConnectionId = Int

type EventId = Int

data Event
  = Event
      { eventOperation :: Text
      }

encodeEvent :: (EventId, Event) -> BL.ByteString
encodeEvent (eventId, event) =
  Aeson.encode $
    Aeson.object
      ["id" .= Aeson.toJSON eventId, "operation" .= Aeson.toJSON (eventOperation event)]

data Room
  = Room
      { roomConnections :: [(ConnectionId, WebSockets.Connection)],
        roomNextConnectionId :: !ConnectionId,
        roomEvents :: Seq (EventId, Event),
        roomNextEventId :: !EventId
      }

emptyRoom :: Room
emptyRoom = Room
  { roomConnections = [],
    roomNextConnectionId = 0,
    roomEvents = Seq.Empty,
    roomNextEventId = 0
  }

connections :: Room -> [(ConnectionId, WebSockets.Connection)]
connections = roomConnections

addConnection :: WebSockets.Connection -> Room -> (Room, ConnectionId)
addConnection conn room =
  let conns = roomConnections room
      next = roomNextConnectionId room
   in ( room
          { roomConnections = (next, conn) : conns,
            roomNextConnectionId = next + 1
          },
        next
      )

removeConnection :: ConnectionId -> Room -> Room
removeConnection connId room =
  let conns = roomConnections room
   in room {roomConnections = filter (\(i, _) -> i /= connId) conns}

events :: Room -> [(EventId, Event)]
events = toList . roomEvents

addEvent :: Event -> Room -> (Room, EventId)
addEvent event room =
  let next = roomNextEventId room
   in ( room
          { roomEvents = roomEvents room :|> (next, event),
            roomNextEventId = next + 1
          },
        next
      )

-- | Broadcast a message to all listeners, dropping those
-- that aren't open anymore.
broadcastMessage :: WebSocketsData a => a -> Room -> RIO App Room
broadcastMessage msg room = do
  let conns = roomConnections room
  liveConns <- catMaybes <$> mapM trySend conns
  return $ room {roomConnections = liveConns}
  where
    trySend c@(_, conn) =
      (liftIO (WebSockets.sendTextData conn msg) >> return (Just c))
        `catch` ( \e -> case e of
                    WebSockets.ConnectionClosed -> do
                      info $ "dropping closed connection"
                      return Nothing
                    _ -> throwIO e
                )

sendHistory :: WebSockets.Connection -> Room -> RIO App ()
sendHistory conn room =
  liftIO $
    mapM_
      (WebSockets.sendTextData conn . encodeEvent)
      (events room)

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
game roomM = websocketsOr WebSockets.defaultConnectionOptions handleConn fallback
  where
    handleConn pendingConn = do
      conn <- liftIO $ WebSockets.acceptRequest pendingConn
      bracket
        ( modifyMVar roomM $ \room -> do
            sendHistory conn room
            return $ addConnection conn room
        )
        ( \connId -> do
            modifyMVar_ roomM (pure . removeConnection connId)
            info $ "dropped connection " <> displayShow connId
        )
        ( \connId -> do
            info $ "accepted connection " <> displayShow connId
            withPingThread conn 30 $ loop conn
        )
    loop conn = do
      mmsg <- liftIO $ receiveDataMessageOrClosed conn
      case mmsg of
        Just msg@(WebSockets.Text _ _) -> do
          let msgText = WebSockets.fromDataMessage msg :: Text
          modifyMVar_ roomM $ \room -> do
            let event = Event {eventOperation = msgText}
                (room', eventId) = addEvent event room
                payload = encodeEvent (eventId, event)
            info $ "received operation " <> display eventId <> ": " <> display msgText
            broadcastMessage payload room'
          loop conn
        Just (WebSockets.Binary _) -> do
          info "ignoring binary message"
          loop conn
        Nothing ->
          info "connection closed"
    fallback _ respond = respond $ Wai.responseLBS status400 [] "not a websocket request"

main :: IO ()
main = runApp $ do
  info $ "listening on port 8787"
  run 8787 toplevel

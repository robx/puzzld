{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WebSockets
import Network.WebSockets (WebSocketsData)
import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map
import Web (WebHandler, run, sourceAddress, websocketsOr, withPingThread)

type Key = Text

type Rooms = Map Key (TVar Room)

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

debug :: Utf8Builder -> RIO App ()
debug msg = do
  logContext <- view $ to appLogContext
  logDebug $ mconcat (map (\c -> c <> ": ") logContext) <> msg

withContext :: [Utf8Builder] -> RIO App a -> RIO App a
withContext ctx = local (\app -> app {appLogContext = ctx})

runApp :: RIO App () -> IO ()
runApp inner = runSimpleApp $ do
  logFunc <- view logFuncL
  rooms <- newMVar emptyRooms
  let app = App {appLogFunc = logFunc, appLogContext = [], appRooms = rooms}
  runRIO app inner

getRoom :: Key -> RIO App (TVar Room)
getRoom key = do
  rooms <- view $ to appRooms
  modifyMVar rooms $ \rs ->
    case Map.lookup key rs of
      Just r -> return (rs, r)
      Nothing -> do
        info $ "creating new room: " <> display key
        r <- atomically $ newTVar emptyRoom
        return $ (Map.insert key r rs, r)

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
        roomEvents :: [(EventId, Event)], -- newest first
        roomNextEventId :: !EventId
      }

emptyRoom :: Room
emptyRoom = Room
  { roomConnections = [],
    roomNextConnectionId = 0,
    roomEvents = [],
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

-- | Events with ID greater or equal to the given, in ascending ID order.
eventsFrom :: Room -> EventId -> [(EventId, Event)]
eventsFrom room start = reverse $ takeWhile ((>= start) . fst) (roomEvents room)

addEvent :: Event -> Room -> (Room, EventId)
addEvent event room =
  let next = roomNextEventId room
   in ( room
          { roomEvents = (next, event) : roomEvents room,
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
                      debug $ "dropping closed connection"
                      return Nothing
                    _ -> throwIO e
                )

-- | Send the current event history down a connection, starting
-- at the given event ID. Returns the ID of the first unsent (future) event.
sendHistoryFrom :: WebSockets.Connection -> Room -> EventId -> RIO App EventId
sendHistoryFrom conn room start =
  liftIO $ do
    mapM_
      (WebSockets.sendTextData conn . encodeEvent)
      (eventsFrom room start)
    return $ roomNextEventId room

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

receive :: WebSockets.Connection -> TChan Text -> RIO App ()
receive conn chan = do
  msg <- liftIO $ WebSockets.receiveDataMessage conn
  case msg of
    m@(WebSockets.Text _ _) -> do
      atomically $ writeTChan chan $ WebSockets.fromDataMessage m
      receive conn chan
    WebSockets.Binary _ -> do
      debug "ignoring binary message"
      receive conn chan

game :: TVar Room -> WebHandler (RIO App)
game room = websocketsOr WebSockets.defaultConnectionOptions handleConn fallback
  where
    handleConn pendingConn = do
      conn <- liftIO $ WebSockets.acceptRequest pendingConn
      chan <- atomically newTChan
      debug $ "accepted connection"
      withPingThread conn 30 $
        race_ (handleMessages conn chan 0) (receive conn chan)
      debug $ "dropped connection" -- <> displayShow connId
    handleMessages conn chan last = do
      action <- atomically $ do
        r <- readTVar room
        sendUpdate conn r last
          <|> do
            msg <- readTChan chan
            return $ handleMessage msg >> return last
      last' <- action
      handleMessages conn chan last'
    handleMessage msg = do
      let event = Event {eventOperation = msg}
      eventId <- atomically $ do
        r <- readTVar room
        let (r', eventId) = addEvent event r
        writeTVar room $! r'
        return eventId
      debug $ "received operation " <> display eventId <> ": " <> display msg
    sendUpdate conn r last = do
      if last == roomNextEventId r
        then retrySTM
        else return $ sendHistoryFrom conn r last
    fallback _ respond = respond $ Wai.responseLBS status400 [] "not a websocket request"

main :: IO ()
main = runApp $ do
  info $ "listening on port 8787"
  run 8787 toplevel

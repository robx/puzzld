{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import App (App (..), debug, info, withContext)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WebSockets
import qualified Options.Applicative as Options
import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map
import Room (roomsPostHandler)
import State (Event (..), EventId, Key, Room (..), emptyRoom, emptyRooms)
import Web (WebHandler, run, sourceAddress, websocketsOr, withPingThread)

data Opts
  = Opts {optPort :: Int}

getOpts :: IO Opts
getOpts = Options.execParser parser
  where
    parser =
      Options.info
        ( Options.helper
            <*> ( Opts
                    <$> Options.option
                      Options.auto
                      ( Options.long "port" <> Options.short 'p' <> Options.value 8787
                          <> Options.metavar "PORT"
                          <> Options.help "listen port"
                      )
                )
        )
        (Options.fullDesc <> Options.progDesc "Websockets puzzle server." <> Options.header "puzzld")

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

encodeEvent :: (EventId, Event) -> BL.ByteString
encodeEvent (eventId, event) =
  Aeson.encode $
    Aeson.object
      ["id" .= Aeson.toJSON eventId, "operation" .= Aeson.toJSON (eventOperation event)]

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
    {-
    /game/key websocket
    
    
    POST /rooms
    /rooms/key
    
    /rooms/roomkey/connkey
    /rooms/roomkey/connkey
    -}
    ["rooms"] -> roomsPostHandler req respond
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

addEventT :: Event -> TVar Room -> RIO App ()
addEventT event room = do
  eventId <- atomically $ do
    r <- readTVar room
    let (r', eventId) = addEvent event r
    writeTVar room $! r'
    return eventId
  debug $ "received operation " <> display eventId <> ": " <> display (eventOperation event)

work :: WebSockets.Connection -> TChan Text -> TVar Room -> EventId -> RIO App ()
work conn chan room start = loop start
  where
    loop last = do
      action <- atomically $ do
        r <- readTVar room
        if last /= roomNextEventId r
          then return $ sendHistoryFrom conn r last
          else do
            msg <- readTChan chan
            return $ do
              let event = Event {eventOperation = msg}
              addEventT event room >> return last
      last' <- action
      loop last'

game :: TVar Room -> WebHandler (RIO App)
game room = websocketsOr WebSockets.defaultConnectionOptions handleConn fallback
  where
    handleConn pendingConn = do
      conn <- liftIO $ WebSockets.acceptRequest pendingConn
      chan <- atomically newTChan
      debug $ "accepted connection"
      withPingThread conn 30 $
        race_ (work conn chan room 0) (receive conn chan)
          `catch` ( \e -> case e of
                      WebSockets.CloseRequest _ _ -> return ()
                      WebSockets.ConnectionClosed -> return ()
                      _ -> throwIO e
                  )
      debug $ "dropped connection"
    fallback _ respond = respond $ Wai.responseLBS status400 [] "not a websocket request"

main :: IO ()
main = runApp $ do
  opts <- liftIO getOpts
  let port = optPort opts
  info $ "listening on port " <> display port
  run port toplevel

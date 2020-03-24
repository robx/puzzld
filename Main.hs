{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Exception (catch)
import Control.Monad (forever)
import Data.Maybe (catMaybes)
import Data.Text
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.WebSockets

type State = [Connection]

emptyState :: State
emptyState = []

addConnection :: Connection -> State -> State
addConnection conn = (conn :)

connections :: State -> [Connection]
connections = id

-- | Broadcast a message to all listeneres, dropping those
-- that aren't open anymore.
broadcastMessage :: WebSocketsData a => a -> State -> IO State
broadcastMessage msg state = do
  let conns = connections state
  catMaybes <$> mapM trySend conns
  where
    trySend :: Connection -> IO (Maybe Connection)
    trySend conn =
      (sendTextData conn msg >> return (Just conn))
        `catch` ( \ConnectionClosed -> do
                    putStrLn $ "dropping closed connection"
                    return Nothing
                )

app :: MVar State -> Application
app state req respond = do
  case pathInfo req of
    [] ->
      respond $
        responseLBS
          status200
          [("Content-Type", "text/plain")]
          "Hello, Web!"
    ("game" : _) -> game state req respond

game :: MVar State -> Application
game state = websocketsOr defaultConnectionOptions app backup
  where
    app :: ServerApp
    app pending_conn = do
      conn <- acceptRequest pending_conn
      modifyMVar_ state (pure . addConnection conn)
      putStrLn $ "accepted connection"
      forever (handleMessage conn)
    handleMessage conn = do
      msg <- receiveDataMessage conn
      case msg of
        Text b _ -> do
          putStrLn $ "received text message: " ++ show b
          modifyMVar_ state $ broadcastMessage b
        Binary _ -> putStrLn "ignoring binary message"
    backup :: Application
    backup _ respond = respond $ responseLBS status400 [] "not a websocket request"

main :: IO ()
main = do
  putStrLn $ "http://localhost:8080/"
  state <- newMVar emptyState
  run 8080 (app state)

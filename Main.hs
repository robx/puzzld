{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
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
      loop conn
    loop conn = do
      msg <- receiveDataMessage conn
      case msg of
        Text b _ -> do
          putStrLn $ "received text message: " ++ show b
          withMVar state $ mapM_ (\c -> sendTextData c b) . connections
        Binary _ -> putStrLn "ignoring binary message"
      putStrLn $ "looping"
      loop conn
    backup :: Application
    backup _ respond = respond $ responseLBS status400 [] "not a websocket request"

main :: IO ()
main = do
  putStrLn $ "http://localhost:8080/"
  state <- newMVar emptyState
  run 8080 (app state)

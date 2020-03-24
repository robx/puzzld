{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Network.WebSockets
import Data.Text
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets

type Conns = MVar [Connection]

app :: Conns -> Application
app conns req respond = do
    case pathInfo req of
      [] -> respond $ responseLBS
               status200
               [("Content-Type", "text/plain")]
               "Hello, Web!"
      ("game":_) -> game conns req respond

game :: Conns -> Application
game conns = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        cs <- takeMVar conns
        putMVar conns (conn:cs)
        putStrLn $ "accepted connection"
        loop conn

    loop conn = do
        msg <- receiveDataMessage conn
        case msg of
           Text b _ -> do putStrLn $ "received text message: " ++ show b
                          cs <- takeMVar conns
                          mapM_ (\ c -> sendTextData c b) cs
                          putMVar conns cs
           Binary _ -> putStrLn "received binary message"
        putStrLn $ "looping"
        loop conn

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    conns <- newMVar ([] :: [Connection])
    run 8080 (app conns)

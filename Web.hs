{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web
  ( WebHandler,
    run,
    WebsocketHandler,
    websocketsOr,
    withPingThread,
    sourceAddress,
    receiveDataMessageOrClosed,
  )
where

import Data.List (find)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Network.WebSockets as WebSockets
import RIO

type WebHandler m = Wai.Request -> (Wai.Response -> m Wai.ResponseReceived) -> m Wai.ResponseReceived

run :: MonadUnliftIO m => Warp.Port -> WebHandler m -> m ()
run port app = withRunInIO $ \runInIO ->
  Warp.run port (\req respond -> runInIO (app req (liftIO . respond)))

type WebsocketHandler m = WebSockets.PendingConnection -> m ()

websocketsOr ::
  MonadUnliftIO m =>
  WebSockets.ConnectionOptions ->
  WebsocketHandler m ->
  WebHandler m ->
  WebHandler m
websocketsOr connectionOptions app backup =
  \req respond -> withRunInIO $ \runInIO ->
    Wai.websocketsOr
      connectionOptions
      (\conn -> runInIO $ app conn)
      (\req1 respond1 -> runInIO $ backup req1 (liftIO . respond1))
      req
      (runInIO . respond)

withPingThread :: MonadUnliftIO m => WebSockets.Connection -> Int -> m a -> m a
withPingThread conn interval action = withRunInIO $ \runInIO ->
  WebSockets.withPingThread conn interval (return ()) (runInIO action)

receiveDataMessageOrClosed :: WebSockets.Connection -> IO (Maybe WebSockets.DataMessage)
receiveDataMessageOrClosed conn =
  ( do
      msg <- WebSockets.receiveDataMessage conn
      return $ Just msg
  )
    `catch` ( \e -> case e of
                WebSockets.CloseRequest _ _ -> return Nothing
                WebSockets.ConnectionClosed -> return Nothing
                _ -> throwIO e
            )

sourceAddress :: Wai.Request -> Utf8Builder
sourceAddress req = fromMaybe socketAddr headerAddr
  where
    headerAddr =
      displayBytesUtf8 . snd
        <$> find
          (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"])
          (Wai.requestHeaders req)
    socketAddr = displayShow . Wai.remoteHost $ req

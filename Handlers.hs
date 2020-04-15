{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers
  ( roomsPostHandler,
  )
where

import App (App)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import Network.HTTP.Types
import qualified Network.Wai as Wai
import RIO
import qualified RIO.Text as T
import System.Random (randomRIO)
import Web (WebHandler)

data RoomPost
  = RoomPost
      {pzvs :: [Text]}
  deriving (Generic, Show, ToJSON, FromJSON)

data PzvLinks
  = PzvLinks
      { pzv :: Text,
        player :: Text,
        spectator :: Text
      }
  deriving (Generic, Show, ToJSON, FromJSON)

type Key = Text

makeKey :: RIO App Key
makeKey = do
  T.pack <$> mapM (const (randomElement chars)) [1 .. len]
  where
    len = 6 :: Int
    randomElement l = do
      i <- liftIO $ randomRIO (0, length l - 1)
      return $ l !! i
    chars = "abcdefghjkmnpqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

data RoomResult
  = RoomResult
      {links :: [PzvLinks]}
  deriving (Generic, Show, ToJSON, FromJSON)

acceptRoomPost :: RoomPost -> RIO App RoomResult
acceptRoomPost rp = do
  roomKey <- makeKey
  ls <- mapM (toLinks roomKey) (pzvs rp)
  return $
    RoomResult
      { links = ls
      }
  where
    toLinks :: Key -> Text -> RIO App PzvLinks
    toLinks roomKey p = do
      playerKey <- makeKey
      spectatorKey <- makeKey
      return $ PzvLinks
        { pzv = p,
          player = "room" <> "/" <> roomKey <> "/" <> playerKey,
          spectator = "room" <> "/" <> roomKey <> "/" <> spectatorKey
        }

roomsPostHandler :: WebHandler (RIO App)
roomsPostHandler = \req respond ->
  case Wai.requestMethod req of
    "POST" -> do
      body <- liftIO $ Wai.strictRequestBody req
      case Aeson.decode body of
        Just roomPost -> do
          res <- acceptRoomPost roomPost
          respond $ Wai.responseLBS status200 [] $ Aeson.encode res
        Nothing -> respond $ Wai.responseLBS status400 [] "bad user"
    _ -> respond $ Wai.responseLBS status405 [] "invalid method"

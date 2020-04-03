{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Room
  ( roomsPostHandler,
  )
where

import App (App)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import Network.HTTP.Types
import qualified Network.Wai as Wai
import RIO
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

data RoomResult
  = RoomResult
      {links :: [PzvLinks]}
  deriving (Generic, Show, ToJSON, FromJSON)

acceptRoomPost :: RoomPost -> RoomResult
acceptRoomPost rp = RoomResult
  { links = map toLinks (pzvs rp)
  }
  where
    toLinks p = PzvLinks {pzv = p, player = "", spectator = ""}

roomsPostHandler :: WebHandler (RIO App)
roomsPostHandler = \req respond ->
  case Wai.requestMethod req of
    "POST" -> do
      body <- liftIO $ Wai.strictRequestBody req
      case Aeson.decode body of
        Just roomPost -> do
          let res = acceptRoomPost roomPost
          respond $ Wai.responseLBS status200 [] $ Aeson.encode res
        Nothing -> respond $ Wai.responseLBS status400 [] "bad user"
    _ -> respond $ Wai.responseLBS status405 [] "invalid method"

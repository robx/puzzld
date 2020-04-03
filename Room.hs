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

roomsPostHandler :: WebHandler (RIO App)
roomsPostHandler = \_req respond ->
  let res = RoomResult
        { links =
            [ PzvLinks "a" "b" "c"
            ]
        }
   in respond $ Wai.responseLBS status200 [] $ Aeson.encode res

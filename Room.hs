{-# LANGUAGE OverloadedStrings #-}

module Room
  ( roomsPostHandler,
  )
where

import App (App)
import Network.HTTP.Types
import qualified Network.Wai as Wai
import RIO
import Web (WebHandler)

{-
data RoomPost = RoomPost
  { pzvs :: [Text] }

data PzvLinks = PzvLinks
  { pzv :: Text
  , player :: Text
  , spectator :: Text
  }

data RoomResult = RoomResult
  { links :: [PzvLinks] }
-}

roomsPostHandler :: WebHandler (RIO App)
roomsPostHandler = \_req respond ->
  respond $ Wai.responseLBS status400 [] "not a websocket request"

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers
  ( roomsPostHandler,
  )
where

import App (App (..), info)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import Network.HTTP.Types
import qualified Network.Wai as Wai
import RIO
import qualified RIO.Map as Map
import qualified RIO.Text as T
import State (GameSlot (..), Key, Room, newRoom)
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

registerRoom :: Room -> RIO App Key
registerRoom room = do
  rooms <- view $ to appRooms
  modifyMVar rooms go
  where
    go rs = do
      key <- makeKey
      case Map.lookup key rs of
        Just _ -> go rs
        Nothing -> do
          info $ "creating new room: " <> display key
          r <- atomically $ newTVar room
          return $ (Map.insert key r rs, key)

acceptRoomPost :: RoomPost -> RIO App RoomResult
acceptRoomPost rp = do
  keys <- mapM makeKeys (pzvs rp)
  let room = newRoom $ Map.fromList $ concatMap toSlots keys
  roomKey <- registerRoom room
  return $
    RoomResult
      { links = map (toLinks roomKey) keys
      }
  where
    makeKeys :: Text -> RIO App (Text, Key, Key)
    makeKeys p = do
      playerKey <- makeKey
      spectatorKey <- makeKey
      return (p, playerKey, spectatorKey)
    toSlots :: (Text, Key, Key) -> [(Key, GameSlot)]
    toSlots (p, playerKey, spectatorKey) =
      [ (playerKey, GameSlot {slotPzv = p, slotRw = True}),
        (spectatorKey, GameSlot {slotPzv = p, slotRw = False})
      ]
    toLinks :: Key -> (Text, Key, Key) -> PzvLinks
    toLinks roomKey (p, playerKey, spectatorKey) =
      PzvLinks
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

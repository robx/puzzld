{-# LANGUAGE OverloadedStrings #-}

module App
  ( App (..),
    info,
    debug,
    withContext,
  )
where

import RIO
import State (Rooms)

data App
  = App
      { appLogFunc :: !LogFunc,
        appLogContext :: [Utf8Builder],
        appRooms :: !(MVar Rooms)
      }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

info :: Utf8Builder -> RIO App ()
info msg = do
  logContext <- view $ to appLogContext
  logInfo $ mconcat (map (\c -> c <> ": ") logContext) <> msg

debug :: Utf8Builder -> RIO App ()
debug msg = do
  logContext <- view $ to appLogContext
  logDebug $ mconcat (map (\c -> c <> ": ") logContext) <> msg

withContext :: [Utf8Builder] -> RIO App a -> RIO App a
withContext ctx = local (\app -> app {appLogContext = ctx})

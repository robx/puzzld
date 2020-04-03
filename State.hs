module State
  ( EventId,
    Event (..),
    Key,
    Room (..),
    emptyRoom,
    Rooms,
    emptyRooms,
  )
where

import RIO
import qualified RIO.Map as Map

type EventId = Int

data Event
  = Event
      { eventOperation :: Text
      }

data Room
  = Room
      { roomEvents :: [(EventId, Event)], -- newest first
        roomNextEventId :: !EventId
      }

emptyRoom :: Room
emptyRoom = Room
  { roomEvents = [],
    roomNextEventId = 0
  }

type Key = Text

type Rooms = Map Key (TVar Room)

emptyRooms :: Rooms
emptyRooms = Map.empty

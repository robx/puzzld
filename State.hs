module State
  ( EventId,
    Event (..),
    Key,
    Room (..),
    emptyRoom,
    newRoom,
    Rooms,
    emptyRooms,
    GameSlot (..),
  )
where

import RIO
import qualified RIO.Map as Map

type EventId = Int

data Event
  = Event
      { eventOperation :: Text
      }

data GameSlot
  = GameSlot
      { slotPzv :: Text,
        slotRw :: Bool
      }

data Room
  = Room
      { roomEvents :: [(EventId, Event)], -- newest first
        roomNextEventId :: !EventId,
        roomSlots :: Map.Map Key GameSlot
      }

emptyRoom :: Room
emptyRoom = Room
  { roomEvents = [],
    roomNextEventId = 0,
    roomSlots = Map.empty
  }

newRoom :: Map.Map Key GameSlot -> Room
newRoom slots = emptyRoom {roomSlots = slots}

type Key = Text

type Rooms = Map Key (TVar Room)

emptyRooms :: Rooms
emptyRooms = Map.empty

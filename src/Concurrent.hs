module Concurrent where

import Data.Set (Set)
import qualified Data.Set as S
import FRP.Rhine.Gloss
import Linear (V2 (..))
import Witch (into)

-- | User input accumulated from Gloss events.
data UserInput = UserInput
  { mouse :: V2 Double
  , keys :: Set Key
  , events :: [Event]
  }

noInput :: UserInput
noInput = UserInput {mouse = 0, keys = mempty, events = []}

handle :: [Event] -> UserInput -> UserInput
handle es ui =
  foldr applyEvent ui {events = es} es
  where
    applyEvent (EventKey key upOrDown _ _) u =
      u
        { keys =
            if upOrDown == Down
              then S.insert key (keys u)
              else S.delete key (keys u)
        }
    applyEvent (EventMotion (x, y)) u =
      u {mouse = V2 (into @Double x) (into @Double y)}
    applyEvent _ u = u

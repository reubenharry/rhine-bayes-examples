-- | Browser-facing input. Deliberately simpler than Gloss Event/Key for the spike:
-- mouse position in canvas coords (origin at centre, y up) plus named buttons.
module UserInput where

import Data.Set (Set)
import qualified Data.Set as Set

data UserInput = UserInput
  { mouseX :: Float
  , mouseY :: Float
  , buttonsDown :: Set String
  }
  deriving (Eq, Show)

noInput :: UserInput
noInput = UserInput 0 0 Set.empty

buttonDown :: String -> UserInput -> Bool
buttonDown name ui = Set.member name (buttonsDown ui)

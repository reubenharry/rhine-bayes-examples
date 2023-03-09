
module Concurrent where

import Control.Lens
import Control.Monad (forever)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import FRP.Rhine.Gloss
import Util
import Linear (V2 (..))
import Witch (into)
import GHC.Generics (Generic)
import Data.Generics.Product (the)

-- a data type to represent user input
data UserInput = UserInput
  { _mouse :: V2 Double,
    _keys :: Set Key,
    _events :: [Event]
  } deriving Generic


-- a lofi converter from keyboard input to a string of characters
getTextFromGloss :: SignalFunction Deterministic UserInput (Maybe Text)
getTextFromGloss = safely foo
  where
    foo = forever $ do
      try proc userInput -> do
        chars <- getCharsFromGloss -< userInput
        let predicateUp = any (\case (EventKey (SpecialKey KeyEnter) Up _ _) -> True; _ -> False)
        let predicateDown = any (\case (EventKey (SpecialKey KeyEnter) Down _ _) -> True; _ -> False)
        throwOn' -< (predicateUp $ userInput ^. the @[Event], ())
        returnA
          -<
            if predicateDown $ userInput ^. the @[Event]
              then Just $ T.concat $ map (\case Char c -> T.singleton c; _ -> "") chars
              else Nothing
      step (const $ pure (Nothing, ()))

getCharsFromGloss :: SignalFunction Deterministic UserInput [Key]
getCharsFromGloss = feedback mempty proc (userInput, oldText) -> do
  let newLetters = (userInput ^. the @(Set Key)) \\ S.fromList oldText
  let newText = oldText <> S.toList newLetters
  returnA -< (newText, newText)

noInput :: UserInput
noInput = UserInput {_mouse = 0, _keys = mempty, _events = []}

handle :: [Event] -> UserInput -> UserInput
handle es =
  (the @[Event] .~ es)
    . ( foldr (.) id . fmap \case
          (EventKey key upOrDown _ _) -> (the @(Set Key) . contains key .~ (upOrDown == Down))
          (EventMotion (x, y)) -> the @(V2 Double) .~ V2 (into @Double x) (into @Double y)
          _ -> id
      )
      es

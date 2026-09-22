module Example where

import Data.Foldable (Foldable (fold))
import FRP.Rhine hiding (Result)
import FRP.Rhine.Gloss hiding (Result)
import GHC.Float (double2Float)
import Linear (V2 (..))
import Numeric.Log (Log (ln))
import Witch (into)

type Observation = V2 Double
type Position = V2 Double

renderObjects :: (Monad m) => Automaton m Result Picture
renderObjects = proc Result {particles, measured, latent} -> do
  observation <- drawBall -< (measured, 0.05, red)
  ball <- drawBall -< (latent, 0.15, makeColorI 255 239 0 255)
  parts <- fold <$> parallely drawParticle -< particles
  returnA -< observation <> ball <> parts

drawBall :: (Monad m) => Automaton m (V2 Double, Double, Color) Picture
drawBall = proc (V2 x y, width, theColor) -> do
  returnA
    -<
      scale 150 150 $
        translate (double2Float x) (double2Float y) $
          color theColor $
            circleSolid $
              into @Float width

drawParticle :: (Monad m) => Automaton m (Position, Log Double) Picture
drawParticle = proc (position, probability) -> do
  drawBall
    -<
      ( position
      , 0.1
      , withAlpha (double2Float $ exp $ 0.1 * ln probability) violet
      )

data Result = Result
  { measured :: Observation
  , latent :: Position
  , particles :: [(Position, Log Double)]
  }
  deriving (Show)

{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}

module Example where

import Control.Arrow (arr)
import Data.Automaton (Automaton)
import Data.Foldable (foldMap')
import Picture
import Util (V2 (..))

type Observation = V2 Double
type Position = V2 Double

data Result = Result
  { measured :: Observation
  , latent :: Position
  , particles :: [(Position, Double)]
  }

renderObjects :: (Monad m) => Automaton m Result Picture
renderObjects = arr \Result {particles, measured, latent} ->
  drawBallPic measured 0.05 red
    <> drawBallPic latent 0.15 (makeColorI 255 239 0 255)
    <> foldMap' drawParticleStatic particles

drawBallPic :: V2 Double -> Double -> Color -> Picture
drawBallPic (V2 x y) width theColor =
  scale 150 150 $
    translate (realToFrac x) (realToFrac y) $
      color theColor $
        circleSolid (realToFrac width)

drawParticleStatic :: (Position, Double) -> Picture
drawParticleStatic (position, logW) =
  drawBallPic position 0.1 (withAlpha (realToFrac $ exp (0.1 * logW)) violet)

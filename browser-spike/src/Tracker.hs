{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Minimal particle tracker — stepping stone toward Convention.main.
module Tracker where

import BayesLite
import Control.Arrow
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Foldable (fold)
import Data.MonadicStreamFunction
import Inference
import Picture
import RhineCompat
import UserInput
import Util

std0 :: Double
std0 = 1

type Observation = V2 Double
type Position = V2 Double

prior :: () >--> Position
prior = proc _ -> do
  x <- brownianMotion1D -< ()
  y <- brownianMotion1D -< ()
  returnA -< V2 x y

brownianMotion1D :: () >--> Double
brownianMotion1D = proc _ -> do
  dacceleration <- constM (normal 0 8) -< ()
  acceleration <- decayingIntegral 1 -< dacceleration
  velocity <- decayingIntegral 1 -< acceleration
  position <- decayingIntegral 1 -< velocity
  returnA -< position

data Result = Result
  { measured :: Observation
  , latent :: Position
  , particles :: [(Position, Double)]
  }

drawBall :: (Monad m) => MSF m (V2 Double, Double, Color) Picture
drawBall = proc (V2 x y, width, theColor) ->
  returnA
    -<
      scale 150 150 $
        translate (realToFrac x) (realToFrac y) $
          color theColor $
            circleSolid (realToFrac width)

drawParticle :: (Monad m) => MSF m (Position, Double) Picture
drawParticle = proc (position, logW) ->
  drawBall
    -<
      (position, 0.1, withAlpha (realToFrac $ exp (0.1 * logW)) violet)

renderObjects :: (Monad m) => MSF m Result Picture
renderObjects = proc Result {particles, measured, latent} -> do
  observation <- drawBall -< (measured, 0.05, red)
  ball <- drawBall -< (latent, 0.15, makeColorI 255 239 0 255)
  parts <- fold <$> mapMSF drawParticle -< particles
  returnA -< observation <> ball <> parts

tracker :: UserInput >--> Picture
tracker = proc ui -> do
  let showParticles = not (buttonDown "pulse" ui)
      noiseStd = max 0.05 (1.5 + realToFrac (mouseX ui) / 320)
  actualPosition <- prior -< ()
  measuredPosition <- observationModelStd -< (actualPosition, noiseStd)
  samples <- particleFilter params {n = 40} (posteriorStd noiseStd) -< measuredPosition
  pic <-
    renderObjects
      -<
        Result
          { measured = measuredPosition
          , latent = actualPosition
          , particles = if showParticles then samples else []
          }
  let label =
        translate (-300) 200 $
          scale 0.15 0.15 $
            color white $
              text "mouse X: noise   Pulse: hide particles"
  returnA -< pic <> label

observationModelStd :: SignalFunction Stochastic (Position, Double) Observation
observationModelStd = proc (p, s) -> do
  xNoise <- arrM (normal 0) -< s
  yNoise <- arrM (normal 0) -< s
  returnA -< p + V2 xNoise yNoise

posteriorStd :: Double -> Observation >-/-> Position
posteriorStd s = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- prior -< ()
  observe -< normalPdf oY s trueY + normalPdf oX s trueX
  returnA -< latent

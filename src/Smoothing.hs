module Smoothing where

import Concurrent (UserInput)
import Control.Monad.Bayes.Class (MonadDistribution (normal), normalPdf)
import Data.MonadicStreamFunction
import Example
  ( Observation,
    Position,
    Result (Result, latent, measured, particles),
    renderObjects,
    stochasticOscillator,
  )
import FRP.Rhine.Gloss.Common (Picture)
import GHC.Float ()
import GUI (slider)
import Inference
import Linear (V2 (..))
import Util 

past :: SignalFunction Stochastic UserInput Picture
past = proc userInput -> do
  (sliderPic, r) <- slider (V2 (-400) 400) 60 -< userInput
  actualPosition <- prior -< ()
  thePast <- shiftBy -< (actualPosition, floor $ r * 100 + 1)
  measuredPosition <- observationModel -< actualPosition
  samples <- particleFilter params (posterior *** returnA >>> shiftBy) -< (measuredPosition, floor $ r * 100 + 1)
  pic <-
    renderObjects
      -<
        Result
          { particles = samples,
            measured = thePast,
            latent = actualPosition
          }
  pic2 <-
    renderObjects
      -<
        Result
          { particles = [],
            measured = measuredPosition,
            latent = 1000
          }
  returnA -< pic <> sliderPic <> pic2

std :: Double
std = 0.5

prior :: SignalFunction Stochastic () Position
prior = proc () -> do
  x <- stochasticOscillator 0 2 -< 1
  y <- stochasticOscillator 2 0 -< 1
  returnA -< V2 x y

posterior :: SignalFunction (Stochastic & Unnormalized) Observation Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- prior -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
  (x, y) <- (noise &&& noise) -< ()
  returnA -< p + V2 x y
  where
    noise = constM (normal 0 0.1)

shift :: Monad m => Int -> MSF m c c
shift n = accumulateWith (\x xs -> take n $ x : xs) [] >>> arr last

-- shiftBy :: Monad m => Int -> MSF m c c
shiftBy :: Monad m => MSF m (c, Int) c
shiftBy = accumulateWith (\(x, n) xs -> take n $ x : xs) [] >>> arr last
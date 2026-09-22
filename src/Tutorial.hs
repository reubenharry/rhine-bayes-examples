module Tutorial where

import Concurrent
import Control.Category ((.))
import Control.Monad.Bayes.Class (MonadDistribution (normal))
import Example (Result (Result, latent, measured, particles), renderObjects)
import FRP.Rhine.Gloss hiding (Result)
import Inference (params, particleFilter)
import Linear (V2 (..))
import Numeric.Log (Log)
import Prelude hiding ((.))
import Util

-----------------------
-- first example
-----------------------

demo1 :: UserInput >--> Picture
demo1 = proc _ -> do
  latent <- worldModel -< ()
  observations <- observationModel -< latent
  beliefAboutState <- inferredPosterior -< observations

  picture <-
    renderObjects
      -<
        Result
          { measured = observations
          , latent = latent
          , particles = beliefAboutState
          }

  returnA -< picture

type Observation = V2 Double
type State = V2 Double

whiteNoise :: () >--> Double
whiteNoise = constM (normal 0 1)

particlePosition1D :: () >--> Double
particlePosition1D = decayingIntegral 1 . decayingIntegral 1 . whiteNoise

worldModel :: () >--> State
worldModel = proc _ -> do
  xPos <- particlePosition1D -< ()
  yPos <- particlePosition1D -< ()
  returnA -< V2 xPos yPos

observationModel :: State >--> Observation
observationModel = proc latent -> do
  xNoise <- whiteNoise -< ()
  yNoise <- whiteNoise -< ()
  returnA -< latent + V2 xNoise yNoise

posteriorDistribution :: Observation >-/-> State
posteriorDistribution = proc obs -> do
  latent <- worldModel -< ()
  observe -< normalPdf2D latent 1 obs
  returnA -< latent

inferredPosterior :: Observation >--> [(State, Log Double)]
inferredPosterior = particleFilter params posteriorDistribution

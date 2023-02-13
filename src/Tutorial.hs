module Tutorial where

import Concurrent
import Control.Monad.Bayes.Class (MonadDistribution (normal))
import Example (Result (Result, latent, measured, particles), decayingIntegral, renderObjects)
import FRP.Rhine.Gloss
import Inference (params, particleFilter, SMCSettings (n))
import Linear (V2 (..))
import Numeric.Log (Log)
import Util
import Control.Lens

whiteNoise :: SignalFunction Stochastic () Double
whiteNoise = constM (normal 0 1)

particlePosition :: SignalFunction Stochastic () Double
particlePosition = whiteNoise >>> decayingIntegral 1 >>> decayingIntegral 1

particlePosition2d :: SignalFunction Stochastic () (V2 Double)
particlePosition2d = proc _ -> do
  xPos <- particlePosition -< ()
  yPos <- particlePosition -< ()
  returnA -< V2 xPos yPos

observationModel :: SignalFunction Stochastic (V2 Double) (V2 Double)
observationModel = proc latent -> do
  xNoise <- whiteNoise -< ()
  yNoise <- whiteNoise -< ()
  returnA -< latent + V2 xNoise yNoise

-- note: the implicit thing is confusing, can I write a direct version?
posteriorDistribution :: SignalFunction (Stochastic & Unnormalized) (V2 Double) (V2 Double)
posteriorDistribution = proc obs -> do
  latent <- particlePosition2d -< ()
  _ <- observe -< normalPdf2D obs 1 latent
  returnA -< latent

inferredPosteriorDistribution :: SignalFunction Stochastic (V2 Double) [(V2 Double, Log Double)]
inferredPosteriorDistribution = particleFilter params posteriorDistribution

demo1 :: SignalFunction Stochastic UserInput Picture
demo1 = proc userInput -> do
  let latent = userInput ^. mouse 
  observations <- observationModel -< latent
  beliefAboutState <- particleFilter params{n=200} posteriorDistribution -< observations

  renderObjects -< Result {measured = observations, latent = latent, particles = beliefAboutState}

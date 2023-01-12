{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE DataKinds #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LiberalTypeSynonyms #-}


module Triangular where
import qualified Example
import Control.Arrow
import Data.Fixed (mod')
import Inference
import Data.MonadicStreamFunction
import Control.Monad.Bayes.Class (MonadSample(..), normalPdf)
import Example (decayingIntegral)
import Circular (angle', Angle (Angle), mkAngle)
import Linear.V2 (unangle, V2(..))
import Linear (angle)
import Example (Position)
import Graphics.Gloss
import Witch (into)
import Data.Text (Text)
import Debug.Trace (traceM)
import FRP.Rhine (integral)
import Example (Observation)
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Foldable (Foldable(..))
import FRP.Rhine.ClSF (BehaviourF)
import FRP.Rhine.Gloss (GlossConcT)
import Control.Monad.IO.Class (MonadIO)
import Numeric.Log (Log (ln))
import Concurrent (GlossInput, keys)
import Control.Lens ((^.), Contains (contains))
import Graphics.Gloss.Interface.Pure.Game (Key(..))

-- walk1D :: SignalFunction Stochastic () Angle
-- walk1D = proc _ -> do
--     dacceleration <- constM (normal 0 4 ) -< ()
--     acceleration <- decayingIntegral 1 -< dacceleration `mod'` (2 * pi)
--     velocity <- decayingIntegral 1 -< acceleration `mod'` (2 * pi) -- Integral, dying off exponentially
--     position <- decayingIntegral 1 -< velocity `mod'` (2 * pi)
--     returnA -< Angle $ position `mod'` (2 * pi)

prior :: SignalFunction Stochastic GlossInput (Position, Angle)
prior = proc glossInput -> do
    -- let direction = mkAngle 1
    -- direction <- mkAngle . (4 *) <$> Example.stochasticOscillator 0 1 -< 0.5
    direction <- mkAngle <$> Example.walk1D -< ()
    let velocity = if glossInput ^. keys . contains (Char 'f') then 1 else 0
    -- dacceleration <- constM (normal 0 8 ) -< ()
    -- acceleration <- decayingIntegral 1 -< dacceleration
    -- velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
    position <-  integral -< (V2 velocity velocity) * angle' direction
    arrM traceM -< show $ angle' direction
    returnA -< (position, direction)

std :: Double
std = 0.5

observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
    (x,y) <- (noise &&& noise) -< ()
    returnA -< p + V2 x y
    where noise = constM (normal 0 std)

posterior :: SignalFunction (Stochastic & Unnormalized) (GlossInput, Observation) (Position, Angle)
posterior = proc (glossInput, V2 oX oY) -> do
  latent@(V2 trueX trueY, _) <- prior -< glossInput
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent


drawTriangle (V2 p1 p2, Angle dir, size, color) = Color color
    $ scale 50 50
    $ translate (into @Float p1) (into @Float p2)
    $ rotate (- (360 / (2 * pi)) * into @Float dir)
    $ scale size size
    $ polygon [(0,0.5), (0, -0.5), (2, 0), (0,0.5)]

drawBall :: Monad m => MSF m (V2 Double, Double, Color) Picture
drawBall = proc (V2 x y, width, theColor) -> do
    returnA -<
        scale 50 50 $
        translate (into @Float x) (into @Float y) $
        color theColor $
        circleSolid $
        into @Float width

gloss :: SignalFunction Stochastic GlossInput Picture
gloss = proc glossInput -> do
    (pos, ang) <- prior -< glossInput
    observation <- observationModel -< pos
    trueObject <- arr drawTriangle -< (pos, ang, 1, yellow)
    particles <- particleFilter params {n = 50} posterior -< (glossInput, observation)
    particlePictures <- fold <$> mapMSF drawParticle  -< particles
    observationPicture <- drawBall -< (observation, 0.03, red)
    returnA -< trueObject <> particlePictures <> observationPicture



drawParticle :: SignalFunction Deterministic ((Position, Angle), Log Double) Picture
drawParticle = proc ((position, angle), probability) -> do
  arr drawTriangle -< (position, angle, 0.35, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet)

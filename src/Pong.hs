{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Pong where
import FRP.Rhine.Gloss hiding (Up, Down)
import Prelude hiding (until)
import qualified Example
import qualified Control.Monad.Morph as MM
import Linear.V2 (V2(..), _x, _y)
import Control.Monad.Bayes.Class
import Numeric.Log
import Control.Monad.Bayes.Sampler
import Control.Monad.Morph
import Inference hiding (V2)
import Control.Monad.Bayes.Population (resampleMultinomial)
import Active (averageOf)
import Witch (into)
import Control.Lens
import Control.Monad (forever)
import qualified Control.Monad.Trans.State as S
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import Example (drawBall')
import Data.Text (Text)


-- bouncing

std :: Double
std = 0.3


data State = State {
    _ballPos :: V2 Double,
    _barPos :: Double
    } deriving Show

-- a simple macro, not important
$(makeLenses ''State)


type Observation = V2 Double
type Action = Double
type Particles = [(State, Log Double)]
data Direction = Up | Down


-- "Process Deterministic x y" is the type of signal functions 
-- where the input signal is (Time -> x)
-- and the output signal is (Time -> y)
-- So: (Time -> x) -> (Time -> y)
-- THIS IS NOT THE SAME AS A TIME-VARYING FUNCTION!! Time -> (x -> y)
-- "Process Stochastic x y" then is (effectively): (Time -> x) -> Distribution (Time -> y)



prior :: Process Stochastic Action State
prior = safely $ S.evalStateT loop (State (V2 0 (-2)) 0) where

    loop = forever do
        initialPosition <- S.get
        stateAtTimeofUpCollision <- until collisionUp (movement initialPosition Up)
        stateAtTimeofDownCollision <- until collisionDown (movement stateAtTimeofUpCollision Down)
        S.put stateAtTimeofDownCollision


    collisionUp (State (V2 x y) bar) = y > 2 || (y < 0 && y > -0.1) && abs (x - bar) < 0.2
    collisionDown ((State (V2 x y) bar)) = negate y > 2 || (y > 0 && y < 0.1) && abs (x - bar) < 0.2

    movement :: State -> Direction -> Process Stochastic Action State
    movement state dir = proc action -> do
        barPosition <- iPre (state ^. barPos) -< action
        ballPosX <- walk1D -< ()
        ballPosY <- (case dir of Up -> id; Down -> negate) <$> time -< ()
        returnA -< (state
            & set barPos barPosition
            & over ballPos (+ V2 ballPosX ballPosY) )



    walk1D :: Process Stochastic () Double
    walk1D = proc _ -> do
        dacceleration <- constM (normal 0 8 ) -< ()
        acceleration <- decayIntegral 1 -< dacceleration
        velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
        position <- decayIntegral 1 -< velocity
        returnA -< position

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

until cond signal = lift $ try proc input -> do
    output <- signal -< input
    _ <- throwOn' -< (cond output, output)
    returnA -< output

observationModel :: Process Stochastic State Observation
observationModel = proc state -> do
    (n1, n2) <- noise &&& noise -< ()
    returnA -< state ^. ballPos + V2 n1 n2

    where
        noise :: Process Stochastic () Double
        noise = constM (normal 0 std)


posterior :: Process (Stochastic & Unnormalized) (Observation, Action) State
posterior = proc (V2 oX oY, action) -> do
  pred <- prior -< action
  observe -< normalPdf (pred ^. ballPos . _x) std oX  * normalPdf (pred ^. ballPos . _y) std oY
  returnA -< pred

control :: Process Stochastic Particles Action
control = arr (averageOf . map (first (view _x . _ballPos)))



mainSignal :: SignalFunction (Stochastic & Feedback) Text Picture -- (State, Particles)
mainSignal = proc _ -> do
    rec
        state <- prior -< action
        observations <- observationModel -< state
        particles <- particleFilter params {n = 150} posterior -< (observations, action)
        action <- control -< particles

    -- returnA -< (state, particles)
    visualisation -< Result {
                                particles = particles
                                , measured = 0
                                , latent = state
                                , bar = state ^. barPos
                                }


visualisation :: Monad m => MSF m Result Picture
visualisation = proc Result { particles, latent, bar} -> do

  parts <- fold <$> mapMSF drawParticle -< first _ballPos <$> particles
  ball <- drawBall' -< (_ballPos latent, 0.1, withAlpha 0.5 green)

  let barPos = translate (into @Float (bar * 150)) 0 $ polygon [(-30, -5), (-30, 5), (30, 5), (30, -5), (-30, -5)]
  returnA -< (parts <> ball <> barPos)


drawParticle :: Monad m => MSF m (V2 Double, Log Double) Picture
drawParticle = proc (position, probability) -> do
  drawBall' -< (position, 0.1, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet)

drawParticles :: MonadIO m => BehaviourF (GlossConcT m) td [(V2 Double, Log Double)] ()
drawParticles = proc particles -> do
  case particles of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps

data Result = Result
  {
   measured :: Observation
  , latent :: State
  , particles :: [(State, Log Double)]
  , bar :: Double
  }
  deriving Show



--  proc y -> do
--     x <- arr (averageOf . fmap (first ((\(V2 x y) -> x) . _ballPos))) -< y
--     n <- count -< ()
--     -- x' <- arr (sin) -< (n/10)
--     returnA -< x -- (3*x') + x

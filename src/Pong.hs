
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
import Control.Monad.Bayes.Class hiding (posterior, prior)
import Numeric.Log
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.Morph
import Inference hiding (V2)
import Control.Monad.Bayes.Population (resampleMultinomial)
import Util hiding (Particles)
import Witch (into)
import Control.Lens
import Control.Monad (forever)
import qualified Control.Monad.Trans.State as S
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import Example (drawBall)
import Data.Text (Text)
import Concurrent (UserInput)
import GUI (slider)


-- bouncing


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


-- "SignalFunction Deterministic x y" is the type of signal functions 
-- where the input signal is (Time -> x)
-- and the output signal is (Time -> y)
-- So: (Time -> x) -> (Time -> y)
-- THIS IS NOT THE SAME AS A TIME-VARYING FUNCTION!! Time -> (x -> y)
-- "SignalFunction Stochastic x y" then is (effectively): (Time -> x) -> Distribution (Time -> y)



prior :: SignalFunction Stochastic Action State
prior = safely $ S.evalStateT loop (State (V2 0 (-2)) 0) where

    loop = forever do
        initialPosition <- S.get
        stateAtTimeofUpCollision <- until collisionUp (movement initialPosition Up)
        stateAtTimeofDownCollision <- until collisionDown (movement stateAtTimeofUpCollision Down)
        S.put stateAtTimeofDownCollision


    collisionUp (State (V2 x y) bar) = y > 2 || (y < 0 && y > -0.1) && abs (x - bar) < 0.2
    collisionDown ((State (V2 x y) bar)) = negate y > 2 || (y > 0 && y < 0.1) && abs (x - bar) < 0.2

    movement :: State -> Direction -> SignalFunction Stochastic Action State
    movement state dir = proc action -> do
        barPosition <- iPre (state ^. barPos) -< action
        ballPosX <- walk1D -< ()
        ballPosY <- (case dir of Up -> id; Down -> negate) <$> time -< ()
        returnA -< (state
            & set barPos barPosition
            & over ballPos (+ V2 ballPosX ballPosY) )



    walk1D :: SignalFunction Stochastic () Double
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

observationModel :: SignalFunction Stochastic (State, Double) Observation
observationModel = proc (state, std) -> do
    (n1, n2) <- noise &&& noise -< std
    returnA -< state ^. ballPos + V2 n1 n2

    where
        noise :: SignalFunction Stochastic Double Double
        noise = arrM (normal 0)


posterior :: SignalFunction (Stochastic & Unnormalized) (Observation, Action, Double) State
posterior = proc (V2 oX oY, action, std) -> do
  pred <- prior -< action
  observe -< normalPdf (pred ^. ballPos . _x) std oX  * normalPdf (pred ^. ballPos . _y) std oY
  returnA -< pred

control :: SignalFunction Stochastic Particles Action
control = arr (averageOf . map (first (view _x . _ballPos)))



mainSignal :: SignalFunction (Stochastic & Feedback) UserInput Picture -- (State, Particles)
mainSignal = proc userInput -> do
    (sliderPic, r) <- slider (V2 (-400) 300) 60 -< userInput
    let std = 4 * r + 0.01
    rec
        state <- prior -< action
        observations <- observationModel -< (state, std)
        particles <- particleFilter params {n = 150} posterior -< (observations, action, std)
        action <- control -< particles

    -- returnA -< (state, particles)
    vis <- visualisation -< Result {
                                particles = particles
                                , measured = observations
                                , latent = state
                                , bar = state ^. barPos
                                }
    returnA -< vis <> sliderPic


visualisation :: Monad m => MSF m Result Picture
visualisation = proc Result { particles, latent, bar, measured} -> do

  parts <- fold <$> mapMSF drawParticle -< first _ballPos <$> particles
  ball <- drawBall -< (_ballPos latent, 0.1, green)

  let barPos = translate (into @Float (bar * 150)) 0 $ polygon [(-30, -5), (-30, 5), (30, 5), (30, -5), (-30, -5)]
  obsBall <- drawBall -< (measured, 0.05, red)

  returnA -< (parts <> ball <> barPos <> obsBall)


drawParticle :: Monad m => MSF m (V2 Double, Log Double) Picture
drawParticle = proc (position, probability) -> do
  drawBall -< (position, 0.1, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet)

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

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TupleSections #-}



{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
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
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}

module MutualStoch where

import FRP.Rhine.Gloss hiding (Up, Down)
import Prelude hiding (until)
import qualified Example hiding (renderObjects)
import qualified Control.Monad.Morph as MM
import Linear.V2 (V2(..), _x, _y, unangle)
import Control.Monad.Bayes.Class
import Numeric.Log
import Control.Monad.Bayes.Sampler
import Control.Monad.Morph
import Inference hiding (V2)
import Control.Monad.Bayes.Population (resampleMultinomial)
import Active (averageOf)
import Witch (into)
import Control.Lens hiding (both)
import Control.Monad (forever, void)
import Control.Concurrent (newMVar, forkIO, swapMVar, MVar, readMVar)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad.Trans.MSF (switch)
import Circular (Angle (Angle))
import Example (drawBall', interpret, Position, Result(..))
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Text (Text)
import qualified Linear as L
import Debug.Trace (traceM)

-- bouncing

std :: Double
std = 0.5


data State where
  State :: {_ball1Pos :: V2 Double,
            _ball2Pos :: V2 Double}
           -> State
  deriving Show

-- a simple macro, not important
$(makeLenses ''State)

type AgentInput = V2 Double
type Observation = V2 Double
type Particles = [(State, Log Double)]


-- "Process Deterministic x y" is the type of signal functions 
-- where the input signal is (Time -> x)
-- and the output signal is (Time -> y)
-- So: (Time -> x) -> (Time -> y)
-- THIS IS NOT THE SAME AS A TIME-VARYING FUNCTION!! Time -> (x -> y)
-- "SignalFunction Stochastic x y" then is (effectively): (Time -> x) -> Distribution (Time -> y)


both :: SignalFunction (Stochastic & Feedback) () (Position, Position)
both = proc () -> do
    rec
        ball1 <- iPre 0 >>> moveAwayFrom -< ball2
        ball2 <- moveAwayFrom -< ball1
    returnA -< (ball1, ball2)

moveAwayFrom :: SignalFunction Stochastic Position Position
moveAwayFrom = feedback 0 proc (otherBall, prevPos@(V2 prevPosX prevPosY)) -> do

    dacceleration <- constM (normal 0 8 ) &&& constM (normal 0 8 ) -< ()
    acceleration <- decayingIntegral 1 -< uncurry V2 dacceleration
    let repulsion = fmap (savediv (safeNorm (prevPos - otherBall) ))
            (prevPos - otherBall )
    velocity <- decayingIntegral 1 -< acceleration + repulsion -- Integral, dying off exponentially
    position <- decayingIntegral 1 -< velocity

    returnA -< (position, position)


moveTowards :: SignalFunction Stochastic Position Position
moveTowards = feedback 0 proc (otherBall, prevPos@(V2 prevPosX prevPosY)) -> do

    dacceleration <- constM (normal 0 8 ) &&& constM (normal 0 8 ) -< ()
    acceleration <- decayingIntegral 1 -< uncurry V2 dacceleration
    let repulsion = (negate $ prevPos - otherBall )
    velocity <- decayingIntegral 1 -< acceleration + repulsion -- Integral, dying off exponentially
    position <- decayingIntegral 1 -< velocity

    returnA -< (position, position)




safeNorm 0 = 0
safeNorm x = norm x

decayingIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)


prior :: SignalFunction Stochastic () Position
prior = proc _ -> do
  x <- walk1D -< ()
  y <- walk1D -< ()
  returnA -< V2 x y

-- prior :: SignalFunction Stochastic AgentInput State
-- prior = movement where

--     movement :: SignalFunction Stochastic AgentInput State
--     movement  = feedback (State 0 0) proc (vel, oldState) -> do
--         ball2Position <- iPre 0 >>> decayIntegral 1 -< vel
--         ball1Position <- ball1 -< (ball2Position, oldState)
--         returnA -< (State {
--             _ball2Pos = ball2Position ,
--             _ball1Pos = ball1Position}, oldState)

--     ball1 :: SignalFunction Stochastic (AgentInput, State) Position
--     ball1 = proc (V2 ball2PositionX ball2PositionY, oldState) -> do
--         ball1PosAx <- walk1D -< ()
--         ball1PosAy <- walk1D -< ()
--         ballPosX <- decayIntegral 1 >>> decayIntegral 1 -< ball1PosAx +
--             2*(oldState ^. ball1Pos . _x - ball2PositionX  ) `savediv`
--             norm (oldState ^. ball1Pos . _x - ball2PositionX) **2
--         ballPosY <- decayIntegral 1  >>> decayIntegral 1 -< ball1PosAy +
--             2*(oldState ^. ball1Pos . _y - ball2PositionY  ) `savediv`
--             norm (oldState ^. ball1Pos . _y - ball2PositionY) **2
--         iPre 0 -< V2 ballPosX ballPosY

    where

    walk1D :: SignalFunction Stochastic () Double
    walk1D = proc _ -> do
        dacceleration <- constM (normal 0 8 ) -< ()
        acceleration <- decayingIntegral 1 -< dacceleration
        velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
        position <- decayingIntegral 1 -< velocity
        returnA -< position


    decayingIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)


observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
    (x,y) <- (noise &&& noise) -< ()
    returnA -< p + V2 x y
    where noise = constM (normal 0 std)

posterior :: Process (Stochastic & Unnormalized) (Observation, Position) Position
posterior = proc (ballAPos, ballBObs) -> do

    ballB <- iPre 0 >>> moveAwayFrom -< ballAPos
    observe -< normalPdf2D ballB std ballBObs
    returnA -< ballB

normalPdf2D :: V2 Double -> Double -> V2 Double -> Log Double
normalPdf2D (V2 x1 y1) std (V2 x2 y2) = normalPdf x1 std x2 * normalPdf y1 std y2

mainSimple :: SignalFunction (Stochastic & Feedback) Text Picture
mainSimple = proc inputText -> do
    rec
        ball1 <- iPre 0 >>> moveAwayFrom -< ball2
        ball2 <- moveAwayFrom -< ball1
    obsBall1 <- observationModel -< ball1
    obsBall2 <- observationModel -< ball2
    inferredBall1 <- particleFilter params {n = 50} posterior -< (ball2, obsBall1)
    inferredBall2 <- particleFilter params {n = 50} posterior -< (ball1, obsBall2)
    pic1 <- renderObjects yellow -< Result obsBall1 ball1 inferredBall1
    pic2 <- renderObjects green -< Result obsBall2 ball2 inferredBall2
    returnA -< pic1 <> pic2


main :: SignalFunction (Stochastic & Feedback) Text Picture
main = proc inputText -> do
  rec
    ballObs1 <- iPre 0 >>> observationModel -< ball1
    inferred1 <- particleFilter params {n = 20} posterior -< (ball2, ballObs1)
    expectedBall1 <- arr expected -< inferred1
    ball2 <- iPre 0 >>> moveAwayFrom -< expectedBall1


    ballObs2 <- iPre 0 >>> observationModel -< ball2
    inferred2 <- particleFilter params {n = 20} posterior -< (ball1, ballObs2)
    expectedBall2 <- arr expected -< inferred2
    ball1 <- moveAwayFrom -< expectedBall2

  pic1 <- renderObjects yellow -< Result ballObs1 ball1 inferred1
  pic2 <- renderObjects green -< Result ballObs2 ball2 inferred2
  returnA -< pic1 <> pic2

mainComplex :: SignalFunction (Stochastic & Feedback) Text Picture
mainComplex = proc inputText -> do
  rec
    ballObs1 <- iPre 0 >>> observationModel -< ball1
    inferred1 <- particleFilter params {n = 20} posterior -< (ball2, ballObs1)
    expectedBall1 <- arr expected -< inferred1
    ball2 <- moveAwayFrom -< expectedBall1


    ballObs2 <- iPre 0 >>> observationModel -< ball2
    inferred2 <- particleFilter params {n = 10} posteriorRank2 -< (ball1, ballObs2)
    expectedBall2 <- arr expected -< inferred2
    ball1 <- moveAwayFrom -< expectedBall2

  pic1 <- renderObjects yellow -< Result ballObs1 ball1 inferred1
  pic2 <- renderObjects green -< Result ballObs2 ball2 inferred2
  returnA -< pic1 <> pic2




selfBelief :: SignalFunction (Stochastic & Feedback) Text Picture
selfBelief = proc inputText -> do
    rec
        ballObs <- iPre 0 >>> observationModel -< ball 
        inferredBall <- particleFilter params {n = 20} post -< (ballObs, expectedBall)
        expectedBall <- arr expected -< inferredBall
        ball <- moveTowards -< expectedBall
    
    pic <- renderObjects yellow -< Result ballObs ball inferredBall
    returnA -< pic 

    where

        post :: SignalFunction (Stochastic & Unnormalized) (Observation, Position) Position
        post = proc (obs, expectedBall) -> do
            -- pos <- iPre 0 >>> moveTowards -< expectedBall
            pos <- Example.prior -< ()
            observe -< normalPdf2D pos std obs
            returnA -< pos

  


posteriorRank2 :: Process (Stochastic & Unnormalized) (Observation, Position) Position
posteriorRank2 = proc (ballAPos, ballBObs) -> do

    ballB <- iPre 0 >>> agent -< ballAPos
    observe -< normalPdf2D ballB std ballBObs
    returnA -< ballB


expected v = V2
        (averageOf (fmap (first (view _x)) v))
        (averageOf (fmap (first (view _y)) v))


agent :: SignalFunction Stochastic Position Position
agent = feedback 0 proc (ball1, oldBall2) -> do
    ballObs1 <- iPre 0 >>> observationModel -< ball1
    inferred1 <- particleFilter params {n = 10} posterior -< (oldBall2, ballObs1)
    expectedBall1 <- arr expected -< inferred1
    ball2 <- moveAwayFrom -< expectedBall1
    returnA -< (ball2, ball2)







renderObjects ::  Monad m => Color -> MSF m Result Picture
renderObjects col = proc Result { particles, measured, latent} -> do

  observation <- drawBall' -< (measured, 0.05, col)
  ball <- drawBall' -< (latent, 0.1, col)
  parts <- fold <$> mapMSF drawParticle' -< (\(x,y) -> (x,y,col)) <$> particles
  returnA -< (observation <> ball <> parts)


drawParticle' ::  Monad m => MSF m (Position, Log Double, Color) Picture
drawParticle' = proc (position, probability, col) -> do
  drawBall' -< (position, 0.05, withAlpha (into @Float $ exp $ 0.2 * ln probability) col)



-- drawBall'' :: Monad m => MSF m (V2 Double, Double, Color) Picture
-- drawBall'' = proc (V2 x y, width, theColor) -> do
--     returnA -<
--         scale 150 150 $
--         translate (double2Float x) (double2Float y) $
--         color theColor $
--         circleSolid $
--         into @Float width





-- drawParticle :: Monad m => MSF m (V2 Double, Log Double) Picture
-- drawParticle = proc (position, probability) -> do
--   drawBall' -< (position, 0.05, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet)



-- data Result = Result
--   {
--    measured :: AgentObservation
--   , latent :: State
--   , particles :: [(State, Log Double)]
--   }
--   deriving Show

-- unangle' :: (Floating p, Ord p) => V2 p -> p
-- unangle' 0 = 0
-- unangle' x = unangle x



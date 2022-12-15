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

module Control where
import FRP.Rhine.Gloss hiding (Up, Down)
import Prelude hiding (until)
import qualified Example
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
import Control.Lens
import Control.Monad (forever, void)
import Control.Concurrent (newMVar, forkIO, swapMVar, MVar, readMVar)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad.Trans.MSF (switch)
import Circular (Angle (Angle))
import Example (drawBall', interpret)
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Text (Text)
import Linear (angle)
import Data.Fixed (mod')
import qualified Control.Category as C
import Debug.Trace (traceM)
import qualified Debug.Trace as D
import Concurrent (GlossInput, noInput, keys)
import Data.Set (toList)
import qualified Debug.Trace as Debug

-- bouncing

std :: Double
std = 1


data State where
  State :: {_ball1Pos :: V2 Double,
            _ball2Pos :: V2 Double,
            _userSetting :: GlossInput}
           -> State
--   deriving Show

-- a simple macro, not important
$(makeLenses ''State)


data AgentObservation = AgentObservation {_ball1Obs :: V2 Double, _userObs :: GlossInput}
$(makeLenses ''AgentObservation)

type UserInput = GlossInput
type AgentInput = Angle
type Particles = [(State, Log Double)]
type UserObservation = State
data Direction = Up | Down


-- "Process Deterministic x y" is the type of signal functions 
-- where the input signal is (Time -> x)
-- and the output signal is (Time -> y)
-- So: (Time -> x) -> (Time -> y)
-- THIS IS NOT THE SAME AS A TIME-VARYING FUNCTION!! Time -> (x -> y)
-- "SignalFunction Stochastic x y" then is (effectively): (Time -> x) -> Distribution (Time -> y)





prior :: SignalFunction Stochastic AgentInput State
prior = movement where



    movement :: SignalFunction Stochastic AgentInput State
    movement  = feedback (State 0 1 noInput) proc (Angle pos, oldState) -> do
        ball1PosAx <- walk1D -< ()
        ball1PosAy <- walk1D -< ()
        pos' <- iPre 0 -< pos
        ball2Angle <- decayIntegral 1  -< pos'
        let ball2Vector = angle (ball2Angle `mod'` (2 * pi))
        -- arrM traceM -< show pos'
        ballPos <- decayIntegral 1 >>> decayIntegral 1 -< V2 ball1PosAx ball1PosAy - ball2Vector 
            -- (ball1PosAx +
            -- 5*(oldState ^. ball1Pos . _x - ball2PositionX  ) `savediv`
            -- norm (oldState ^. ball1Pos . _x - ball2PositionX) **2)
            -- (ball1PosAy +
            -- 5*(oldState ^. ball1Pos . _y - ball2PositionY  ) `savediv`
            -- norm (oldState ^. ball1Pos . _y - ball2PositionY) **2)
        -- ballPosY <- decayIntegral 1  >>> decayIntegral 1 -< ball1PosAy +
        --     5*(oldState ^. ball1Pos . _y - ball2PositionY  ) `savediv`
        ball1Position <- iPre 0 -< ballPos
        let userSetting = noInput
        let out = State {
            _ball2Pos = ball1Position + ball2Vector ,
            _ball1Pos = ball1Position,
            _userSetting = userSetting}
        returnA -< (out, out)



    walk1D :: SignalFunction Stochastic () Double
    walk1D = proc _ -> do
        dacceleration <- constM (normal 0 8 ) -< ()
        acceleration <- decayIntegral 1 -< dacceleration
        returnA -< acceleration

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)


observationModel :: SignalFunction Stochastic State AgentObservation
observationModel = proc state -> do
    (n1, n2) <- noise &&& noise -< std
    returnA -< AgentObservation
                (state ^. ball1Pos + V2 n1 n2)
                (state ^. userSetting)

    where
        noise :: SignalFunction Stochastic Double Double
        noise = proc userInput -> do
            arrM (normal 0) -< userInput

posterior :: SignalFunction (Stochastic & Unnormalized) (V2 Double, AgentObservation, AgentInput) State
posterior = proc (pos, obs, agentInput) -> do
  pred <- prior -< agentInput
  observe -<
    normalPdf (pred ^. ball1Pos . _x) std (obs ^. ball1Obs . _x)
    * normalPdf (pred ^. ball1Pos . _y) std (obs ^. ball1Obs . _y)
  arrM condition -< case obs ^. userObs . keys . to toList of
    SpecialKey KeyUp:_ -> pred ^. ball1Pos . _y  > pos ^. _y
    SpecialKey KeyDown:_ ->  pred ^. ball1Pos . _y  < pos ^. _y
    SpecialKey KeyLeft:_ ->  pred ^. ball1Pos . _x  < pos ^. _x
    SpecialKey KeyRight:_ -> pred ^. ball1Pos . _x  > pos ^. _x
    _ -> True
--   observe -< case obs ^. userObs of
--     _ -> normalPdf 0 0.1 (pred ^. ball1Pos . _x) * normalPdf 0 2 (pred ^. ball1Pos . _y)
--     _ -> 1
  returnA -< set userSetting (obs ^. userObs) pred -- hardcode update for user input

control :: SignalFunction Stochastic Particles AgentInput
control = Angle . unangle' <$> x

    where

    x = switch (withSwitch initial "go to the center") (const y)
    y = switch (withSwitch center "follow") (const x)


    initial = proc particles -> do
        let expectedX = V2
                (averageOf (fmap (first (view _x . _ball1Pos)) particles))
                (averageOf (fmap (first (view _y . _ball1Pos)) particles))
        let vel = expectedX + fmap (`savediv` (norm expectedX * 5)) expectedX
        returnA -< vel

    center = constM $ pure 0

    withSwitch a t = proc particles -> do
        out <- a -< particles
        sampledUserInput <- arrM empirical -< first (^. userSetting) <$> particles
        returnA -< (out, if False then Just out else Nothing )

main :: SignalFunction (Stochastic & Feedback) GlossInput (AgentObservation, AgentInput)
main = proc inputText -> do
    rec

        agentObs <- system -< agentInput
        agentInput <- agent -< agentObs

    returnA -< (agentObs, agentInput)

    where


    system :: SignalFunction Stochastic AgentInput AgentObservation
    system = proc agentInput -> do
        state <- prior -< agentInput
        observation <- observationModel -< state
        returnA -< observation

    agent :: Process (Stochastic & Feedback) AgentObservation AgentInput
    agent = proc agentObs -> do
        rec
            particles <- particleFilter params {n = 150} posterior -< (0, agentObs, agentInput)
            agentInput <- control -< particles
        returnA -< agentInput


mainSignal :: SignalFunction (Stochastic & Feedback & InputOutput) GlossInput (UserObservation, Particles, AgentObservation)
mainSignal = proc event -> do
    rec

        (agentObs, userObs) <- system -< (agentInput, userInput)
        (agentInput, particles) <- agent -< (userObs ^. ball1Pos, agentObs)
        userInput <- user -< (userObs, event)

    returnA -< (userObs, particles, agentObs)

    where

    user :: Process (Stochastic & InputOutput) (UserObservation, GlossInput) UserInput
    user = proc (userObs, message) -> do
        displayState -< userObs
        returnA -< message

    system :: SignalFunction Stochastic (AgentInput, UserInput) (AgentObservation, UserObservation)
    system = proc (agentInput, userInput) -> do
        previousUserInput <- iPre noInput -< userInput
        state <- prior -< agentInput
        observation <- observationModel -< set userSetting previousUserInput state
        returnA -< (observation, state)

    agent :: Process (Stochastic & Feedback) (V2 Double, AgentObservation) (AgentInput, Particles)
    agent = proc (pos, agentObs) -> do
        rec
            particles <- particleFilter params {n = 150} posterior -< (pos, agentObs, agentInput)
            agentInput <- control -< particles
        returnA -< (agentInput, particles)

displayState :: Arrow a => a b b
displayState = returnA

readInput :: MonadIO m => MVar T.Text -> MSF m a T.Text
readInput mvar = constM (liftIO $ readMVar mvar)

gloss :: SignalFunction (Stochastic & Feedback & InputOutput) GlossInput Picture
gloss = proc message -> do


                    -- "set the noise level to"
                -- (showParticles, showObservations) <- interpret -< message
                let (showParticles, showObservations) = (True, True)
                (state, particles, measured) <- mainSignal -< message
                visualisation -< Result {
                                    particles = if showParticles then particles else []
                                    , measured = if showObservations then measured else AgentObservation 100 noInput
                                    , latent = state
                                    }

visualisation :: Monad m => MSF m Result Picture
visualisation = proc Result { particles, latent, measured} -> do

  true1 <- drawBall' -< (_ball1Pos latent, 0.1, yellow)
  true2 <- drawTriangle -< ( latent, 0.1, blue)
  parts <- fold <$> mapMSF drawParticle -< first _ball1Pos <$> particles
  obs <- drawBall' -< (measured ^. ball1Obs, 0.05, red)
  returnA -< parts <> true1 <> true2 <> obs

drawTriangle :: Monad m => MSF m (State, Double, Color) Picture
drawTriangle = proc (latent, width, theColor) -> do
    let ang = unangle (_ball2Pos latent - _ball1Pos latent) * (360 / (2 * pi))
    let (V2 x y) = _ball2Pos latent
    returnA -<
        scale 150 150 $
        translate (into @Float x) (into @Float y) $
        rotate (180 - into @Float ang) $
        color theColor $
        polygon [(0,0), (0,0.1), (0.2, 0.05), (0,0)]



drawParticle :: Monad m => MSF m (V2 Double, Log Double) Picture
drawParticle = proc (position, probability) -> do
  drawBall' -< (position, 0.05, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet)



data Result = Result
  {
   measured :: AgentObservation
  , latent :: State
  , particles :: [(State, Log Double)]
  }

unangle' :: (Floating p, Ord p) => V2 p -> p
unangle' 0 = 0
unangle' x = unangle x







empirical :: MonadSample m => [(a, Log Double)] -> m a
empirical population = do
    let (vs, ps) = unzip population
    i <- logCategorical $ V.fromList ps
    return $ vs !! i


instance VectorSpace Angle Double where
    zeroVector = Angle 0
    Angle x ^+^ Angle y  = Angle $ x + y
    x *^ Angle y = Angle (x * y)
    Angle x `dot` Angle y = abs $ x - y

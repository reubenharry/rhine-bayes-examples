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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}

module Coordination where
import FRP.Rhine.Gloss hiding (Up, Down)
import Prelude hiding (until)
import qualified Example
import qualified Control.Monad.Morph as MM
import Linear.V2 (V2(..), _x, _y, angle, unangle)
import Control.Monad.Bayes.Class
import Numeric.Log
import Control.Monad.Bayes.Sampler
import Control.Monad.Morph
import Inference hiding (V2)
import Control.Monad.Bayes.Population (resampleMultinomial)
import Active (averageOf)
import Witch (into)
import Control.Lens
import Control.Monad (forever, replicateM, void)
import qualified Control.Monad.Trans.State as S
import Data.Fixed (mod')
import Control.Monad.Fix (MonadFix)
import Numeric.LinearAlgebra.Static.Backprop hiding ((&))
import qualified Numeric.LinearAlgebra.Static.Backprop as VV hiding ((&))
import Numeric.Backprop
import GHC.Generics (Generic)
import Generic.Data (Generically(..))
import GHC.TypeLits (KnownNat, Nat)
import Data.Foldable (Foldable(foldl'))
import Data.Sequence (replicateM)
import Data.Vector hiding (foldl')
import qualified Numeric.LinearAlgebra.Static as A
import System.Console.Haskeline (getInputChar, runInputT)
import qualified System.Console.Haskeline as H
import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import Control.Concurrent (newMVar, forkIO, swapMVar)
import qualified Control.Category as C
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Data.Kind (Type)


-- bouncing

std :: Double
std = 1


data State = State {
    _ball1Pos :: V2 Double,
    _ball2Pos :: V2 Double
    } deriving Show

-- a simple macro, not important
$(makeLenses ''State)

newtype Angle = Angle Double deriving Show

mkAngle :: Double -> Angle
mkAngle a = Angle $ a `mod'` (2 * pi)

type Observation = V2 Double
type Agent2Action = Angle
type Agent1Action = Angle 
type Particles = [(State, Log Double)]
data Direction = Up | Down


-- "Process Deterministic x y" is the type of signal functions 
-- where the input signal is (Time -> x)
-- and the output signal is (Time -> y)
-- So: (Time -> x) -> (Time -> y)
-- THIS IS NOT THE SAME AS A TIME-VARYING FUNCTION!! Time -> (x -> y)
-- "Process Stochastic x y" then is (effectively): (Time -> x) -> Distribution (Time -> y)

instance VectorSpace Angle Double where
    zeroVector = Angle 0
    Angle x ^+^ Angle y  = Angle $ x + y 
    x *^ Angle y = Angle (x * y)
    Angle x `dot` Angle y = abs $ x - y

prior :: Process Stochastic (Agent1Action, Agent2Action) State
prior = movement where

    movement  = proc (agent1Action, agent2Action) -> do
        ball1PosVx <- walk1D -< ()
        ball1PosVy <- walk1D -< ()
        (Angle ball2Angle) <- iPre (Angle 1) >>> decayIntegral 1  -< agent1Action
        let ball2Position@(V2 ball2PositionX ball2PositionY) = angle ball2Angle
        ballPosX <- decayIntegral 1 -< ball1PosVx + (-ball2PositionX * 0.2 )
        ballPosY <- decayIntegral 1 -< ball1PosVy + (-ball2PositionY * 0.2) 
        ball1Position <- iPre 0 -< V2 ballPosX ballPosY
        returnA -< State {
            _ball2Pos = ball2Position + ball1Position ,
            _ball1Pos = ball1Position}



    walk1D :: Process Stochastic () Double
    walk1D = proc _ -> do
        dacceleration <- constM (normal 0 8 ) -< ()
        acceleration <- decayIntegral 1 -< dacceleration
        velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
        returnA -< velocity

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)


observationModel :: Process Stochastic State Observation
observationModel = proc state -> do
    (n1, n2) <- noise &&& noise -< ()
    returnA -< (state ^. ball1Pos) + V2 n1 n2

    where
        noise = constM (normal 0 1) 

type System = Process Stochastic (Agent1Action, Agent2Action) State

type family ActionOf (i :: Nat) where
    ActionOf 1 = Agent1Action
    ActionOf 2 = Agent2Action

type Agent (i :: Nat) = Process Stochastic Observation (ActionOf i)

posteriorA1 :: System 
    -> Agent 2
    -> Process (Stochastic & Unnormalized) (Observation, Agent1Action) State
posteriorA1 system a2 = proc (obs@(V2 oX oY), agent1Action) -> do
  agent2Action <- iPre 0 >>> a2 -< obs
  pred <- system -< (agent1Action, agent2Action)
  observe -< normalPdf (pred ^. ball1Pos . _x) std oX  * normalPdf (pred ^. ball1Pos . _y) std oY
  returnA -< pred

controlA1 :: Process Stochastic Particles Agent1Action
controlA1 = proc particles -> do
    let expectedX = V2 (averageOf (fmap (first (view _x . _ball1Pos)) particles)) (averageOf (fmap (first (view _y . _ball1Pos)) particles))
    returnA -< Angle $ unangle' expectedX 



mainSignal :: Process (Stochastic & Feedback) () (State, Particles, Observation)
mainSignal = proc () -> do
    rec
        (observation, state) <- system prior -< (agent1Action, agent2Action)
        (agent1Action, particles) <- agent1 prior -< (observation, agent1Action)
        (agent2Action, particles2) <- agent2 prior -< (observation )

    returnA -< (state, particles, observation)

    where

    system :: System -> Process Stochastic (Agent1Action, Agent2Action) (Observation, State)
    system prior = proc (agent1Action, agent2Action) -> do
        state <- prior -< (agent1Action, agent2Action)
        observation <- observationModel -< state
        returnA -< (observation, state)

    agent1 :: System -> Process Stochastic (Observation, Agent1Action) (Agent1Action, Particles)
    agent1 prior = proc (observation, currentagent1Action) -> do

        particles <- particleFilter 150 resampleMultinomial (posteriorA1 prior (fst <$> agent2 prior)) -< (observation, currentagent1Action)
        agent1Action <- controlA1 -< particles
        returnA -< (agent1Action, particles)

    agent2 :: System -> Process Stochastic (Observation) (Agent2Action, Particles)
    agent2 = proc o -> do
        returnA -< undefined
    
    prior2 = undefined

    -- controlA2 = undefined



gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ do 
            mvar <- liftIO $ newMVar ""
            _ <- liftIO $ void $ forkIO $ forever do 
                x <- getLine 
                swapMVar mvar x
            reactimateCl Example.glossClock proc () -> do

                message :: String <- constM (liftIO $ swapMVar mvar "") -< ()
                showParticles <- hold False -< case message of
                    "show particles" -> Just True
                    "don't show particles" -> Just False
                    _ -> Nothing
                showObservations <- hold False -< case message of
                    "show observations" -> Just True
                    "don't show observations" -> Just False
                    _ -> Nothing
                -- arrMCl paintIO -< text $ show shift
                (state, particles, measured) <- morphS (MM.hoist lift) mainSignal -< ()
                (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                    particles = if showParticles then particles else []
                                    , measured = if showObservations then measured else 100
                                    , latent = state
                                    }

visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
visualisation = proc Result { particles, latent, measured} -> do

  drawParticles -< first _ball1Pos <$> particles
  drawBall -< (measured, 0.05, red)
  drawBall -< (_ball1Pos latent, 0.1, withAlpha 0.5 green)
  drawBall -< (_ball2Pos latent, 0.1, withAlpha 0.5 blue)


drawBall :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) ()
drawBall = proc (V2 x y, width, theColor) -> do
    arrMCl paintIO -<
        scale 150 150 $
        translate (into @Float x) (into @Float y) $
        color theColor $
        circleSolid $
        into @Float width

drawParticle :: MonadIO m => BehaviourF (GlossConcT m) td (V2 Double, Log Double) ()
drawParticle = proc (position, probability) -> do
  drawBall -< (position, 0.05, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet)

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
  }
  deriving Show

unangle' :: (Floating p, Ord p) => V2 p -> p
unangle' 0 = 0
unangle' x = unangle x







-- data Net = N { _nWeights1 :: L 20 1
--              , _nBias1    :: R 20
--              , _nWeights2 :: L  1  20
--              , _nBias2    :: R  1
--              }
--   deriving  Generic
--   deriving Show via Generically Net



-- instance Backprop Net

-- -- requires -XTemplateHaskell
-- makeLenses ''Net

-- minusNet n1 n2 = n1
--     & over nWeights1 (+ negate ((n2 ^. nWeights1)/ 100))
--     & over nWeights2 (+ negate ((n2 ^. nWeights2) / 100)) 
--     & over nBias1 (+ negate ((n2 ^. nBias1) / 100)) 
--     & over nBias2 (+ negate ((n2 ^. nBias2) / 100)) 

-- scaleNet (x) n2 = n2
--     & over nWeights1 ( undefined)
--     & over nWeights2 (+ (n2 ^. nWeights2)) 
--     & over nBias1 (+ (n2 ^. nBias1)) 
--     & over nBias2 (+ (n2 ^. nBias2)) 

-- runNet net x = z
--   where
--     -- run first layer
--     y = logistic $ (net ^^. nWeights1) #> x + (net ^^. nBias1)
--     -- run second layer
--     z = logistic $ (net ^^. nWeights2) #> y + (net ^^. nBias2)

-- logistic :: Floating a => a -> a
-- logistic x = 1 / (1 + exp (-x))


-- squaredError target output = error `VV.dot` error
--   where
--     error = target - output

-- netError target input net = squaredError (auto target)
--                                          (runNet net (auto input))

-- input = 0
-- output =  0

-- bar = evalBP (netError output input) net 

-- baz = gradBP (netError output input) net



-- net = (N {} 
--     & set nWeights1 0 
--     & set nWeights2 0 
--     & set nBias1 0 
--     & set nBias2 0 
--     )

-- -- trainStep
-- --       :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
-- --       => Double             -- ^ learning rate
-- --       -> R i                -- ^ input
-- --       -> R o                -- ^ target
-- --       -> Net   -- ^ initial Net
-- --       -> Net 
-- trainStep r !x !targ !n = n `minusNet` gradBP (netError x targ) n

-- -- > trainList
-- -- >     :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
-- -- >     => Double             -- ^ learning rate
-- -- >     -> [(R i, R o)]       -- ^ input and target pairs
-- -- >     -> Network i h1 h2 o  -- ^ initial network
-- -- >     -> Network i h1 h2 o
-- trainList r = flip $ foldl' (\n (x,y) -> trainStep r x y n)

-- m = sampleIO do
--     dat <- Control.Monad.replicateM 50 $ (\x -> (fromDouble x :: R 1 , fromDouble (sin x) :: R 1 )) <$> normal 0 1
--     let trainedNet = (Prelude.iterate (trainList 0 dat) net) !! 30
--     -- liftIO $ print 1
--     inp <- normal 0 1
--     liftIO $ print $  evalBP (netError (fromDouble $ sin inp) (fromDouble inp)) ( net)
--     liftIO $ print $  evalBP (netError (fromDouble $ sin inp) (fromDouble inp)) ( trainedNet)
--     -- print $ netError trainedNet


-- fromDouble x = A.fromList [x]
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

module PongExample where
import qualified Pong
import Pong (until)
import Prelude hiding (until)
import FRP.Rhine.Gloss hiding (Up, Down)
import Linear.V2 (V2(..), _x, _y)
import Control.Monad.Bayes.Class
import Numeric.Log
import Control.Monad.Morph
import Inference hiding (V2)
import Active (averageOf)
import Control.Lens
import Control.Monad (forever)
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Morph as MM
import qualified Example
import Control.Monad.Bayes.Sampler (sampleIO)
import Witch (into)
import Control.Monad.Bayes.Population (resampleMultinomial)


-- data State = State {
--     _ballPos :: V2 Double,
--     _barPos :: Double
--     } deriving Show
-- -- a simple macro to create lenses, not important to understand
-- $(makeLenses ''State)


-- type Observation = V2 Double
-- type Action = Double
-- type Particles = [(State, Log Double)]
-- data Direction = Up | Down

-- std :: Double
-- std = 0.3


-- -- "Process Deterministic x y" is the type of signal functions 
-- -- where the input signal is (Time -> x)
-- -- and the output signal is (Time -> y)
-- -- So: (Time -> x) -> (Time -> y)
-- -- THIS IS NOT THE SAME AS A TIME-VARYING FUNCTION!! Time -> (x -> y)
-- -- "Process Stochastic x y" then is (effectively): (Time -> x) -> Distribution (Time -> y)


-- prior :: Process Stochastic Action State
-- prior = safely $ S.evalStateT loop (State (V2 0 (-2)) 0) where

--     loop = forever do
--         initialPosition <- S.get
--         stateAtTimeofUpCollision <- until collisionUp (movement initialPosition Up) 
--         stateAtTimeofDownCollision <- until collisionDown (movement stateAtTimeofUpCollision Down) 
--         S.put stateAtTimeofDownCollision
                    

--     collisionUp, collisionDown :: State -> Bool
--     collisionUp (State (V2 x y) bar) = y > 2 || ((y < 0 && y > -0.1) && abs (x - bar) < 0.2)
--     collisionDown ((State (V2 x y) bar)) = negate y > 2 || ((y > 0 && y < 0.1) && abs (x - bar) < 0.2)

--     movement :: State -> Direction -> Process Stochastic Action State
--     movement state dir = proc action -> do
--         barPosition <- iPre (state ^. barPos) -< action
--         ballPosX <- walk1D -< ()
--         ballPosY <- (case dir of Up -> id; Down -> negate) <$> time -< ()
--         returnA -< (state
--             & set barPos barPosition 
--             & over ballPos (+ V2 ballPosX ballPosY) )

--     walk1D :: Process Stochastic () Double
--     walk1D = proc _ -> do
--         dacceleration <- constM (normal 0 8 ) -< ()
--         acceleration <- decayIntegral 1 -< dacceleration
--         velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
--         position <- decayIntegral 1 -< velocity
--         returnA -< position

--     decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)



-- mainSignal :: Process (Stochastic & Feedback) () (State, Maybe Observation, Particles)
-- mainSignal = proc _ -> do
--     rec
--         state <- prior -< action -- the ground truth state (it's drawn from the prior)
--         observation <- observationModel -< state
--         particles <- particleFilter params {n = 150} posterior -< (observation, action)
--         action <- control -< particles

--     returnA -< (state, Just observation, particles )


-- observationModel :: Process Stochastic State Observation
-- observationModel = proc state -> do
--     (n1, n2) <- noise &&& noise -< ()
--     returnA -< (state ^. ballPos) + V2 n1 n2

--     where
--         noise :: Process Stochastic () Double
--         noise = constM (normal 0 std)

-- posterior :: Process (Stochastic & Unnormalized) (Observation, Action) State
-- posterior = proc (V2 oX oY, action) -> do
--   pred <- prior -< action
--   observe -< normalPdf (pred ^. ballPos . _x) std oX  * normalPdf (pred ^. ballPos . _y) std oY
--   returnA -< pred

-- control :: Process Stochastic Particles Action
-- control = arr (averageOf . map (first (view _x . _ballPos)))











































-- gloss :: IO ()
-- gloss = sampleIO $
--         launchGlossThread defaultSettings
--             { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
--         $ reactimateCl Example.glossClock proc () -> do

--             (state, obs, particles) <- morphS (MM.hoist lift) mainSignal -< ()
--             (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
--                                 particles = particles
--                                 , measured = obs
--                                 , latent = state
--                                 , bar = state ^. barPos
--                                 }

-- visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
-- visualisation = proc Result { particles, latent, bar, measured} -> do

--   Pong.drawParticles -< first _ballPos <$> particles
--   case measured of 
--     Nothing -> returnA -< ()
--     Just m -> Pong.drawBall -< (m, 0.05, red)
--   Pong.drawBall -< (_ballPos latent, 0.1, withAlpha 0.5 green)

--   arrMCl paintIO -< translate (into @Float (bar * 150)) 0 $ polygon [(-30, -5), (-30, 5), (30, 5), (30, -5), (-30, -5)]


-- data Result = Result
--   {
--     --   estimate :: Position
--     -- stdDev :: Double
--    measured :: Maybe Observation
--   , latent :: State
--   , particles :: [(State, Log Double)]
--   , bar :: Double
--   }
--   deriving Show
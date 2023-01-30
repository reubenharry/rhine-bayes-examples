{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Coordination where
import Control.Monad.Bayes.Class (MonadSample (normal), normalPdf)
import Data.Vector ()
import Example (walk1D, Result (..), empirical, prior, posterior)
import FRP.Rhine.Gloss
    ( Arrow((&&&)), constM, returnA, green, yellow, violet )
import GHC.TypeLits (Nat)
import Inference
  ( SignalFunction,
    Stochastic,
    type (&), SignalFunction, Feedback, SMCSettings (n), particleFilter, params, Unnormalized, observe
  )
import Linear.V2 (V2 (..))
import Numeric.Backprop ()
import Numeric.LinearAlgebra.Static.Backprop ()
import Numeric.Log (Log (ln))
import Prelude hiding (until)
import Control.Lens
import Data.MonadicStreamFunction ( iPre, arrM, feedback )
import Control.Arrow
import Data.Coerce (coerce)
import Concurrent (GlossInput)
import Graphics.Gloss (Picture)
import MutualStoch (expected, normalPdf2D)
import MutualStoch (renderObjects)
import Debug.Trace (traceM)
import Data.Singletons
import Data.Singletons.TH ( Sing, genSingletons, Void )


data AgentNumber = One | Two
genSingletons [''AgentNumber]

type family ToOutputType (i :: AgentNumber) where
    ToOutputType One = Bool
    ToOutputType Two = ()

foo :: Sing (i :: AgentNumber) -> ToOutputType i
foo i = case i of
    SOne -> True
    STwo -> ()

std :: Double
std = 0.5

newtype Agent1Action = Agent1Action {_agent1Action :: V2 Double} deriving Show
$(makeLenses ''Agent1Action)

newtype Agent2Action = Agent2Action {_agent2Action :: V2 Double} deriving Show
$(makeLenses ''Agent2Action)

data Observation where
  Observation :: {_stateObs :: V2 Double, _action1Obs :: Agent1Action, _action2Obs :: Agent2Action} -> Observation
$(makeLenses ''Observation)



type Population a = [(a, Log Double)]

data State = State
  { _ball :: V2 Double,
    _actions :: (Agent1Action, Agent2Action)
  }
  deriving (Show)

-- a simple macro, not important
$(makeLenses ''State)

type World = SignalFunction Stochastic (Agent1Action, Agent2Action) (State, Observation)
type WorldFromPerspectiveOf i = SignalFunction Stochastic (ActionOf i) (State, Observation)

type family ActionOf (i :: Nat) where
  ActionOf 1 = Agent1Action
  ActionOf 2 = Agent2Action


type Agent (i :: Nat) = SignalFunction Stochastic Observation (ActionOf i, Population State)

movement :: SignalFunction Stochastic (Agent1Action, Agent2Action) State
movement = proc actions -> do
    ballX <- walk1D -< ()
    ballY <- walk1D -< ()
    returnA -< State
                    { _ball = V2 ballX ballY,
                    _actions = actions
                    }

observationModel :: SignalFunction Stochastic State Observation
observationModel = proc state -> do
    (n1, n2) <- noise &&& noise -< ()
    returnA -< Observation {
            _stateObs = state ^. ball + V2 n1 n2,
            _action1Obs = state ^. actions . _1,
            _action2Obs = state ^. actions . _2
            }

noise :: SignalFunction Stochastic a Double
noise = constM (normal 0 std)

world :: World
world = proc actions -> do
    trueState <- movement -< actions
    observation <- observationModel -< trueState
    returnA -< (trueState, observation)


-- agentIPrior :: 
--     Sing (i :: AgentNumber) -> 
--     WorldFromPerspectiveOf i
-- -- WorldFromPerspectiveOf i
-- agentIPrior agentID = proc action -> do
--     ballX <- walk1D -< ()
--     ballY <- walk1D -< ()
--     returnA -<
--         -- V2 ballX ballY 
--         (State {_ball = V2 ballX ballY, _actions = (action, Agent2Action 0)}, undefined)

agent1Prior :: 
    -- forall (i :: Nat) .
    WorldFromPerspectiveOf 1
-- WorldFromPerspectiveOf i
agent1Prior = proc action -> do
    ballX <- walk1D -< ()
    ballY <- walk1D -< ()
    returnA -<
        -- V2 ballX ballY 
        (State {_ball = V2 ballX ballY, _actions = (action, Agent2Action 0)}, undefined)

agent2Prior :: 
    -- forall (i :: Nat) .
    WorldFromPerspectiveOf 2
-- WorldFromPerspectiveOf i
agent2Prior = proc action -> do
    ballX <- walk1D -< ()
    ballY <- walk1D -< ()
    returnA -<
        -- V2 ballX ballY 
        (State {_ball = V2 ballX ballY, _actions = (Agent1Action 0, action)}, undefined)

agent2PriorComplex :: WorldFromPerspectiveOf 2
-- WorldFromPerspectiveOf i
agent2PriorComplex = feedback (Agent1Action 0) proc (action, a1Act) -> do

    state <- movement -< (a1Act, action)
    obs <- observationModel -< state
    (newA1Act, ps) <- agent1 -< obs
    -- ps' <- arr expected -< first (^. ball ) <$> ps
    -- observe -< (normalPdf2D 0 0.1 ps')
    returnA -<
        ((state, obs), newA1Act)

-- agentIPosterior :: forall (i :: Nat) . SignalFunction (Stochastic & Unnormalized) (Observation, ActionOf i) State
-- agentIPosterior = proc (observation, agentIAction) -> do
--     -- ballPos <- agentIPrior @i -< agentIAction
--     (statePrediction, _) <- agentIPrior @i -< agentIAction
--     observe -< normalPdf2D (statePrediction ^. ball) std (observation ^. stateObs)
--     -- observe -< normalPdf2D ballPos std (observation ^. stateObs)
--     -- observe -< normalPdf2D (statePrediction ^. ball) (std*2) (observation ^. action2Obs . agent2Action)
--     returnA -< statePrediction

agent1Posterior :: SignalFunction (Stochastic & Unnormalized) (Observation, ActionOf 1) State
agent1Posterior = proc (observation, agentIAction) -> do
    -- ballPos <- agentIPrior @i -< agentIAction
    (statePrediction, _) <- agent1Prior -< agentIAction
    observe -< normalPdf2D (statePrediction ^. ball) std (observation ^. stateObs)
    -- observe -< normalPdf2D ballPos std (observation ^. stateObs)
    observe -< normalPdf2D (statePrediction ^. ball) (std/2) (observation ^. action2Obs . agent2Action)
    returnA -< statePrediction

agent2Posterior :: SignalFunction (Stochastic & Unnormalized) (Observation, ActionOf 2) State
agent2Posterior = proc (observation, agentIAction) -> do
    -- ballPos <- agentIPrior @i -< agentIAction
    (statePrediction, _) <- agent2Prior -< agentIAction
    observe -< normalPdf2D (statePrediction ^. ball) std (observation ^. stateObs)
    -- observe -< normalPdf2D ballPos std (observation ^. stateObs)
    observe -< normalPdf2D (statePrediction ^. ball) (std/2) (observation ^. action1Obs . agent1Action)
    returnA -< statePrediction

agent2PosteriorComplex :: SignalFunction (Stochastic & Unnormalized) (Observation, ActionOf 2) State
agent2PosteriorComplex = proc (observation, agentIAction) -> do
    -- ballPos <- agentIPrior @i -< agentIAction
    (statePrediction, _) <- agent2PriorComplex -< agentIAction
    observe -< normalPdf2D (statePrediction ^. ball) std (observation ^. stateObs)
    -- observe -< normalPdf2D ballPos std (observation ^. stateObs)
    observe -< normalPdf2D (statePrediction ^. actions . _1 . agent1Action) (std/2) (observation ^. action1Obs . agent1Action)
    returnA -< statePrediction

decisionModelAgent2 :: SignalFunction (Stochastic & Unnormalized) State (ActionOf 2)
decisionModelAgent2 = proc state -> do
    action <- Agent2Action <$> Example.prior -< ()
    (outcome, _) <- agent2Prior -< action
    observe -< normalPdf2D (outcome ^. actions . _2 . agent2Action) 0.1 3
    -- (state ^. actions . _2 . agent2Action)
    returnA -< action 

agent1 :: Agent 1
agent1 = feedback (Agent1Action 0) proc (observation, a1Act) -> do


    belief <- particleFilter params {n=10} agent1Posterior -< (observation, a1Act)
    nextA1Act <- arr (Agent1Action . expected) -< first (^. ball ) <$> belief
    
    -- let nextA1Act = Agent1Action 1
    returnA -<  ((nextA1Act, belief), nextA1Act)


agent2 :: Agent 2
agent2 = feedback (Agent2Action 0) proc (observation, a2Act) -> do


    belief <- particleFilter params {n=10} agent2Posterior -< (observation, a2Act)
    -- nextA2Act <- arr (Agent2Action . expected) -< first (^. ball ) <$> belief
    belief' <- arrM empirical -< belief 
    action <- particleFilter params{n=10} decisionModelAgent2 -< belief'
    nextA2Act <- arr (Agent2Action . expected) -< first (^. agent2Action) <$> action 
    returnA -<  ((nextA2Act, belief), nextA2Act)

agent2Complex :: Agent 2
agent2Complex = feedback (Agent2Action 0) proc (observation, a2Act) -> do


    belief <- particleFilter params {n=10} agent2PosteriorComplex -< (observation, a2Act)
    nextA2Act <- arr (Agent2Action . expected) -< first (^. ball ) <$> belief
    returnA -<  ((nextA2Act, belief), nextA2Act)





main :: SignalFunction Stochastic GlossInput Picture
main = feedback (Agent1Action 0, Agent2Action 0) proc (glossInput, (a1act, a2act)) -> do



    -- (prev1, prev2) <- iPre (Agent1Action 0, Agent2Action 0) -< (a1act, a2act)
    (trueState, trueObservation) <- world -< (a1act, a2act)

    (a2actNew, beliefAgent2) <- agent2 -< trueObservation
    (a1actNew, beliefAgent1) <- agent1 -< trueObservation


    pic1 <- renderObjects violet -< Result {
        measured = trueObservation ^. stateObs,
        latent = trueState ^. ball,
        particles = []
        }

    pic2 <- renderObjects green -< Result {
        measured = 1000,
        latent = a1actNew ^.  agent1Action,
        particles =  first (^. ball) <$>  beliefAgent1
        }

    pic3 <- renderObjects yellow -< Result {
        measured = 1000,
        latent = a2actNew ^. agent2Action,
        particles =  first (^. ball) <$> beliefAgent2
        }

    returnA -< (pic1
        <> pic2 <> pic3, (a1actNew, a2actNew))














-- agent2ActionModel :: SignalFunction Stochastic ( V2 Double) Agent2Action
-- agent2ActionModel = arr Agent2Action



-- main2 :: SignalFunction (Stochastic & Feedback) GlossInput Picture
-- main2 = proc glossInput -> do
--     rec
--         prevObs <- iPre 0 -< observation
--         (trueAction, action) <- agent -< prevObs
--         observation <- world -< action
--     renderObjects green -< Result {measured = trueAction, latent = action, particles = []}

--     where

--         world :: SignalFunction Stochastic (V2 Double) (V2 Double)
--         world = proc action -> do
--             p <- Example.prior -< ()
--             returnA -< p + action


--         posterior :: SignalFunction (Stochastic & Unnormalized) (V2 Double) (V2 Double)
--         posterior = proc obs -> do
--             possibleAction <- Example.prior -< ()
--             possibleObservation <- world -< possibleAction
--             observe -< normalPdf2D possibleObservation 0.01 obs
--             returnA -< possibleAction

--         agent :: SignalFunction Stochastic (V2 Double) (V2 Double, V2 Double)
--         agent = proc obs -> do

--             let trueAction = 0
--             belief <- particleFilter params posterior -< obs

--             action <- arr expected -< belief




--             returnA -< (trueAction, action )






-- agent2PriorComplex :: SignalFunction Stochastic Agent2Action State
-- agent2PriorComplex = proc agent2Action -> do
--     _ <- complexWorld -< undefined
--     returnA -< undefined
-- agent2PosteriorComplex :: SignalFunction (Stochastic & Unnormalized) (Observation, Agent2Action) State
-- agent2PosteriorComplex = proc (observation, agent2action) -> do
--     latent <- complexWorld -< agent2action
--     observe -< normalPdf2D (latent ^. ball) std (observation ^. stateObs)
--     observe -< normalPdf2D (latent ^. actions . _1 . agent1Action) (std/10) (observation ^. action1Obs . agent1Action)
--     returnA -< latent

-- agent2ActionModel :: SignalFunction Stochastic (Population (V2 Double), Agent2Action) (Agent2Action, Population (V2 Double))
-- agent2ActionModel = (\x -> (Agent2Action . expected . fmap (first fst) $ x, fmap (first snd) x)) <$> particleFilter params proc (population, prevAgent2Action) -> do
--     myBelief@(V2 x y) <- arrM empirical -< population
--     (obsX, obsY) <- constM (normal 0 std ) *** constM (normal 0 std) -< (x ,y)
--     agent2act <- Example.prior -< ()
--     -- let putativeState = State {_ball = myBelief, _actions = (undefined, Agent2Action agent2act)}
--     post <- agent1Posterior -< (Observation {_stateObs = V2 obsX obsY, _action1Obs = undefined, _action2Obs = prevAgent2Action}, undefined)
--     observe -< normalPdf2D post std myBelief
--     returnA -< (agent2act, post)

-- agent2ActionModel :: SignalFunction (Stochastic & Feedback) (Population (V2 Double), Agent2Action) (Agent2Action)
-- agent2ActionModel = proc (population, prevAgent2Action) -> do
--     agent2act <- Example.prior -< ()
--     (Agent1Action a, _) <- agent1 -< undefined
--     belief <- arr expected -< population 
--     returnA -< undefined

-- agent2Complex :: Agent 2
-- agent2Complex = proc observation -> do

--     rec
--         prevAgent2Action <- iPre (Agent2Action 0) -< agent2Action
--         obs <- particleFilter params  agent2PosteriorComplex -< (undefined, prevAgent2Action)
--         -- agent2Actions <- particleFilter params {n=50} (agent2Posterior >>> agent2ActionModel) -< (observation, prevAgent2Action)
--         -- -- (agent2Action, guessedPost) <- agent2ActionModel -< (belief, prevAgent2Action) -- first (^. ball ) <$> belief
--         -- (agent2Action) <- arr (Agent2Action . expected) -< (first (_agent2Action) <$> agent2Actions) -- first (^. ball ) <$> belief
--         (agent2Action) <- undefined -< (undefined) -- first (^. ball ) <$> belief
--     returnA -< undefined


-- agent :: Sing (i :: Nat) -> Agent i
-- agent = undefined 

-- complexWorld ::  SignalFunction (Stochastic) Agent2Action State
-- complexWorld = feedback undefined proc (a2act, a1act) -> do


--     (prev1, prev2) <- iPre (Agent1Action 0, Agent2Action 0) -< (a1act, a2act)
--     trueState <- movement -< (prev1 , prev2)
--     trueObservation <- observationModel -< trueState

--     -- (a2act, beliefAgent2) <- agent2Simple -< trueObservation
--     (a1actNew, beliefAgent1) <- agent1 -< trueObservation
--     returnA -< (trueState, a1actNew)


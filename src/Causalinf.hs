
{-# LANGUAGE Arrows #-}


module CausalInf where

import Concurrent
import Prelude hiding ((.))
import Control.Monad.Bayes.Class (MonadDistribution (normal, uniformD), condition)
import Example (Result (Result, latent, measured, particles), decayingIntegral, renderObjects, toTable, visualizeTable)
import FRP.Rhine.Gloss
import Inference (params, particleFilter, SMCSettings (n))
import Linear (V2 (..))
import Numeric.Log (Log)
import Util
import Control.Lens
import Data.Generics.Product (the)
import Witch (into)
import Convention (AgentID, AgentNumber (One, Two), SAgentNumber (SOne, STwo), one, two, AgentAction, Other, other)
import Control.Category ((.))
import Control.Monad.Trans.MSF (performOnFirstSample)
import GUI (button, buttonParams)

data Action = Red | Blue deriving (Eq, Show)

data Observation = RedObs | BlueObs deriving (Eq, Show)

causal :: System Stochastic Action Observation
causal = proc action ->
    case action of 
        Red -> constM $ uniformD [RedObs, BlueObs] -< ()
        -- Red -> returnA -< BlueObs 
        Blue -> returnA -< RedObs 

nonCausal :: System Stochastic Action Observation
nonCausal = proc action -> 
    constM (uniformD [RedObs, BlueObs]) -< action

prior :: System Stochastic Action (Observation, Bool)
prior = proc action -> do

    itIsCausal <- performOnFirstSample (uniformD [constM (pure True), constM (pure False)]) -< ()

    obs <- case itIsCausal of
        True -> causal -< action
        _ -> nonCausal -< action

    returnA -< (obs, itIsCausal)

posteriorDistribution :: System (Stochastic & Unnormalized) (Action, Observation) Bool
posteriorDistribution = proc (prevAction, obs) -> do

    (obs', itIsCausal) <- prior -< prevAction
    arrM condition -< obs == obs'
    returnA -< itIsCausal

inferredPosterior :: System Stochastic (Action, Observation) [(Bool, Log Double)]
inferredPosterior = particleFilter params posteriorDistribution



main :: System (Stochastic & Feedback) UserInput Picture
main = feedback (Red, BlueObs) proc (userInput, (prevAct, prevObs)) -> do
    (picture, b) <- button buttonParams -< userInput
    (action, boolDist) <- agent -< (b, prevObs)
    obs <- causal -< prevAct
    returnA -< (boolDist <> picture, (action, obs))

agent :: System (Stochastic & Feedback) (Bool, Observation) (Action, Picture)
agent = feedback Red proc ((b, obs), prevAct) -> do
    belief <- particleFilter params {n=1000} posteriorDistribution -< (prevAct, obs)
    action <- if b 
        then constM (pure Blue) -< belief
        else constM (pure Blue) -< belief
    -- prevAct <- iPre Red -< action
    tab <- arr toTable -< first ((), ) <$> belief 
    table <- visualizeTable -< tab
    returnA -< ((action, table), action)


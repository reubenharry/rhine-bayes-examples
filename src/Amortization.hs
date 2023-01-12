{-# LANGUAGE TypeOperators #-}

module Amortization where 

import Data.MonadicStreamFunction
import Inference (SignalFunction, Deterministic, Feedback)


-- parametrized signal function:

-- SignalFunction (Observation, State) (Latent, State) 


-- SignalFunction (Observation, Params) Latent

type Params = Double

-- ex :: SignalFunction (Feedback) () Params 
-- ex = proc () -> do
--     (latent, observations :: Double) <- joint -< () 
--     rec
--         prediction <- model -< (observations, params)
--         lossFn <- getLossFn -< (prediction, latent)
--         gradient <- getGradient -< (lossFn params)
--         params <- optimizer -< (gradient, params)
--     returnA -< params

--     where

--         joint = undefined
--         model = MSF undefined
--         getLossFn = undefined
--         getGradient = undefined
--         optimizer = undefined

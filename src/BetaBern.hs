{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE StandaloneKindSignatures #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TupleSections #-}

module BetaBern where
import Inference (StochasticSignal, StochasticSignalTransform, StochasticSignalTransformUnnormalized, particleFilter, pattern V2, V2(..), SignalFunction, Stochastic, type (&), Unnormalized)
import FRP.Rhine.Gloss
import Example (glossClock, drawBall, Result (Result, particles, measured, latent), renderObjects, drawBall', drawParticles')
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Population
import Control.Monad.Trans.Class (MonadTrans(lift))
import Numeric.Log (Log (Exp, ln))
import Control.Monad.Bayes.Class
import qualified Data.Vector.Sized as V
import Witch (into)
import Control.Monad.Trans.MSF (ReaderT)
import qualified Data.Text as T



std :: Double
std = 0.5

type Observation = Bool
type Position = Double


prior :: SignalFunction Stochastic () Position
prior = fmap sigmoid walk1D

  where

    walk1D = proc _ -> do
        dacceleration <- constM (normal 0 2 ) -< ()
        acceleration <- decayIntegral 1 -< dacceleration
        velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
        position <- decayIntegral 1 -< velocity
        returnA -< position

    sigmoid x = 1 / (1 + exp x)

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
    n <- arrM bernoulli -< p
    returnA -< n

posterior :: SignalFunction (Stochastic & Unnormalized) Observation Position
posterior = proc bool -> do
  latent <- prior -< ()
  predicted <- observationModel -< latent
  arrM condition -< predicted == bool
  returnA -< latent


--   observe -< Exp $ log $ if bool then latent else (1 - latent)
-- posteriorPredictive ::StochasticSignalTransformUnnormalized Observation (Position, Int)
-- posteriorPredictive = proc obs -> do
--   predicted <- posterior -< obs
--   e <- edgeBy ((>0.5) . norm) -< predicted
--   returnA -< (predicted, e)







----------
-- display
----------

gloss :: SignalFunction Stochastic T.Text Picture
gloss = proc _ -> do
            actualPosition <- prior -< ()
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter 150 resampleMultinomial posterior -< measuredPosition
            visualisation -< Result {
                                particles = first (\y -> V.fromTuple (0,(y-0.5)*5) ) <$> samples
                                , measured = if measuredPosition then V.fromTuple (-1,-2) else V.fromTuple (1,-2)
                                , latent = (\y -> V.fromTuple (0,(y-0.5)*5) ) actualPosition
                                }


-- data Result = Result
--   {
--     --   estimate :: Position
--     -- stdDev :: Double
--    measured :: Observation
--   , latent :: Position
--   , particles :: [(Position, Log Double)]
--   }
--   deriving Show

visualisation :: Monad m => MSF m Result Picture
visualisation = proc Result { particles, measured, latent} -> do

  let V2 mx my = measured

  parts <- drawParticles' -< particles
  let obs =
        -- scale 150 150 $
        translate (into @Float (mx*150)) (into @Float (my*150)) $
        text $ if mx < 0 then "True" else "False" 
        
        --  -< (measured, 0.05, red)
  history <- accumulateWith (\x l -> take 200 $ x : l) [] -< latent
--   drawBall -< (latent, 0.02, withAlpha 0.5 green)
--   let l@(V2 x y) = latent

  pic <- drawBalls -< (zip history (repeat 1), green)
  returnA -< pic <> obs <> parts

drawBalls :: (Monad m) => MSF
  m
  ([(V2 Double, Log Double)], Color)
  Picture
drawBalls = proc (history, c) -> do 
    returnA -< scale 150 150 $ pictures
        [translate (into @Float x-(z/50)) (into @Float y) $
        color (withAlpha (into @Float $ exp $ 0.2 * ln a) c) $
        
        circleSolid 0.02 | (((x,y), a), z) <- zip (first toTuple <$> history) [1..] ]

toTuple (V2 x y) = (x,y)

drawParticle :: Monad m => MSF m (V.Vector 2 Double, Log Double) Picture
drawParticle = proc (position, probability) -> do
  drawBall' -< (position, 0.04, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet)



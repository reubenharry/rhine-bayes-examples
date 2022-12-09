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

module Active where
import Data.MonadicStreamFunction
    ( returnA,
      (>>>),
      arrM,
      constM,
      morphS,
      iPre,
      withSideEffect_,
      Arrow(first, second, (***), arr) )
import Control.Monad.Bayes.Class (factor, MonadSample (normal), normalPdf)
import Control.Monad.Bayes.Sampler ( sampleIO, SamplerIO )
import Inference
import Control.Monad.Bayes.Population
    ( fromWeightedList, resampleMultinomial, runPopulation )
import Control.Monad.IO.Class ()
import Numeric.Log ( Log(ln) )
import Control.Monad.Fix (MonadFix (mfix))
import Data.Tuple (swap)
import Data.Functor.Identity ( Identity(runIdentity) )
import Data.Text (Text)
import FRP.Rhine.Gloss
    ( reactimateCl,
      BehaviourF,
      TimeDomain(Diff),
      VectorSpace(norm),
      defaultSettings,
      clearIO,
      launchGlossThread,
      Display(InWindow),
      GlossSettings(display) )
import Example ( Result(..), glossClock, std, visualisation, prior, renderObjects, toGlossC', toGlossC'', Position )
import Control.Monad.Trans.Class ( MonadTrans(lift) )

import qualified Control.Monad.Morph as MM
import FRP.Rhine.Gloss.Common (Picture)
import qualified Data.Text as T
import FRP.Rhine.Gloss.IO (GlossConcT)
import Linear (V2)
import Linear.V2 (V2(..))


















type Observation = Either Double Double


posterior :: SignalFunction (Stochastic & Unnormalized) Observation Position
posterior = proc observation -> do

    state <- prior -< ()
    prediction <- observationModel -< (state, case observation of Left _ -> ViewX; _ -> ViewY)
    arrM factor -< normalPdf (either id id prediction) std (either id id observation)
    returnA -< state

control :: StochasticSignalTransform [(Position, Log Double)] Action
control = proc particles -> do
            let (s1,s2) = calculateXandYVariances particles -- calculate the variance of the population along the X and Y axes
            action <- arr (\(s1,s2) -> if s1>s2 then ViewX else ViewY) -< (s1,s2) -- choose axis with highest variance
            returnA -< action

calculateXandYVariances :: [(Position, Log Double)] -> (Double, Double)
calculateXandYVariances particles = 
    let (p, p1, p2) = (fromWeightedList $ pure particles, (\(V2 x _) -> x) <$> p, (\(V2 _ y) -> y) <$> p)
        stdOf = stdDevOf . runIdentity . runPopulation
    in stdOf *** stdOf $ (p1, p2)


data Action = ViewX | ViewY

observationModel :: SignalFunction Stochastic (Position, Action) Observation
observationModel = second (iPre ViewX) >>> proc (V2 d1 d2, action) -> do
    case action of
        ViewX -> returnA -< Left d1
        ViewY -> returnA -< Right d2

noiseModel :: SignalFunction Stochastic Observation Observation
noiseModel = proc observation -> do
    n <- constM (normal 0 std) -< ()
    returnA -< either (Left . (+n)) (Right . (+n)) observation

mainSignal :: SignalFunction (Stochastic & Feedback) Text Picture
mainSignal = proc _ -> do 
    state <- Example.prior -< ()
    rec obs <- (observationModel >>> noiseModel) -< (state, action)
        particles <- particleFilter params {n = 150} posterior -< obs
        action <- control -< particles
    renderObjects -< Result (uncurry V2 $ either (,-2) (-2,) obs) state particles
                        











--- helper code

averageOf :: (Functor t, Floating a, Foldable t) => t (a, Log a) -> a
averageOf things =
  let
    properThings = first (exp . ln) . swap <$> things
    fullWeight = Prelude.sum $ fst <$> properThings
    sumOfThings = sum $ fmap (uncurry (*)) properThings
  in sumOfThings / fullWeight

stdDevOf :: (Functor t, Foldable t, VectorSpace b b) => t (b, Log b) -> b
stdDevOf things =
  let
    average = averageOf things
    squares = first (\x -> norm (x - average) ** 2) <$> things
  in sqrt $ averageOf squares

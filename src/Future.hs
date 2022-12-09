{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}



{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}



{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE LiberalTypeSynonyms #-}


{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Future where


import Control.Monad.Bayes.Class ( normalPdf )
import FRP.Rhine
    ( reactimateCl,
      Arrow(arr),
      MSF,
      returnA,
      (>>>),
      accumulateWith,
      withSideEffect_,
       )
import Inference (particleFilter, observe, SignalFunction, Stochastic, type (&), Unnormalized, params, SMCSettings (n))
import FRP.Rhine.Gloss
    ( defaultSettings,
      launchGlossThread,
      Display(InWindow),
      GlossSettings(display),
      clearIO,
      makeColorI,
      red,
      withAlpha,
      violet,
      yellow,
      mixColors )
import Numeric.Log (Log (ln))
import GHC.Float ()
import Control.Monad.Bayes.Sampler ( sampleIO )
import Control.Monad.Bayes.Population
    ( resampleMultinomial )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Example
    ( glossClock,
      Result(Result, particles, measured, latent),
      Position,
      std,
      prior,
      observationModel,
      posterior,
      visualisation, drawBall', drawParticle' )
import Linear (V2(..))
import Data.Text (Text)
import FRP.Rhine.Gloss.Common (Picture)
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import Control.Lens (ifoldMap)
import Witch (into)

-- futurePosterior :: (MonadInfer m, Diff td ~ Double, TimeDomain td) => BehaviourF m td Observation Position
futurePosterior :: SignalFunction (Stochastic & Unnormalized)
  (V2 Double)
  Position
futurePosterior = proc (V2 oX oY) -> do
  latent <- prior -< ()
  shifted@(V2 trueX trueY) <- shift 100 -< latent
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent


future, past, pastFilter, allPast :: SignalFunction Stochastic Text Picture
future = proc _ -> do
            actualPosition <- prior -< ()
            thePast <- shift 100 -< actualPosition
            measuredPosition <- observationModel -< thePast
            samples <- particleFilter params futurePosterior -< measuredPosition
            renderObjects -< Result {
                                particles = samples
                                , measured = thePast
                                , latent = actualPosition
                                }

past = proc _ -> do
            actualPosition <- prior -< ()
            thePast <- shift 100 -< actualPosition
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter params (posterior >>> shift 100) -< measuredPosition
            renderObjects -< Result {
                                particles = samples
                                , measured = thePast
                                , latent = actualPosition
                                }

allPast = proc _ -> do
            actualPosition <- prior -< ()
            thePast <- shift 50 -< actualPosition
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter params {n = 50} (posterior >>> accumulateWith (\x xs -> take 50 $ x : xs) []) -< measuredPosition
            let flattened = (\(ps, ld) -> ifoldMap (\i x -> if i `mod` 5 == 0 then [(x,i :: Int)] else mempty) $ zip ps (repeat ld)) =<< samples


            past <- drawBall' -< (thePast, 0.1, red)
            ball <- drawBall' -< (actualPosition, 0.1, yellow)
            parts <- fold <$> mapMSF drawParticle -< flattened
            returnA -< (past <> ball <> parts)

drawParticle ::  Monad m => MSF m ((Position, Log Double), Int) Picture
drawParticle = proc ((position, probability), i) -> do
  let ratio = fromIntegral i / 60
  drawBall' -< (position, 0.05, withAlpha (into @Float $ exp $ 0.2 * ln probability)
    (mixColors ratio (1 - ratio) red yellow))


pastFilter = proc _ -> do
            actualPosition <- prior -< ()
            thePast <- shift 100 -< actualPosition
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter params posterior >>> shift 100 -< measuredPosition
            renderObjects -< Result {
                                particles = samples
                                , measured = thePast
                                , latent = actualPosition
                                }

renderObjects ::  Monad m => MSF m Result Picture
renderObjects = proc Result { particles, measured, latent} -> do

  observation <- drawBall' -< (measured, 0.1, red)
  ball <- drawBall' -< (latent, 0.1, makeColorI 255 239 0 255)
  parts <- fold <$> mapMSF drawParticle' -< particles
  returnA -< (observation <> ball <> parts)

shift :: Monad m => Int -> MSF m c c
shift n = accumulateWith (\x xs -> take n $ x : xs) [] >>> arr last
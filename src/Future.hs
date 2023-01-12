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
{-# LANGUAGE LambdaCase #-}

module Future where


import Control.Monad.Bayes.Class ( normalPdf, MonadSample (normal) )
import FRP.Rhine
    ( reactimateCl,
      Arrow(arr),
      MSF,
      returnA,
      (>>>),
      accumulateWith,
      withSideEffect_,
       )
import Inference (particleFilter, observe, SignalFunction, Stochastic, type (&), Unnormalized, params, SMCSettings (n), Deterministic)
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
      posterior,
      visualisation, drawBall', drawParticle', Observation, decayingIntegral )
import Linear (V2(..))
import Data.Text (Text)
import FRP.Rhine.Gloss.Common (Picture)
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import Control.Lens (ifoldMap)
import Witch (into)
import Control.Arrow ((&&&))
import Data.MonadicStreamFunction (constM)
import Data.MonadicStreamFunction
import FRP.Rhine (integralFrom)
import Text.Megaparsec.Char.Lexer (decimal, float)
import Text.Megaparsec (MonadParsec(eof), runParser)
import Data.Void (Void)
import Concurrent (GlossInput, keys)
import Graphics.Gloss.Data.Picture (text)
import FRP.Rhine.Gloss (translate)
import Control.Lens.Getter ((^.))
import Control.Lens (to)
import Data.Set (toList)
import FRP.Rhine.Gloss (Key(SpecialKey))
import FRP.Rhine.Gloss (SpecialKey(KeyUp))
import FRP.Rhine.Gloss (SpecialKey(KeyDown))
import FRP.Rhine.Gloss (Key(Char))
import Control.Lens.At (Contains(..))

-- futurePosterior :: (MonadInfer m, Diff td ~ Double, TimeDomain td) => BehaviourF m td Observation Position
futurePosterior :: SignalFunction (Stochastic & Unnormalized)
  (V2 Double, Double)
  Position
futurePosterior = proc (V2 oX oY, std) -> do
  latent <- stochasticOscillator 0 1 &&& stochasticOscillator 0 1 -< std
  shifted@(trueX, trueY) <- shift 100 -< latent
  observe -< normalPdf oY 0.1 trueY * normalPdf oX 0.1 trueX
  returnA -< uncurry V2 latent


-- | Harmonic oscillator with white noise
stochasticOscillator :: 
  -- | Starting position
  Double ->
  -- | Starting velocity
  Double ->
  SignalFunction Stochastic Double Double
stochasticOscillator initialPosition initialVelocity = feedback 0 $ proc (stdDev, position') -> do
  impulse <- arrM (normal 0) -< stdDev
  -- FIXME make -3 input, sample once at the beginning, or on every key stroke
  let acceleration = (-3) * position' + impulse
  -- Integral over roughly the last 100 seconds, dying off exponentially, as to model a small friction term
  velocity <- arr (+ initialVelocity) <<< decayingIntegral 100 -< acceleration
  position <- integralFrom initialPosition -< velocity
  returnA -< (position, position)


interpret str = either (const 0.1) id $ runParser @Void (float <* eof) "" str

toggle :: Char -> Bool -> SignalFunction Deterministic GlossInput Bool 
toggle char initialVal = proc glossInput -> do
    accumulateWith id initialVal -< glossInput ^. keys . contains (Char char) . to \case 
        True -> not
        False -> id

observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
    (x,y) <- (noise &&& noise) -< ()
    returnA -< p + V2 x y
    where noise = constM (normal 0 0.1)

past, pastFilter, allPast :: SignalFunction Stochastic Text Picture
future :: SignalFunction Stochastic GlossInput Picture 
future = proc glossInput -> do
            std <- accumulateWith id 0.1 -< glossInput ^. keys . to ((\case 
                (SpecialKey KeyUp : _) -> (+0.1)
                (SpecialKey KeyDown : _) -> (\x -> max (x - 0.1) 0.1)
                _ -> id
                ) . toList)

            showMeasured <- toggle 'o' True -< glossInput
            actualPosition <- stochasticOscillator 0 1 &&& stochasticOscillator 0 1 -< std
            thePast <- shift 100 -< actualPosition
            measuredPosition <- observationModel -< uncurry V2 thePast
            samples <- particleFilter params futurePosterior -< (measuredPosition, std)
            pic <- renderObjects -< Result {
                                particles = samples
                                , measured = if showMeasured then measuredPosition else 100
                                , latent = uncurry V2 actualPosition
                                }
            returnA -< pic <> translate 300 300 (text (show std))

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
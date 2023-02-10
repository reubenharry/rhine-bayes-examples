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



module Example where


import FRP.Rhine
import Inference (particleFilter, Stochastic, type (&), Unnormalized, SignalFunction, Deterministic, particleFilter, observe, hold, params)
import FRP.Rhine.Gloss
import Numeric.Log
import GHC.Float
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Population
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator (enumerate)
import Data.List (intersperse)
import qualified Data.Text.IO as T
import Control.Concurrent
import Control.Monad (void, forever)
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Foldable (Foldable(fold))
import Prelude hiding (Real)
import qualified Control.Monad.Morph as MM
import Data.Text (Text)
import Control.Monad.Trans.MSF.Except (performOnFirstSample)
import qualified Data.Vector as VV
import Linear (V2)
import Linear.V2 (V2(..))
import qualified Linear as L
import Witch (into)


std :: Double
std = 1

type Real = Double
type Observation = V2 Double
type Position = V2 Double

prior :: SignalFunction Stochastic () Position
prior = proc _ -> do
  x <- walk1D -< ()
  y <- walk1D -< ()
  returnA -< V2 x y

observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
    (x,y) <- (noise &&& noise) -< ()
    returnA -< p + V2 x y
    where noise = constM (normal 0 std)

dot :: SignalFunction Stochastic Text Picture
dot = proc _ -> do
  actualPosition <- prior -< ()
  observed <- observationModel -< actualPosition
  renderObjects -< Result observed actualPosition []

posterior :: SignalFunction (Stochastic & Unnormalized) Observation Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- prior -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

gloss :: SignalFunction Stochastic Text Picture
gloss = proc message -> do
  actualPosition <- prior -< ()
  measuredPosition <- observationModel -< actualPosition
  samples <- (particleFilter params posterior) -< measuredPosition
  (showObs, showParts) <- interpret -< message
  renderObjects -< Result 
    (if showObs then measuredPosition else 1000) 
    actualPosition 
    (if showParts then samples else [])


interpret :: Monad m => MSF
  m
  Text
  (Bool, Bool)
interpret = proc message -> do
  showParticles <- hold True -< case message of
                    "show particles" -> Just True
                    "don't show particles" -> Just False
                    _ -> Nothing
  showObservations <- hold True -< case message of
      "show observations" -> Just True
      "don't show observations" -> Just False
      _ -> Nothing
  returnA -< (showParticles, showObservations)


posteriorWeak :: SignalFunction (Stochastic & Unnormalized) Observation Position
posteriorWeak = proc (V2 oX oY) -> do
  (trueX, trueY) <- randomPosition &&& randomPosition -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< V2 trueX trueY

  where

    randomPosition = constM do
      uniform01 <- random
      return (10 * (uniform01 - 0.5))

weakPrior :: SignalFunction Stochastic Text Picture
weakPrior = proc _ -> do
            actualPosition <- prior -< ()
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter params posteriorWeak -< measuredPosition
            renderObjects -< Result {
                                particles = samples
                                , measured = measuredPosition
                                , latent = actualPosition
                                }



walk1D :: SignalFunction Stochastic () Double
walk1D = proc _ -> do
    dacceleration <- constM (normal 0 8 ) -< ()
    acceleration <- decayingIntegral 1 -< dacceleration
    velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
    position <- decayingIntegral 1 -< velocity
    returnA -< position

  -- where
decayingIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)


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


countCrosses :: SignalFunction Deterministic Position Int
countCrosses = proc pos -> do
  isInsideRadius <- arr ((>0.5) . L.norm) -< pos
  bool <- edge -< isInsideRadius
  returnA -< bool

postPred :: SignalFunction (Stochastic & Unnormalized) Observation (Position, Int)
postPred = proc obs -> do
  inferredPos <- posterior -< obs
  numTimesCrossRadius <- countCrosses -< inferredPos
  returnA -< (inferredPos, numTimesCrossRadius)

main :: SignalFunction Stochastic Text Picture
main = proc _ -> do
  actualPos <- prior -< ()
  measPos <- observationModel -< actualPos
  samples <- particleFilter params postPred -< measPos
  numberOfCrosses <- arr toTable -< samples
  objects <- renderObjects -< Result measPos actualPos (first fst <$> samples)
  probTable <- visualizeTable -< numberOfCrosses
  returnA -< objects <> probTable


----------
-- display
----------

-- gloss :: IO ()



toGlossC'' x  = do
  mvar <- liftIO $ newMVar ""
  _ <- liftIO $ void $ forkIO $ forever do
        x <- T.getLine
        swapMVar mvar x
  toGloss (constM (liftIO (swapMVar mvar "")) >>> x >>> arrM (lift . paintAllIO))


toGlossC' x  = do
  mvar <- liftIO $ newMVar ""
  _ <- liftIO $ void $ forkIO $ forever do
        x <- T.getLine
        swapMVar mvar x
  toGloss (constM (liftIO (swapMVar mvar "")) >>> morphS (MM.hoist lift) x >>> arrM (lift . paintAllIO))

toGloss = sampleIO .
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1724, 1260) (10, 10) }
        . reactimateCl glossClock






noObservations :: SignalFunction Stochastic Text Picture
noObservations = proc _ -> do
  actualPosition <- prior -< ()
  samples <- particleFilter params prior -< ()
  renderObjects -< Result {
                      particles = samples
                      , measured = 1000
                      , latent = actualPosition
                      }






toTable :: (Show b, Ord b) => [((a, b), Log Double)] -> [String]
toTable = intersperse "\n" . fmap (\(x,p) -> "Number: " <> show x <> "       Probability: " <> show p) . enumerate . empirical . fmap (first snd)

empirical :: MonadSample m => [(b, Log Double)] -> m b
empirical ls = do
  let (vs, ps) = unzip ls
  i <- logCategorical $ VV.fromList ps
  return (vs !! i)

edge :: Monad m => MSF m Bool Int
edge = feedback 0 proc (b, prev) -> do
  oldB <- iPre True -< b
  returnA -< (prev + if oldB==b then 0 else 1, prev + if oldB==b then 0 else 1)

edge' :: Monad m => MSF m Bool Bool
edge' = feedback False proc (b, prev) -> do
  returnA -< (prev==b, b)

edgeBy :: Monad m => (a -> Bool) -> MSF m a Int
edgeBy p = proc a -> do
  b <- arr p -< a
  e <- edge -< b
  returnA -< e

rep :: IO ()
rep = reactimate $ proc () -> do
  b <- constM (read @Integer <$> getLine) -< ()
  c <- edgeBy even -< b
  arrM print -< c

visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
visualisation = proc Result { particles, measured, latent} -> do

  drawBall -< (measured, 0.05, red)
  drawBall -< (latent, 0.15, makeColorI 255 239 0 255)
  drawParticles -< particles

renderObjects ::  Monad m => MSF m Result Picture
renderObjects = proc Result { particles, measured, latent} -> do

  observation <- drawBall' -< (measured, 0.05, red)
  ball <- drawBall' -< (latent, 0.15, makeColorI 255 239 0 255)
  parts <- fold <$> mapMSF drawParticle' -< particles
  returnA -< (observation <> ball <> parts)

visualizeTable :: Monad m => MSF m [String] Picture
visualizeTable = proc str -> do

  let table = translate 0 200 $ scale 0.1 0.1 $ pictures $ [translate 0 (height * 100) $ text l | (l, height) <- zip (reverse str) [1..]]
  let circ = circle 75
  returnA -< table <> circ

drawBall :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) ()
drawBall = proc (V2 x y, width, theColor) -> do
    arrMCl paintIO -<
        scale 150 150 $
        translate (double2Float x) (double2Float y) $
        color theColor $
        circleSolid $
        double2Float width

drawParticle :: MonadIO m => BehaviourF (GlossConcT m) td (Position, Log Double) ()
drawParticle = proc (position, probability) -> do
  drawBall -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) violet)

drawParticles :: MonadIO m => BehaviourF (GlossConcT m) td [(Position, Log Double)] ()
drawParticles = proc particles -> do
  case particles of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps

-- drawBall' :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) Picture
drawBall' :: Monad m => MSF m (V2 Double, Double, Color) Picture
drawBall' = proc (V2 x y, width, theColor) -> do
    returnA -<
        scale 150 150 $
        translate (double2Float x) (double2Float y) $
        color theColor $
        circleSolid $
        into @Float width

drawParticle' ::  Monad m => MSF m (Position, Log Double) Picture
drawParticle' = proc (position, probability) -> do
  drawBall' -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) violet)



data Result = Result
  {
    --   estimate :: Position
    -- stdDev :: Double
   measured :: Observation
  , latent :: Position
  , particles :: [(Position, Log Double)]
  }
  deriving Show

-- glossClock :: LiftClock (GlossConcT SamplerIO) IdentityT GlossSimClockIO
-- glossClock = liftClock GlossSimClockIO



glossClock :: RescaledClock GlossSimClockIO Double
glossClock = RescaledClock
  { unscaledClock = GlossSimClockIO
  , rescale = float2Double
  }


-- posterior2 :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td Observation Position
-- posterior2 :: MSF
--   (ReaderT
--      (TimeInfo (RescaledClock GlossSimClockIO Double))
--      (Population (GlossConcT SamplerIO)))
--   (V2 a)
--   Position




    -- isOutlier <- constM (bernoulli 0.1) -< ()
    -- if isOutlier then fmap (uncurry V2) $ outlier &&& outlier-< () else returnA -< p + n 
        -- outlier = constM ((\x -> 10 * (x - 0.5)) <$> random)

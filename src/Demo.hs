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



module Demo where


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
import Example ( renderObjects, Result (Result))
import Concurrent (GlossInput, keys, events)
import Control.Lens
import Future (toggle)
import Data.Set (toList)
import qualified Data.Set as S
import qualified Debug.Trace as Debug
import Control.Monad.Trans.MSF (MSFExcept)
import Control.Monad.Trans.MSF.Reader (ReaderT)
import qualified Example
import GUI (button, withFailure, slider, multipleSwitch)
import GUI (switch)

-- std :: Double
-- std = 1

type Real = Double
type Observation = Maybe (V2 Double)
type Position = V2 Double

type Std = Double

-- p :: SignalFunction Stochastic GlossInput Position


priorFrom v = proc glossInput -> do
            (+v) <$> Example.prior -< ()

moveWithArrows pos = proc glossInput -> do
            let velD = if glossInput ^. keys . contains (Char 'd') then V2 0.1 0 else 0
            let velA = if glossInput ^. keys . contains (Char 'a') then V2 (-0.1) 0 else 0
            let velW = if glossInput ^. keys . contains (Char 'w') then V2 0 0.1 else 0
            let velS = if glossInput ^. keys . contains (Char 's') then V2 0 (-0.1) else 0
            fmap (+pos) integral -< velD + velA + velW + velS


weakPrior :: SignalFunction Stochastic a Position
weakPrior = proc _ -> do
  (trueX, trueY) <- randomPosition &&& randomPosition -< ()
  returnA -< V2 trueX trueY

  where

    randomPosition = constM do
      uniform01 <- random
      return (10 * (uniform01 - 0.5))

prior :: SignalFunction Stochastic GlossInput Position
prior =
    multipleSwitch 1 signals where

        signals 1 = priorFrom
        signals 2 = const weakPrior
        signals _ = moveWithArrows

-- groundTruth :: SignalFunction Stochastic GlossInput Position
-- groundTruth =
--     multipleSwitch  signals where

--         signals 3 = priorFrom
--         signals 4 = const weakPrior
--         signals _ = moveWithArrows
        

    -- switch (withFailure priorFrom) (withFailure moveWithArrows)

generativeModel :: SignalFunction Stochastic (Position, Bool) Observation
generativeModel = proc (p, showObservation) -> do
    returnA -< if showObservation then Just p else Nothing


observationModel :: SignalFunction Stochastic (Position, Bool, Std) Observation
observationModel = proc (p, showObservation, std) -> do
    (x,y) <- (noise &&& noise) -< std
    returnA -< if showObservation then Just $ p + V2 x y else Nothing
    where noise = arrM (normal 0)





posterior :: SignalFunction (Stochastic & Unnormalized) (Observation, Bool, Std, GlossInput) Position
posterior = proc (obs, showObs, std, stay) -> do
  latent <- prior -< stay
  predictedObs <- generativeModel -< (latent, showObs)
  case (obs, predictedObs) of
    (Just (V2 oX oY), Just (V2 trueX trueY) ) ->
          observe -< normalPdf oY std trueY * normalPdf oX std trueX
    (Nothing, Nothing) -> returnA -< ()
    _ -> arrM factor -< 0
  returnA -< latent

gloss :: SignalFunction Stochastic GlossInput Picture
gloss = proc glossInput -> do

  (sliderPic, r) <- slider (V2 (-400) 400) 60 -< glossInput
  let std = 2 * r + 0.01
--   std <- accumulateWith id 0.1 -< glossInput ^. keys . to ((\case
--                 (SpecialKey KeyUp : _) -> (+0.1)
--                 (SpecialKey KeyDown : _) -> (\x -> max (x - 0.1) 0.1)
--                 _ -> id
--                 ) . toList)
  (buttonPic, withObservation) <- button True 20 (V2 400 400) -< glossInput
  actualPosition <- Example.prior -< ()
  measuredPosition <- observationModel -< (actualPosition, withObservation, std)
  samples <- particleFilter params posterior -< (measuredPosition, withObservation, std, glossInput)
--   (showObs, showParts) <- interpret -< message
  pic <- renderObjects -< Result
    (case measuredPosition of Just x -> x; _ -> 1000)
    actualPosition
    (if True then samples else [])
  returnA -< pic <> buttonPic <> sliderPic


-- interpret :: Monad m => MSF
--   m
--   Text
--   (Bool, Bool)
-- interpret = proc message -> do
--   showParticles <- hold True -< case message of
--                     "show particles" -> Just True
--                     "don't show particles" -> Just False
--                     _ -> Nothing
--   showObservations <- hold True -< case message of
--       "show observations" -> Just True
--       "don't show observations" -> Just False
--       _ -> Nothing
--   returnA -< (showParticles, showObservations)


-- posteriorWeak :: SignalFunction (Stochastic & Unnormalized) Observation Position
-- posteriorWeak = proc (V2 oX oY) -> do
--   (trueX, trueY) <- randomPosition &&& randomPosition -< ()
--   observe -< normalPdf oY std trueY * normalPdf oX std trueX
--   returnA -< V2 trueX trueY

--   where

--     randomPosition = constM do
--       uniform01 <- random
--       return (10 * (uniform01 - 0.5))

-- weakPrior :: SignalFunction Stochastic Text Picture
-- weakPrior = proc _ -> do
--             actualPosition <- prior -< ()
--             measuredPosition <- observationModel -< actualPosition
--             samples <- particleFilter params posteriorWeak -< measuredPosition
--             renderObjects -< Result {
--                                 particles = samples
--                                 , measured = measuredPosition
--                                 , latent = actualPosition
--                                 }



-- walk1D :: SignalFunction Stochastic () Double
-- walk1D = proc _ -> do
--     dacceleration <- constM (normal 0 8 ) -< ()
--     acceleration <- decayingIntegral 1 -< dacceleration
--     velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
--     position <- decayingIntegral 1 -< velocity
--     returnA -< position

--   -- where
-- decayingIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)


-- -- | Harmonic oscillator with white noise
-- stochasticOscillator :: 
--   -- | Starting position
--   Double ->
--   -- | Starting velocity
--   Double ->
--   SignalFunction Stochastic Double Double
-- stochasticOscillator initialPosition initialVelocity = feedback 0 $ proc (stdDev, position') -> do
--   impulse <- arrM (normal 0) -< stdDev
--   -- FIXME make -3 input, sample once at the beginning, or on every key stroke
--   let acceleration = (-3) * position' + impulse
--   -- Integral over roughly the last 100 seconds, dying off exponentially, as to model a small friction term
--   velocity <- arr (+ initialVelocity) <<< decayingIntegral 100 -< acceleration
--   position <- integralFrom initialPosition -< velocity
--   returnA -< (position, position)




-- countCrosses :: SignalFunction Deterministic Position Int
-- countCrosses = proc pos -> do
--   isInsideRadius <- arr ((>0.5) . L.norm) -< pos
--   bool <- edge -< isInsideRadius
--   returnA -< bool

-- postPred :: SignalFunction (Stochastic & Unnormalized) Observation (Position, Int)
-- postPred = proc obs -> do
--   inferredPos <- posterior -< obs
--   numTimesCrossRadius <- countCrosses -< inferredPos
--   returnA -< (inferredPos, numTimesCrossRadius)

-- main :: SignalFunction Stochastic Text Picture
-- main = proc _ -> do
--   actualPos <- prior -< ()
--   measPos <- observationModel -< actualPos
--   samples <- particleFilter params postPred -< measPos
--   numberOfCrosses <- arr toTable -< samples
--   objects <- renderObjects -< Result measPos actualPos (first fst <$> samples)
--   probTable <- visualizeTable -< numberOfCrosses
--   returnA -< objects <> probTable


-- ----------
-- -- display
-- ----------

-- -- gloss :: IO ()



-- toGlossC'' x  = do
--   mvar <- liftIO $ newMVar ""
--   _ <- liftIO $ void $ forkIO $ forever do
--         x <- T.getLine
--         swapMVar mvar x
--   toGloss (constM (liftIO (swapMVar mvar "")) >>> x >>> arrM (lift . paintAllIO))


-- toGlossC' x  = do
--   mvar <- liftIO $ newMVar ""
--   _ <- liftIO $ void $ forkIO $ forever do
--         x <- T.getLine
--         swapMVar mvar x
--   toGloss (constM (liftIO (swapMVar mvar "")) >>> morphS (MM.hoist lift) x >>> arrM (lift . paintAllIO))

-- toGloss = sampleIO .
--         launchGlossThread defaultSettings
--             { display = InWindow "rhine-bayes" (1724, 1260) (10, 10) }
--         . reactimateCl glossClock






-- noObservations :: SignalFunction Stochastic Text Picture
-- noObservations = proc _ -> do
--   actualPosition <- prior -< ()
--   samples <- particleFilter params prior -< ()
--   renderObjects -< Result {
--                       particles = samples
--                       , measured = 1000
--                       , latent = actualPosition
--                       }






-- toTable :: (Show b, Ord b) => [((a, b), Log Double)] -> [String]
-- toTable = intersperse "\n" . fmap (\(x,p) -> "Number: " <> show x <> "       Probability: " <> show p) . enumerate . empirical . fmap (first snd)

-- empirical :: MonadSample m => [(b, Log Double)] -> m b
-- empirical ls = do
--   let (vs, ps) = unzip ls
--   i <- logCategorical $ VV.fromList ps
--   return (vs !! i)

-- edge :: Monad m => MSF m Bool Int
-- edge = feedback 0 proc (b, prev) -> do
--   oldB <- iPre True -< b
--   returnA -< (prev + if oldB==b then 0 else 1, prev + if oldB==b then 0 else 1)

-- edgeBy :: Monad m => (a -> Bool) -> MSF m a Int
-- edgeBy p = proc a -> do
--   b <- arr p -< a
--   e <- edge -< b
--   returnA -< e

-- rep :: IO ()
-- rep = reactimate $ proc () -> do
--   b <- constM (read @Integer <$> getLine) -< ()
--   c <- edgeBy even -< b
--   arrM print -< c

-- visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
-- visualisation = proc Result { particles, measured, latent} -> do

--   drawBall -< (measured, 0.05, red)
--   drawBall -< (latent, 0.3, makeColorI 255 239 0 255)
--   drawParticles -< particles

-- renderObjects ::  Monad m => MSF m Result Picture
-- renderObjects = proc Result { particles, measured, latent} -> do

--   observation <- drawBall' -< (measured, 0.05, red)
--   ball <- drawBall' -< (latent, 0.3, makeColorI 255 239 0 255)
--   parts <- fold <$> mapMSF drawParticle' -< particles
--   returnA -< (observation <> ball <> parts)

-- visualizeTable :: Monad m => MSF m [String] Picture
-- visualizeTable = proc str -> do

--   let table = translate 0 200 $ scale 0.1 0.1 $ pictures $ [translate 0 (height * 100) $ text l | (l, height) <- zip (reverse str) [1..]]
--   let circ = circle 75
--   returnA -< table <> circ

-- drawBall :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) ()
-- drawBall = proc (V2 x y, width, theColor) -> do
--     arrMCl paintIO -<
--         scale 150 150 $
--         translate (double2Float x) (double2Float y) $
--         color theColor $
--         circleSolid $
--         double2Float width

-- drawParticle :: MonadIO m => BehaviourF (GlossConcT m) td (Position, Log Double) ()
-- drawParticle = proc (position, probability) -> do
--   drawBall -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) violet)

-- drawParticles :: MonadIO m => BehaviourF (GlossConcT m) td [(Position, Log Double)] ()
-- drawParticles = proc particles -> do
--   case particles of
--     [] -> returnA -< ()
--     p : ps -> do
--       drawParticle -< p
--       drawParticles -< ps

-- -- drawBall' :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) Picture
-- drawBall' :: Monad m => MSF m (V2 Double, Double, Color) Picture
-- drawBall' = proc (V2 x y, width, theColor) -> do
--     returnA -<
--         scale 150 150 $
--         translate (double2Float x) (double2Float y) $
--         color theColor $
--         circleSolid $
--         into @Float width

-- drawParticle' ::  Monad m => MSF m (Position, Log Double) Picture
-- drawParticle' = proc (position, probability) -> do
--   drawBall' -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) violet)



-- data Result = Result
--   {
--     --   estimate :: Position
--     -- stdDev :: Double
--    measured :: Observation
--   , latent :: Position
--   , particles :: [(Position, Log Double)]
--   }
--   deriving Show

-- -- glossClock :: LiftClock (GlossConcT SamplerIO) IdentityT GlossSimClockIO
-- -- glossClock = liftClock GlossSimClockIO



-- glossClock :: RescaledClock GlossSimClockIO Double
-- glossClock = RescaledClock
--   { unscaledClock = GlossSimClockIO
--   , rescale = float2Double
--   }


-- -- posterior2 :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td Observation Position
-- -- posterior2 :: MSF
-- --   (ReaderT
-- --      (TimeInfo (RescaledClock GlossSimClockIO Double))
-- --      (Population (GlossConcT SamplerIO)))
-- --   (V2 a)
-- --   Position




--     -- isOutlier <- constM (bernoulli 0.1) -< ()
--     -- if isOutlier then fmap (uncurry V2) $ outlier &&& outlier-< () else returnA -< p + n 
--         -- outlier = constM ((\x -> 10 * (x - 0.5)) <$> random)


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
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}


module Communication where
import Inference (SignalFunction, Stochastic, type (&), Unnormalized, particleFilter, InputOutput, withReaderS, SMCSettings (n), params)
import Control.Monad.Bayes.Class (MonadSample(uniformD, logCategorical, bernoulli), condition, factor, MonadInfer)
import Data.MonadicStreamFunction (arrM)
import Control.Arrow (returnA)
import Data.Text (Text)
import FRP.Rhine.Gloss (Picture)
import Graphics.Gloss.Data.Picture
import Data.MonadicStreamFunction (morphS)
import Control.Monad.Bayes.Enumerator (enumerate, explicit, Enumerator, logExplicit)
import Data.MonadicStreamFunction (morphGS)
import Control.Monad.Morph (MFunctor(hoist))
import Data.MonadicStreamFunction (MSF)
import Data.MonadicStreamFunction (constM)
import Control.Monad.Bayes.Population (resampleMultinomial)
import qualified Data.Vector as VV
import Numeric.Log (Log (..))
import Data.MonadicStreamFunction (iPre)
import Control.Monad.IO.Class (MonadIO(..))
import FRP.Rhine (ClSF)
import Data.MonadicStreamFunction.InternalCore (MSF(unMSF, MSF))
import Data.MonadicStreamFunction (Arrow(..))
import Data.Functor (($>))
import Control.Monad.Bayes.Sampler (SamplerIO, sampleIO)
import qualified Control.Monad.Trans.MSF.Reader as DunaiReader
import Control.Monad.Trans.Class (MonadTrans(lift))
import FRP.Rhine.ClSF.Except (throwOn')
import FRP.Rhine.ClSF.Except (try)
import FRP.Rhine.ClSF.Except (safely)
import Debug.Trace (traceM)
import Control.Arrow ((>>>))
import FRP.Rhine (waitClock)
import FRP.Rhine (reactimateCl)
import Control.Monad.Trans.MSF (performOnFirstSample)
import Data.MonadicStreamFunction (feedback)
-- import Prelude hiding (sum)

-- exact :: forall cl a b m . (Ord b, Monad m) =>
--   ClSF (Enumerator) cl a b
--   -> ClSF m cl a [(b, Double)]
-- exact = withReaderS runEnumerator 

-- runEnumerator :: forall m a b . (Ord b, Monad m) =>
--   -- | Number of particles
--   MSF (Enumerator) a b
--   -> MSF m a [(b, Double)]
-- runEnumerator msf = particleFilter'' $ pure msf
--   where
--     particleFilter'' :: Ord b => Enumerator (MSF (Enumerator) a b) -> MSF m a [(b, Double)]
--     particleFilter'' msfs = MSF $ \a -> do
--       traceM "1"
--       -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
--       let bAndMSFs = empirical $ take 32 $ fmap (second (Exp . log)) $ explicit $ (removeZeros $ flip unMSF a =<< msfs)
--       -- FIXME This abominal lambda could be done away by using Weighted?
--       traceM "2"
--       let (currentPopulation, continuations) = (enumerate $ fst <$> bAndMSFs, snd <$> bAndMSFs)
--       traceM "3"
--         --unzip $ (\((b, msf), weight) -> ((b, weight), (msf, weight))) <$> bAndMSFs
--       -- FIXME This normalizes, which introduces bias, whatever that means
--       return (currentPopulation, particleFilter'' continuations)

type World = Bool
type Utterance = Bool

-- removeZeros :: Enumerator a -> Enumerator a
-- removeZeros x = 
--         let y = logExplicit x
--         in empirical $ filter ((/=0) . snd) y 

-- type Observation = Maybe Position 

-- ex :: SignalFunction (Stochastic & Unnormalized) () World
-- ex :: SignalFunction Stochastic () World 
ex :: MonadInfer m => ClSF m cl () Bool
ex = proc () -> do
    x <- arrM $ const $ lift $ uniformD [True, False] -< ()
    arrM condition -< (x == True)
    returnA -< x

-- main :: MonadInfer m => MSF m  Text Picture
-- main :: SignalFunction Stochastic Text Picture
-- main = proc _ -> do 
--     s <- (exact ex )   -< ()
--     returnA -< scale 0.1 0.1 $ text $ show s

l0 :: SignalFunction Stochastic () World
l0 = bsta True
    -- constM $ lift $ uniformD [True, False] -< ()

-- m :: IO ()
-- m = reactimateCl (waitClock @100) $ exact 
--     (csta)
--     -- (performOnFirstSample $ lift $ uniformD [constM $ pure True, constM $ pure False]) 
--     >>> arrM (liftIO . print)


-- csta = feedback True proc ((), b) -> do
--     newB <- constM $ bernoulli 0.99 -< ()
--     let newB' = if newB then b else not b
--     returnA -< (newB', newB')

bsta :: MonadSample m => Bool -> ClSF m cl () Bool
bsta b = safely (loop b) where 
    loop b = do
        y <- try $ fsta b
        loop $ not y
-- abortivePrior :: (MonadSample m, Diff (Time cl) ~ Double, Time cl ~ Double) => ClSF (ExceptT Position m) cl () Position
fsta a = proc () -> do
    x <- constM $ pure a -< ()
    s <- constM $ lift $ lift $ uniformD [True, False] -< ()
    -- s <- constM $ lift $ lift $ bernoulli 0.05 -< ()
    _ <- throwOn' -< (s, x)
    returnA -< x

speaker :: SignalFunction Stochastic World Utterance 
speaker = proc world -> do
     noise <- constM $ lift $ uniformD [not, id, id, id] -< ()
     returnA -< noise world 

listener :: SignalFunction (Stochastic & Unnormalized) Utterance World
listener = proc obs -> do 
    p <- l0 -< ()
    arrM traceM -< show p
    s <- speaker -< p
    arrM condition -< s == obs
    returnA -< p 

speaker2 :: SignalFunction (Stochastic & Unnormalized) World Utterance
speaker2 = proc world -> do

    utterance <- speaker -< world
    prevUtt <- iPre True -< utterance
    population <- particleFilter params {n = 30} listener -< prevUtt
    mass  <- arr (\(p,w) -> sum $ fmap snd $ filter ((== w) . fst) p) -< (population, world)
    arrM factor -< mass
    returnA -< utterance 

main :: SignalFunction (Stochastic & InputOutput) Text Picture
main = proc _ -> do
    constM $ liftIO getLine -< ()
    w <- bsta True -< ()
    arrM traceM -< show w
    -- w <- constM ((read :: String -> Bool) <$> liftIO getLine) -< ()
    utt <- speaker -< w
    -- arrM traceM -< show utt
    samples <- particleFilter params {n = 30} listener -< utt
    -- arrM traceM -< show samples

    samples2 <- particleFilter params {n = 30} speaker2 -< w
    returnA -< pictures [
        scale 0.1 0.1 $ text $ show $ enumerate $ empirical samples, 
        translate 0 (-100) $ scale 0.1 0.1 $ text $ ("Speaker: " <> show (enumerate $ empirical samples2)), 
        translate 0 (-200) $ scale 0.1 0.1 $ text $ ("World: " <> show w ),
        translate 0 (-300) $ scale 0.1 0.1 $ text $ ("Utterance: " <> show utt )
        ]




-- GOOD: sequentially enumerate plz...


-- s1 :: (MonadInfer m) => MSF m Bool (Maybe Bool)
-- s1 = proc state -> do

--   utterance <- s0 -< state
--   state' <- particleFilter' 100 resampleMultinomial l0 -< utterance
--   state'' <- arrM fromCategorical -< state'
--   arrM condition -< state'' == state
--   returnA -< utterance

empirical :: MonadSample m => [(b, Log Double)] -> m b
empirical ls = do
  let (vs, ps) = unzip ls
  i <- logCategorical $ VV.fromList ps
  return $ vs !! i

-- l0 :: MonadInfer m => MSF m (Maybe Bool) Bool
-- l0 = proc u -> do
--   noise <- constM (bernoulli 0.1) -< ()
--   s <- performOnFirstSample (uniformD [constM $ pure True, constM $ pure False]) -< ()
--   case u of 
--     Just u' -> arrM condition -< u' == (if noise then not s else s)
--     Nothing -> returnA -< ()
--   returnA -< s

-- s0 :: MonadSample m => MSF m Bool (Maybe Bool)
-- s0 = proc s -> do
--   noise <- constM (bernoulli 0.1) -< ()
--   silence <- constM (bernoulli 0.5 ) -< ()
--   returnA -< if silence then Nothing else Just if noise then not s else s

-- pragmatics = sampleIO $ reactimate proc () -> do 
--   w <- constM (read <$> liftIO getLine) -< ()
--   u <- particleFilter' 100 (resampleMultinomial) s1 -< w
--   arrM (liftIO . print) -< foldr (\(k,v) -> M.alter (\case Nothing -> Just v; Just a -> Just (v+a)) k) mempty u
--   -- averageOf $ fmap (first (\case True -> 1; False -> 0)) u
--   returnA -< ()









-- a semantics: SF Utterance World


-- truthfulSpeaker :: MonadInfer m => MSF m World Utterance
-- truthfulSpeaker = proc state -> do
--   utterance <- constM $ uniformD [1, 2, 3] -< ()
--   arrM condition -< sem state utterance
--   returnA -< utterance

-- sem :: World -> Utterance -> Bool
-- sem = (>=)

-- listener :: MonadInfer m => MSF m Utterance World
-- listener = proc utterance -> do
--   state <- constM $ uniformD [1,2,3] -< ()
--   utterance' <- truthfulSpeaker -< state 
--   arrM condition -< utterance == utterance'
--   -- utterance' <- particleFilter' 10 resampleMultinomial  truthfulSpeaker -< state
--   -- let pmf = foldr (\(k,v) -> M.alter (\case Nothing -> Just v; Just a -> Just (v+a)) k) mempty utterance'
--   -- observe -< fromMaybe 0 $ M.lookup utterance pmf
--   returnA -< state

-- speaker :: (MonadInfer m, MonadIO m) => MSF m World Utterance
-- speaker = proc state -> do
--   utterance <- truthfulSpeaker -< state
--   -- state' <- listener -< utterance
--   -- arrM condition -< state' == state
--   state' <- particleFilter' 100 resampleMultinomial listener -< utterance
--   let pmf = foldr (\(k,v) -> M.alter (\case Nothing -> Just v; Just a -> Just (v+a)) k) mempty state'
--   let mass = (**10) $ fromMaybe 1 $ M.lookup state (pmf)
--   -- arrM (liftIO . print) -< (utterance, mass)
--   observe -< mass
--   returnA -< utterance

-- -- traceIt x = D.trace (show x) x



-- pragmaticsL = sampleIO $ reactimate proc () -> do 
--   u <- constM (read <$> liftIO getLine) -< ()
--   w <- particleFilter' 1000 (resampleMultinomial . resampleMultinomial) listener -< u
--   arrM (liftIO . print) -< averageOf $ fmap (first fromIntegral) w
--   returnA -< ()

-- -- simplest model:
--   -- 

-- unbiased x = do
--    let ps = VV.fromList $ snd <$> x
--    let xs = fst <$> x
--    i <- logCategorical ps
--    return (xs !! i)

--   -- utterance <- truthfulSpeaker -< state
-- time = sampleIO $ reactimate proc () -> do
--   rec
--     samples <- particleFilter' 100 (resampleMultinomial) speaker -< 2
--     u <- arrM unbiased -< samples
--     samples2 <- particleFilter' 100 resampleMultinomial listener -< u
--     state <- arrM unbiased -< samples2
--   arrM (liftIO . print) -< (u, state)
--   returnA -< ()


-- -- time :: (MonadInfer m, MonadIO m) => MSF m () ()
-- -- time = proc () -> do
-- --   -- rec
-- --     u <-  truthfulSpeaker -< 1
-- --     state <- listener -< u
-- --     arrM (liftIO . print) -< (u, state)

-- -- run = sampleIO $ reactimate $ (particleFilter' 100 resampleMultinomial time >>> constM (pure ()))

--     -- (lState, prior) <- smc listener <- (utterance, prior)
--     -- condition $ state == lState
--     -- returnA <- utterance

--   -- listener = proc utterance -> do
--       -- prior <- hyperprior
--       -- state <- prior
--       -- condition $ sem state utterance
--       -- returnA state

--     -- recursive do for interacting agents? for speaker??

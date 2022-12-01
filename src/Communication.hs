{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Communication where
import Data.MonadicStreamFunction
import qualified Control.Category as C
import Control.Monad.Bayes.Class (factor, MonadInfer, condition, MonadSample (uniformD, random, normal, logCategorical, bernoulli), normalPdf)
import Control.Monad.Bayes.Sampler
import Inference
import Control.Monad.Bayes.Population
import TwoStream (liftMSF)
import Control.Monad.IO.Class
import qualified Data.Map as M
import Numeric.Log
import Data.List (sortOn)
import Data.Ord (Down(..))
import Control.Monad.Fix
import FRP.Rhine ((^-^), VectorSpace (norm, zeroVector, (^+^), (*^), (^/)), ClSF)
-- import Prelude hiding (sum)
import Data.Tuple (swap)
import Data.Functor.Identity
import FRP.Rhine.Gloss
import Example (Result(..), glossClock, std)
import Control.Monad.Trans.Class
import Example (visualisation)
import qualified Data.Vector.Sized as V
import GHC.Float
import qualified Example
import Control.Monad.Trans.Reader (ReaderT)
import Data.Coerce (coerce)
import qualified Control.Monad.Morph as MM
import Data.Maybe
import qualified Debug.Trace as D
import qualified Data.Vector as VV
import Control.Monad.Bayes.Weighted
import Control.Monad.Trans.MSF (performOnFirstSample)


type World = Int
type Utterance = Int


-- s1 :: (MonadInfer m) => MSF m Bool (Maybe Bool)
-- s1 = proc state -> do
  
--   utterance <- s0 -< state
--   state' <- particleFilter' 100 resampleMultinomial l0 -< utterance
--   state'' <- arrM fromCategorical -< state'
--   arrM condition -< state'' == state
--   returnA -< utterance

-- fromCategorical :: MonadSample m => [(b, Log Double)] -> m b
-- fromCategorical ls = do
--   let (vs, ps) = unzip ls
--   i <- logCategorical $ VV.fromList ps
--   return $ vs !! i

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

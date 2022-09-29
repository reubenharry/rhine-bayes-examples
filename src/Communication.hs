{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Communication where
import Data.MonadicStreamFunction
import qualified Control.Category as C
import Control.Monad.Bayes.Class (factor, MonadInfer, condition, MonadSample (uniformD, random, normal, logCategorical), normalPdf)
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

type World = Int
type Utterance = Int



truthfulSpeaker :: MonadInfer m => MSF m World Utterance
truthfulSpeaker = proc state -> do
  utterance <- constM $ uniformD [1, 2, 3] -< ()
  arrM condition -< sem state utterance
  returnA -< utterance

sem :: World -> Utterance -> Bool
sem = (>=)

listener :: MonadInfer m => MSF m Utterance World
listener = proc utterance -> do
  state <- constM $ uniformD [1,2,3] -< ()
  utterance' <- truthfulSpeaker -< state 
  arrM condition -< utterance == utterance'
  -- utterance' <- onlineSMC' 10 resampleMultinomial  truthfulSpeaker -< state
  -- let pmf = foldr (\(k,v) -> M.alter (\case Nothing -> Just v; Just a -> Just (v+a)) k) mempty utterance'
  -- arrM factor -< fromMaybe 0 $ M.lookup utterance pmf
  returnA -< state

speaker :: (MonadInfer m, MonadIO m) => MSF m World Utterance
speaker = proc state -> do
  utterance <- truthfulSpeaker -< state
  -- state' <- listener -< utterance
  -- arrM condition -< state' == state
  state' <- onlineSMC' 100 resampleMultinomial listener -< utterance
  let pmf = foldr (\(k,v) -> M.alter (\case Nothing -> Just v; Just a -> Just (v+a)) k) mempty state'
  let mass = (**10) $ fromMaybe 1 $ M.lookup state (pmf)
  -- arrM (liftIO . print) -< (utterance, mass)
  arrM factor -< mass
  returnA -< utterance

-- traceIt x = D.trace (show x) x

pragmatics = sampleIO $ reactimate proc () -> do 
  w <- constM (read <$> liftIO getLine) -< ()
  u <- onlineSMC' 100 (resampleMultinomial . resampleMultinomial) speaker -< w
  arrM (liftIO . print) -< averageOf $ fmap (first fromIntegral) u
  returnA -< ()

pragmaticsL = sampleIO $ reactimate proc () -> do 
  u <- constM (read <$> liftIO getLine) -< ()
  w <- onlineSMC' 1000 (resampleMultinomial . resampleMultinomial) listener -< u
  arrM (liftIO . print) -< averageOf $ fmap (first fromIntegral) w
  returnA -< ()

-- simplest model:
  -- 

unbiased x = do
   let ps = VV.fromList $ snd <$> x
   let xs = fst <$> x
   i <- logCategorical ps
   return (xs !! i)

  -- utterance <- truthfulSpeaker -< state
time = sampleIO $ reactimate proc () -> do
  rec
    samples <- onlineSMC' 100 (resampleMultinomial) speaker -< 2
    u <- arrM unbiased -< samples
    samples2 <- onlineSMC' 100 resampleMultinomial listener -< u
    state <- arrM unbiased -< samples2
  arrM (liftIO . print) -< (u, state)
  returnA -< ()


-- time :: (MonadInfer m, MonadIO m) => MSF m () ()
-- time = proc () -> do
--   -- rec
--     u <-  truthfulSpeaker -< 1
--     state <- listener -< u
--     arrM (liftIO . print) -< (u, state)

-- run = sampleIO $ reactimate $ (onlineSMC' 100 resampleMultinomial time >>> constM (pure ()))

    -- (lState, prior) <- smc listener <- (utterance, prior)
    -- condition $ state == lState
    -- returnA <- utterance

  -- listener = proc utterance -> do
      -- prior <- hyperprior
      -- state <- prior
      -- condition $ sem state utterance
      -- returnA state

    -- recursive do for interacting agents? for speaker??

type State = (Double, Double)

type Observation = Either Double Double

type Action = Bool

-- prior :: MonadSample m => ClSF m td () State
-- prior = constM (pure (0,0)) >>> liftMSF (do 
--     x <- random
--     y <- random
--     return $ constM (pure (x,y))
--     -- (uniformD [constM (pure (x,y)) | x <- [1,2,3], y <- [1,2,3]])
--     )

generativeModel :: MonadSample m => BehaviourF m td (State, Action) Observation
generativeModel = proc ((d1,d2), bool) -> do
    if bool then returnA -< Left d1 else returnA -< Right d2

instance MonadFix SamplerIO where
    mfix f = liftIO (mfix (sampleIO . f))

-- instance MonadFix (GlossConcT SamplerIO) where
--     mfix = undefined


-- posterior :: (MonadInfer m) => MSF m Observation State
posterior :: (Diff td ~ Double, MonadInfer m) => BehaviourF m td
  (Either Double Double)
  (Double, Double)
posterior = proc observation -> do

    state <- fmap (\(V2 x y) -> (x,y)) Example.prior -< ()
    prediction <- generativeModel -< (state, case observation of Left _ -> True; _ -> False)
    arrM factor -< normalPdf 0 std (abs (either id id prediction - either id id observation))
    returnA -< state

example :: IO ()
example = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl Example.glossClock proc () -> do

                    -- actualPosition <- prior -< ()
                    -- measuredPosition <- generativeModel -< actualPosition
                    -- samples <- onlineSMC 200 resampleMultinomial posterior -< measuredPosition

                    (state, obs, samples) <- morphS (MM.hoist (lift :: Monad m => m a -> GlossConcT m a)) mainClSf -< ()
                    -- state <- Example.prior -< ()
                    (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                        particles = fmap (first V.fromTuple) samples
                                        , measured = V.fromTuple $ either (,-2) (-2,) obs
                                        , latent = V.fromTuple state
                                        }
                    -- undefined -< undefined

mainClSf :: (Diff td ~ Double) => BehaviourF SamplerIO td () (State, Observation, [((Double, Double), Log Double)])
mainClSf = proc () -> do
    state <- fmap (\(V2 x y) -> (x,y)) Example.prior -< ()
    rec
        delayed <- C.id *** iPre True -< (state, action)
        observation <- generativeModel -< delayed
        n <- constM (normal 0 std) -< ()
        let noised = case observation of Left o -> Left (o + n); Right r -> Right (r + n)
        samples <- onlineSMC 200 resampleMultinomial posterior -< noised
        let (p, p1, p2) = (fromWeightedList $ (pure :: a -> Identity a) samples, fst <$> p, snd <$> p)
        let stdOf = stdDevOf . runIdentity . runPopulation
        let (s1, s2) = stdOf *** stdOf $ (p1, p2)
        action <- arr (uncurry (>)) -< (s1,s2)
    returnA -< (state, noised , samples)

-- stdDevOf :: [(Int, Log Double)] -> Double
-- stdDevOf :: [(Pos, Log Double)] -> Double
-- averageOf :: VectorSpace v n => [(v, Log n)] -> v
averageOf things =
  let
    properThings = first (exp . ln) . swap <$> things
    fullWeight = Prelude.sum $ fst <$> properThings
    sumOfThings = foldr (+) 0 $ fmap (uncurry (*)) properThings
  in sumOfThings / fullWeight

-- stdDevOf :: [(Pos, Log Double)] -> Double
stdDevOf things =
  let
    average = averageOf things
    -- FIXME norm ^2 is wasteful
    squares = first (\x -> norm (x - average) ** 2) <$> things
  in sqrt $ averageOf squares

-- (constM (read <$> getLine) >>> counter >>> arrM print)

-- counter :: MSF IO () Int

-- counter = proc () -> do
--         rec
--                 -- num <- arr 
--             input <- iPre 0 >>> arr (+1) -< output
--             output <- arr (+1) -< input
--                 -- input <- count -< if n > 100 then 0 else 0 -- if input > 10 then 0 else next
--                 -- next <- (arrM (\x -> fmap (+x) (read <$> getLine))) -< input
--                 -- reset' <- arr not -< reset
--         returnA -< output
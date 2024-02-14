{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImpredicativeTypes #-}
module SDE where

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}


import Control.Monad ( (<=<) )
import GHC.Float (float2Double)

import Linear.V ( dim, V(V), Finite (fromV, toV), fromVector )
import Linear ((^*), (*^), Additive ((^+^)), Metric (norm, dot, signorm), (^/), V2 (V2))
import qualified Data.Vector as Vec
import GHC.TypeLits (Nat, KnownNat)
import Numeric.AD (grad)
import Data.MonadicStreamFunction (arrM, Arrow (first, arr, (***)), returnA, morphS, accumulateWith, constM)
import Data.MonadicStreamFunction.InternalCore ( MSF, feedback )
import Control.Monad.Bayes.Class (MonadDistribution(..), Log (..), MonadMeasure, condition, normalPdf)
import Control.Category ( Category((.), id) )
import Prelude hiding (id, (.))
import FRP.Rhine (timeless, ClSF, TimeDomain (Diff), Clock (Time, Tag), RescaledClock (unscaledClock, RescaledClock, rescale), tagS, collect, (>--), (-->), (@@), (>->), arrMCl, TimeInfo, keepLast, Rhine, Millisecond (Millisecond), LiftClock, RescaledClockS (RescaledClockS, unscaledClockS, rescaleS), UTCTime, MonadIO (liftIO), waitClock, liftClock, sinceLastS)
import FRP.Rhine.Gloss (red, GlossSettings (display), defaultSettings, Display (InWindow), GlossSimClockIO (GlossSimClockIO), scale, Picture, flowGlossIO, paintAllIO, Event, GlossEventClockIO (GlossEventClockIO))
import Control.Monad.Morph (MFunctor (hoist), MonadTrans (lift))
import Example (drawBall, drawParticle, countCrosses)
import Inference (particleFilter, params, SMCSettings (n))
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Foldable (Foldable(..))
import Util ( normalPdf2D, observe, type (>-->) )
import MainSF (toGloss, eventClock)
import GUI
    ( button, buttonParams, slider, ButtonConfig(buttonPos) )
import Control.Monad.Bayes.Sampler.Strict
import Concurrent (UserInput, handle, noInput)
import Control.Monad.Trans.MSF (ReaderT, performOnFirstSample)
import FRP.Rhine.Gloss.IO (GlossConcT)
import Data.Time (addUTCTime, getCurrentTime)
import Witch (into)
import Debug.Trace
import Demo (countDemoObservationModel)
import qualified Data.Vector as Vect
import Data.Maybe (fromMaybe)
import Control.Foldl (Fold(Fold))
import Numeric.Sum (KBNSum, kbn, Summation (..))
import qualified Data.Foldable as F
import qualified Control.Foldl as FOLD

-- to implement

-- OOH: constructing the distribution as separate sdes: consider: normal normal
-- stochastic gradient: change noise live 
-- smc? slowly anneal the distribution: make temp a parameter of the sde
-- do microcanonical ensemble mcmc directly: ooh: can we explicitly to the time rescaling (e.g. as a rescaled clock)? to get from microcanonical to the sde formulation?


-- questions: 

-- to what extend is mclmc close to geodesic motion on the information manifold


-- recall how to get a density from a ppl

-- hierarchical sdes? a ppl based on the sdes


-- what about non-markovian kernels? i.e. MSF PhaseSpace PhaseSpace: essentially that's already what HMC is: MSF Space Space (ony markovian in phase space)
-- OOOH: can I represent HMC directly as a process on space, rather than a markov process on phase space???

-- the sde as a coelgebra corresponding to the distribution (which is the fixState of the corecursion): reweight the coalgebra instead of reweighting the distribution
-- anything asynchronous here that's interesting?
-- feedback?

-- renormalization constructively: define MSF -> MSF which groups times 

-- what if you don't want to sample a fully stationary distribution? e.g. gaussian with moving mean

-- jakob:

-- the mixture: two ways of doing it: 1. two separate sdes, 2. one sde with a mixture of densities
-- particle filter to rule out excessive crossing of typical set: an automatic tuning method: we're uncertain of the correct L, so have an unnormalized SDE


data PhaseSpace (n :: Nat) a where
  State :: {position :: V dim a, momentum :: V dim a} -> PhaseSpace dim a
  deriving (Show, Eq)

type Operator m dim = PhaseSpace dim Double -> m (PhaseSpace dim Double)

type DensityFunction dim = forall a . Floating a => V dim a -> a

momentumUpdate :: (MonadDistribution m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> m (PhaseSpace dim Double)
momentumUpdate logdensity stepSize (State x u) = pure $ State x uu
    where
    g = negate $ grad logdensity x 
    g_norm = Linear.norm g
    d = fromIntegral $ dim g
    e = negate $ g ^/ g_norm
    delta = stepSize * g_norm / (d-1)
    uu = (u + e^*(sinh delta + dot e (u ^* ( cosh delta -1)))) Linear.^/ (   cosh delta  + Linear.dot e (u ^* sinh delta))

positionUpdate :: (Applicative f, KnownNat n, Num a) => a -> PhaseSpace n a -> f (PhaseSpace n a)
positionUpdate e (State x u) = pure $ State (x ^+^ e Linear.*^ u) u

positionUpdateEuclidean :: (Applicative f, KnownNat n, Num a, Floating a) => a -> PhaseSpace n a -> f (PhaseSpace n a)
positionUpdateEuclidean e (State x u) = pure $ State (x ^+^ e Linear.*^ g) u where
  g = grad kineticEnergy u
  
  
kineticEnergy :: (Foldable f, Functor f, Floating a) => f a -> a
kineticEnergy u = 0.5 * F.sum (fmap (**2) u)



momentumUpdateEuclidean :: forall dim m . (MonadDistribution m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> m (PhaseSpace dim Double)
momentumUpdateEuclidean logdensity stepSize (State x u) = pure $ State x uu
    where
    g = grad logdensity x  :: V dim Double
    uu = u + (stepSize *^ g)

-- = jax.tree_util.tree_map(
--             lambda x, grad: x + step_size * coef * grad,
--             momentum,
--             logdensity_grad,

maruyama :: forall n m . (KnownNat n, MonadDistribution m) => Double -> Double -> PhaseSpace n Double -> m (PhaseSpace n Double)
maruyama l stepSize (State x u) = State x <$> partialRefresh stepSize l (u :: V n Double)
-- maruyama l stepSize (State x u) = pure $ State x u

partialRefresh :: (KnownNat n, MonadDistribution f) => Double -> Double -> V n Double -> f (V n Double)
partialRefresh stepSize l momentum = signorm . (^+^ momentum) <$> z
    where
        d = fromIntegral $ Linear.V.dim momentum
        nu = sqrt ((exp (2 * stepSize / l) - 1.0) / d)
        z = (fmap . fmap) (nu *) (mapM (const (normal 0 1)) momentum)

partialRefresh2 :: (KnownNat n, MonadDistribution f) => Double -> Double -> V n Double -> f (V n Double)
partialRefresh2 stepSize l momentum = do
  x <- normal 0 1
  y <- normal 0 1
  b <- bernoulli 0.001
  let p = signorm $ fromMaybe undefined $ fromVector $ Vect.fromList [x, y]
  return $ if b then p ^* norm momentum else momentum

mclmc :: (MonadDistribution m, KnownNat n) => (DensityFunction n, DensityFunction n) -> m (PhaseSpace n Double) -> ClSF m cl (Double, Double, Bool) (V n Double)
mclmc (d1, d2) initial = arr position . proc (stepSize, l, b) -> do
    timeless (accumulateWithM integrator initial) -< (
        positionUpdate,
        momentumUpdate (if b then d1 else d2),
        mclachlanCoeffs,
        l,
        stepSize)

hmc :: (MonadDistribution m, KnownNat n) => (DensityFunction n, DensityFunction n) -> m (PhaseSpace n Double) -> ClSF m cl (Double, Double, Bool) (PhaseSpace n Double)
hmc (d1, d2) initial = proc (stepSize, l, b) -> do
    timeless (accumulateWithM integrator  initial) -< (
        positionUpdateEuclidean,
        -- (if b then momentumUpdateEuclidean else momentumUpdate) (if b then d1 else d2),
        momentumUpdateEuclidean (if b then d1 else d2),
        mclachlanCoeffs,
        l,
        stepSize)



mixture :: MonadDistribution m => MSF (ReaderT (TimeInfo cl) m) (Double, Double, Bool) (V 2 Double)
mixture = proc slb -> do
  a <- mclmc (normalLogDensity, normalLogDensity) initialState -< slb
  c <- mclmc (normalLogDensity, \x -> negate $ 0.5 * F.sum (fmap (**2) (x+5)))  initialState -< slb
  bool <- constM $ bernoulli 0.5 -< ()
  returnA -< if bool then a else c

-- mixture' :: MonadDistribution m => MSF (ReaderT (TimeInfo cl) m) (Double, Double, Bool) (PhaseSpace 2 Double)
-- mixture' = proc slb@(s,l,b) -> do
--   a <- mclmc (normalLogDensity, normalLogDensity) initialState -< slb
--   c <- mclmc (normalLogDensity, \x -> negate $ 0.5 * sum (fmap (**2) (x+5)))  initialState -< (s,l, norm (position a) < 1)
--   returnA -< c

directMixture :: MonadDistribution m => MSF (ReaderT (TimeInfo cl) m) (Double, Double, Bool) (V 2 Double)
directMixture = mclmc (\x -> negate (0.5 * F.sum (fmap (**2) (x+5))) - 0.5 * F.sum (fmap (**2) (x)) ,\x -> negate $ 0.5 * F.sum (fmap (**2) x)) initialState
  where
    -- d = \x -> (negate $ 0.5 * sum (fmap (**2) (x+5))) + (negate $ 0.5 * sum (fmap (**2) (x)))


instance (Num a, KnownNat n) => Num (PhaseSpace n a) where
  (State p m) + (State p' m') = State (p+p') (m + m')

posterior :: (MonadMeasure m) => ClSF m cl (V2 Int, Bool) (V 2 Double)
posterior = proc (obs, b) -> do
  p <- mclmc (banana, normalLogDensity) initialState -< (0.1, 1, b)
  i <- countDemoObservationModel -< fromV $  p
  -- arrM traceM -< show i
  -- arrM condition -< i == obs
  -- observe -< normalPdf 0 1 (norm $ position p)
  -- observe -< normalPdf2D (fromV $ position obs)  1 (fromV $ position p)
  -- last10 <- arr (take 100) . accumulateWith (:) [] -< p
  -- observe -<  (Exp $ log $ sum $ norm . momentum <$> last10)
  returnA -< p


constrainedHmc :: (MonadMeasure m, KnownNat n) => (DensityFunction n, DensityFunction n) -> m (PhaseSpace n Double) -> ClSF m cl (Double, Double, Bool) (PhaseSpace n Double)
constrainedHmc (d1, d2) initial = proc slb@(_,_,b) -> do
  p <- hmc (d1, d2) initial -< slb
  -- arrM traceM -< show (norm $ momentum p)
  if b then
    observe -< normalPdf 1 2 (norm $ momentum p)
  else returnA -< ()
  -- observe -< normalPdf 1 0.1 (norm $ position p)
  returnA -< p




leapfrogCoeffs, mclachlanCoeffs :: [Double]
leapfrogCoeffs = [0.5, 1]
mclachlanCoeffs = [0.1931833275037836, 0.5, 1 - 2 * head mclachlanCoeffs]



sf :: (Diff (Time cl) ~ Double, MonadDistribution m, TimeDomain (Time cl), Ord (Diff (Time cl)), Fractional (Diff (Time cl))) => MSF
  (ReaderT
     (TimeInfo cl)
     m)
  UserInput
  Picture
sf = proc userInput -> do
  (buttonPic, useBanana) <- button buttonParams {buttonPos = V2 (-350) 300} -< userInput
  (sliderPic, radius) <- slider (V2 (-300) 300) 60 -< userInput
  -- (sliderPic2, radius2) <- slider (V2 (-200) 300) 60 -< userInput
  -- picture <- arr (\b -> (if b then visualisation . particleFilter params {n=200} (mclmc banana initialState) else visualisation . particleFilter params {n=200} (mclmc normalLogDensity initialState)))
  -- picture <- visualisation . particleFilter params {n=200} (mclmc banana initialState) -< (radius)


  -- ob <- mclmc (banana, normalLogDensity) initialState -< (0.1, 1, useBanana)
  -- i <- countDemoObservationModel -< fromV $ position ob
  -- result <- particleFilter params{n=200} posterior -< (i, useBanana)
  -- pic <- arr (scale 0.1 0.1) . drawBall -< (fromV $ position ob , 0.1, red)
  -- picture <- visualisation -< result
  -- returnA -< picture <> buttonPic <> pic

  -- timeInterval <- sinceLastS -< ()
  -- result <- particleFilter params{n=200} (mclmc (banana, normalLogDensity) initialState) -< (10 * radius * timeInterval, 1, useBanana)
  -- -- result <- particleFilter params{n=200} mixture -< (10 * radius * timeInterval, 2, useBanana)
  -- -- result <- particleFilter params{n=200} mixture -< 10 * radius * timeInterval
  -- picture <- visualisation -< result
  -- returnA -< picture <> buttonPic <> sliderPic 


  timeInterval <- sinceLastS -< ()
  -- result <- particleFilter params{n=200} (mclmc (banana, normalLogDensity) initialState) -< (10.0 * radius * timeInterval, 1, useBanana)

  -- result2 <- mapMSF (baz (variance 0) *** baz (variance 0)) . arr ( (\(V2 x y) -> zip x y) . (fromV) . sequence . fmap (position . fst) . take 10000) -< result
  -- -- arrM traceM -< show result2
  -- b1 <- drawBall -< ((/20) $ uncurry V2 (dAv ( result2)), 0.1, red)



  -- result <- particleFilter params{n=200} (constrainedHmc (banana, banana) initialState) -< (10 * radius * timeInterval, 1, useBanana)
  result <- if useBanana then
    particleFilter params{n=200} (mclmc (\x -> log (exp (normalLogDensity x) + exp(normalLogDensity (x + 5)))  ,\x -> log ((exp (normalLogDensity x) + exp(normalLogDensity (x + 5))))) initialState) -< (10 * radius * timeInterval, 1, useBanana)
    else particleFilter params{n=200} mixture -< (10 * radius * timeInterval, 1, useBanana)
  -- result <- particleFilter params{n=200} directMixture -< (10 * radius * timeInterval, 2, useBanana)
  -- result <- particleFilter params{n=200} mixture -< 10 * radius * timeInterval
  picture <- visualisation -< result
  returnA -< picture <> buttonPic <> sliderPic 


-- dAv :: [(Double, Double)] -> (Double, Double)
-- dAv ldub = (FOLD.fold (FOLD.mean) *** FOLD.fold (FOLD.mean)) $ unzip  ldub


-- baz :: Monad m => Fold Double Double -> MSF m Double Double
-- baz (Fold a b c ) = arr c . accumulateWith (flip a) b 

-- main :: IO ()
-- main =
--   void $ sampleIO $ launchInGlossThread glossSettings $ reactimateCl glossClock $
--     visualisation . particleFilter params {n = 200} 
--       -- posterior
--       (mclmc banana initialState)

glossSettings :: GlossSettings
glossSettings = defaultSettings
    { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }

-- visualisation :: MSF (ReaderT (TimeInfo cl) (GlossConcT SamplerIO)) [(PhaseSpace 2 Double, Log Double)] ()
-- visualisation :: [(PhaseSpace 2 Double, Log Double)] >--> Picture
visualisation :: Monad m => MSF
  m
  [(V 2 Double, Log Double)]
  Picture
visualisation = proc p -> do
  -- constMCl clearIO -< ()
  picture <- (fold <$> mapMSF drawParticle) . arr (fmap (first (\(p ) -> fromV p))) -< p
  returnA -< scale 0.1 0.1 picture

normalLogDensity :: DensityFunction 2
normalLogDensity x = negate $ 0.5 * F.sum (fmap (**2) x)

banana :: DensityFunction 2
banana  = (\(V2 x y) -> negate $ 0.5 * (square (x / 10.0) + square (y - mu2 x))) . fromV -- negate $ 0.5 * (square(x[0] / 10.0) + jnp.square(x[1] - mu2))
  where
        mu2 x = curvature * (x ** 2 - 100)
        square i = i*i
        curvature = 0.03

initialState :: MonadDistribution m => m (PhaseSpace 2 Double)
initialState = do
  x <- normal 0 1
  y <- normal 0 1
  pure $ State (V $ Vec.fromList [x, y]) (signorm $ V $ Vec.fromList [1.0, 1.0])

data Params = Params {stepSize :: Double, typicalLength :: Double}

-- params :: Params
-- params = Params {stepSize = 0.01, typicalLength = 1.0}







---- util


type GlossClock = RescaledClock GlossSimClockIO Double

glossClock :: GlossClock
glossClock =
  RescaledClock
    { unscaledClock = GlossSimClockIO
    , rescale = float2Double
    }



accumulateWithM' :: Monad m => (a -> c -> m c) -> c -> MSF m a c
accumulateWithM' f s0 = feedback s0 $ arrM g
  where
    g (a, s) = do
        s' <- f a s
        pure (s', s')

accumulateWithM :: Monad m => (a -> c -> m c) -> m c -> MSF m a c
accumulateWithM f s0 = performOnFirstSample $ accumulateWithM' f <$> s0



integrator :: (KnownNat n, MonadDistribution m) =>
  (Double -> Operator m n
  , Double -> Operator m n
  , [Double]
  , Double
  , Double)
  -> Operator m n
integrator (o1, o2, coeffs, l, stepSize) = 
  maruyama l stepSize <=< 
  foldr (<=<) pure steps

  where
  steps = zipWith id (cycle [o1,o2]) ((*stepSize) <$> mirror coeffs)
  mirror x = x <> reverse x


{-# INLINE robustSumVar #-}
robustSumVar :: Double -> Fold Double TS
robustSumVar m = Fold step (TS zero 0) id where
    step  (TS s n) x = TS (add s . square . subtract m $ x) (n+1)

square :: Double -> Double
square x = x*x

-- | Maximum likelihood estimate of a sample's variance.  Also known
-- as the population variance, where the denominator is /n/.
{-# INLINE variance #-}
variance :: Double -> Fold Double Double
variance m =
    (\(TS sv n) -> if n > 1 then kbn sv / fromIntegral n else 0)
    <$> robustSumVar m

data TS  = TS  {-# UNPACK #-}!KBNSum {-# UNPACK #-}!Int


type GlossClockUTC cl = RescaledClockS (GlossConcT IO) cl UTCTime (Tag cl)

glossClockUTC :: (Real (Time cl)) => cl -> GlossClockUTC cl
glossClockUTC cl =
  RescaledClockS
    { unscaledClockS = cl
    , rescaleS = const $ do
        now <- liftIO getCurrentTime
        return (arr $ \(timePassed, event) -> (addUTCTime (realToFrac timePassed) now, event), now)
    }

main :: IO ()
main = flowGlossIO defaultSettings {display = InWindow "rhine-bayes" (1200, 1000) (10, 10)} $
    tagS @@ glossClockUTC GlossEventClockIO
    >-- collect -->
    ((intoGloss sf . accumulateWith handle noInput) @@ liftClock waitClock
          :: Rhine (GlossConcT IO) (LiftClock IO GlossConcT (Millisecond 10)) [Event] Picture)
    >-- keepLast mempty -->
    (arrMCl paintAllIO @@ glossClockUTC GlossSimClockIO
          :: Rhine (GlossConcT IO) (GlossClockUTC GlossSimClockIO) Picture ())

intoGloss :: ClSF SamplerIO cl a b -> ClSF (GlossConcT IO) cl a b
intoGloss = morphS (hoist (lift . sampleIO))

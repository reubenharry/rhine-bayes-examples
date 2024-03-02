{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module SDE where

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}


import Control.Monad ( (<=<), forever, replicateM )
import GHC.Float (float2Double)

import Linear.V ( dim, V(V), Finite (fromV, toV), fromVector, Dim )
import Linear ((^*), (*^), Additive ((^+^)), Metric (norm, dot, signorm), (^/), V2 (V2), V3 (V3), R1 (_x))
import qualified Data.Vector as Vec
import GHC.TypeLits (Nat, KnownNat, natVal)
import qualified GHC.TypeLits as TP
import Numeric.AD (grad)
import Data.MonadicStreamFunction (arrM, Arrow (first, arr, second, (***)), returnA, morphS, accumulateWith, constM)
import Data.MonadicStreamFunction.InternalCore ( MSF, feedback )
import Control.Monad.Bayes.Class (MonadDistribution(..), Log (..), MonadMeasure, normalPdf, condition)
import Control.Category ( Category((.), id) )
import Prelude hiding (id, (.))
import FRP.Rhine (timeless, ClSF, TimeDomain (Diff), Clock (Time, Tag), RescaledClock (unscaledClock, RescaledClock, rescale), tagS, collect, (>--), (-->), (@@), arrMCl, TimeInfo, keepLast, Rhine, LiftClock, RescaledClockS (RescaledClockS, unscaledClockS, rescaleS), UTCTime, MonadIO (liftIO), waitClock, liftClock, sinceLastS, Millisecond, sinceInitS, hoistClSF, timer, runClSFExcept, ClSFExcept)
import FRP.Rhine.Gloss (GlossSettings (display), defaultSettings, Display (InWindow), GlossSimClockIO (GlossSimClockIO), scale, Picture, flowGlossIO, paintAllIO, Event, GlossEventClockIO (GlossEventClockIO), withAlpha, translate, text, white, red, safely, Color, blue, green)
import Control.Monad.Morph (MFunctor (hoist), MonadTrans (lift))
import Example ( decayingIntegral, drawBall)
import Inference (particleFilter, params, SMCSettings (n))
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Foldable (Foldable(..))
import Util ( normalPdf2D, observe, type (>-->) )
import GUI
    ( button, buttonParams, slider, ButtonConfig(buttonPos), multipleSwitch, multipleSwitch' )
import Control.Monad.Bayes.Sampler.Strict
import Concurrent (UserInput, handle, noInput)
import Control.Monad.Trans.MSF (ReaderT, performOnFirstSample, throwOn, exceptS, ExceptT, throwS, StateT (runStateT), put, runStateS, get, runWriterS, WriterT (runWriterT), tell)
import FRP.Rhine.Gloss.IO (GlossConcT)
import Data.Time (addUTCTime, getCurrentTime)
import qualified Data.Vector as Vect
import Data.Maybe (fromMaybe)
import Control.Foldl (Fold(Fold))
import Numeric.Sum (KBNSum, kbn, Summation (..))
import qualified Data.Foldable as F
import Statistics.Distribution.Beta
import Statistics.Distribution (ContDistr(density))
import Witch (into)
import FRP.Rhine.Gloss.Common (violet)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Bayes.Population (PopulationT, runPopulationT)
import qualified Control.Foldl as FOLD
import Debug.Trace (traceM)
import Data.Data (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Demo (oscillator)
import GHC.Stack (callStack, prettyCallStack)
import GHC.Base (Any)
import Relude.Monad (MonadReader)
import Data.Void (Void)
import qualified FRP.Rhine.ClSF as C
import FRP.Rhine.ClSF (try)
import FRP.Rhine.ClSF.Except (throwOn')
import qualified Control.Monad.Trans.MSF.State as D
import Data.Monoid (Sum(Sum, getSum))



-- to implement

-- add pause
-- implement dependently typed multiple choice
-- implement clean switching of hmc vs mclmc vs mhmclmc, and choice of model
-- moving the particle around with asdf
-- separate demo for posterior
-- add all the existing demo


-- truly intractable kernel

-- the interesting noising strategy: pf over both the prior draw and the result

-- to think about:

-- smc as homotopy idea: think more!!!

-- think about multiple chains, with different params

-- hmc, so you can compare the two

-- think about preconditioning as change in integrator vs change in target

-- think about jakob's idea re gibbs

-- what about constraining the momentum space in a way that doesn't add bias?

-- think more about clocks and continuity: an MSF from time domain

-- to what extent do we have commutativity of mcmc and ppl?: fully, i think?
  -- what about `observe` in the mcmc?
    -- even that?

-- treat tuning of L as an inference problem...


-- foo = do
--   x <- normal 0 1
--   factor $ normalPdf 0 1 x
--   return x

-- adaptive tuning
-- a running ess_corr calculation, that updates L

-- stochastic gradient: change noise live 
-- do microcanonical ensemble mcmc directly: ooh: can we explicitly do the time rescaling (e.g. as a rescaled clock)? to get from microcanonical to the sde formulation?


-- questions: 

-- to what extend is mclmc close to geodesic motion on the information manifold


-- recall how to get a density from a ppl

-- hierarchical sdes? a ppl based on the sdes


-- the sde as a coelgebra corresponding to the distribution (which is the fixpoint of the corecursion): reweight the coalgebra instead of reweighting the distribution
-- anything asynchronous here that's interesting?
-- feedback?

-- renormalization constructively: define MSF -> MSF which groups times 

-- what if you don't want to sample a fully stationary distribution? e.g. gaussian with moving mean

-- jakob:

-- the mixture: two ways of doing it: 1. two separate sdes, 2. one sde with a mixture of densities
-- particle filter to rule out excessive crossing of typical set: an automatic tuning method: we're uncertain of the correct L, so have an unnormalized SDE
-- what about adaptively changing step size?
-- hierarchical cases
-- what about adaptively changing step size, e.g. lowering it when momentum is high? or even try different ones and weight by energy bias...

-- a direct representation of the flow of the MCLMC SDE (conditional on the parameters and pdf)
mclmc :: forall dist dim . (Distribution dist, Natural dim) =>
  dist (PhaseSpace dim Double) -> StochasticProcess dist (Params, PDF dim) (Position dim)
mclmc initial = arr position . mclmcFull initial

mclmcFull :: forall dist dim . (Distribution dist, Natural dim) =>

  dist (PhaseSpace dim Double) -> StochasticProcess dist (Params, PDF dim) (PhaseSpace dim Double)

mclmcFull initial = proc (Params stepSize l temp, PDF df ) -> do
    timeless (accumulateWithM integrator initial) -< (
        positionUpdate,
        momentumUpdate ((/fromDouble' temp) . df),
        mclachlanCoeffs,
        l,
        stepSize)

mchmcProposal :: KnownNat dim => Proposal dim
mchmcProposal init = runWriterS proc (Params stepSize _ temp, PDF df) -> do
    (accumulateWithM integratorDet ( lift $ lift init)) -< (
        positionUpdate,
        momentumUpdate' ((/fromDouble' temp) . df),
        mclachlanCoeffs,
        stepSize)

hmc :: KnownNat dim => Proposal dim
hmc init = runWriterS proc (Params stepSize _ temp, PDF df) -> do
    (accumulateWithM integratorDet ( lift $ lift init)) -< (
        positionUpdateEuclidean,
        momentumUpdateEuclidean ((/fromDouble' temp) . df),
        mclachlanCoeffs,
        stepSize)


type Proposal dim = (forall dist2 . MonadDistribution dist2 => dist2 (PhaseSpace dim Double) -> StochasticProcess dist2 (Params, PDF dim) (Info, PhaseSpace dim Double))

mh :: forall dist dim . (Distribution dist, Natural dim) => dist (PhaseSpace dim Double) -> 
  Proposal dim ->
  StochasticProcess dist (Params, PDF dim) (PhaseSpace dim Double, [PhaseSpace dim Double], Bool)
mh init proposal = C.safely $ loop init where
  loop s = do
    len <- C.once $ \(Params _ l _, b) -> poisson l
    traceM $ show len
    (kineticEnergyChange, states) <- try $ proc inp -> do
      (kineticEnergyChange, state) <- proposal (lift s) -< inp
      ss :: [PhaseSpace dim Double] <- accumulateWith (:) [] -< state
      n <- C.count -< ()
      C.throwOn' -< (n > len, (kineticEnergyChange, ss))
      returnA -< (state, ss, True) -- all 
    let startState = last states
        endState = head states

    deltaEnergy <- C.once \(_, PDF logDensity) -> pure (logDensity (position startState) - logDensity (position startState) + getSum kineticEnergyChange)


    (newP, _, _) <- C.once $ acceptance deltaEnergy startState endState
    momentum <- C.once $ const generateUnitVector
    let newP' = State (position newP) momentum
    loop (pure newP')


acceptance :: MonadDistribution m => Double -> PhaseSpace dim Double -> PhaseSpace dim Double -> (Params, PDF dim) -> m (PhaseSpace dim Double, Bool, Double)
acceptance deltaEnergy startState endState (_, PDF logDensity) = do
    let
        pAccept = min 1 (exp deltaEnergy)
    doAccept <- bernoulli pAccept
    traceM $ show doAccept
    return (if doAccept then endState else startState, doAccept, pAccept)


tuning :: (Monad dist, Dim dim) => StochasticProcess dist (Position dim, PDF dim) Params
tuning = C.safely $ forever do
  -- xs <- try $ proc (pos, pdf) -> do 
  --   (xAvg, xSquaredAvg) <- mean (0,0) -< pos
  --   time <- sinceInitS -< ()
  --   throwOn' -< (time > 50, (xAvg, xSquaredAvg) )
  --   returnA -< Params 0.1 5 1
  --   -- Params 0.1 5 1 <$ timer 5 -< ()

    -- returnA -< undefined
  try proc (pos, pdf) -> do
    (xAvg, xSquaredAvg) <- mean (0,0) -< pos
    arrM traceM -< show (sqrt (F.sum (xSquaredAvg - xAvg**2)))
    t <- sinceInitS -< ()
    returnA -< Params 0.1 (if t < 5 then 5 else sqrt (F.sum (xSquaredAvg - xAvg**2))) 1
    -- returnA -< Params 0.1 (sqrt (F.sum (xSquaredAvg - xAvg**2))) 1

    where mean (avg1, avg2) = proc pos -> do
              avg <- streamingMean -< pos
              xSquaredAvg <- streamingMean -< pos ** 2
              returnA -< (avg+avg1, xSquaredAvg+avg2)



groundTruth :: (Monad m, MonadMeasure m) => MSF
  (ReaderT
     (TimeInfo (LiftClock IO GlossConcT (Millisecond 10)))
     m)
  (UserInput, Params)
  (V 2 Double)
groundTruth =
  arr position . multipleSwitch' signals (State ones ones :: PhaseSpace 2 Double) 1
  where
      -- signals 1 = id
    signals 1 = \v -> proc params -> do

      p <- mclmcFull (pure v) -< (params, PDF normalLogDensity )
      returnA -< p
    signals 2 = \v -> proc params -> do
      p <- mclmcFull (pure v) -< (params, PDF banana )
      returnA -< p
    signals 3 = \v -> proc params -> do
      p <- mclmcFull (pure v) -< (params, PDF mixtureDensity )
      returnA -< p
    signals 4 = \v -> proc params -> do
      p <- mixture (pure v) -< (params, PDF banana )
      returnA -< p
    signals 5 = \v -> proc params -> do
      p <- constrained -< (params, PDF normalLogDensity)
      returnA -< p
    signals 6 = \v -> proc params -> do
      p <- mclmcFull (pure v) -< (params, PDF $ log . (\vec -> (let (V2 x y) = fromV vec in if y > 0 then 1 else 0.1) * exp (normalLogDensity vec)))
      returnA -< p
    signals 7 = \v -> proc (Params eps _ temp) -> do
      x <- full (pure v) -< (PDF banana, (eps, temp))
      returnA -< x
    signals 8 = \v -> proc params -> do
      latent <- mclmc (pure v) -< (params, PDF  mixtureDensity)
      noisy <- arrM (\(V2 x y) -> do 
        x' <- normal x std
        y' <- normal y std
        return $ toV (V2 x' y')
        
         ) -< fromV latent
      p <- posterior -< (noisy, params, PDF  mixtureDensity)
      returnA -< p
    signals _ = undefined
    -- -- signals 2 = \v -> proc _ -> (+ v) <$> Example.prior -< ()
    -- signals 3 = const weakPrior
    -- signals 4 = moveWithArrows
    -- signals 5 = moveInLines
    -- signals 6 = gravity
    -- signals _ = gravity


std = 2

streamingMean :: (Dim dim, Monad dist) => MSF (ReaderT (TimeInfo cl) dist) (Position dim) (V dim Double)
streamingMean = toMSF FOLD.mean

-- streamingMean :: MSF (ReaderT (TimeInfo cl) dist) (Position dim) (t0 Params)
-- streamingMean = _

toMSF :: Monad m => Fold a b -> MSF m a b
toMSF (FOLD.Fold a b c) = arr c . accumulateWith (flip a) b





-- x_squared_average - jnp.square(x_average)

-- foo = streamingAverage (\(x :: Double) -> (V2 x (x**2)))

-- streamingAverage :: Fractional b => (t -> b) -> t -> (b, b) -> (b, b)
-- streamingAverage o x streaming_avg   =
--     let (total, avg) = streaming_avg
--         -- avgN = (total * avg + weight * o(x)) / (total + weight + zero_prevention)
--         avgN = (total * avg + o(x)) / (total)
--     in (total, avgN)

-- full :: (Monad m, MonadDistribution m, KnownNat dim, Num (Diff (Time cl)), Ord (Diff (Time cl)), TimeDomain (Time cl), Floating (Diff (Time cl))) => m (PhaseSpace dim Double) -> MSF
--   (ReaderT (TimeInfo cl) m)
--   (Params)
--   (Position dim)
full :: forall dist dim . (Distribution dist, Natural dim) =>

  dist (PhaseSpace dim Double) -> StochasticProcess dist (PDF dim, (Double, Double)) (PhaseSpace dim Double )
full init = feedback (Params 0.1 1 1) proc ((pdf, (t,temp)), params) -> do

  p <- mclmcFull init -< (params {stepSize = t, temperature = temp}, pdf)
  paramsNew <- tuning -< (position p, pdf)
  arrM traceM -< show paramsNew
  returnA -< (p, paramsNew)

data PhaseSpace (n :: Nat) a where
  State :: {position :: V dim a, momentum :: V dim a} -> PhaseSpace dim a
  deriving (Show, Eq)


type Info = Sum Double

type Position dim = Vector dim

type Operator m dim = PhaseSpace dim Double -> m (PhaseSpace dim Double)
type OperatorWithInfo m dim = PhaseSpace dim Double -> WriterT Info m (PhaseSpace dim Double)

type DensityFunction dim = forall a . (Floating a, Ord a) => V dim a -> a

momentumUpdate' :: (MonadDistribution m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> WriterT Info m (PhaseSpace dim Double)
momentumUpdate' logdensity stepSize (State x u) = tell (Sum kinetic_energy_change) >> pure (State x uu)
    where
    g = negate $ grad logdensity x
    g_norm = Linear.norm g
    d = fromIntegral $ dim g
    e = negate $ g ^/ g_norm
    delta = stepSize * g_norm / (d-1)
    uu = (u + e^*(sinh delta + dot e (u ^* ( cosh delta -1)))) Linear.^/ (   cosh delta  + Linear.dot e (u ^* sinh delta))

    ue = dot u e
    zeta = exp (-delta)
    kinetic_energy_change = delta - log 2 + log (1 + ue + (1-ue)*zeta**2)

momentumUpdate :: (MonadDistribution m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> m (PhaseSpace dim Double)
momentumUpdate x y z = fst <$> runWriterT (momentumUpdate' x y z)

positionUpdate :: (Applicative f, KnownNat n, Num a) => a -> PhaseSpace n a -> f (PhaseSpace n a)
positionUpdate e (State x u) = pure $ State (x ^+^ e Linear.*^ u) u

positionUpdateEuclidean :: (Applicative f, KnownNat n, Num a, Floating a) => a -> PhaseSpace n a -> f (PhaseSpace n a)
positionUpdateEuclidean e (State x u) = pure $ State (x ^+^ e Linear.*^ g) u where
  g = grad kineticEnergy u




momentumUpdateEuclidean :: forall dim m . (MonadDistribution m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> WriterT Info m (PhaseSpace dim Double)
momentumUpdateEuclidean logdensity stepSize (State x u) = tell (kin u - kin uu) >> pure (State x uu)
    where
    kin p = Sum $ 0.5 * dot p p
    g = grad logdensity x  :: V dim Double
    uu = u + (stepSize *^ g)

kineticEnergy :: (Foldable f, Functor f, Floating a) => f a -> a
kineticEnergy u = 0.5 * F.sum (fmap (**2) u)

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

data PDF n = PDF {df :: DensityFunction n}



type Natural = KnownNat

type Vector dim = V dim Double

type Distribution = MonadDistribution

type StochasticProcess m a b = forall cl. (TimeDomain (Time cl), Floating (Diff (Time cl)),  Ord (Diff (Time cl))) => ClSF m cl a b







-- mixture :: MonadDistribution m => StochasticProcess m (Params, PDF 2) (V 2 Double)
mixture init = proc (params, input) -> do
  a <- mclmcFull init -< (params, input {df = normalLogDensity})
  c <- mclmcFull init -< (params, input {df = \x -> negate $ 0.5 * F.sum (fmap (**2) (x+5))})
  bool <- constM $ bernoulli 0.5 -< ()
  returnA -< if bool then a else c

mixtureDensity :: DensityFunction 2
mixtureDensity x = log (exp (normalLogDensity x) + exp (normalLogDensity (x + 5)))


-- beta2d :: Floating a => V2 a -> a
-- beta2d (V2 x y )= fromDouble' (density (betaDistr 2 2) (x )) + fromDouble' (density (betaDistr 2 2) y)

hierarchical :: MonadDistribution m => StochasticProcess m (Params, PDF 2) (V 2 Double)
hierarchical = proc (params, _) -> do
  x <- mclmc initialState -< (params, PDF banana)
  y <- mclmc initialState -< (params, PDF (banana . (+ fmap fromDouble' x)))
  returnA -< y


posterior :: (MonadMeasure m) => StochasticProcess m (V 2 Double, Params, PDF 2) (PhaseSpace 2 Double)
posterior = proc (obs, params, b) -> do
  p <- mclmcFull initialState -< (params, b)
  -- i <- countDemoObservationModel -< fromV p
  -- arrM traceM -< show i
  -- arrM condition -< i == obs
  -- observe -< normalPdf 0 1 (norm $ position p)
  observe -< normalPdf2D (fromV obs) std (fromV $ position p)
  -- last10 <- arr (take 100) . accumulateWith (:) [] -< p
  -- observe -<  (Exp $ log $ sum $ norm . momentum <$> last10)
  returnA -< p

constrained :: (MonadMeasure m) => StochasticProcess m (Params, PDF 2) (PhaseSpace 2 Double)
constrained = proc b -> do
  p <- mclmcFull initialState -< b
  -- i <- countDemoObservationModel -< fromV p
  -- arrM traceM -< show i
  -- arrM condition -< i == obs
  -- observe -< normalPdf 1 0.4 ((\(V2 x y) -> y) $ fromV $ position p)
  arrM condition -< (>0) $ (\(V2 x y) -> y) (fromV $ position p)
  -- observe -< normalPdf2D (fromV obs) 1 (fromV p)
  -- last10 <- arr (take 100) . accumulateWith (:) [] -< p
  -- observe -<  (Exp $ log $ sum $ norm . momentum <$> last10)
  returnA -< p


-- constrainedHmc :: (MonadMeasure m, KnownNat n) => (DensityFunction n, DensityFunction n) -> m (PhaseSpace n Double) -> ClSF m cl (Double, Double, Bool) (PhaseSpace n Double)
-- constrainedHmc (d1, d2) initial = proc slb@(_,_,b) -> do
--   p <- hmc (d1, d2) initial -< slb
--   -- arrM traceM -< show (norm $ momentum p)
--   if b then
--     observe -< normalPdf 1 2 (norm $ momentum p)
--   else returnA -< ()
--   returnA -< p




leapfrogCoeffs, mclachlanCoeffs :: [Double]
leapfrogCoeffs = [0.5, 1]
mclachlanCoeffs = [0.1931833275037836, 0.5, 1 - 2 * head mclachlanCoeffs]



glossSettings :: GlossSettings
glossSettings = defaultSettings
    { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }


visualisation :: Monad m => Color -> MSF
  m
  [(V 2 Double, Log Double)]
  Picture
visualisation c = proc p -> do
  picture <- (fold <$> mapMSF (drawParticle c)) . arr (fmap (first fromV)) -< p
  returnA -< scale 0.1 0.1 picture

normalLogDensity :: DensityFunction 2
normalLogDensity x = negate $ 0.5 * F.sum (fmap (**2) x)

banana :: DensityFunction 2
banana  = (\(V2 x y) -> negate $ 0.5 * (square (x / 10.0) + square (y - mu2 x))) . fromV
  where
        mu2 x = curvature * (x ** 2 - 100)
        square i = i*i
        curvature = 0.03

initialState :: MonadDistribution m => m (PhaseSpace 2 Double)
initialState = do
  x <- normal 0 1
  y <- normal 0 1
  pure $ State (V $ Vec.fromList [x, y]) (signorm $ V $ Vec.fromList [1.0, 1.0])

generateUnitVector :: forall dim m . (Distribution m, KnownNat dim) => m (V dim Double)
generateUnitVector = let d = natVal (Proxy @dim) in (signorm . fromMaybe (error "length") . fromVector . Vec.fromList <$> replicateM (fromIntegral d) (normal 0 1))

ones :: V 2 Double
ones = signorm $ V $ Vec.fromList [1.0, 1.0]

data Params = Params {stepSize :: Double, typicalLength :: Double, temperature :: Double} deriving Show







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

integratorDet :: (KnownNat n, MonadDistribution m) =>
  (Double -> OperatorWithInfo m n
  , Double -> OperatorWithInfo m n
  , [Double]
  , Double)
  -> OperatorWithInfo m n
integratorDet (o1, o2, coeffs, stepSize) =
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



intoGloss :: ClSF SamplerIO cl a b -> ClSF (GlossConcT IO) cl a b
intoGloss = morphS (hoist (lift . sampleIO))

instance (Num a, KnownNat n) => Num (PhaseSpace n a) where
  (State p m) + (State p' m') = State (p+p') (m + m')
  fromInteger :: (Num a, KnownNat n) => Integer -> PhaseSpace n a
  fromInteger = error $ prettyCallStack callStack

main :: IO ()
main = flowGlossIO defaultSettings {display = InWindow "rhine-bayes" (1200, 1000) (10, 10)} $
    tagS @@ glossClockUTC GlossEventClockIO
    >-- collect -->
    ((intoGloss sf . accumulateWith handle noInput) @@ liftClock waitClock
          :: Rhine (GlossConcT IO) (LiftClock IO GlossConcT (Millisecond 10)) [Event] Picture)
    >-- keepLast mempty -->
    (arrMCl paintAllIO @@ glossClockUTC GlossSimClockIO
          :: Rhine (GlossConcT IO) (GlossClockUTC GlossSimClockIO) Picture ())

drawParticle :: Monad m => Color -> MSF m (V2 Double, Log Double) Picture
drawParticle c = proc (position, probability) -> do
  -- drawBall -< (position, 0.1, withAlpha ( (double2Float $ exp $ 0.1 * ln probability)) violet)
  drawBall -< (position, 0.2, withAlpha ( into @Float $ exp $ 0.1 * ln probability) c)
  -- drawBall -< (position, 0.1, white)

sf :: MSF
  (ReaderT
     (TimeInfo (LiftClock IO GlossConcT (Millisecond 10))) SamplerIO)
  UserInput
  Picture
sf = proc userInput -> do
  (buttonPic, useBanana) <- GUI.button buttonParams {buttonPos = V2 (-350) 300} -< userInput
  (sliderPic, radius) <- slider (V2 (-300) 300) 60 -< userInput
  (sliderPic2, radius2) <- slider (V2 (-250) 300) 60 -< userInput
  (sliderPic3, temp) <- arr (second (\x -> max 0.1 $ 10*x - 4)) .  slider (V2 (-275) 300) 60 -< userInput



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

  -- arrM traceM -< show radius2

  let withTemp n = ((*fromDouble n) .)
  -- let with_temperature :: forall a . Floating a => Double -> V 2 a -> a
  --     with_temperature b = (\x -> (*(fromDouble b :: a)) $ (normalLogDensity :: forall a. Floating a => V 2 a -> a) x )

  -- let input = Input useBanana (\x -> mixtureDensity x * fromDouble (10 * radius2))
  let input = PDF (if useBanana then banana else normalLogDensity)

  -- ob <- mclmc initialState -< (Params 0.1 1, Input useBanana (if useBanana then banana else normalLogDensity))
  -- i <- countDemoObservationModel -< fromV $ position ob
  -- result <- particleFilter params{n=200} posterior -< (i, useBanana)
  -- pic <- arr (scale 0.1 0.1) . drawBall -< (fromV $ position ob , 0.1, red)
  -- picture <- visualisation -< result
  -- returnA -< picture <> buttonPic <> pic

  -- ob <- mclmc initialState -< (Params 0.1 1, input)
  -- result <- particleFilter params{n=200} posterior -< (ob, input)
  -- pic <- arr (scale 0.1 0.1) . drawBall -< (fromV ob , 0.1, red)

  -- result <- particleFilter params{n=200} (mixture) -< (Params (10 * radius * timeInterval) 1, input)
  -- result <- if useBanana then
  --   particleFilter params{n=200} directMixture -< (Params (10 * radius * timeInterval) 1, input )
  --   else particleFilter params{n=200} mixture -< (Params (10 * radius * timeInterval) 1, input)
  -- result <- particleFilter params{n=200} directMixture -< (10 * radius * timeInterval, 2, useBanana)
  -- result <- particleFilter params{n=200} mixture -< 10 * radius * timeInterval

  -- stepSize <- max (0.1) . abs <$> brownianMotion1D -< ()
  let stepSize = 1.0
  let l = max 0.1 (10 * radius2)
  -- result <- particleFilter params{n=200} (mclmc initialState) -< (Params (stepSize * radius * 10 * timeInterval) l, input)
  -- result <- particleFilter params{n=200} (full initialState ) -< (PDF True banana, timeInterval*10)

  -- x <- mclmcFull initialState -< (Params (stepSize * radius * 10 * timeInterval) l temp, PDF banana)
  -- particleFilter params{n=200} (p groundTruth) -< (x, userInput, Params (stepSize * radius * 10 * timeInterval) l temp)
  -- pic <- drawBall -< (fromV x, 0.1, red)
  -- result <- if not useBanana then

  result <- particleFilter params{n=200} (groundTruth ) -< (userInput, Params (stepSize * radius * 10 * timeInterval) l temp)
  picture <- visualisation red -< result
  --   else
  --     particleFilter params{n=200} (p groundTruth) -< (x, userInput, Params (stepSize * radius * 10 * timeInterval) l temp)

  -- picture <- arr (scale 0.1 0.1) . drawBall -< (fromV $ position ((undefined . fst) <$> resultI), 0.2, red)
  -- resultI <- particleFilter params{n=20} (mh initialState mchmcProposal ) -< (Params (stepSize * radius * 20 * timeInterval) (10*l) temp, PDF banana)
  -- resultJ <- particleFilter params{n=20} (mh initialState hmc ) -< (Params (stepSize * radius * 20 * timeInterval) (10*l) temp, PDF banana)

  -- picture <- visualisation red -< fmap ( first (position . fst3) )  resultI
  -- picture2 <- arr fold . mapMSF (visualisation blue) -< fmap (,1) <$> fmap (fmap position . snd3 . fst) resultI -- (,1.0) . position <$> k
  
  -- picture' <- visualisation green -< fmap ( first (position . fst3) )  resultJ
  -- picture2' <- arr fold . mapMSF (visualisation violet) -< fmap (,1) <$> fmap (fmap position . snd3 . fst) resultJ -- (,1.0) . position <$> k

  returnA -< 
    -- picture2 <> picture  <> picture2' <> picture' <> 
    picture <>
    buttonPic <> sliderPic <> sliderPic2 <> translate (-200) 400 (scale 0.1 0.1 (text "1: Normal  2: Banana   3: Mixture (direct)   4: Mixture (indurect)   5: Unnormalized   6: Bayesian")) <> scale 0.1 0.1 mempty <> sliderPic3

fst3 :: (a,b,c) -> a
fst3 (a,b,c)= a

snd3 :: (a,b,c) -> b
snd3 (a,b,c)= b

p sf = proc (obs, inp, params) -> do
  p <- sf -< (inp, params)
  observe -< normalPdf2D (fromV obs) 0.5 (fromV p)
  returnA -< p

brownianMotion1D :: (Monad m, Diff (Time cl) ~ Double, MonadDistribution m) => MSF
  (ReaderT
     (TimeInfo cl)
     m)
  t
  Double
brownianMotion1D = proc _ -> do
  dacceleration <- constM (normal 0 20) -< ()
  acceleration <- decayingIntegral 1 -< dacceleration
  velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
  position <- decayingIntegral 1 -< velocity
  returnA -< position

fromDouble :: forall a . Floating a => Double -> a
fromDouble d = 0.1 + fromIntegral (floor d)


fromDouble' :: forall a . Floating a => Double -> a
fromDouble' d = (/1000) $ fromIntegral (floor (d*1000))



newtype MooreT m a b = MooreT {runMooreT :: m (b, a -> MooreT m a b)}

pf :: Monad m => MooreT (PopulationT m) a b -> MooreT m a [(b, Log Double)]
pf (MooreT moore) = MooreT do
  x <- runPopulationT moore
  let pop = (\((x,y),z) -> (x,z)) <$> x
  -- let cont = mapM \((x,y),z) -> y x
  pure (pop, pf <$> undefined)


data D (m :: Nat) (l :: Nat) (t :: Nat) = D Double

data Unit m l t = Unit (D m l t) (V3 Text )

m :: forall m l t m' l' t'. (KnownNat m, KnownNat l, KnownNat t) => D m l t -> D m' l' t' -> D (m TP.+m') (l TP.+l') (t TP.+ t')
m (D d) (D d') = D (d + d')

coord ::forall m l t. (KnownNat m, KnownNat l, KnownNat t) => Unit m l t -> (Double, Text)
coord (Unit (D d) (V3 a b c)) = (d, pp i a<>pp j b<>pp k c) where
  i = natVal (Proxy @m)
  j = natVal (Proxy @l)
  k = natVal (Proxy @t)

a :: Unit 1 1 0
a = Unit (D 5) (V3 "kg" "meter" "second")

c =  D 5 :: D 1 1 0
c' =  D 4.5  :: D 2 3 1

c'' = m c c'

b :: (Double, Text)
b = coord a

pp :: Integer -> Text -> Text
pp 0 _ = mempty
pp 1 t = t <> " "
pp i t = t <> "^" <> T.pack (show i) <> " "


-- >>> coord a
-- (5.0,"kg^1")

x :: Any
x = undefined
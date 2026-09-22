{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}


module SDE where



import qualified Data.Sequence as Seq

import Control.Monad ( (<=<), forever, replicateM )
import GHC.Float (float2Double)
import Linear.V ( V(V), Finite (fromV, toV), fromVector, Dim )
import Linear ((^*), (*^), Additive ((^+^), (^-^)), Metric (norm, dot, signorm), (^/), V2 (V2))
import qualified Data.Vector as Vec
import GHC.TypeLits (Nat, KnownNat, natVal)
import qualified GHC.TypeLits as TP
import Numeric.AD (grad)
import qualified Linear.V as V
import Data.MonadicStreamFunction (arrM, Arrow (first, arr, second), returnA, morphS, accumulateWith, constM)
import Data.MonadicStreamFunction.InternalCore ( MSF(MSF), feedback )
import Control.Monad.Bayes.Class (MonadDistribution(..), Log (..), MonadMeasure, condition, mvNormal)
import Control.Category ( Category((.), id) )
import Prelude hiding (id, (.))
import FRP.Rhine (timeless, ClSF, TimeDomain (Diff), Clock (Time, Tag), RescaledClock (unscaledClock, RescaledClock, rescale), TimeInfo (TimeInfo, sinceLast), RescaledClockS (RescaledClockS, unscaledClockS, rescaleS), UTCTime, MonadIO (liftIO), sinceLastS, sinceInitS, ResamplingBuffer, SN (..), Out, In, keepLast, SequentialClock (SequentialClock), Rhine (Rhine), arrMCl, timeInfo, historySince)
import FRP.Rhine.Gloss (GlossSettings (display), defaultSettings, Display (InWindow), GlossSimClockIO (GlossSimClockIO), scale, Picture, withAlpha, translate, text, white, red, Color, green, paintAllIO, circleSolid, line)
import Control.Monad.Morph (MFunctor (hoist), MonadTrans (lift))
import Example ( decayingIntegral, drawBall, stochasticOscillator)
import Inference (particleFilter, params, SMCSettings (n))
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Foldable (Foldable(..))
import Util ( normalPdf2D, observe, type (>-->), type (>-/->), type (>-&->), SignalFunction, Stochastic )
import qualified Data.Matrix as VecMat
import Control.Monad.Trans.Class
import GUI
    ( button, buttonParams, slider, ButtonConfig(buttonPos), multipleSwitch' )
import Control.Monad.Bayes.Sampler.Strict
import Concurrent (UserInput)
import Control.Monad.Trans.MSF (ReaderT, performOnFirstSample, runWriterS, WriterT (runWriterT))
import FRP.Rhine.Gloss.IO (GlossConcT)
import Data.Time (addUTCTime, getCurrentTime)
import qualified Data.Vector as Vect
import Data.Maybe (fromMaybe, isJust)
import Control.Foldl (Fold(Fold))
import Numeric.Sum (KBNSum, kbn, Summation (..))
import qualified Data.Foldable as F
import Witch (into)
import FRP.Rhine.Gloss.Common (violet)
import qualified Control.Foldl as FOLD
import GHC.Stack (callStack, prettyCallStack)
import qualified FRP.Rhine.ClSF as C
import FRP.Rhine.ClSF (try)
import Data.Monoid (Sum(Sum, getSum))
import Smoothing (shift, delayBy')
import Control.Lens ((^.), _1, _2)
import qualified Control.Monad.Trans.MSF as D
import Data.Singletons
import GHC.Generics (Generic)
import FRP.Rhine.Gloss (circle)

import Control.Lens hiding (Fold, element)
import Control.Lens.TH
import Control.Monad.Writer (MonadWriter (..))
import Control.Applicative (Applicative(liftA2))
import Debug.Trace (trace, traceM)

import Data.Generics.Product (the)


data PhaseSpace (n :: Nat) a where
  State :: {_position :: V dim a, _momentum :: V dim a} -> PhaseSpace dim a
  deriving (Show, Eq)
$(makeLenses ''PhaseSpace)

-- view autocorrelation by time shifting

-- the mixture: two ways of doing it: 1. two separate sdes, 2. one sde with a mixture of densities

-- smc:
  -- a new trajectory of targets: by compromising the path in weird ways using a particle filter

-- particle filter: an automatic tuning method: we're uncertain of the correct L, so have an unnormalized SDE


-- do microcanonical ensemble mcmc directly: ooh: can we explicitly do the time rescaling (e.g. as a rescaled clock)? to get from microcanonical to the sde formulation?

-- truly intractable kernel

-- the interesting noising strategy: pf over both the prior draw and the result

-- smc as homotopy idea

-- think about multiple chains, with different params


-- to what extent do we have commutativity of mcmc and ppl?: fully, i think?
  -- what about `observe` in the mcmc?


-- online feedback for tuning

-- the sde as a coelgebra corresponding to the distribution

-- renormalization constructively: define MSF -> MSF which groups times 


-- a direct representation of the flow of the MCLMC SDE (conditional on the input variables)
unadjustedMclmc ::
  forall dist dim . (Distribution dist, Natural dim) =>
  dist (PhaseSpace dim Double) -> StochasticProcess dist (InputVariables dim) (Position dim)
unadjustedMclmc initial = proc inputVars -> do
  phasepoint <-  (mclmcDynamicsOnPhaseSpace initial) -< inputVars
  returnA -< phasepoint ^. position

mclmcDynamicsOnPhaseSpace ::
  forall dist dim . (Distribution dist, Natural dim) =>
  dist (PhaseSpace dim Double) -> StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
mclmcDynamicsOnPhaseSpace initial = proc (Params stepSize l temp, PDF df, coeffs ) -> do
    timeless (accumulateWithM integrator initial) -< (
        positionUpdateIsokinetic,
        momentumUpdateIsokinetic ((/fromDouble' temp) . df),
        coeffs,
        l,
        stepSize)

instance (MonadDistribution m, Monoid a) => MonadDistribution (WriterT a m) where
  -- normal m s = lift $ normal m s
  -- bernoulli p = lift $ bernoulli p

  random = lift random

-- cutoffstreamingmean :: (Fractional a) => Fold a a 
-- cutoffstreamingmean = do
--   x <- FOLD.mean
--   y <- FOLD.genericLength
--   return (x) -- * if y > 100 then 0 else 1)

-- essEstimatorBias :: forall dim . (Natural dim) => FOLD.Fold (V dim Double) Double
-- essEstimatorBias = FOLD.premap f FOLD.mean
--   where

f :: forall dim. (Natural dim) => V dim Double -> Double   
f x2 = 
  let 
      normalE_x2 = fmap (const 1) x2
      normalVar_x2 = fmap (\y -> 2 * (y**2)) normalE_x2
      bias = liftA2 (/) (fmap square (x2 - normalE_x2))  normalVar_x2
      meanbias = F.sum bias / (getdim @dim)
  in meanbias




mclmcDynamicsOnPhaseSpace' ::
  forall dist dim . (Distribution dist, MonadWriter (Sum Double) dist, Natural dim) =>
  dist (PhaseSpace dim Double) -> StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
mclmcDynamicsOnPhaseSpace' initial = proc (Params stepSize l temp, PDF df, coeffs ) -> do
    timeless (accumulateWithM integrator initial) -< (
        positionUpdateIsokinetic,
        momentumUpdateIsokinetic' ((/fromDouble' temp) . df),
        coeffs,
        l,
        stepSize)

type KineticEnergy dim = forall a . Floating a => V dim a -> a

hmcDynamicsOnPhaseSpace ::
  forall dist dim . (Distribution dist, Natural dim) =>
  dist (PhaseSpace dim Double) ->
  KineticEnergy dim ->
  StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
hmcDynamicsOnPhaseSpace initial kineticEnergy = proc (Params stepSize l temp, PDF df, coeffs ) -> do
    timeless (accumulateWithM integrator initial) -< (
        positionUpdateEuclidean kineticEnergy,
        momentumUpdateEuclidean ((/fromDouble' temp) . df),
        coeffs,
        l,
        stepSize)

hmcDynamicsOnPhaseSpace' ::
  forall dist dim . (Distribution dist, Natural dim) =>
  dist (PhaseSpace dim Double) ->
  KineticEnergy dim ->
  StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
hmcDynamicsOnPhaseSpace' initial kineticEnergy = proc (Params stepSize l temp, PDF df, coeffs ) -> do
    timeless (accumulateWithM integrator' initial) -< (
        positionUpdateEuclidean kineticEnergy,
        momentumUpdateEuclidean ((/fromDouble' temp) . df),
        coeffs,
        l,
        stepSize)

mchmcDynamicsOnPhaseSpace :: forall dist dim . (Distribution dist, Natural dim) => dist (PhaseSpace dim Double) -> KineticEnergy dim -> StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
mchmcDynamicsOnPhaseSpace initial kineticEnergy = proc (Params stepSize l temp, PDF df, _ ) -> do
    timeless (accumulateWithM integrator initial) -< (
        positionUpdateEuclidean kineticEnergy,
        momentumUpdateEuclidean ((/fromDouble' temp) . df),
        mclachlanCoeffs,
        l,
        stepSize)



overdampedLangevin :: forall dist dim . (Distribution dist, Natural dim) => dist (PhaseSpace dim Double) -> StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
overdampedLangevin initial = proc (Params stepSize l temp, PDF pdf1, _ ) -> do
    timeless (accumulateWithM step initial) -< (grad pdf1, stepSize)
    where

      step :: (V dim Double -> V dim Double, Double) -> PhaseSpace dim Double -> dist (PhaseSpace dim Double)
      step (graddf, stepsize)(State x u) = do
        -- g <- normal 0 1
        g <- mvNormal (Vec.fromList $ replicate (getdim @dim) 0) (VecMat.identity (getdim @dim))
        let h  = fromMaybe undefined (fromVector g) :: V dim Double
        let x' = x + graddf x  ^* stepsize  + sqrt (2 * stepsize) *^ h
        -- let u' = fmap (+ sqrt (2 * stepsize) * g) (u - (graddf x ^* stepsize))
        -- let x' = x + stepsize * u'
        return $ State x' u


  -- where dim :: Floating a => a
        -- dim = fromDouble' $ fromIntegral $ TP.natVal (Proxy @dim)

mchmcProposal :: (Natural dim, Distribution dist) => Proposal dim dist
mchmcProposal init = runWriterS proc (Params stepSize _ temp, PDF df, _) -> do
    (accumulateWithM deterministicIntegrator ( lift $ lift init)) -< (
        positionUpdateIsokinetic,
        momentumUpdateIsokinetic' ((/fromDouble' temp) . df),
        mclachlanCoeffs,
        stepSize)

hmcProposal :: (Natural dim, Distribution dist) => Proposal dim dist
hmcProposal init = runWriterS proc (Params stepSize _ temp, PDF df, _) -> do
    (accumulateWithM deterministicIntegrator ( lift $ lift init)) -< (
        positionUpdateEuclidean (\x -> 0.5 * F.sum (fmap (**2) x)),
        momentumUpdateEuclidean' ((/fromDouble' temp) . df),
        mclachlanCoeffs,
        stepSize)


type Proposal dim dist = dist (PhaseSpace dim Double) -> StochasticProcess dist (InputVariables dim) (Info, PhaseSpace dim Double)

type InputVariables dim = (Params,  PDF dim, [Double])



mh :: forall dist dim . (Distribution dist, Natural dim) => dist (PhaseSpace dim Double) ->
  (forall d . Distribution d => Proposal dim d) ->
  StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double, [PhaseSpace dim Double], Bool)
mh init proposal = C.safely $ loop init where
  loop s = do
    len <- C.once $ \(Params _ l _, b, _) -> poisson l
    -- traceM $ show len
    (kineticEnergyChange, states) <- try $ proc inp -> do
      (kineticEnergyChange, state) <- proposal (lift s) -< inp
      ss :: [PhaseSpace dim Double] <- accumulateWith (:) [] -< state
      n <- C.count -< ()
      C.throwOn' -< (n > len, (kineticEnergyChange, ss))
      returnA -< (state, ss, True) -- all 
    let startState = last states
        endState = head states

    deltaEnergy <- C.once \(_, PDF logDensity, _) -> pure (logDensity (startState ^. position) - logDensity (startState ^. position) + getSum kineticEnergyChange)


    (newP, _, _) <- C.once $ acceptance deltaEnergy startState endState
    momentum <- C.once $ const generateUnitVector
    let newP' = State (newP ^. position) momentum
    loop (pure newP')


acceptance :: MonadDistribution m => Double -> PhaseSpace dim Double -> PhaseSpace dim Double -> InputVariables dim -> m (PhaseSpace dim Double, Bool, Double)
acceptance deltaEnergy startState endState (_, PDF logDensity, _) = do
    let
        pAccept = min 1 (exp deltaEnergy)
    doAccept <- bernoulli pAccept
    -- traceM $ show doAccept
    return (if doAccept then endState else startState, doAccept, pAccept)


tuning :: (Monad dist, Dim dim) => StochasticProcess dist (Position dim, PDF dim) Params
tuning = C.safely $ forever do

  try proc (pos, pdf) -> do
    (xAvg, xSquaredAvg) <- mean (0,0) -< pos
    -- arrM traceM -< show (sqrt (F.sum (xSquaredAvg - xAvg**2)))
    t <- sinceInitS -< ()
    returnA -< Params 0.1 (if t < 5 then 5 else sqrt (F.sum (xSquaredAvg - xAvg**2))) 1
    -- returnA -< Params 0.1 (sqrt (F.sum (xSquaredAvg - xAvg**2))) 1

    where mean (avg1, avg2) = proc pos -> do
              avg <- streamingMean -< pos
              xSquaredAvg <- streamingMean -< pos ** 2
              returnA -< (avg+avg1, xSquaredAvg+avg2)


type Integrator = [Double]




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

  p <- mclmcDynamicsOnPhaseSpace init -< (params {stepSize = t, temperature = temp}, pdf, mclachlanCoeffs)
  paramsNew <- tuning -< (p ^. position, pdf)
  -- arrM traceM -< show paramsNew
  returnA -< (p, paramsNew)




type Info = Sum Double
type DensityFunction dim = forall a . (Floating a, Ord a) => V dim a -> a

type Position dim = Vector dim

type Operator m dim = PhaseSpace dim Double -> m (PhaseSpace dim Double)
type OperatorWithInfo m dim = PhaseSpace dim Double -> WriterT Info m (PhaseSpace dim Double)


momentumUpdateIsokinetic' :: (MonadDistribution m, MonadWriter Info m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> m (PhaseSpace dim Double)
momentumUpdateIsokinetic' logdensity stepSize (State x u) = tell (Sum kinetic_energy_change) >> pure (State x uu)
    where
    g = negate $ grad logdensity x
    g_norm = Linear.norm g
    d = fromIntegral $ V.dim g
    e = negate $ g ^/ g_norm
    delta = stepSize * g_norm / (d-1)
    uu = (u + e^*(sinh delta + dot e (u ^* ( cosh delta -1)))) Linear.^/ (   cosh delta  + Linear.dot e (u ^* sinh delta))

    ue = dot u e
    zeta = exp (-delta)
    kinetic_energy_change = delta - log 2 + log (1 + ue + (1-ue)*zeta**2)


positionUpdateIsokinetic :: (Applicative f, KnownNat n, Num a) => a -> PhaseSpace n a -> f (PhaseSpace n a)
positionUpdateIsokinetic eps state = pure (state & position +~ eps *^ u)
  where u = state ^. momentum

positionUpdateEuclidean :: (Applicative f, KnownNat n, Num a, Floating a) => (forall b. Floating b => V n b -> b) -> a -> PhaseSpace n a -> f (PhaseSpace n a)
positionUpdateEuclidean kineticEnergy eps state = pure (state & position +~ eps *^ g)
  where g = grad kineticEnergy $ state ^. momentum


momentumUpdateIsokinetic :: (MonadDistribution m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> m (PhaseSpace dim Double)
momentumUpdateIsokinetic x y z = fst <$> runWriterT (momentumUpdateIsokinetic' x y z)

momentumUpdateEuclidean :: (MonadDistribution m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> m (PhaseSpace dim Double)
momentumUpdateEuclidean x y z = fst <$> runWriterT (momentumUpdateEuclidean' x y z)

momentumUpdateEuclidean' :: forall dim m . (MonadDistribution m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> WriterT Info m (PhaseSpace dim Double)
momentumUpdateEuclidean' logdensity stepSize (State x u) = tell (kin u - kin uu) >> pure (State x uu)
    where
    kin p = Sum $ 0.5 * dot p p
    g = grad logdensity x  :: V dim Double
    uu = u + (stepSize *^ g)


maruyama :: forall n m . (KnownNat n, MonadDistribution m) => Double -> Double -> PhaseSpace n Double -> m (PhaseSpace n Double)
maruyama l stepSize (State x u) = State x <$> partialRefresh stepSize l (u :: V n Double)
-- maruyama l stepSize state = state & momentum (partialRefresh stepSize l)  
-- maruyama l stepSize (State x u) = pure $ State x u

maruyama' :: forall n m . (KnownNat n, MonadDistribution m) => Double -> Double -> PhaseSpace n Double -> m (PhaseSpace n Double)
maruyama' l stepSize state = state & momentum (mapM (const (normal 0 (sqrt (stepSize/l)) )  ))

partialRefresh :: (KnownNat n, MonadDistribution f) => Double -> Double -> V n Double -> f (V n Double)
partialRefresh stepSize l u = signorm . (^+^ u) <$> z
    where
        d = fromIntegral $ V.dim u
        nu = sqrt ((exp (2 * stepSize / l) - 1.0) / d)
        z = (fmap . fmap) (nu *) (mapM (const (normal 0 1)) u)

data PDF n = PDF {df :: DensityFunction n}



type Natural = KnownNat

type Vector dim = V dim Double

type Distribution = MonadDistribution

type StochasticProcess m a b = forall cl. (TimeDomain (Time cl), Floating (Diff (Time cl)),  Ord (Diff (Time cl))) => ClSF m cl a b

type StochasticProcess' m a b = forall cl. (TimeDomain (Time cl), Double ~ Diff (Time cl),  Ord (Diff (Time cl))) => ClSF m cl a b







-- mixture :: MonadDistribution m => StochasticProcess m (Params, PDF 2) (V 2 Double)
mixture init = proc (params, input) -> do
  a <- mclmcDynamicsOnPhaseSpace init -< (params, input {df = normalLogDensity}, mclachlanCoeffs)
  c <- mclmcDynamicsOnPhaseSpace init -< (params, input {df = \x -> negate $ 0.5 * F.sum (fmap (**2) (x+5))}, mclachlanCoeffs)
  bool <- constM $ bernoulli 0.5 -< ()
  returnA -< if bool then a else c

mixtureDensity :: DensityFunction 2
mixtureDensity x = log (exp (normalLogDensity x) + exp (normalLogDensity (x + 5)))


-- beta2d :: Floating a => V2 a -> a
-- beta2d (V2 x y )= fromDouble' (density (betaDistr 2 2) (x )) + fromDouble' (density (betaDistr 2 2) y)

hierarchical :: MonadDistribution m => StochasticProcess m (Params, PDF 2) (V 2 Double)
hierarchical = proc (params, _) -> do
  x <- unadjustedMclmc initialState -< (params, PDF banana, mclachlanCoeffs)
  y <- unadjustedMclmc initialState -< (params, PDF (banana . (+ fmap fromDouble' x)), mclachlanCoeffs)
  returnA -< y

type STD = Double

posterior :: (MonadMeasure m) => STD -> StochasticProcess m (V 2 Double, Params, PDF 2) (Vector 2)
posterior std = proc (obs, params, b) -> do
  p <- unadjustedMclmc initialState -< (params, b, mclachlanCoeffs)
  -- i <- countDemoObservationModel -< fromV p
  -- arrM traceM -< show i
  -- arrM condition -< i == obs
  -- observe -< normalPdf 0 1 (norm $ position p)
  observe -< normalPdf2D (fromV obs) std (fromV p)
  -- last10 <- arr (take 100) . accumulateWith (:) [] -< p
  -- observe -<  (Exp $ log $ sum $ norm . momentum <$> last10)
  returnA -< p

constrained :: (InputVariables 2)  >-/-> PhaseSpace 2 Double
constrained = proc b -> do
  p <- mclmcDynamicsOnPhaseSpace initialState -< b
  arrM condition -< (>0) $ (\(V2 x y) -> y) (fromV $ p ^. position)
  returnA -< p







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

-- normalLogDensity :: DensityFunction 2
normalLogDensity :: (Foldable f, Functor f, Floating a) => f a -> a
normalLogDensity x = negate $ 0.5 * F.sum (fmap (**2) x)

banana :: DensityFunction 2
banana  = (\(V2 x y) -> negate $ 0.5 * (square (x / 10.0) + square (y - mu2 x))) . fromV
  where
        mu2 x = curvature * (x ** 2 - 100)
        square i = i*i
        curvature = 0.03

badBanana :: DensityFunction 2
badBanana  = (\(V2 x y) -> negate $ 0.5 * (square (x / 10.0) + square y  ) ) . fromV
  where
        mu2 x = curvature * (x ** 2 - 100)
        square i = i*i
        curvature = 0.03

bananaCorrection :: DensityFunction 2
bananaCorrection  = (\(V2 x y) -> negate $ 0.5 * (square (mu2 x) - 2* y* mu2 x)) . fromV
  where
        mu2 x = curvature * (x ** 2 - 100)
        square i = i*i
        curvature = 0.03

-- instance (Floating num, Eq num, Num num, Num n, Floating n) => VectorSpace (V dim num) n where 

goodBanana :: forall dist dim . (Distribution dist, Natural dim, dim ~ 2, MonadMeasure dist) => dist (PhaseSpace dim Double) -> StochasticProcess' dist (InputVariables dim) (V dim Double)
goodBanana init = proc vars -> do
  phasepoint@(State x u) <- overdampedLangevin init -< vars
  xLast <- shift 1 -< x
  (TimeInfo {sinceLast}) <- timeInfo -< ()
  let xdot =  (x ^-^ xLast) ^/ sinceLast
  -- let xdot = (x - xprev) / 2
  let f2 = Exp $ bananaCorrection $ fromV $  phasepoint ^. position
  let f1 = Exp $ badBanana $ fromV $ phasepoint ^. position
  let gradf2 = grad bananaCorrection $ fromV $ phasepoint ^. position
  observe -< f2 ** 2 + 2*f1*f2 + Exp (norm gradf2 / 2) + Exp (2 * norm (xdot ^* bananaCorrection (fromV $ phasepoint ^. position)))
  returnA -< phasepoint ^. position

-- (negate $ 0.5 * square (mu2 x) - 2 * mu2 x * y)

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
  deterministicIntegrator (o1, o2, coeffs, stepSize)

integrator' :: (KnownNat n, MonadDistribution m) =>
  (Double -> Operator m n
  , Double -> Operator m n
  , [Double]
  , Double
  , Double)
  -> Operator m n
integrator' (o1, o2, coeffs, l, stepSize) =
  maruyama' l stepSize <=<
  deterministicIntegrator (o1, o2, coeffs, stepSize)

deterministicIntegrator' :: (KnownNat n, MonadDistribution m) =>
  (Double -> Operator m n
  , Double -> Operator m n
  , [Double]
  , Double
  , Double)
  -> Operator m n
deterministicIntegrator' (o1, o2, coeffs, l, stepSize) =
  -- maruyama l stepSize <=<
  deterministicIntegrator (o1, o2, coeffs, stepSize)



deterministicIntegrator :: (Num d, Monad m) => (d -> c -> m c, d -> c -> m c, [d], d) -> c -> m c
deterministicIntegrator (o1, o2, coeffs, stepSize) =
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
  negate (State p m) = State (negate p) (negate m)
  fromInteger :: (Num a, KnownNat n) => Integer -> PhaseSpace n a
  fromInteger = error $ prettyCallStack callStack
  -- fromInteger i = State (fromInteger i) (fromInteger i)


drawParticle :: Monad m => Color -> MSF m (V2 Double, Log Double) Picture
drawParticle c = proc (position, probability) -> do
  -- drawBall -< (position, 0.1, withAlpha ( (double2Float $ exp $ 0.1 * ln probability)) violet)
  drawBall -< (position, 0.2, withAlpha ( into @Float $ exp $ 0.1 * ln probability) c)
  -- drawBall -< (position, 0.1, white)




sf :: UserInput >-&-> Picture
sf = proc userInput -> do
  (buttonPic, leapfrog) <- GUI.button buttonParams {buttonPos = V2 (-350) 300} -< userInput
  (sliderPic, stepsizeFactor) <- slider (V2 (-300) 300) 60 -< userInput
  (sliderPic2, lFactor) <- slider (V2 (-250) 300) 60 -< userInput
  (sliderPic3, temp) <- arr (second (\x -> max 0.1 $ 10*x - 4)) .  slider (V2 (-275) 300) 60 -< userInput
  let mousePos = userInput ^. the @(V2 Double)
  let mousePosV = toV mousePos

  timeInterval <- sinceLastS -< ()
  -- result <- particleFilter params{n=200} (mclmc (banana, normalLogDensity) initialState) -< (10.0 * radius * timeInterval, 1, useBanana)

  -- result2 <- mapMSF (baz (variance 0) *** baz (variance 0)) . arr ( (\(V2 x y) -> zip x y) . (fromV) . sequence . fmap (position . fst) . take 10000) -< result
  -- -- arrM traceM -< show result2
  -- b1 <- drawBall -< ((/20) $ uncurry V2 (dAv ( result2)), 0.1, red)

  -- arrM traceM -< show radius2

  -- let withTemp n = ((*fromDouble n) .)
  -- let with_temperature :: forall a . Floating a => Double -> V 2 a -> a
  --     with_temperature b = (\x -> (*(fromDouble b :: a)) $ (normalLogDensity :: forall a. Floating a => V 2 a -> a) x )

  -- let input = Input useBanana (\x -> mixtureDensity x * fromDouble (10 * radius2))

  -- ob <- mclmc initialState -< (Params 0.1 1, Input useBanana (if useBanana then banana else normalLogDensity))
  -- i <- countDemoObservationModel -< fromV $ position ob
  -- result <- particleFilter params{n=200} posterior -< (i, useBanana)
  -- pic <- arr (scale 0.1 0.1) . drawBall -< (fromV $ position ob , 0.1, red)
  -- picture <- visualisation -< result
  -- returnA -< picture <> buttonPic <> pic


  let l = sqrt 2 --  max 0.1 (10 * lFactor)
  let stepSize = l / 5
  -- result <- particleFilter params{n=200} (mclmc initialState) -< (Params (stepSize * radius * 10 * timeInterval) l, input)
  -- result <- particleFilter params{n=200} (full initialState ) -< (PDF True banana, timeInterval*10)

  -- x <- mclmcDynamicsOnPhaseSpace initialState -< (Params (stepSize * radius * 10 * timeInterval) l temp, PDF banana)
  -- particleFilter params{n=200} (p process) -< (x, userInput, Params (stepSize * radius * 10 * timeInterval) l temp)
  -- pic <- drawBall -< (fromV x, 0.1, red)
  -- result <- if not useBanana then

  -- resultI <- particleFilter params{n=20} (mh initialState hmcProposal ) -< (Params (stepSize * radius * 20 * timeInterval) (10*l) temp, PDF banana)

  let coeffs = if leapfrog then leapfrogCoeffs else mclachlanCoeffs
  let neff = 100.0

  -- count <- C.count -< ()

  -- let result = fmap ( first (position . fst3) )  resultI
  -- rec
  --   let usedL = (if count < 500 then l else lTuning )
  --   result <- particleFilter params{n=50} process -< (userInput, (Params (stepSize) usedL temp, coeffs))
  --   lTuning <- arr (sqrt . F.sum . F.sum) . mapMSF (toMSF FOLD.variance) -< fst <$> result
  
  
  let usedL = l
      lTuning = l
  result <- particleFilter params{n=200} process -< (userInput, (Params (stepSize) usedL temp, coeffs))
  

    
  -- (biases ) <- mapMSF calculateBiases' -< result
  -- let meanbiases = foldr1 (Seq.zipWith (+)) biases
  -- let num_gradient_calls = Seq.lookup 0 $ fmap (fst) $ Seq.dropWhileL (\(_, d) -> d > (1/neff)) (Seq.zip (Seq.fromList (take (length biases) [(1 :: Int)..])) meanbiases)
  -- let num_gradient_calls = Seq.dropWhileL (\(_, d) -> d > (1/neff)) (Seq.zip (Seq.fromList (take (length biases) [(1 :: Int)..])) meanbiases)
  -- let (num_gradient_calls) = fromMaybe (-1) . Seq.lookup 0 $ (fmap ((4*) . fromIntegral . fst)) . Seq.takeWhileR snd $ Seq.zip (Seq.fromList (take (length meanbiases) [(1 :: Int)..])) $ fmap (< (1/neff)) meanbiases

  -- num_gradient_calls' <- toMSF (FOLD.prefilter (>0) FOLD.mean) -< num_gradient_calls


  -- arrM traceM -< show (take 10 $ F.toList $ Seq.viewr meanbiases)
  -- arrM traceM -< show (Seq.zip (Seq.fromList (take (length meanbiases) [(1 :: Int)..])) $ fmap (< (1/neff)) meanbiases)
  -- arrM traceM -< show "AND"
  -- arrM traceM -< show (num_gradient_calls')
    -- (((4.0)*) . fromIntegral . fromMaybe (-1) . findIndex (< (1/neff))) meanbiases
  -- let ess = (neff/)  num_gradient_calls'
  picture <- visualisation red -< result

  lvisual <- arr (\l -> line [(0, 0), (15*l, 0)]) -< into @Float lTuning

  let V2 mousePosX mousePosY = userInput ^. the @(V2 Double)

  let circ = translate (into @Float mousePosX) (into @Float mousePosY) (circle 0.1)
  let circ2 = translate (into @Float mousePosX) (into @Float mousePosY) (circle 0.3)

  -- circ <- circle -< (mousePosV, 0.1, red)


  returnA -<
    -- picture2 <> picture  <> picture2' <> picture' <> 
    picture <> circ <> circ2
    
    -- <>
    -- buttonPic <> sliderPic <> sliderPic2 <> translate (-200) 400 (scale 0.1 0.1 (text "1: Normal  2: Banana   3: Mixture (direct)   4: Mixture (indurect)   5: Unnormalized   6: Bayesian")) <> scale 0.1 0.1 mempty <> sliderPic3
    
    -- <> translate (-200) (-400) (scale 0.1 0.1 (text ("L: " ++ show usedL ++ "  StepSize: " ++ show stepsizeFactor ++ "  Temp: " ++ show temp)))
    -- <> translate (-200) (-350) (scale 0.1 0.1 (text ("Avg ess: " ++ show ess)))
    -- <> translate (-200) (-375) (scale 0.1 0.1 (text ("L (stage 2): " ++ show (lTuning))))
    -- <> lvisual

  where

    -- process :: (UserInput, (Params, Integrator)) >-/-> V 2 Double
    process = arr (^. position) . multipleSwitch' signals (State ones ones :: PhaseSpace 2 Double) 1


    normal2D x1 x2 v1 v2 = negate $ 0.5 * ((x1 - v1)**2 + (x2 - v2)**2)

    

    -- process :: (UserInput, (Params, Integrator)) >-/-> V 2 Double
    -- process = proc (userInput, (params, coeffs)) -> do
    --   let V2 mousePosX mousePosY = userInput ^. the @(V2 Double)
    --   -- let abstractedMousePos = (V2 (fromDouble mousePosX) (fromDouble mousePosY)) :: Floating a => V2 a
    --   p <- mclmcDynamicsOnPhaseSpace (pure $ (State ones ones :: PhaseSpace 2 Double)) -< (params, PDF (\x -> let V2 x1 x2 = fromV x in normalLogDensity (toV (V2 (x1 + 0.1 * negate (fromDouble mousePosX)) (x2 + 0.1 * negate (fromDouble mousePosY))))), coeffs )
    --   -- p <- mclmcDynamicsOnPhaseSpace (pure 0) -< (params, PDF (\x -> normal x mousePosV), coeffs )
    --   returnA -< p ^. position

    
    
    -- calculateBiases' = proc results -> do
    --   resultsHistory <- arr (fmap snd) . historySince 10 -< fst results
    --   e_x2 <- (arr (FOLD.postscan (FOLD.mean))) -< fmap (**2) (resultsHistory)
    --   returnA -< f <$> e_x2
      

    -- mean x = F.sum x / fromIntegral (length x)



    signals 1 = \v -> proc (params, coeffs) -> do

      p <- mclmcDynamicsOnPhaseSpace (pure v) -< (params, PDF (\x -> normalLogDensity (x )), coeffs )
      -- p <- gibbsDynamics ( v) (PDF normalLogDensity) -< ( )
      returnA -< p
    signals 2 = \v -> proc (params, coeffs) -> do
      p <- mclmcDynamicsOnPhaseSpace (pure v) -< (params, PDF banana, coeffs )
      -- p <- gibbsDynamics ( v) (PDF banana) -< ( )
      returnA -< p
    signals 3 = \v -> proc (params, coeffs) -> do
      p <- hmcDynamicsOnPhaseSpace (pure v) (\x -> (norm x)^2) -< (params, PDF banana, coeffs )
      -- p <- gibbsDynamics ( v) (PDF banana) -< ( )
      returnA -< p
    signals 4 = \v -> proc (params, coeffs) -> do
    --   -- p <- mclmcDynamicsOnPhaseSpace (pure v) -< (params, PDF mixtureDensity, coeffs )
      p <- mclmcDynamicsOnPhaseSpace (pure v)  -< (params, PDF mixtureDensity, coeffs )
      -- p <- gibbsDynamics ( v) (PDF mixtureDensity) -< ( )
      returnA -< p
    -- signals 4 = \v -> proc (params, coeffs) -> do
    --   -- p <- mixture (pure v) -< (params, PDF banana )
    --   p <- mchmcDynamicsOnPhaseSpace (pure v) (\v -> log ((v `dot` v) / (getdim @2)) * (0.5 * getdim @2))  -< (params, PDF banana, coeffs )
    --   returnA -< p
    signals 5 = \v -> proc (params, coeffs) -> do
      p <- constrained -< (params, PDF normalLogDensity, coeffs)
      returnA -< p
    signals 6 = \v -> proc (params, coeffs) -> do
      p <- mclmcDynamicsOnPhaseSpace (pure v) -< (params, PDF $ log . (\vec -> (let (V2 x y) = fromV vec in if y > 0 then 1 else 0.1) * exp (normalLogDensity vec)), coeffs)
      returnA -< p
    signals 7 = \v -> proc (Params eps _ temp, coeffs) -> do
      x <- full (pure v) -< (PDF banana, (eps, temp))
      returnA -< x
    signals _ = undefined


instance Num (Maybe Double) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = Just . fromInteger

instance Fractional (Maybe Double) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = Just . fromRational

traceIt :: (Monad m, Show a) => MSF m a a
traceIt = arr (\x -> trace (show x) x)

sf2 :: UserInput >--> Picture
sf2 = proc userInput -> do

  let params = Params 0.1 20 1
      pdf = PDF banana

  result <- fullFilter -< ()
  picture <- visualisation violet -< result
  returnA -< picture

  where

    fullFilter :: a >--> [(Vector 2, Log Double)]
    fullFilter = particleFilter params{n=200} proc _ -> do

      let params = Params 0.1 20 1
          pdf = PDF banana

      latent <- unadjustedMclmc initialState -< (params, pdf, mclachlanCoeffs)
      noise1 <- constM (normal 0 std) -< ()
      noise2 <- constM (normal 0 std) -< ()
      let noise = toV $ V2 noise1 noise2
      result <- posterior std -< (latent + noise, params, pdf)
      returnA -< result

      where std = 2


sf3 :: UserInput >--> Picture
sf3 = proc userInput -> do

  let


  latent <- unadjustedMclmc initialState -< (prms, pdf, mclachlanCoeffs)
  noise1 <- constM (normal 0 std) -< ()
  noise2 <- constM (normal 0 std) -< ()
  let noise = toV $ V2 noise1 noise2
  result <- particleFilter params{n=200} (posterior std) -< (latent + noise, prms, pdf)
  picture <- visualisation violet -< result
  ball <- drawBall -< (fromV latent, 0.15, red)
  obs <- arr (scale 0.1 0.1) . drawBall -< (fromV (latent + noise), 0.1, white)
  returnA -< picture <> scale 0.1 0.1 ball <> obs

  where
    std = 10
    prms = Params 0.1 20 1
    pdf = PDF banana


-- proj :: PhaseSpace 2 Double -> Int -> PhaseSpace 1 Double
-- proj (State x u) lens = State (x ^. lens)(u ^. lens)

  -- proc _ -> do
  -- p1 <- mchmcDynamicsOnPhaseSpace (pure $ State 1 1) -< (Params 0.1 1 1, PDF normalLogDensity)
  -- p2 <- mchmcDynamicsOnPhaseSpace (pure $ State 1 1) -< (Params 0.1 1 1, PDF normalLogDensity)
  -- -- p2 <- banana1D -< 0
  -- returnA -< concatPhaseSpacePoints p1 p2

-- normalLogDensity :: (Foldable f, Functor f, Floating a) => f a -> a
-- normalLogDensity x = negate $ 0.5 * F.sum (fmap (**2) x)

couple :: (a, b) -> ((a,c) >--> b) -> ((b,c) >--> a) -> (c >--> (a, b))
couple initial x y = feedback initial proc (c, (a,b)) -> do
  a' <- y -< (b, c)
  b' <- x -< (a, c)
  let out = (a',b')
  returnA -< (out, out)

concatPhaseSpacePoints :: PhaseSpace 1 Double -> PhaseSpace 1 Double -> PhaseSpace 2 Double
-- concatPhaseSpacePoints :: PhaseSpace n1 a -> PhaseSpace n2 a -> PhaseSpace n3 a
concatPhaseSpacePoints (State (V pos) (V mom)) (State (V pos2) (V mom2)) = State (V (pos Vec.++ pos2) ) (V (mom Vec.++ mom2))



sf4 :: UserInput >--> Picture
sf4 = proc userInput -> do



  timeInterval <- sinceLastS -< ()


  let stepSize = 1.0
  let l = 5.0



  -- result <- particleFilter params{n=200} (arr position . gibbsDynamics (State 1 1) (PDF banana)) -< (userInput, Params (stepSize * 10 * timeInterval) l 1)
  -- picture <- visualisation red -< result


  -- result <- particleFilter params{n=200} (arr position . mchmcDynamicsOnPhaseSpace (pure $ State 1 1 ) ) -< (Params (stepSize * 10 * timeInterval) l 1, PDF banana)
  -- picture <- visualisation red -< result

  result2 <- particleFilter params{n=200} (goodBanana (pure $ State 1 1) ) -< (Params (stepSize * 10 * timeInterval) l 1, PDF badBanana, mclachlanCoeffs)
  picture2 <- visualisation violet -< result2

  -- result2 <- particleFilter params{n=200} (arr position . rescaled (pure $ State 1 1 ) ) -< (Params (stepSize * 10 * timeInterval) l 1, PDF banana)
  -- picture2 <- visualisation violet -< result2

  result3 <- particleFilter params{n=200} (arr (^. position) . overdampedLangevin (pure $ State 1 1 ) ) -< (Params (stepSize * 10 * timeInterval) l 1, PDF (\x -> badBanana x + bananaCorrection x), mclachlanCoeffs)
  picture3 <- visualisation green -< result3

  -- result2 <- particleFilter params{n=200} (arr position . constrained) -< (Params (stepSize * 10 * timeInterval) l 1, PDF banana)
  -- picture2 <- visualisation violet -< result2

  -- p = normalLogDensity

  -- x <- foo -< ()
  -- y :: m (V2 Double) <- attemptR foo -< ()
  -- let z = replicateM 20 y
  -- z' :: [(V2 Double, Log Double)] <- arr (fmap (\x -> (x,1))) . baz2 -< z
  -- picture3 <- visualisation red -< first toV <$> z'
  -- picture2 <- visualisation violet -< [(toV x, 1)]


  returnA  -< 

    -- picture <>


    picture3 <>
    picture2 <>
    translate (-200) 400 (scale 0.1 0.1 (text "Banana")) <> scale 0.1 0.1 mempty




foo :: (Time t ~ Double, MonadDistribution m) => ClSF m t () (V2 Double)
foo = proc () -> do
    x <- stochasticOscillator 1 1 -< 1
    y <- stochasticOscillator 1 1 -< 1
    returnA -< V2 x y

-- bar :: (Monad c, Applicative b, MonadDistribution c) => MSF
--   (ReaderT
--      (TimeInfo a)
--      b)
--   ()
--   (ReaderT
--      (TimeInfo t) c Double)
-- bar :: MonadDistribution m => ClSF Identity t () (m Double)

bar :: (Time cl ~ Double, MonadDistribution n, MonadDistribution m) => ClSF n cl () (m (V2 Double))
bar = attemptR foo

baz :: Monad m => MSF m (m a) a
baz = MSF (\ma -> do
                      x <- ma
                      return (x, baz))

baz2 :: Monad m => ClSF m t (m a) a
baz2 = timeless baz

-- ban :: (Time t ~ Double)=> ClSF SamplerIO t () Double
-- ban = baz2 . bar


attempt :: (Monad m, Applicative n) => MSF m a b -> MSF n a (m b)
attempt (MSF inp) = MSF \x -> let b = inp x in pure (fst <$> b, attempt $ performOnFirstSample $ snd <$> b)

attemptR :: (Monad m, Applicative n, Monad n) => ClSF m t a b -> ClSF n t a (m b)
-- attemptR :: ClSF m t a1 b -> ClSF m t a1 (m b)
attemptR = D.readerS  . attempt . D.runReaderS

attempt' :: (MonadMeasure m, Applicative n) => MSF m a b -> MSF n a [(b, Log Double)]
attempt' x = undefined -- fmap (sampleIO . runPopulationT) $ attempt x

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

getdim :: forall dim a . (Natural dim, Num a) => a
getdim = fromIntegral $ TP.natVal (Proxy @dim)

fromDouble' :: forall a . Floating a => Double -> a
fromDouble' d = (/1000) $ fromIntegral (floor (d*1000))



--------------------------------------------------------------------------------
-- * Example: discrete rescaling: I think this is not correct, need to use clocks directly

-- rescaled :: forall dist dim . (Distribution dist, Natural dim) => dist (PhaseSpace dim Double) -> StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
-- rescaled init = proc inputVars -> do

--   phasepoint <- mchmcDynamicsOnPhaseSpace init -< inputVars
--   let scale = (fromDouble  (dim @dim / norm (momentum phasepoint )))
--   phasepoint_new <- delayBy' 1 -< (phasepoint, scale)
--   returnA -< phasepoint_new

-- -- -- -- | A geometric object with a certain 'Dimension'.
-- -- class KnownNat (Dimension x) => Manifold x where
-- --     type Dimension x :: Nat

-- -- dimension0 :: Manifold x => Proxy (Dimension x) -> Proxy x -> Int
-- -- {-# INLINE dimension0 #-}
-- -- dimension0 prxy _ = integerToInt $ TP.natVal prxy

-- -- -- | The 'Dimension' of the given 'Manifold'.
-- -- dimension :: Manifold x => Proxy x -> Int
-- -- {-# INLINE dimension #-}
-- -- dimension = dimension0 Proxy



--------------------------------------------------------------------------------
-- * Example: asynchronous gibbs 

-- sdeRhine :: Rhine
--   (GlossConcT IO)
--   (SequentialClock
--      (RescaledClock GlossSimClockIO Double)
--      (RescaledClock GlossSimClockIO Double))
--   ()
--   ()
-- sdeRhine =

--    Sequential
--         (Synchronous (morphS
--           (hoist (lift . sampleIO))

--           (proc ((), V (d)) -> do
--             x <- hmcDynamicsOnPhaseSpace (pure $ State 1 1) -< (Params 1 5 1, PDF (\(V y) -> banana (V $ Vec.fromList [y Vec.! 0, fromDouble' (d Vec.! 0)])), mclachlanCoeffs)
--               -- (y Vec.! 0) $  fromDouble' (d Vec.! 0)))
--             arr _position -< x)

--           ) :: SN (GlossConcT IO) (RescaledClock GlossSimClockIO Double) ((), XX) YY)
--         (keepLast 1 :: ResamplingBuffer (GlossConcT IO) (Out (RescaledClock GlossSimClockIO Double)) (In (RescaledClock GlossSimClockIO Double)) XX XX)
--         (Synchronous (morphS
--           (hoist (lift . sampleIO))

--           (proc y@(V (d)) -> do
--             x <- hmcDynamicsOnPhaseSpace (pure $ State 1 1) -< (Params 1 5 1, PDF (\(V y) -> flipV banana (V $ Vec.fromList [y Vec.! 0, fromDouble' (d Vec.! 0)])), mclachlanCoeffs)
--               -- (y Vec.! 0) $  fromDouble' (d Vec.! 0)))
--             returnA -< ((x ^. position,y), x ^. position)))

--           :: SN (GlossConcT IO) (RescaledClock GlossSimClockIO Double) YY ((XX, YY), XX))



    --- (SequentialClock glossClock glossClock)

-- type XX = Position 1
-- type YY = Position 1


-- feR :: Rhine
--   (GlossConcT IO) (SequentialClock (RescaledClock GlossSimClockIO Double) (RescaledClock GlossSimClockIO Double)) () ()
-- feR =
--   Rhine
--   (Postcompose
--   (
--     Feedback (keepLast 1 ) sdeRhine
--   )
--   (proc (V y, V x) -> do
--               -- arrM traceM -< show x
--               arrMCl paintAllIO -< scale 20 20 $ translate (into @Float $ x Vec.! 0) (into @Float $ y Vec.! 0) $ circleSolid 1) )
--   (SequentialClock glossClock glossClock)

--------------------------------------------------------------------------------
-- * Example: synchronous gibbs


-- curryV :: PDF 2 -> (PhaseSpace 1 Double, (UserInput, Params)) >--> PhaseSpace 1 Double
-- curryV (PDF pdf) = proc (State (V d) _, (_, params)) -> do
--   (x, _,_) <- (mh (pure $ State 1 1) hmcProposal) -< (params, PDF (\(V y) -> pdf (V $ Vec.fromList [y Vec.! 0, fromDouble' (d Vec.! 0)])), mclachlanCoeffs)
--     -- (y Vec.! 0) $  fromDouble' (d Vec.! 0)))
--   returnA -< x

-- curryV' :: PDF 2 -> (PhaseSpace 1 Double, (UserInput, Params)) >--> PhaseSpace 1 Double
-- curryV' (PDF pdf) = proc (State (V d) _, (_, params)) -> do
--   x <- hmcDynamicsOnPhaseSpace (pure $ State 1 1) -< (params, PDF (\(V y) -> pdf (V $ Vec.fromList [y Vec.! 0, fromDouble' (d Vec.! 0)])), mclachlanCoeffs)
--     -- (y Vec.! 0) $  fromDouble' (d Vec.! 0)))
--   returnA -< x

-- flipV :: forall {k1} {k2} {n1 :: k1} {a} {c} {n2 :: k2}. (V n1 a -> c) -> V n2 a -> c
-- flipV = (. \(V x) -> V $ Vec.reverse x)

-- gibbsDynamics :: PhaseSpace 2 Double -> PDF 2 -> (UserInput, Params) >--> PhaseSpace 2 Double
-- gibbsDynamics (State x u) (PDF pdf) = arr (uncurry concatPhaseSpacePoints) . couple (State (V $ Vec.fromList [x ^. _1]) (V $ Vec.fromList [u ^. _2]), State (V $ Vec.fromList [x ^. _1]) (V $ Vec.fromList [u ^. _2])) (curryV' $ PDF $ flipV pdf) (curryV' $ PDF pdf )


--------------------------------------------------------------------------------
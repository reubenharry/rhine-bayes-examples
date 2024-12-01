{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module SDE where




import Control.Monad ( (<=<), forever, replicateM )
import GHC.Float (float2Double)
import Linear.V ( dim, V(V), Finite (fromV, toV), fromVector, Dim )
import Linear ((^*), (*^), Additive ((^+^), (^-^)), Metric (norm, dot, signorm), (^/), V2 (V2), V3 (V3))
import qualified Data.Vector as Vec
import GHC.TypeLits (Nat, KnownNat, natVal)
import qualified GHC.TypeLits as TP
import Numeric.AD (grad)
import Data.MonadicStreamFunction (arrM, Arrow (first, arr, second), returnA, morphS, accumulateWith, constM)
import Data.MonadicStreamFunction.InternalCore ( MSF(MSF), feedback )
import Control.Monad.Bayes.Class (MonadDistribution(..), Log (..), MonadMeasure, condition, Measure, MonadFactor, mvNormal)
import Control.Category ( Category((.), id) )
import Prelude hiding (id, (.))
import FRP.Rhine (timeless, ClSF, TimeDomain (Diff), Clock (Time, Tag), RescaledClock (unscaledClock, RescaledClock, rescale), TimeInfo (TimeInfo, sinceLast), RescaledClockS (RescaledClockS, unscaledClockS, rescaleS), UTCTime, MonadIO (liftIO), sinceLastS, sinceInitS, ResamplingBuffer, SN (..), Out, In, keepLast, SequentialClock (SequentialClock), Rhine (Rhine), arrMCl, VectorSpace, timeInfo)
import FRP.Rhine.Gloss (GlossSettings (display), defaultSettings, Display (InWindow), GlossSimClockIO (GlossSimClockIO), scale, Picture, withAlpha, translate, text, white, red, Color, green, paintAllIO, circleSolid)
import Control.Monad.Morph (MFunctor (hoist), MonadTrans (lift))
import Example ( decayingIntegral, drawBall, stochasticOscillator)
import Inference (particleFilter, params, SMCSettings (n))
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Foldable (Foldable(..))
import Util ( normalPdf2D, observe, type (>-->), type (>-/->) )
import qualified Data.Matrix as VecMat 
import GUI
    ( button, buttonParams, slider, ButtonConfig(buttonPos), multipleSwitch' )
import Control.Monad.Bayes.Sampler.Strict
import Concurrent (UserInput)
import Control.Monad.Trans.MSF (ReaderT, performOnFirstSample, runWriterS, WriterT (runWriterT), tell)
import FRP.Rhine.Gloss.IO (GlossConcT)
import Data.Time (addUTCTime, getCurrentTime)
import qualified Data.Vector as Vect
import Data.Maybe (fromMaybe)
import Control.Foldl (Fold(Fold))
import Numeric.Sum (KBNSum, kbn, Summation (..))
import qualified Data.Foldable as F
import Witch (into)
import FRP.Rhine.Gloss.Common (violet)
import qualified Control.Foldl as FOLD
import Debug.Trace (traceM)
import Data.Data (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (callStack, prettyCallStack)
import qualified FRP.Rhine.ClSF as C
import FRP.Rhine.ClSF (try)
import Data.Monoid (Sum(Sum, getSum))
import Smoothing (shift, delayBy')
import Control.Lens ((^.), _1, _2, Identity)
import Control.Monad.Bayes.Population (runPopulationT)
import qualified Control.Monad.Trans.MSF as D
import qualified Linear.Matrix as Mat
import GHC.Num (integerToInt)
import Data.Singletons
import Data.Singletons.TH ( genSingletons, SuppressUnusedWarnings (suppressUnusedWarnings) )
import Data.Kind (Type)

-- -- | A geometric object with a certain 'Dimension'.
class KnownNat (Dimension x) => Manifold x where
    type Dimension x :: Nat

dimension0 :: Manifold x => Proxy (Dimension x) -> Proxy x -> Int
{-# INLINE dimension0 #-}
dimension0 prxy _ = integerToInt $ TP.natVal prxy

-- | The 'Dimension' of the given 'Manifold'.
dimension :: Manifold x => Proxy x -> Int
{-# INLINE dimension #-}
dimension = dimension0 Proxy


-- data Fiber (m :: BaseManifold) where
--   Fiber :: Double -> Fiber m

-- foo' :: Fiber (Q d :: BaseManifold)
-- foo' = Fiber 3

-- data Fiber (m :: BaseManifold) where
--     Section :: (d :: Double) -> Fiber (Q d)

-- type FiberBundle (m :: BaseManifold) = (BaseManifold, Fiber m)

-- foo :: FiberBundle
-- foo = undefined

-- | A 'Point' on a 'Manifold'. The phantom type @m@ represents the 'Manifold', and the phantom type
-- @c@ represents the coordinate system, or chart, in which the 'Point' is represented.
newtype Point c x =
    Point { coordinates :: V (Dimension x) Double }
    deriving (Eq,Ord,Show)

data Bundle x n = Bundle (x n)

data Euclidean (n :: Nat)

-- | 'Cartesian' coordinates on 'Euclidean' space.
data Cartesian

-- | 'Polar' coordinates on 'Euclidean' space.
data Polar


foo' :: Bundle Euclidean 2
foo' = undefined

data BundleCoords x = BundleCoords x

type c # x = Point c x

infix 3 #

foobar' :: Point (BundleCoords Cartesian) (Bundle Euclidean 2)
foobar' = Point $ V $ Vec.fromList [1, 2, 3 ,4]

baz' :: Point (BundleCoords Polar) (Bundle Euclidean 2)
baz' = transition foobar'

class Transition c d x where
    transition :: c # x -> d # x

listCoordinates :: c # x -> [Double]
{-# INLINE listCoordinates #-}
listCoordinates = toList . coordinates

instance Transition (BundleCoords Cartesian)  (BundleCoords Polar) (Bundle Euclidean 2) where
    {-# INLINE transition #-}
    transition rphi =
        let [r, phi, tr, tphi] = listCoordinates rphi
            x = r * cos phi
            y = r * sin phi
         in Point $ V $ Vec.fromList [x, y, tr, tphi]

instance Transition Polar Cartesian (Euclidean 2) where
    {-# INLINE transition #-}
    transition rphi =
        let [r, phi] = listCoordinates rphi
            x = r * cos phi
            y = r * sin phi
         in Point $ V $ Vec.fromList [x, y]

instance Transition Cartesian Polar (Euclidean 2) where
    {-# INLINE transition #-}
    transition xy =
        let [x, y] = listCoordinates xy
            r = sqrt $ (x * x) + (y * y)
            phi = atan2 y x
         in Point $ V $ Vec.fromList [r, phi]

    -- Bundle :: forall m . (Manifold m) => Point (Chart m) m -> Fiber m -> Bundle m

-- tangent space

-- fiber

-- fiber bundle

-- phase space

data PhaseSpace' = P {q :: Double, p' :: Double}

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
mclmc :: forall dist dim . (Distribution dist, Natural dim) =>
  InitialDistribution dim dist -> StochasticProcess dist (InputVariables dim) (Position dim)
mclmc initial = proc params -> do
  phasepoint <- mclmcDynamicsOnPhaseSpace initial -< params
  returnA -< position phasepoint

rescaled :: forall dist dim . (Distribution dist, Natural dim) => InitialDistribution dim dist -> StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
rescaled init = proc inputVars -> do

  phasepoint <- mchmcDynamicsOnPhaseSpace init -< inputVars
  let scale = (fromDouble  (dim / norm (momentum phasepoint )))
      dim = fromIntegral $ TP.natVal (Proxy @dim)
  phasepoint_new <- delayBy' 1 -< (phasepoint, scale)
  returnA -< phasepoint_new



mclmcDynamicsOnPhaseSpace :: forall dist dim . (Distribution dist, Natural dim) => InitialDistribution dim dist -> StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
mclmcDynamicsOnPhaseSpace initial = proc (Params stepSize l temp, PDF df ) -> do
    timeless (accumulateWithM integrator initial) -< (
        positionUpdate,
        momentumUpdate ((/fromDouble' temp) . df),
        mclachlanCoeffs,
        l,
        stepSize)

hmcDynamicsOnPhaseSpace :: forall dist dim . (Distribution dist, Natural dim) => InitialDistribution dim dist -> StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
hmcDynamicsOnPhaseSpace initial = proc (Params stepSize l temp, PDF df ) -> do
    timeless (accumulateWithM integrator initial) -< (
        positionUpdateEuclidean (\u -> 0.5 * F.sum (fmap (**2) u)),
        momentumUpdateEuclidean ((/fromDouble' temp) . df),
        mclachlanCoeffs,
        l,
        stepSize)

overdampedLangevin :: forall dist dim . (Distribution dist, Natural dim) => InitialDistribution dim dist -> StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
overdampedLangevin initial = proc (Params stepSize l temp, PDF pdf1 ) -> do
    timeless (accumulateWithM step initial) -< (grad pdf1, stepSize)
    where
      dim :: Int 
      dim = fromInteger $ TP.natVal (Proxy @dim)
      step :: (V dim Double -> V dim Double, Double) -> PhaseSpace dim Double -> dist (PhaseSpace dim Double)
      step (graddf, stepsize)(State x u) = do
        -- g <- normal 0 1
        g <- mvNormal (Vec.fromList $ replicate ( dim) 0) (VecMat.identity dim)
        let h  = fromMaybe undefined (fromVector g) :: V dim Double
        let x' = x + graddf x  ^* stepsize  + sqrt (2 * stepsize) *^ h
        -- let u' = fmap (+ sqrt (2 * stepsize) * g) (u - (graddf x ^* stepsize))
        -- let x' = x + stepsize * u'
        return $ State x' u

mchmcDynamicsOnPhaseSpace :: forall dist dim . (Distribution dist, Natural dim) => InitialDistribution dim dist -> StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double)
mchmcDynamicsOnPhaseSpace initial = proc (Params stepSize l temp, PDF df ) -> do
    timeless (accumulateWithM integrator initial) -< (
        positionUpdateEuclidean (\v -> log (norm v / dim) * dim),
        momentumUpdateEuclidean ((/fromDouble' temp) . df),
        mclachlanCoeffs,
        l,
        stepSize)

  where dim :: Floating a => a
        dim = fromDouble' $ fromIntegral $ TP.natVal (Proxy @dim)

mchmcProposal :: (Natural dim, Distribution dist) => Proposal dim dist
mchmcProposal init = runWriterS proc (Params stepSize _ temp, PDF df) -> do
    (accumulateWithM deterministicIntegrator ( lift $ lift init)) -< (
        positionUpdate,
        momentumUpdate' ((/fromDouble' temp) . df),
        mclachlanCoeffs,
        stepSize)

hmcProposal :: (Natural dim, Distribution dist) => Proposal dim dist
hmcProposal init = runWriterS proc (Params stepSize _ temp, PDF df) -> do
    (accumulateWithM deterministicIntegrator ( lift $ lift init)) -< (
        positionUpdateEuclidean (\x -> 0.5 * F.sum (fmap (**2) x)),
        momentumUpdateEuclidean' ((/fromDouble' temp) . df),
        mclachlanCoeffs,
        stepSize)

type InitialDistribution dim dist = dist (PhaseSpace dim Double)

type Proposal dim dist = dist (PhaseSpace dim Double) -> StochasticProcess dist (InputVariables dim) (Info, PhaseSpace dim Double)

type InputVariables dim = (Params,  PDF dim)



mh :: forall dist dim . (Distribution dist, Natural dim) => dist (PhaseSpace dim Double) ->
  (forall d . Distribution d => Proposal dim d) ->
  StochasticProcess dist (InputVariables dim) (PhaseSpace dim Double, [PhaseSpace dim Double], Bool)
mh init proposal = C.safely $ loop init where
  loop s = do
    len <- C.once $ \(Params _ l _, b) -> poisson l
    -- traceM $ show len
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


acceptance :: MonadDistribution m => Double -> PhaseSpace dim Double -> PhaseSpace dim Double -> InputVariables dim -> m (PhaseSpace dim Double, Bool, Double)
acceptance deltaEnergy startState endState (_, PDF logDensity) = do
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



demoProcess :: (UserInput, Params) >-/-> V 2 Double
demoProcess =
  arr position . multipleSwitch' signals (State ones ones :: PhaseSpace 2 Double) 1
  where
      -- signals 1 = id
    signals 1 = \v -> proc params -> do

      p <- mclmcDynamicsOnPhaseSpace (pure v) -< (params, PDF normalLogDensity )
      -- p <- gibbsDynamics ( v) (PDF normalLogDensity) -< ( )
      returnA -< p
    signals 2 = \v -> proc params -> do
      p <- mclmcDynamicsOnPhaseSpace (pure v) -< (params, PDF banana )
      -- p <- gibbsDynamics ( v) (PDF banana) -< ( )
      returnA -< p
    -- signals 2 = \v -> proc params -> do
    --   p <- mclmcDynamicsOnPhaseSpace (pure v) -< (params, PDF banana )
    --   p2 <- mclmcDynamicsOnPhaseSpace (pure v) -< (params, PDF banana )
    --   p2' <- shift 100 -< p2
      -- returnA -< p - p2'
    signals 3 = \v -> proc params -> do
      p <- mclmcDynamicsOnPhaseSpace (pure v) -< (params, PDF mixtureDensity )
      -- p <- gibbsDynamics ( v) (PDF mixtureDensity) -< ( )
      returnA -< p
    signals 4 = \v -> proc params -> do
      p <- mixture (pure v) -< (params, PDF banana )
      returnA -< p
    signals 5 = \v -> proc params -> do
      p <- constrained -< (params, PDF normalLogDensity)
      returnA -< p
    signals 6 = \v -> proc params -> do
      p <- mclmcDynamicsOnPhaseSpace (pure v) -< (params, PDF $ log . (\vec -> (let (V2 x y) = fromV vec in if y > 0 then 1 else 0.1) * exp (normalLogDensity vec)))
      returnA -< p
    signals 7 = \v -> proc (Params eps _ temp) -> do
      x <- full (pure v) -< (PDF banana, (eps, temp))
      returnA -< x
    -- signals 8 = \v -> proc params -> do
    --   latent <- mclmc (pure v) -< (params, PDF  mixtureDensity)
    --   noisy <- arrM (\(V2 x y) -> do 
    --     x' <- normal x std
    --     y' <- normal y std
    --     return $ toV (V2 x' y')

    --      ) -< fromV latent
    --   p <- posterior -< (noisy, params, PDF  mixtureDensity)
    --   returnA -< p
    signals _ = undefined
    -- -- signals 2 = \v -> proc _ -> (+ v) <$> Example.prior -< ()
    -- signals 3 = const weakPrior
    -- signals 4 = moveWithArrows
    -- signals 5 = moveInLines
    -- signals 6 = gravity
    -- signals _ = gravity




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

  p <- mclmcDynamicsOnPhaseSpace init -< (params {stepSize = t, temperature = temp}, pdf)
  paramsNew <- tuning -< (position p, pdf)
  -- arrM traceM -< show paramsNew
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


positionUpdate :: (Applicative f, KnownNat n, Num a) => a -> PhaseSpace n a -> f (PhaseSpace n a)
positionUpdate e (State x u) = pure $ State (x ^+^ e Linear.*^ u) u

positionUpdateEuclidean :: (Applicative f, KnownNat n, Num a, Floating a) => (forall b. Floating b => V n b -> b) -> a -> PhaseSpace n a -> f (PhaseSpace n a)
positionUpdateEuclidean kineticEnergy e (State x u) = pure $ State (x ^+^ e Linear.*^ g) u where
  g = grad kineticEnergy u


momentumUpdate :: (MonadDistribution m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> m (PhaseSpace dim Double)
momentumUpdate x y z = fst <$> runWriterT (momentumUpdate' x y z)

momentumUpdateEuclidean :: (MonadDistribution m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> m (PhaseSpace dim Double)
momentumUpdateEuclidean x y z = fst <$> runWriterT (momentumUpdateEuclidean' x y z)

momentumUpdateEuclidean' :: forall dim m . (MonadDistribution m, KnownNat dim) => DensityFunction dim -> Double -> PhaseSpace dim Double -> WriterT Info m (PhaseSpace dim Double)
momentumUpdateEuclidean' logdensity stepSize (State x u) = tell (kin u - kin uu) >> pure (State x uu)
    where
    kin p = Sum $ 0.5 * dot p p
    g = grad logdensity x  :: V dim Double
    uu = u + (stepSize *^ g)

-- kineticEnergy :: (Foldable f, Functor f, Floating a) => f a -> a
-- kineticEnergy u = 0.5 * F.sum (fmap (**2) u)

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

type StochasticProcess' m a b = forall cl. (TimeDomain (Time cl), Double ~ (Diff (Time cl)),  Ord (Diff (Time cl))) => ClSF m cl a b







-- mixture :: MonadDistribution m => StochasticProcess m (Params, PDF 2) (V 2 Double)
mixture init = proc (params, input) -> do
  a <- mclmcDynamicsOnPhaseSpace init -< (params, input {df = normalLogDensity})
  c <- mclmcDynamicsOnPhaseSpace init -< (params, input {df = \x -> negate $ 0.5 * F.sum (fmap (**2) (x+5))})
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

type STD = Double

posterior :: (MonadMeasure m) => STD -> StochasticProcess m (V 2 Double, Params, PDF 2) (Vector 2)
posterior std = proc (obs, params, b) -> do
  p <- mclmc initialState -< (params, b)
  -- i <- countDemoObservationModel -< fromV p
  -- arrM traceM -< show i
  -- arrM condition -< i == obs
  -- observe -< normalPdf 0 1 (norm $ position p)
  observe -< normalPdf2D (fromV obs) std (fromV p)
  -- last10 <- arr (take 100) . accumulateWith (:) [] -< p
  -- observe -<  (Exp $ log $ sum $ norm . momentum <$> last10)
  returnA -< p

constrained :: (Params, PDF 2)  >-/-> PhaseSpace 2 Double
constrained = proc b -> do
  p <- mclmcDynamicsOnPhaseSpace initialState -< b
  arrM condition -< (>0) $ (\(V2 x y) -> y) (fromV $ position p)
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

goodBanana :: forall dist dim . (Distribution dist, Natural dim, dim ~ 2, MonadMeasure dist) => InitialDistribution dim dist -> StochasticProcess' dist (InputVariables dim) (V dim Double)
goodBanana init = proc vars -> do
  phasepoint@(State x u) <- overdampedLangevin init -< vars
  xLast <- shift 1 -< x
  (TimeInfo {sinceLast}) <- timeInfo -< ()
  let xdot =  (x ^-^ xLast) ^/ ( sinceLast)
  -- let xdot = (x - xprev) / 2
  let f2 = Exp $ bananaCorrection $ fromV $ position phasepoint
  let f1 = Exp $ badBanana $ fromV $ position phasepoint
  let gradf2 = grad bananaCorrection $ fromV $ position phasepoint
  observe -< (f2 ** 2) + 2*f1*f2 + Exp (norm (gradf2) / 2) + Exp (2 * norm (xdot ^* (bananaCorrection $ fromV $ position phasepoint)))
  returnA -< position phasepoint

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


drawParticle :: Monad m => Color -> MSF m (V2 Double, Log Double) Picture
drawParticle c = proc (position, probability) -> do
  -- drawBall -< (position, 0.1, withAlpha ( (double2Float $ exp $ 0.1 * ln probability)) violet)
  drawBall -< (position, 0.2, withAlpha ( into @Float $ exp $ 0.1 * ln probability) c)
  -- drawBall -< (position, 0.1, white)



sf :: UserInput >--> Picture
sf = proc userInput -> do
  (buttonPic, useBanana) <- GUI.button buttonParams {buttonPos = V2 (-350) 300} -< userInput
  (sliderPic, radius) <- slider (V2 (-300) 300) 60 -< userInput
  (sliderPic2, radius2) <- slider (V2 (-250) 300) 60 -< userInput
  (sliderPic3, temp) <- arr (second (\x -> max 0.1 $ 10*x - 4)) .  slider (V2 (-275) 300) 60 -< userInput




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


  let stepSize = 1.0
  let l = max 0.1 (10 * radius2)
  -- result <- particleFilter params{n=200} (mclmc initialState) -< (Params (stepSize * radius * 10 * timeInterval) l, input)
  -- result <- particleFilter params{n=200} (full initialState ) -< (PDF True banana, timeInterval*10)

  -- x <- mclmcDynamicsOnPhaseSpace initialState -< (Params (stepSize * radius * 10 * timeInterval) l temp, PDF banana)
  -- particleFilter params{n=200} (p demoProcess) -< (x, userInput, Params (stepSize * radius * 10 * timeInterval) l temp)
  -- pic <- drawBall -< (fromV x, 0.1, red)
  -- result <- if not useBanana then

  -- resultI <- particleFilter params{n=20} (mh initialState hmcProposal ) -< (Params (stepSize * radius * 20 * timeInterval) (10*l) temp, PDF banana)


  -- let result = fmap ( first (position . fst3) )  resultI
  result <- particleFilter params{n=200} demoProcess -< (userInput, Params (stepSize * radius * 10 * timeInterval) l temp)
  picture <- visualisation red -< result

  returnA -<
    -- picture2 <> picture  <> picture2' <> picture' <> 
    picture <>
    buttonPic <> sliderPic <> sliderPic2 <> translate (-200) 400 (scale 0.1 0.1 (text "1: Normal  2: Banana   3: Mixture (direct)   4: Mixture (indurect)   5: Unnormalized   6: Bayesian")) <> scale 0.1 0.1 mempty <> sliderPic3


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

      latent <- mclmc initialState -< (params, pdf)
      noise1 <- constM (normal 0 std) -< ()
      noise2 <- constM (normal 0 std) -< ()
      let noise = toV $ V2 noise1 noise2
      result <- posterior std -< (latent + noise, params, pdf)
      returnA -< result

      where std = 2


sf3 :: UserInput >--> Picture
sf3 = proc userInput -> do

  let


  latent <- mclmc initialState -< (prms, pdf)
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


curryV :: PDF 2 -> (PhaseSpace 1 Double, (UserInput, Params)) >--> PhaseSpace 1 Double
curryV (PDF pdf) = proc (State (V (d)) _, (_, params)) -> do
  (x, _,_) <- (mh (pure $ State 1 1) hmcProposal) -< (params, PDF (\(V y) -> pdf (V $ Vec.fromList [y Vec.! 0, fromDouble' (d Vec.! 0)])))
    -- (y Vec.! 0) $  fromDouble' (d Vec.! 0)))
  returnA -< x

curryV' :: PDF 2 -> (PhaseSpace 1 Double, (UserInput, Params)) >--> PhaseSpace 1 Double
curryV' (PDF pdf) = proc (State (V (d)) _, (_, params)) -> do
  x <- hmcDynamicsOnPhaseSpace (pure $ State 1 1) -< (params, PDF (\(V y) -> pdf (V $ Vec.fromList [y Vec.! 0, fromDouble' (d Vec.! 0)])))
    -- (y Vec.! 0) $  fromDouble' (d Vec.! 0)))
  returnA -< x

flipV :: forall {k1} {k2} {n1 :: k1} {a} {c} {n2 :: k2}. (V n1 a -> c) -> V n2 a -> c
flipV = (. \(V x) -> V $ Vec.reverse $ x)

gibbsDynamics :: PhaseSpace 2 Double -> PDF 2 -> (UserInput, Params) >--> PhaseSpace 2 Double
gibbsDynamics (State x u) (PDF pdf) = arr (uncurry concatPhaseSpacePoints) . couple (State (V $ Vec.fromList [(x ^. _1)]) (V $ Vec.fromList [u ^. _2]), State (V $ Vec.fromList [(x ^. _1)]) (V $ Vec.fromList [u ^. _2])) (curryV' $ PDF $ flipV pdf) (curryV' $ PDF pdf )


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

  result2 <- particleFilter params{n=200} (goodBanana (pure $ State 1 1) ) -< (Params (stepSize * 10 * timeInterval) l 1, PDF badBanana)
  picture2 <- visualisation violet -< result2

  -- result2 <- particleFilter params{n=200} (arr position . rescaled (pure $ State 1 1 ) ) -< (Params (stepSize * 10 * timeInterval) l 1, PDF banana)
  -- picture2 <- visualisation violet -< result2

  result3 <- particleFilter params{n=200} (arr position . overdampedLangevin (pure $ State 1 1 ) ) -< (Params (stepSize * 10 * timeInterval) l 1, PDF (\x -> badBanana x + bananaCorrection x))
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


  returnA -<
    scale 0.5 0.5 $

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




































-- sdeRhine :: Rhine
--   (GlossConcT IO)
--   (SequentialClock
--      (RescaledClock GlossSimClockIO Double)
--      (RescaledClock GlossSimClockIO Double))
--   ()
--   ()
sdeRhine =

   Sequential
        (Synchronous (morphS
          (hoist (lift . sampleIO))

          (proc ((), (V (d))) -> do
            x <- hmcDynamicsOnPhaseSpace (pure $ State 1 1) -< (Params 1 5 1, PDF (\(V y) -> banana (V $ Vec.fromList [y Vec.! 0, fromDouble' (d Vec.! 0)])))
              -- (y Vec.! 0) $  fromDouble' (d Vec.! 0)))
            returnA -< position x)

          ) :: SN (GlossConcT IO) (RescaledClock GlossSimClockIO Double) ((), XX) (YY))
        (keepLast 1 :: ResamplingBuffer (GlossConcT IO) (Out (RescaledClock GlossSimClockIO Double)) (In (RescaledClock GlossSimClockIO Double)) (XX) (XX))
        (Synchronous (morphS
          (hoist (lift . sampleIO))

          (proc (y@(V (d))) -> do
            x <- hmcDynamicsOnPhaseSpace (pure $ State 1 1) -< (Params 1 5 1, PDF (\(V y) -> (flipV banana) (V $ Vec.fromList [y Vec.! 0, fromDouble' (d Vec.! 0)])))
              -- (y Vec.! 0) $  fromDouble' (d Vec.! 0)))
            returnA -< ((position x,y), position x)))

          :: SN (GlossConcT IO) (RescaledClock GlossSimClockIO (Double)) (YY) ((XX, YY), XX))



    --- (SequentialClock glossClock glossClock)

type XX = Position 1
type YY = Position 1


feR :: Rhine
  (GlossConcT IO) (SequentialClock (RescaledClock GlossSimClockIO Double) (RescaledClock GlossSimClockIO Double)) () ()
feR =
  Rhine
  (Postcompose
  (
    Feedback (keepLast 1 ) (sdeRhine)
  )
  (proc (V y, V x) -> do
              -- arrM traceM -< show x
              arrMCl paintAllIO -< scale 20 20 $ translate (into @Float $ ( x) Vec.! 0) (into @Float $ ( y) Vec.! 0) $ circleSolid 1) )
  (SequentialClock glossClock glossClock)

attempt :: (Monad m, Applicative n) => MSF m a b -> MSF n a (m b)
attempt (MSF inp) = MSF \x -> let b = inp x in pure (fst <$> b, attempt $ performOnFirstSample $ snd <$> b)

attemptR :: (Monad m, Applicative n, Monad n) => ClSF m t a b -> ClSF n t a (m b)
-- attemptR :: ClSF m t a1 b -> ClSF m t a1 (m b)
attemptR = D.readerS  . attempt . D.runReaderS

attempt' :: (MonadMeasure m, Applicative n) => MSF m a b -> MSF n a ([(b, Log Double)])
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


fromDouble' :: forall a . Floating a => Double -> a
fromDouble' d = (/1000) $ fromIntegral (floor (d*1000))


module Paths where
import FRP.Rhine.Gloss hiding (normalize, shift)
import Example hiding (posterior, observationModel, prior, glossClock)
import Inference
import Control.Monad.Bayes.Sampler
import Control.Monad.Trans.Class
import qualified Example hiding (posterior, observationModel, prior)
import Control.Monad.Bayes.Population
import qualified Data.Vector.Sized as V
import Control.Monad.Bayes.Class
import Control.Comonad.Env
import Control.Applicative
import Control.Comonad.Identity
import Communication ()
import Numeric.Log
import Data.Tuple
import GHC.TypeLits
import Control.Monad.Trans.MSF
import qualified Data.Vector as VV
import Data.List
import qualified Data.Ord as M

type SystemState = Env Double Position

prior :: (MonadInfer m, Diff td ~ Double, TimeDomain td) => BehaviourF m td () SystemState
prior = fmap (\x -> env 2 (V.fromTuple x)) $ walk1D &&& walk1D where

    walk1D = proc _ -> do
        dacceleration <- constM (normal 0 8 ) -< ()
        acceleration <- decayIntegral 1 -< dacceleration
        velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
        position <- decayIntegral 1 -< velocity
        -- observe -< normalPdf acceleration 1 position
        -- pastPosition <- shift 50 -< position 
        -- observe -< normalPdf position 1 pastPosition
        returnA -< position

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

restrictedPrior :: (MonadInfer m, Diff td ~ Double, TimeDomain td) => BehaviourF m td () SystemState

restrictedPrior = proc () -> do
    s@(radius, pos) <- fmap runEnv prior -< ()
    observe -< normalPdf radius 0.1 (norm pos)
    returnA -< uncurry env s


observationModel :: (MonadSample m, Diff td ~ Double, TimeDomain td) => BehaviourF m td SystemState Observation

-- observationModel :: StochasticSignalTransform Position Observation
observationModel = proc p -> do
    n <- fmap V.fromTuple $ noise &&& noise -< ()
    returnA -< n + snd (runEnv p)

    where 
        noise = constM (normal 0 std)


-- posterior ::StochasticSignalTransformUnnormalized Observation Position
posterior = proc (V2 oX oY) -> do
  latent@(EnvT _ (Identity (V2 trueX trueY))) <- restrictedPrior -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

shift :: Monad m => Int -> MSF m c c
shift n = accumulateWith (\x xs -> take n $ x : xs) [] >>> arr last

gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl Example.glossClock proc () -> do
            -- (actualPosition, measuredPosition) <- undefined
            actualPosition <- particleFilter 200 resampleMultinomial (snd . runEnv <$> restrictedPrior) -< ()
            chooseOne <- pick -< actualPosition
            measuredPosition <- observationModel -< env 2 chooseOne
            samples <- particleFilter 200 resampleMultinomial posterior -< measuredPosition
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = fmap (first (snd . runEnv)) samples
                                , measured = measuredPosition
                                , latent = chooseOne
                                }

pick :: MSF (ReaderT (TimeInfo (RescaledClock GlossSimClockIO Double)) (GlossConcT SamplerIO)) [(Position, Log Double)] Position
pick = proc inp -> do
    out <- arrM (\x -> do 
        let ps = snd <$> x
        let xs = fst <$> x
        let y = sortOn (M.Down . snd) x
        return $ fst $ head y) -< inp
    returnA -< out


averageOf :: KnownNat i => [(V.Vector i Double, Log Double)] -> V.Vector i Double
averageOf things =
  let

    normalized = runIdentity $ runPopulation $ normalize $ fromWeightedList $ Identity things
  in foldr (\(pos, d) n -> n + fmap (* (exp $ ln d)) pos) 0 things
--     fullWeight = Prelude.sum $ fst <$> properThings
--     sumOfThings = foldr (undefined) 0 $ undefined -- fmap (uncurry (*)) properThings
--   in (/ fullWeight) <$> sumOfThings 

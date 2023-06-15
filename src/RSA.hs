import Util
import Data.MonadicStreamFunction
import Control.Monad.Bayes.Class
import Inference (particleFilter, SMCSettings (n), params, particleFilterDiscreteTime)
import Control.Monad.Bayes.Enumerator
import qualified Data.Vector as V
import FRP.Rhine.Gloss (reactimateCl)
import FRP.Rhine
import Control.Monad.Trans.Class
import Control.Monad.Morph
import Control.Monad.Bayes.Sampler.Strict (sampleIO)
import Control.Monad.Trans.MSF (performOnFirstSample)


-- -- think through 

type Utterance = Maybe Bool
type World = Bool



l0 :: MonadMeasure m => MSF m Utterance World
-- l0 :: Utterance >-/-> World
l0 = proc u -> do
    w <- performOnFirstSample (uniformD [constM $ pure True, constM $ pure False]) -< ()
    arrM condition -< case
        u of
            Just b -> b == w
            Nothing -> True
    returnA -< w

uniformly :: [(a, Log Double)] -> Enumerator a
uniformly particles = do
    let (ps,vs) = unzip particles
    i <- logCategorical $ V.fromList vs
    pure $ ps !! i

s1 :: MonadMeasure m => MSF m  World (Utterance, [(World, Log Double)])
s1 = proc w -> do
    u <- constM (uniformD [Just True, Just False, Nothing, Nothing, Nothing, Nothing]) -< ()
    (_, l0Result) <- particleFilterDiscreteTime params{n=100} l0 -< u
    arrM factor -< Exp $ log $ mass (uniformly l0Result ) w
    returnA -< (u, l0Result)


-- m :: (Time cl ~ Int) => ClSF IO cl () ()
m :: MSF IO () ()
m = proc inp -> do
    i <- constM getLine -< ()
    let j = read i
    (_, out) <- morphS sampleIO (particleFilterDiscreteTime params{n=100} l0) -< j



    -- let out' =
    --         (length (filter (==Just True) $ fst . fst <$> out), length (filter (==Just False) $ fst . fst <$> out),
    --             length (filter (==Nothing) $ fst . fst <$> out))
    -- let out'' = (length (filter (== True) $ fst <$> ((snd . fst) =<< out)), length (filter (== False) $ fst <$> ((snd . fst) =<< out)))
    arrM print -< (out)


main :: IO ()
main = reactimate m
    -- reactimateCl (FixedStep @1) undefined 

-- l1 :: Utterance >--> (World, Convention)
-- l1 = proc u -> do
--     c <- hyperprior -< ()
--     w <- prior -< c
--     s1Result <- mass s1 -< w
--     arrM factor -< mass u s1Result
--     returnA -< (c, w)


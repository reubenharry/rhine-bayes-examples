{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}


module TwoObjects where
import Control.Monad.Bayes.Population
    ( resampleMultinomial )
import Data.MonadicStreamFunction
    ( returnA, (>>>), Arrow((&&&), (***)), withSideEffect_ )
import Control.Monad.Bayes.Class
    ( MonadSample(logCategorical) )
import FRP.Rhine
    ( reactimateCl )
import FRP.Rhine.Gloss
    ( Display(InWindow),
      defaultSettings,
      clearIO,
      launchGlossThread,
      GlossSettings(display) )
import Control.Monad.Bayes.Sampler ( sampleIO )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Numeric.Hamilton ()
import Numeric.LinearAlgebra.Static ()
import Inference (pattern V2,  particleFilter)
import Example
import qualified Data.Vector as VVV
import Control.Monad.Bayes.Enumerator



gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do

                actualPosition <- prior &&& prior -< ()
                measuredPosition <- observationModel *** observationModel -< actualPosition
                samples <- particleFilter 100 resampleMultinomial (posterior *** posterior) -< measuredPosition
                let (samples1, samples2) = let dist = fromListE samples in (logExplicit (fst <$> dist), logExplicit (snd <$> dist))
                (withSideEffect_ (lift clearIO) >>> (visualisation *** visualisation)) -< (Result {
                                    particles = samples1
                                    , measured = fst measuredPosition
                                    , latent = fst actualPosition
                                    },
                                    Result {
                                    particles = samples2
                                    , measured = snd measuredPosition
                                    , latent = snd actualPosition
                                    })
                returnA -< ()


fromListE foo = let (as, ps) = unzip foo in do
    i <- logCategorical (VVV.fromList ps)
    return (as !! i)









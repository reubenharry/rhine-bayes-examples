module SDE where

import FRP.Rhine 
import Util ( type (>-->) ) 
import Example
import Concurrent (UserInput)
import FRP.Rhine.Gloss
import Linear
import Control.Monad.Bayes.Class (MonadDistribution)
import Control.Monad.Trans.MSF (ReaderT)


m :: () >--> Double
m = proc _ -> do
    time <- sinceInitS -< ()
    let out = sin time
    c <- integral -< out 
    returnA -< (c)


-- mai :: UserInput >--> Picture
mai :: (Time cl ~ Double, MonadDistribution m, MonadIO m) => MSF
  (ReaderT
     (TimeInfo cl)
     m)
  t
  Picture
mai = proc _ -> do
    u <- m -< ()
    arrM (liftIO . print) -< show u
    renderObjects -< Result {measured = V2 u 0, latent = 1000, particles = []}

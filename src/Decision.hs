module Decision where 
import Util
import Linear
import FRP.Rhine.Gloss
import Concurrent
import Witch
import Control.Monad.Bayes.Class
import Inference
import Control.Category
import Prelude hiding (id, (.))
import Example (empirical)
import Data.MonadicStreamFunction.InternalCore
import Debug.Trace (traceM)

type (--||->) a b = SignalFunction Stochastic a b
type (--|/|->) a b = SignalFunction (Stochastic & Unnormalized) a b

-- walk1D :: SignalFunction Stochastic () Double
-- walk1D = proc _ -> do
--   dacceleration <- constM (normal 0 12) -< ()
--   acceleration <- decayingIntegral 5 -< dacceleration
--   velocity <- decayingIntegral 5 -< acceleration -- Integral, dying off exponentially
--   position <- decayingIntegral 5 -< velocity
--   returnA -< position

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp(-x))

agent :: () --|/|-> V2 Double
agent = proc _ -> do
    -- dir <- constM (uniformD [-2,2]) -< ()
    -- ((* (2 * pi)) . sigmoid) <$> Example.walk1D -< ()
    -- (* (2 * pi)) <$> constM (random) -< ()
    -- currentMS
    -- let vel = 20 * angle dir
    vel <- (\case True -> -20; False -> 20) <$> bernoulliProcess False 0.001 -< ()
    (pos, nextStep) <- stepper world -< vel 
    futurePos :: V2 Double <- arrM id -< runNSteps nextStep 1 vel
    let utility = 
        -- normalPdf (1.25 * pi) 0.1 dir
        -- negate $ Exp $ log $ pos `Linear.dot` vel
            Exp $ log $ Linear.norm futurePos
    arrM traceM -< show (pos, futurePos, vel, utility)
    observe -< 1 / utility
    returnA -< vel

world :: V2 Double --||-> V2 Double
world = (+200) <$> integral

main :: UserInput --||-> Picture
main = proc _ -> do
    vel <- arrM empirical . particleFilter params{n=100} agent -< ()
    V2 posX posY <- world -< vel
    returnA -< 
        translate (into @Float posX) (into @Float posY) (circle 20) <> circleSolid 10

    
stepper :: Monad m => MSF m a b -> MSF m a (b, MSF m a b)
stepper (MSF sf) = MSF \x -> do
        (result, newSF) <- sf x
        pure ((result, newSF), stepper newSF)
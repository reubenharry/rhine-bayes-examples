module MCMC where 

import Util
import Concurrent
import FRP.Rhine.Gloss
-- import FRP.Rhine.Gloss (MSF(..))
import Data.MonadicStreamFunction.InternalCore ( MSF(MSF) )
import Control.Monad.Bayes.Class
import Witch
import qualified Data.Map as Map
import Control.Category ((.))
import Prelude hiding ((.))
import Linear
import Example (Result(..), renderObjects, decayingIntegral, empirical)
import Inference
import Control.Lens
import Data.Generics.Product
import Text.Megaparsec (Parsec, optional, runParser, MonadParsec (eof))
import Data.Void (Void)
import qualified Data.Text as T
import Data.Maybe
import Text.Megaparsec.Char.Lexer (decimal)
import GHC.Float (int2Double)
import Control.Monad.Trans.MSF.List
import Debug.Trace (traceM)

getCommand :: Parsec Void T.Text (Either Int Int)
getCommand = do
    isPicture <- isJust <$> optional ":p "
    (if isPicture then Left else Right) <$> decimal

joint :: System (Stochastic & Unnormalized) (V2 Double) (Theta, V2 Double)
joint = proc obs -> do
    theta <- decayingIntegral 1 . decayingIntegral 1 . constM (normal 0 5) -< ()
    state <- posteriorDistribution -< (theta, obs)
    returnA -< (theta, state)

demo1 :: System (Stochastic & Feedback) UserInput Picture
demo1 = proc ui -> do

    -- inputText <- getTextFromGloss -< ui
    -- x <- hold (Left undefined) -< runParser (getCommand <* eof) "" <$> inputText
    -- let theta = case x of 
    --         Right (Right i) -> i
    --         _ -> 0
  
    latent <- worldModel -< -2.0
    observations <- observationModel -< latent

    (theta, beliefAboutState) <- onlinePMMH -< observations
    -- (beliefAboutStateAndTheta) <- particleFilter params joint -< observations
    -- theta <- arr fst . arrM empirical -< beliefAboutStateAndTheta
    -- (evidence, beliefAboutState) <- inferredPosterior -< (int2Double theta, observations)
    -- m <- arr (/100) . arr sum . arr (take 100) . accumulateWith (:) [] -< evidence
    -- evidence <- arr (fmap snd) . arr (drop 95) -< beliefAboutState

    pic <- renderObjects -< Result 
      {measured = observations, 
      latent = latent, 
    --   particles = fmap (first snd) beliefAboutStateAndTheta}
      particles = beliefAboutState}
    returnA -< pic <> text (show theta) 
    -- <> scale 0.1 0.1 (translate 50 0 (text (show m)))

var = 0.1
type Theta = Double
type Observation = V2 Double
type State = V2 Double

whiteNoise :: System Stochastic Theta Double
whiteNoise = arrM (flip normal var)

particlePosition1D :: System Stochastic Theta Double
particlePosition1D = decayingIntegral 1 . decayingIntegral 1 . whiteNoise

worldModel :: System Stochastic Theta State
worldModel = proc d -> do
  xPos <- particlePosition1D -< d
  yPos <- particlePosition1D -< d
  returnA -< V2 xPos yPos

observationModel :: System Stochastic State Observation
observationModel = proc latent -> do
  xNoise <- whiteNoise -< 0
  yNoise <- whiteNoise -< 0
  returnA -< latent + V2 xNoise yNoise

posteriorDistribution :: System (Stochastic & Unnormalized) (Theta, Observation) State
posteriorDistribution = proc (d, obs) -> do
  latent <- worldModel -< d
  observe -< (normalPdf2D obs var latent)
  returnA -< latent

inferredPosterior :: System Stochastic (Theta, Observation) (Log Double, [(State, Log Double)])
inferredPosterior = particleFilterWithEvidence params posteriorDistribution



initTheta = 0.0

logp x = sum ((x**2)/2)
logp' x = -((x**2)/2)

--

onlinePMMH :: System (Stochastic) (V2 Double) (Double, [(State, Log Double)])
onlinePMMH = feedback 0 $ proc (obs, theta) -> do
    
    theta' <- arrM (`normal` 0.2) -< theta
    c <- count -< ()
    [(pTheta, post), (pTheta', _)] <- mapMSF (particleFilterWithEvidence params{n=200} posteriorDistribution) -< [(theta, obs), (theta', obs)]
    pThetas <- arr product . arr (take 30) . accumulateWith (:) [] -< pTheta
    pThetas' <- arr product . arr (take 30) . accumulateWith (:) [] -< pTheta'
    let ratio = min 1 (pThetas' / pThetas) 
    -- let ratio = min 1 (
    --         (((exp (-(theta'**2)/2)) * ln (exp pTheta') ))
    --         / (((exp (-(theta**2)/2)) ) * ln (exp pTheta) ))
    accept <- arrM bernoulli -< ln $ exp ratio
    arrM traceM -< show (theta', ln pThetas', theta, ln pThetas, ratio, "theta' p, theta p")
    newTheta <- case c`mod`30==0 of 
        True -> arr (\((a,b), accept) -> if accept then a else b) -< ((theta', theta), accept)
        False -> returnA -< theta
    returnA -< ((newTheta, post), newTheta)


-- loop theta = do
--     evidence <- pf theta
--     newTheta <- mh evidence
--     loop newTheta
    

msf :: MonadDistribution m => MSF m (V2 Double) (V2 Double)
msf = MSF (\v@(V2 d1 d2) -> do
    proposed1 <- normal d1 1 
    proposed2 <- (normal d2 1)
    let proposed = V2 proposed1 proposed2
    let ratio = logp v - logp proposed
    let p = exp ratio
    accept <- bernoulli p
    return (if accept then proposed else v, msf))

main :: SignalFunction Stochastic UserInput Picture
main = proc _ -> do


  out <- msf -< initTheta
  numbers <- arr (take 1000) . accumulateWith (:) [] -< out
  let pic = Pictures [translate (into @Float v1 * 20) (into @Float v2 * 20) $ circle 2 | (V2 v1 v2) <- numbers]
  returnA -< pic


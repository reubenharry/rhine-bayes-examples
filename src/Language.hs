{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}


module Language where

import qualified Data.Vector.Sized as V
import FRP.Rhine
import Inference (pattern V2, V2, onlineSMC, StochasticSignal, NormalizedDistribution, StochasticSignalTransform, StochasticSignalTransformUnnormalized)
import FRP.Rhine.Gloss
import Numeric.Log
import GHC.Float
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Population
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Class
import Control.Monad.Trans.MSF (performOnFirstSample, ReaderT)
import qualified Data.Map as M
import Data.Maybe
import Witch

std :: Double
std = 0.5

type Observation = V.Vector 2 Double
type Position = V.Vector 2 Double

type Utterance = String

squareConstraint p v
    | abs p > 0.5 && abs p < 0.8 = - v
    | otherwise = v
-- if oldX >= 0.5 || oldY >= 0.5 then 0 else

basePrior :: V.Vector 2 Double -> StochasticSignal Position
basePrior v@(V2 x y) = fmap ((+v) . V.fromTuple) $ model1D x &&& model1D y where

    model1D d = feedback 0 proc (_, oldPosition) -> do
        acceleration <- constM (normal 0 8 ) -< ()
        -- acceleration <- decayIntegral 2 -< dacceleration
        velocity <- decayIntegral 2 -< squareConstraint (oldPosition+d) acceleration -- Integral, dying off exponentially
        position <- decayIntegral 2 -< squareConstraint (oldPosition+d) velocity
        let boundedPosition = position
            --  max (-0.5) $ min 0.5 position
        returnA -< (boundedPosition, boundedPosition)

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

generativeModel :: StochasticSignalTransform (Position, Color) Observation
generativeModel = proc (p, c) -> do
    n <- fmap V.fromTuple $ noise &&& noise -< ()
    -- outlier <- constM $ bernoulli 0.1 -< ()
    -- r <- uni *** uni -< ((),()) 
    returnA -< p + n -- if outlier then V.fromTuple r else p + n

    where 
        noise = constM (normal 0 std)
        -- uni = constM (fmap ((*3) . (\x -> x -0.5)) random)


posterior ::StochasticSignalTransformUnnormalized (Maybe Observation, Maybe Utterance) (Position, Color)
posterior = proc (obs, utterance) -> do
  latent@(V2 trueX trueY, color) <- prior -< ()
--   pred@(V2 trueX trueY) <- generativeModel -< latent
  case obs of 
    Just (V2 oX oY) -> arrM factor -< normalPdf oY std trueY * normalPdf oX std trueX
    Nothing -> returnA -< ()
  case utterance of 
    Just u -> arrM condition -< semantics u latent
    -- Just "the particle is green" -< arrM condition -< (if b then trueX < 0.5 && trueX > (-0.5) &&  trueY < 0.5 && trueY > (-0.5) else True)
    _ -> returnA -< ()
  returnA -< latent

semantics "the particle is in the box" latent@(V2 trueX trueY, _) = trueX < 0.5 && trueX > (-0.5) &&  trueY < 0.5 && trueY > (-0.5)
semantics "the particle is green" (_ , color) = color == green
semantics _ _ = True

prior :: StochasticSignal (Position, Color)
prior = pos &&& col

    where 
        col = constM (uniformD [red, green])
        
        pos = performOnFirstSample $ do 
            x <- random 
            y <- random 
            return $ basePrior $ (V.fromTuple (x,y) - 0.5) * 2


-- loopPrior :: V.Vector 2 Double -> StochasticSignal Position
-- loopPrior vec = feedback vec proc (_, p) -> do
--     v@(V2 x y) <- basePrior vec -< ()
--     let new = if norm p < 0.5 && norm v >= 0.5 then p else v
--     returnA -< (new , new)
--     -- returnA -< undefined


----------
-- display
----------

gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            actualPosition <- basePrior $ V.fromTuple (0,0) -< () -- basePrior (V.fromTuple (0,0)) -< ()
            measuredPosition <- generativeModel -< (actualPosition, green)
            n <- count -< ()
            if n > 40 && n < 75 then arrMCl paintIO -<  translate (-80) 200 $ scale 0.2 0.2 $ text "The particle is in the box" else returnA -< ()
            if n > 160 then arrMCl paintIO -<  translate (-80) 200 $ scale 0.2 0.2 $ text "The particle is green" else returnA -< () 
            let obs = if n < 100 then Nothing else Just measuredPosition
            samples <- onlineSMC 200 resampleMultinomial posterior -< (obs, if n > 40 && n < 50 then Just "the particle is in the box" else if n > 160 then Just "the particle is green" else Nothing)
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = first fst <$> samples
                                , measured = obs
                                , latent = actualPosition
                                , colorEstimate = first ((\x -> if x ==red then "red" else "green") . snd) <$> samples
                                }


visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
visualisation = proc Result { particles, measured, latent, colorEstimate} -> do

  arrMCl paintIO -< color red $ line [(-95, -95), (95, -95), (95, 95), (-95, 95), (-95, -95)]
--   arrMCl paintIO -< color red $ circleSolid 0.7 --  [(-0.7, -0.7), (0.7, -0.7), (0.7, 0.7), (-0.7, 0.7)]
  drawParticles -< particles
  case measured of 
    Just m -> drawBall -< (m, 0.05, red)
    Nothing -> returnA -< ()
  drawBall -< (latent, 0.1, withAlpha 0.5 green)
  drawColor -< colorEstimate
  
  

drawColor :: MonadIO m => MSF (ReaderT (TimeInfo cl) (GlossConcT m)) [(String, Log Double)] ()
drawColor = proc colorEstimate -> do
    let dict = foldr (\(k,v) -> M.alter (\case Nothing -> Just v; Just a -> Just (v+a)) k) mempty colorEstimate
    -- arrMCl paintIO -< translate (-80) (-200) $ scale 0.2 0.2 $ text $ show $ dict
    let pos1 = exp $ ln $ fromMaybe 0 $ M.lookup "green" dict
    let pos2 = exp $ ln $ fromMaybe 0 $ M.lookup "red" dict
    let pos = pos1 
    arrM (liftIO . print) -< pos
    arrMCl paintIO -< translate 0 (-300) $ line [(-100, 0), (100, 0)]
    arrMCl paintIO -< translate (-30) (-275) $ scale 0.1 0.1 $ text "What color is the ball?"
    arrMCl paintIO -< translate (-130) (-300) $ scale 0.1 0.1 $ text "Red"
    arrMCl paintIO -< translate (110) (-300) $ scale 0.1 0.1 $ text "Green"
    arrMCl paintIO -< translate (into @Float $ (pos - 0.5) * 200) (-330) $ polygon [(-10,0), (10, 0), (0, 15)]

drawBall :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) ()
drawBall = proc (V2 x y, width, theColor) -> do
    arrMCl paintIO -<
        scale 150 150 $
        translate (double2Float x) (double2Float y) $
        color theColor $
        circleSolid $
        double2Float width

drawParticle :: MonadIO m => BehaviourF (GlossConcT m) td (Position, Log Double) ()
drawParticle = proc (position, probability) -> do
  drawBall -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) violet)

drawParticles :: MonadIO m => BehaviourF (GlossConcT m) td [(Position, Log Double)] ()
drawParticles = proc particles -> do
  case particles of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps


data Result = Result
  {
    --   estimate :: Position
    -- stdDev :: Double
   measured :: Maybe Observation
  , latent :: Position
  , particles :: [(Position, Log Double)]
  , colorEstimate :: [(String, Log Double)]
  }
  deriving Show

-- glossClock :: LiftClock (GlossConcT SamplerIO) IdentityT GlossSimClockIO
-- glossClock = liftClock GlossSimClockIO



glossClock :: RescaledClock GlossSimClockIO Double
glossClock = RescaledClock
  { unscaledClock = GlossSimClockIO
  , rescale = float2Double
  }



{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TupleSections #-}



{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MutualStoch where

import FRP.Rhine.Gloss hiding (Up, Down)
import Prelude hiding (until)
import qualified Example hiding (renderObjects)
import qualified Control.Monad.Morph as MM
import Linear.V2 (V2(..), _x, _y, unangle)
import Control.Monad.Bayes.Class
import Numeric.Log
import Control.Monad.Bayes.Sampler
import Control.Monad.Morph
import Inference hiding (V2)
import Control.Monad.Bayes.Population (resampleMultinomial)
import Active (averageOf, calculateXandYVariances)
import Witch (into)
import Control.Lens hiding (both)
import Control.Monad (forever, void)
import Control.Concurrent (newMVar, forkIO, swapMVar, MVar, readMVar)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad.Trans.MSF (switch, performOnFirstSample)
import Circular (Angle (Angle))
import Example (drawBall', interpret, Position, Result(..))
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Text (Text)
import qualified Linear as L
import Debug.Trace (traceM)
import Communication (empirical)
import Control.Monad.Bayes.Enumerator (enumerate)
import Concurrent (GlossInput, keys)
import qualified Data.Set as S
import Example (walk1D)
import Data.Maybe (fromMaybe)
import qualified Debug.Trace as Debug
import qualified Control.Category as C

-- bouncing

std :: Double
std = 0.5


data State where
  State :: {_ball1Pos :: V2 Double,
            _ball2Pos :: V2 Double}
           -> State
  deriving Show

-- a simple macro, not important
$(makeLenses ''State)

type AgentInput = V2 Double
type Observation = V2 Double
type Particles = [(State, Log Double)]


-- "Process Deterministic x y" is the type of signal functions 
-- where the input signal is (Time -> x)
-- and the output signal is (Time -> y)
-- So: (Time -> x) -> (Time -> y)
-- THIS IS NOT THE SAME AS A TIME-VARYING FUNCTION!! Time -> (x -> y)
-- "SignalFunction Stochastic x y" then is (effectively): (Time -> x) -> Distribution (Time -> y)


both :: SignalFunction (Stochastic & Feedback) () (Position, Position)
both = proc () -> do
    rec
        ball1 <- iPre 0 >>> moveAwayFrom -< ball2
        ball2 <- moveAwayFrom -< ball1
    returnA -< (ball1, ball2)

moveAwayFrom :: SignalFunction Stochastic Position Position
moveAwayFrom = feedback 0 proc (otherBall, prevPos@(V2 prevPosX prevPosY)) -> do

    dacceleration <- constM (normal 0 8 ) &&& constM (normal 0 8 ) -< ()
    acceleration <- decayingIntegral 1 -< uncurry V2 dacceleration
    let repulsion = fmap (savediv (safeNorm (prevPos - otherBall) ))
            (prevPos - otherBall )
    velocity <- decayingIntegral 1 -< acceleration + repulsion -- Integral, dying off exponentially
    position <- decayingIntegral 1 -< velocity

    returnA -< (position, position)


moveTowards :: SignalFunction Stochastic Position Position
moveTowards = feedback 0 proc (otherBall, prevPos@(V2 prevPosX prevPosY)) -> do

    dacceleration <- constM (normal 0 8 ) &&& constM (normal 0 8 ) -< ()
    acceleration <- decayingIntegral 1 -< uncurry V2 dacceleration
    let repulsion = (negate $ prevPos - otherBall )
    velocity <- decayingIntegral 1 -< acceleration + repulsion -- Integral, dying off exponentially
    position <- decayingIntegral 1 -< velocity

    returnA -< (position, position)

moveTowardsWhenCertain :: SignalFunction Stochastic [(Position, Log Double)] Position
moveTowardsWhenCertain = feedback 0 proc (otherBall, prevPos@(V2 prevPosX prevPosY)) -> do

    -- dacceleration <- constM (normal 0 8 ) &&& constM (normal 0 8 ) -< ()
    -- acceleration <- decayingIntegral 1 -< uncurry V2 dacceleration
    -- let repulsion = (negate $ prevPos - otherBall )
    let (varianceX, varianceY) = calculateXandYVariances otherBall
    let expectation = expected otherBall
    let force = if varianceX < 0.3 && varianceY < 0.3 then (negate $ prevPos - expectation) else 0
    -- arrM traceM -< show (varianceX, varianceY)
    velocity <- decayingIntegral 1 -< force * 4 -- Integral, dying off exponentially
    position <- decayingIntegral 1 -< velocity

    returnA -< (position, position)




safeNorm 0 = 0
safeNorm x = norm x

decayingIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)


prior :: SignalFunction Stochastic () Position
prior = proc _ -> do
  x <- walk1D -< ()
  y <- walk1D -< ()
  returnA -< V2 x y

-- prior :: SignalFunction Stochastic AgentInput State
-- prior = movement where

--     movement :: SignalFunction Stochastic AgentInput State
--     movement  = feedback (State 0 0) proc (vel, oldState) -> do
--         ball2Position <- iPre 0 >>> decayIntegral 1 -< vel
--         ball1Position <- ball1 -< (ball2Position, oldState)
--         returnA -< (State {
--             _ball2Pos = ball2Position ,
--             _ball1Pos = ball1Position}, oldState)

--     ball1 :: SignalFunction Stochastic (AgentInput, State) Position
--     ball1 = proc (V2 ball2PositionX ball2PositionY, oldState) -> do
--         ball1PosAx <- walk1D -< ()
--         ball1PosAy <- walk1D -< ()
--         ballPosX <- decayIntegral 1 >>> decayIntegral 1 -< ball1PosAx +
--             2*(oldState ^. ball1Pos . _x - ball2PositionX  ) `savediv`
--             norm (oldState ^. ball1Pos . _x - ball2PositionX) **2
--         ballPosY <- decayIntegral 1  >>> decayIntegral 1 -< ball1PosAy +
--             2*(oldState ^. ball1Pos . _y - ball2PositionY  ) `savediv`
--             norm (oldState ^. ball1Pos . _y - ball2PositionY) **2
--         iPre 0 -< V2 ballPosX ballPosY

    where

    walk1D :: SignalFunction Stochastic () Double
    walk1D = proc _ -> do
        dacceleration <- constM (normal 0 8 ) -< ()
        acceleration <- decayingIntegral 1 -< dacceleration
        velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
        position <- decayingIntegral 1 -< velocity
        returnA -< position


    decayingIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)


observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
    (x,y) <- (noise &&& noise) -< ()
    returnA -< p + V2 x y
    where noise = constM (normal 0 std)

posterior :: Process (Stochastic & Unnormalized) (Observation, Position) Position
posterior = proc (ballAPos, ballBObs) -> do

    ballB <- iPre 0 >>> moveAwayFrom -< ballAPos
    observe -< normalPdf2D ballB std ballBObs
    returnA -< ballB


posteriorMaybeObs :: SignalFunction (Stochastic & Unnormalized) (Observation, Bool) Position
posteriorMaybeObs = proc (V2 oX oY, condition) -> do
  latent@(V2 trueX trueY) <- prior -< ()
  if condition then
    observe -< normalPdf oY std trueY * normalPdf oX std trueX
    else returnA -< ()
  returnA -< latent

normalPdf2D :: V2 Double -> Double -> V2 Double -> Log Double
normalPdf2D (V2 x1 y1) std (V2 x2 y2) = normalPdf x1 std x2 * normalPdf y1 std y2

mainSimple :: SignalFunction (Stochastic & Feedback) Text Picture
mainSimple = proc inputText -> do
    rec
        ball1 <- iPre 0 >>> moveAwayFrom -< ball2
        ball2 <- moveAwayFrom -< ball1
    obsBall1 <- observationModel -< ball1
    obsBall2 <- observationModel -< ball2
    inferredBall1 <- particleFilter params {n = 50} posterior -< (ball2, obsBall1)
    inferredBall2 <- particleFilter params {n = 50} posterior -< (ball1, obsBall2)
    pic1 <- renderObjects yellow -< Result obsBall1 ball1 inferredBall1
    pic2 <- renderObjects green -< Result obsBall2 ball2 inferredBall2
    returnA -< pic1 <> pic2




main :: SignalFunction (Stochastic & Feedback) Text Picture
main = proc inputText -> do
  rec
    ballObs1 <- (iPre 0 >>> observationModel) -< ball1
    inferred1 <- (particleFilter params {n = 20} posterior) -< (ball2, ballObs1)
    expectedBall1 <- (arr expected) -< inferred1
    ball2 <- (iPre 0 >>> moveAwayFrom) -< expectedBall1


    ballObs2 <- (iPre 0 >>> observationModel) -< ball2
    inferred2 <- (particleFilter params {n = 20} posterior) -< (ball1, ballObs2)
    expectedBall2 <- (arr expected) -< inferred2
    ball1 <- moveAwayFrom -< expectedBall2

  pic1 <- renderObjects yellow -< Result ballObs1 ball1 inferred1
  pic2 <- renderObjects green -< Result ballObs2 ball2 inferred2
  returnA -< pic1 <> pic2







mainComplex :: SignalFunction (Stochastic & Feedback) Text Picture
mainComplex = proc inputText -> do
  rec
    ballObs1 <- iPre 0 >>> observationModel -< ball1
    inferred1 <- particleFilter params {n = 20} posterior -< (ball2, ballObs1)
    expectedBall1 <- arr expected -< inferred1
    ball2 <- moveAwayFrom -< expectedBall1


    ballObs2 <- iPre 0 >>> observationModel -< ball2
    inferred2 <- particleFilter params {n = 10} posteriorRank2 -< (ball1, ballObs2)
    expectedBall2 <- arr expected -< inferred2
    ball1 <- moveAwayFrom -< expectedBall2

  pic1 <- renderObjects yellow -< Result ballObs1 ball1 inferred1
  pic2 <- renderObjects green -< Result ballObs2 ball2 inferred2
  returnA -< pic1 <> pic2




selfBelief :: SignalFunction (Stochastic & Feedback) Text Picture
selfBelief = proc inputText -> do
    rec
        ballObs <- iPre 0 >>> observationModel -< ball
        inferredBall <- particleFilter params {n = 20} post -< (ballObs, ball)
        expectedBall <- arr expected -< inferredBall
        ball <- moveTowards -< expectedBall

    pic <- renderObjects yellow -< Result ballObs ball inferredBall
    returnA -< pic

    where

        post :: SignalFunction (Stochastic & Unnormalized) (Observation, Position) Position
        post = proc (obs, ball) -> do
            pos <- iPre 0 >>> moveTowards -< ball
            -- pos <- Example.prior -< ()
            observe -< normalPdf2D pos std obs
            returnA -< pos

followWhenCertain :: SignalFunction (Stochastic & InputOutput) GlossInput Picture
followWhenCertain = proc glossInput  -> do

    ball1 <- Example.prior -< ()
    -- (_, showObs) <- interpret -< inputText
    showObs <- hold mempty >>> arr (^. contains (Char 'o')) -< if S.null (glossInput ^. keys ) then Nothing else Just (glossInput ^. keys )
    -- arrM (liftIO . print) -< undefined
    ballObs <- iPre 0 >>> observationModel -< ball1
    inferredBall <- particleFilter params {n = 100} posteriorMaybeObs -< (ballObs, showObs)
    ball2 <- moveTowardsWhenCertain -< inferredBall

    pic <- renderObjects yellow -< Result (if showObs then ballObs else 1000) ball1 inferredBall
    pic2 <- renderObjects green -< Result 1000 ball2 []
    returnA -< pic <> pic2

    where

        post :: SignalFunction (Stochastic & Unnormalized) (Observation, Position) Position
        post = proc (obs, ball) -> do
            pos <- iPre 0 >>> moveTowards -< ball
            -- pos <- Example.prior -< ()
            observe -< normalPdf2D pos std obs
            returnA -< pos


convention :: SignalFunction (Stochastic & Feedback) Text Picture
convention = proc inputText -> do
  rec

    convention1 <- iPre [(True, 1)] >>> arrM empirical -< c1
    ballObs1 <- iPre (0, True) >>> observationModel -< (ball1, convention1)
    inferred1 <- particleFilter params {n = 100} languagePosterior -< (ball2, ballObs1)
    let (inferredBall1, c2) = (first fst <$> inferred1, first snd <$> inferred1)
    expectedBall1 <- arrM empirical -< inferredBall1
    ball2 <- iPre 0 >>> moveAwayFrom -< expectedBall1

    convention2 <- arrM empirical -< c2
    ballObs2 <- iPre (0, True) >>> observationModel -< (ball2, convention2)
    inferred2 <- particleFilter params {n = 100} languagePosterior -< (ball1, ballObs2)
    let (inferredBall2, c1) = (first fst <$> inferred2, first snd <$> inferred2)
    expectedBall2 <- arrM empirical -< inferredBall2
    ball1 <- moveAwayFrom -< expectedBall2

  pic1 <- renderObjects yellow -< Result ballObs1 ball1 inferredBall1
  pic2 <- renderObjects green -< Result ballObs2 ball2 inferredBall2
  returnA -< pic1 <> pic2 <>
    translate (-300) 0 (text (show convention1)) <> translate 300 0 (text (show convention2))

  where
    languagePosterior :: SignalFunction (Stochastic & Unnormalized) (Position, Observation) (Position, Bool)
    languagePosterior = proc (ballAPos, ballBObs) -> do

        ballB <- iPre 0 >>> moveAwayFrom -< ballAPos
        language <- performOnFirstSample (uniformD [constM $ pure True, constM $ pure False]) -< ()
        observe -< normalPdf2D ballB std ((if language then id else negate) ballBObs)
        returnA -< (ballB, language)

    observationModel :: SignalFunction Stochastic (Position, Bool) Observation
    observationModel = proc (p, lang) -> do
        (x,y) <- (noise &&& noise) -< ()
        returnA -< (if lang then id else negate) (p + V2 x y)

        where noise = constM (normal 0 std)












language :: SignalFunction (Stochastic & Feedback) GlossInput Picture
language = proc glossInput -> do
    let communicate = glossInput ^. keys . contains (Char 'c')
    ball <- prior -< ()
    std <- accumulateWith id 0.1 -< glossInput ^. keys . to ((\case
                (SpecialKey KeyUp : _) -> (+0.1)
                (SpecialKey KeyDown : _) -> (\x -> max (x - 0.1) 0.1)
                _ -> id
                ) . S.toList)
    obs1  <- observationModel -< (ball, std)
    obs2 <- observationModel -< (ball, std)
    rec
        (belief, l1, utterance) <- agent -< (if communicate then Just utterance2 else Nothing, Nothing, std)
        (belief2, l2, utterance2) <- agent -< (if communicate then Just utterance else Nothing, Just obs1, std)
    pic1 <- renderObjects yellow -< Result obs1 100 belief
    pic2 <- renderObjects green -< Result obs2 1000 belief2
    -- pic3 <- renderObjects yellow -< Result 1000 1000 l1
    -- pic4 <- renderObjects green -< Result 1000 1000 l2
    -- pic3 <- arr (\(V2 x y) -> translate (into @Float x) (into @Float y) $ polygon [(0,0), (0,1), (1,1), (1,0), (0,0)]) -< expected l1
    -- pic4 <- arr (\(V2 x y) -> translate (into @Float x) (into @Float y) $ polygon [(0,0), (0,1), (1,1), (1,0), (0,0)]) -< expected l2


    pic5 <- renderObjects violet -< Result 1000 ball []
    returnA -< pic1 <> pic2 <> pic5 
        <> 
            translate (-300) 300 (scale 0.1 0.1 if communicate then text "Communication on" else mempty)

  where


    observationModel :: SignalFunction Stochastic (Position, Double) Observation
    observationModel = proc (p, std) -> do
        (x,y) <- (noise &&& noise) -< std
        returnA -< p + V2 x y
        where noise = arrM (normal 0)

    posterior :: SignalFunction (Stochastic & Unnormalized)
        (Maybe Utterance, Maybe Observation, Double)
        (Position, Language)
    posterior = proc (utt, obs, std) -> do

        latent@(V2 trueX trueY) <- prior -< ()
        -- lang <- prior -< ()
        let lang = 2
        case obs of
            Just (V2 oX oY) -> observe -< normalPdf oY std trueY * normalPdf oX std trueX
            Nothing -> returnA -< ()
        meaning :: Maybe Position <- arr (\(u,l) -> fmap (l `subtract`) u) -< (utt, lang)
        case meaning of
            Just u -> observe -< normalPdf2D u std latent
            Nothing -> returnA -< ()
        returnA -< (latent, lang)


    prior :: SignalFunction Stochastic () Position
    prior = proc _ -> do
        x <- walk1D -< ()
        y <- walk1D -< ()
        returnA -< V2 x y

    agent :: SignalFunction Stochastic
        (Maybe Utterance, Maybe Observation, Double)
        ([(Position, Log Double)], [(Language, Log Double)], Utterance)
    agent = proc (utt, obs, std) -> do
        delayedUtt <- iPre Nothing -< utt
        belief <- particleFilter params{n=100} posterior -< (delayedUtt, obs, std)
        let (posbelief, langbelief) = (first fst <$> belief, first snd <$> belief)
        pos <- arrM empirical -< posbelief
        l <- arr expected -< langbelief
        utt <- arr (uncurry (+)) -< (pos, l)
        returnA -< (posbelief, first (+1) <$> langbelief, utt)



    posteriorFixedLang :: SignalFunction (Stochastic & Unnormalized)
        (Maybe Utterance, Maybe Observation)
        (Position, Language)
    posteriorFixedLang = proc (utt, obs) -> do

        latent@(V2 trueX trueY) <- prior -< ()
        -- lang <- prior -< ()
        let lang = 2
        case obs of
            Just (V2 oX oY) -> observe -< normalPdf oY std trueY * normalPdf oX std trueX
            Nothing -> returnA -< ()
        -- arrM traceM -< show (utt, lang, "utt, lang (fixed")
        meaning :: Maybe Position <- arr (\(u,l) -> fmap (l `subtract` ) u) -< (utt, lang)
        -- let meaning :: Maybe Position = utt
        case meaning of
            Just (V2 oX oY) -> observe -< normalPdf oY (std/4) trueY * normalPdf oX (std/4) trueX
            Nothing -> returnA -< ()
        returnA -< (latent, lang)

    agentFixedLang :: SignalFunction Stochastic
        (Maybe Utterance, Maybe Observation)
        ([(Position, Log Double)], Language, Utterance)
    agentFixedLang = proc (utt, obs) -> do
        delayedUtt <- iPre Nothing -< utt
        belief <- particleFilter params{n=100} posteriorFixedLang -< (delayedUtt, obs)
        let (posbelief, langbelief) = ( first fst <$> belief, first snd <$> belief)
        pos <- arr expected -< posbelief
        l <- arr expected -< langbelief
        utt <- arr (uncurry (+)) -< (pos, 2)
        returnA -< (posbelief, l, utt)

    -- agent2 :: SignalFunction Stochastic (Observation, Utterance, Language) ([(Position, Log Double)]) 
    -- agent2 = undefined


    -- languagePosterior :: SignalFunction (Stochastic & Unnormalized) (Position, Observation) (Position, Language)
    -- languagePosterior = proc (ballAPos, ballBObs) -> do

    --     ballB <- iPre 0 >>> moveAwayFrom -< ballAPos
    --     language <- performOnFirstSample (uniformD [constM $ pure True, constM $ pure False]) -< ()
    --     observe -< normalPdf2D ballB std ((if language then id else negate) ballBObs)
    --     returnA -< (ballB, language)

    -- observationModel :: SignalFunction Stochastic (Position, Language) Observation
    -- observationModel = proc (p, lang) -> do
    --     (x,y) <- (noise &&& noise) -< ()
    --     returnA -< (if lang then id else negate) (p + V2 x y)

    --     where noise = constM (normal 0 std)



languageInf :: SignalFunction (Stochastic & Feedback) GlossInput Picture
languageInf = proc glossInput -> do
    let communicate = glossInput ^. keys . contains (Char 'c')
    ball <- prior -< ()
    std <- accumulateWith id 0.1 -< glossInput ^. keys . to ((\case
                (SpecialKey KeyUp : _) -> (+0.1)
                (SpecialKey KeyDown : _) -> (\x -> max (x - 0.1) 0.1)
                _ -> id
                ) . S.toList)
    obs1  <- observationModel -< (ball, std)
    obs2 <- observationModel -< (ball, std)
    rec
        (belief, l1, utterance) <- agent (-2) -< (if communicate then Just utterance2 else Nothing, Just obs2, std)
        (belief2, l2, utterance2) <- agent 2 -< (if communicate then Just utterance else Nothing, Just obs1, std)
    pic1 <- renderObjects yellow -< Result obs1 100 belief
    pic2 <- renderObjects green -< Result obs2 1000 belief2
    pic3 <- renderObjects yellow -< Result 1000 1000 l1
    pic4 <- renderObjects green -< Result 1000 1000 l2
    -- pic3 <- arr (\(V2 x y) -> translate (into @Float x) (into @Float y) $ polygon [(0,0), (0,1), (1,1), (1,0), (0,0)]) -< expected l1
    -- pic4 <- arr (\(V2 x y) -> translate (into @Float x) (into @Float y) $ polygon [(0,0), (0,1), (1,1), (1,0), (0,0)]) -< expected l2


    pic5 <- renderObjects violet -< Result 1000 ball []
    returnA -< pic1 <> pic2 <> pic5  <> pic3 <> pic4
        <> 
            translate (-300) 300 (scale 0.1 0.1 if communicate then text "Communication on" else mempty)

  where


    observationModel :: SignalFunction Stochastic (Position, Double) Observation
    observationModel = proc (p, std) -> do
        (x,y) <- (noise &&& noise) -< std
        returnA -< p + V2 x y
        where noise = arrM (normal 0)

    posterior :: Double -> SignalFunction (Stochastic & Unnormalized)
        (Maybe Utterance, Maybe Observation, Double)
        (Position, Language)
    posterior d = proc (utt, obs, std) -> do

        latent@(V2 trueX trueY) <- prior -< ()
        lang <- (+ V2 d 0) <$> priorH -< ()
        -- let lang = 2
        case obs of
            Just (V2 oX oY) -> observe -< normalPdf oY std trueY * normalPdf oX std trueX
            Nothing -> returnA -< ()
        let meaning = fmap (lang `subtract`) utt
        case meaning of
            Just u -> observe -< normalPdf2D u std latent
            Nothing -> returnA -< ()
        returnA -< (latent, lang)


    prior :: SignalFunction Stochastic () Position
    prior = proc _ -> do
        x <- walk1D -< ()
        y <- walk1D -< ()
        returnA -< V2 x y

    priorH :: SignalFunction Stochastic () Position
    priorH = proc _ -> do
        x <- walk1DH -< ()
        y <- walk1DH -< ()
        returnA -< V2 x y

    walk1DH :: SignalFunction Stochastic () Double
    walk1DH = proc _ -> do
        dacceleration <- constM (normal 0 16 ) -< ()
        acceleration <- decayingIntegral 1 -< dacceleration
        velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
        -- position <- decayingIntegral 1 -< velocity
        returnA -< velocity

    agent :: Double -> SignalFunction Stochastic
        (Maybe Utterance, Maybe Observation, Double)
        ([(Position, Log Double)], [(Language, Log Double)], Utterance)
    agent d = proc (utt, obs, std) -> do
        delayedUtt <- iPre Nothing -< utt
        belief <- particleFilter params{n=75} (posterior d) -< (delayedUtt, obs, std)
        let (posbelief, langbelief) = (first fst <$> belief, first snd <$> belief)
        pos <- arrM empirical -< posbelief
        l <- arr expected -< langbelief
        utt <- arr (uncurry (+)) -< (pos, l)
        returnA -< (posbelief, first (+1) <$> langbelief, utt)


languageInf2 :: SignalFunction (Stochastic & Feedback) GlossInput Picture
languageInf2 = proc glossInput -> do
    let communicate = glossInput ^. keys . contains (Char 'c')
    ball <- prior -< ()
    std <- accumulateWith id 0.1 -< glossInput ^. keys . to ((\case
                (SpecialKey KeyUp : _) -> (+0.1)
                (SpecialKey KeyDown : _) -> (\x -> max (x - 0.1) 0.1)
                _ -> id
                ) . S.toList)
    V2 obs1X obs1Y  <- observationModel True -< (ball, std)
    V2 obs2X obs2Y <- observationModel False -< (ball, std)
    rec
        (belief, l1, utterance) <- agent (-2) -< (if communicate then Just utterance2 else Nothing, V2 Nothing ( obs2Y), std)
        (belief2, l2, utterance2) <- agent 2 -< (if communicate then Just utterance else Nothing, V2 ( obs1X) Nothing, std)
    pic1 <- renderObjects yellow -< Result (V2 (fromMaybe (error "") obs1X) 0) 100 belief
    pic2 <- renderObjects green -< Result (V2 0 (fromMaybe (error "") obs2Y)) 1000 belief2
    pic3 <- renderObjects yellow -< Result 1000 1000 l1
    pic4 <- renderObjects green -< Result 1000 1000 l2
    -- pic3 <- arr (\(V2 x y) -> translate (into @Float x) (into @Float y) $ polygon [(0,0), (0,1), (1,1), (1,0), (0,0)]) -< expected l1
    -- pic4 <- arr (\(V2 x y) -> translate (into @Float x) (into @Float y) $ polygon [(0,0), (0,1), (1,1), (1,0), (0,0)]) -< expected l2


    pic5 <- renderObjects violet -< Result 1000 ball []
    returnA -< pic1 <> pic2 <> pic5  <> pic3 <> pic4
        <> 
            translate (-300) 300 (scale 0.1 0.1 if communicate then text "Communication on" else mempty)

  where


    observationModel :: Bool -> SignalFunction Stochastic (Position, Double) (V2 (Maybe Double))
    observationModel b = proc (V2 px py, std) -> do
        (x,y) <- (noise &&& noise) -< std
        returnA -< if b then V2 (Just $ px+x) Nothing else V2 Nothing (Just $ py + y)
        where noise = arrM (normal 0)

    posterior :: Double -> SignalFunction (Stochastic & Unnormalized)
        (Maybe Utterance, V2 (Maybe Double), Double)
        (Position, Language)
    posterior d = proc (utt, obs, std) -> do

        latent@(V2 trueX trueY) <- prior -< ()
        -- lang <- (+ V2 d 0) <$> priorH -< ()
        let lang = 2
        case obs of
            V2 (Just oX) _ -> observe -< normalPdf oX std trueX
            V2 _ (Just oY) -> observe -< normalPdf oY std trueY
            _ -> returnA -< ()
        meaning :: Maybe Position <- arr (\(u,l) -> fmap (l `subtract`) u) -< (utt, lang)
        case meaning of
            Just u -> observe -< normalPdf2D u std latent
            Nothing -> returnA -< ()
        returnA -< (latent, lang)


    prior :: SignalFunction Stochastic () Position
    prior = proc _ -> do
        x <- walk1D -< ()
        y <- walk1D -< ()
        returnA -< V2 x y

    priorH :: SignalFunction Stochastic () Position
    priorH = proc _ -> do
        x <- walk1DH -< ()
        y <- walk1DH -< ()
        returnA -< V2 x y

    walk1DH :: SignalFunction Stochastic () Double
    walk1DH = proc _ -> do
        dacceleration <- constM (normal 0 16 ) -< ()
        acceleration <- decayingIntegral 1 -< dacceleration
        velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
        -- position <- decayingIntegral 1 -< velocity
        returnA -< velocity

    agent :: Double -> SignalFunction Stochastic
        (Maybe Utterance, V2 (Maybe Double), Double)
        ([(Position, Log Double)], [(Language, Log Double)], Utterance)
    agent d = proc (utt, obs, std) -> do
        delayedUtt <- iPre Nothing -< utt
        belief <- particleFilter params{n=75} (posterior d) -< (delayedUtt, obs, std)
        let (posbelief, langbelief) = (first fst <$> belief, first snd <$> belief)
        pos <- arrM empirical -< posbelief
        l <- arr expected -< langbelief
        utt <- arr (uncurry (+)) -< (pos, l)
        returnA -< (posbelief, first (+1) <$> langbelief, utt)






type Expected a = a
type Utterance = Observation

type Language = V2 Double
  -- particle marginal problems
  -- rmsmc
  -- todos: movement given language (warmer, cooler)
  -- the ball is in the box. find the ball
  -- no observations: toggle observations, control loop follows ball when variance is below threshold, print variance (do this in mutualstoch)
  -- diagram of what depends on what
  -- proper user input

-- convention:
    -- latent state: the language :: Bool

    -- agent:: SignalFunction Stochastic (Position, Language) (Observation, Utterance :: V2 Double)


posteriorRank2 :: Process (Stochastic & Unnormalized) (Observation, Position) Position
posteriorRank2 = proc (ballAPos, ballBObs) -> do

    ballB <- iPre 0 >>> agent -< ballAPos
    observe -< normalPdf2D ballB std ballBObs
    returnA -< ballB


expected v = V2
        (averageOf (fmap (first (view _x)) v))
        (averageOf (fmap (first (view _y)) v))


agent :: SignalFunction Stochastic Position Position
agent = feedback 0 proc (ball1, oldBall2) -> do
    ballObs1 <- iPre 0 >>> observationModel -< ball1
    inferred1 <- particleFilter params {n = 10} posterior -< (oldBall2, ballObs1)
    expectedBall1 <- arr expected -< inferred1
    ball2 <- moveAwayFrom -< expectedBall1
    returnA -< (ball2, ball2)







renderObjects ::  Monad m => Color -> MSF m Result Picture
renderObjects col = proc Result { particles, measured, latent} -> do

  observation <- drawBall' -< (measured, 0.05, col)
  ball <- drawBall' -< (latent, 0.1, col)
  parts <- fold <$> mapMSF drawParticle' -< (\(x,y) -> (x,y,col)) <$> particles
  returnA -< (observation <> ball <> parts)


drawParticle' ::  Monad m => MSF m (Position, Log Double, Color) Picture
drawParticle' = proc (position, probability, col) -> do
  drawBall' -< (position, 0.05, withAlpha (into @Float $ exp $ 0.2 * ln probability) col)


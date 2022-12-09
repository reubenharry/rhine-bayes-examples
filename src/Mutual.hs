


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}

module Mutual where
import Inference
import Data.Text (Text)
import FRP.Rhine.Gloss (Picture)
import Circular (Angle (Angle), mkAngle, angle')
import Control.Arrow ( returnA, (>>>) )
import FRP.Rhine (integral)
import Data.MonadicStreamFunction (feedback)
import Data.MonadicStreamFunction (constM)
import Example ( Result(..), drawBall')
import Data.MonadicStreamFunction (iPre)
import Data.Fixed (mod')
import Debug.Trace (traceM)
import Data.MonadicStreamFunction (arrM)
import FRP.Rhine (average)
import Control.Arrow (Arrow(arr))
import FRP.Rhine ((*^))
import Circular (Position)
import Data.MonadicStreamFunction (MSF)
import FRP.Rhine.Gloss.Common (red)
import FRP.Rhine.Gloss (yellow)
-- import Example (Position)




main :: SignalFunction (Deterministic & Feedback) Text Picture
main = proc inputText -> do
    rec
        agent2Pos <- iPre (mkAngle 0) >>> agent2 -< agent1Pos
        agent1Pos <- agent1 -< agent2Pos
    picture <- renderObjects -< (angle' agent2Pos, angle' agent1Pos)
    returnA -< picture

agent2 :: SignalFunction Deterministic Angle Angle
agent2 = feedback (Angle 0) proc (Angle agent1Pos, Angle oldPos) -> do
    let vel = (( (((agent1Pos + pi) `mod'`  (2 * pi)) - oldPos)) / (30))
    arrM (traceM) -< show ( vel,  agent1Pos,  oldPos, agent1Pos - oldPos)
    pos <- decayingIntegral 10 >>> decayingIntegral 10 -< vel
    returnA -< (mkAngle pos, mkAngle pos)

    where

        decayingIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)


agent1 :: SignalFunction Deterministic Angle Angle
agent1 = feedback (Angle (0.1)) proc (Angle agent1Pos, Angle oldPos) -> do
    let vel = (( (((agent1Pos) `mod'`  (2 * pi)) - oldPos)) / (20))
    arrM (traceM) -< show ( vel,  agent1Pos,  oldPos, agent1Pos - oldPos)
    pos <- decayingIntegral 10 >>> decayingIntegral 10 -< vel
    returnA -< (mkAngle pos, mkAngle pos)

    where

        decayingIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)



renderObjects ::  Monad m => MSF m (Position, Position) Picture
renderObjects = proc (ball1, ball2) -> do

  ball1Pic <- drawBall' -< (ball1, 0.05, red)
  ball2Pic <- drawBall' -< (ball2, 0.05, yellow)
  returnA -< (ball1Pic <> ball2Pic)

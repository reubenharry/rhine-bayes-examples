


{-# LANGUAGE ScopedTypeVariables #-}

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
{-# LANGUAGE LambdaCase #-}

module Deterministic where
import Inference (SignalFunction, Deterministic, time, hold)
import Data.Text (Text)
import FRP.Rhine.Gloss.Common (Picture)
import Control.Arrow (returnA)
import Example ( drawBall')
import Linear (V2(..))
import Graphics.Gloss ( Color, text )
import FRP.Rhine.Gloss ( red, blue, yellow, translate )
import Data.MonadicStreamFunction (MSF)
import GlossInput (edgeBy)

oscillator :: SignalFunction Deterministic () Double
oscillator = proc () -> do
    t <- time -< ()
    returnA -< sin t

main :: SignalFunction Deterministic Text Picture
main = proc inputText -> do
    position <- oscillator -< ()
    color <- hold red -< toColor inputText
    counter <- edgeBy (>0.01) -< position
    picture <- renderObjects -< (position, color, counter)
    returnA -< picture


toColor :: Text -> Maybe Color
toColor = \case
    "red" -> Just red
    "blue" -> Just blue
    "yellow" -> Just yellow
    _ -> Nothing


renderObjects ::  Monad m => MSF m (Double, Color, Int) Picture
renderObjects = proc (pos, color, i) -> do

  ball <- drawBall' -< (V2 pos 0, 0.3, color)
  returnA -< (ball <> translate 0 100 (text (show i)))

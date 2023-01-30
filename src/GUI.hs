{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module GUI where

import Concurrent
import Control.Lens hiding (from)
import Control.Monad (forever)
import Debug.Trace (traceM)
import FRP.Rhine.Gloss hiding (norm, scale)
import Inference
import Linear
import Witch (From (from), into)
import Witch.From (From)
import qualified FRP.Rhine.Gloss as F
import Data.Maybe

makeLenses ''Event
makePrisms ''Event
makeLenses ''Key
makePrisms ''Key
makeLenses ''KeyState
makePrisms ''KeyState


instance From a b => From (V2 a) (V2 b) where
  from :: From a b => V2 a -> V2 b
  from (V2 x y) = V2 (from x) (from y)

gui :: SignalFunction Deterministic GlossInput Picture
gui = proc glossInput -> do
  (picture, press) <- button False 20 100 -< glossInput
  (sc, _) <- slider 0 60 -< glossInput
  V2 x y <- foo -< glossInput

  returnA -< picture <> sc <> translate (into @Float x) (into @Float y) (circle 20)

switch firstSignal secondSignal = safely $ loop 0
  where
    loop v = do
      pos <- firstSignal v
      try $ pos <$ timer 0.01
      pos2 <- secondSignal pos
      try $ pos2 <$ timer 0.01
      loop pos2


multipleSwitch initialI signals = safely $ loop initialI 0
  where
    loop i v = do
      (pos, j) <- withFailure' signals i v
      try $ pos <$ timer 0.01
      loop j pos

foo :: SignalFunction Deterministic GlossInput (V2 Double)
foo = multipleSwitch 0 signals

multipleChoice :: GlossInput -> Maybe Int
multipleChoice = arr (^? events . ix 0 . _EventKey . (_1 . _Char . to pure . _Show @Int) )


withFailure' b i pos = try proc glossInput -> do
  pos2 <- b i pos -< glossInput
  let iMay = multipleChoice glossInput
--   let b = glossInput ^. events . to (any (\case (EventKey (Char 'f') Up _ _) -> True; _ -> False))
  case iMay of
    Just i' -> throwOn' -< (isJust iMay, (pos2, i'))
    Nothing -> returnA -< ()
  returnA -< pos2

-- signals :: SignalFunction Deterministic GlossInput (V2 Double)
signals 0 _ = constM $ pure 0
signals 1 _ = constM $ pure 1

slider :: V2 Float -> Float -> SignalFunction Deterministic GlossInput (Picture, Double)
slider pos@(V2 p1 p2) range =
  let cond = (\case (EventKey (MouseButton LeftButton) _ _ _) -> True; _ -> False)
      toPicture v@(V2 x y) =
        let r = v ^. _2 . to (into @Double . (/ range) . (+ range / 2))
         in (
                translate (x + p1) (y + p2) (circleSolid 10)
                <> line [(p1, p2), (p1, p2+range)]
                <> translate p1 (p2 + 30) (F.scale 0.1 0.1 (text (show r))),
              r
            )
   in toPicture
        <$> switch
          (withFailureGen cond stayStill)
          (withFailureGen cond (slide pos range))

stayStill pos = constM $ pure pos

withFailure b pos = try proc glossInput -> do
  pos2 <- b pos -< glossInput
  let bo = glossInput ^. events . to (any (\case (EventKey (Char 'f') Up _ _) -> True; _ -> False))
  throwOn' -< (bo, pos2)
  returnA -< pos2

withFailureGen cond b pos = try proc glossInput -> do
  pos2 <- b pos -< glossInput
  let b = glossInput ^. events . to (any cond)
  throwOn' -< (b, pos2)
  returnA -< pos2

slide :: V2 Float -> Float -> a -> SignalFunction Deterministic GlossInput (V2 Float)
slide pos range a = proc glossInput -> do
  let mousePos@(V2 _ y) = (glossInput ^. mouse . to (into @(V2 Float))) - pos
      (upper, lower) = (range / 2, - range / 2)
  returnA -< V2 0 (min upper $ max lower y)


button :: Bool -> Float -> V2 Float -> SignalFunction Deterministic GlossInput (Picture, Bool)
button initialVal size pos@(V2 xPos yPos) = proc glossInput -> do
  let mousePos = glossInput ^. mouse . to (into @(V2 Float))
  let circleSize = size
  let hover = norm (mousePos - pos) <= circleSize
  let click = glossInput ^. events . to (any (\case (EventKey (MouseButton LeftButton) Down _ _) -> True; _ -> False))
  buttonOn <- toggle True -< (hover && click)
  let circ redOutline redSolid
        | redSolid = color red $ circleSolid circleSize
        | redOutline = color red $ circle circleSize
        | otherwise = circle circleSize
  returnA -< (translate xPos yPos $ circ hover buttonOn, buttonOn)

toggle :: Bool -> SignalFunction Deterministic Bool Bool
toggle initialVal = safely $ forever do
  try proc bool -> do
    pos <- constM (pure initialVal) -< ()
    throwOn' -< (bool, pos)
    returnA -< pos
  try $ not initialVal <$ timer 0.01
  try proc bool -> do
    pos <- constM (pure (not initialVal)) -< ()
    throwOn' -< (bool, pos)
    returnA -< pos
  try $ initialVal <$ timer 0.01










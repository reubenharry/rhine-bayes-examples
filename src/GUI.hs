-- exports some useful GUI related widgets, like buttons and sliders


{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DerivingVia #-}

module GUI where

import Concurrent
import Data.Generics.Sum.Any


import Control.Lens hiding (from)
import Control.Monad (forever)
import FRP.Rhine.Gloss hiding (loop, norm, scale)
import Linear hiding (rotate)
import Witch (into)
import Util
import Generic.Data
import Data.Generics.Product (the)
import FRP.Rhine.Gloss (scale)
import Example (edge)
import Data.Set (Set)
import qualified Control.Monad.Trans.MSF as S
import Prelude hiding (until)
import Control.Monad.Morph (MonadTrans(lift))
import Debug.Trace (traceM, trace)
import qualified FRP.Rhine.ClSF as F


deriving instance Generic Event
deriving instance Generic Key
deriving instance Generic KeyState


-- withPause :: (a -> SignalFunction Stochastic UserInput a) -> (a -> SignalFunction Stochastic UserInput a)
withPause sf init = safely $ S.evalStateT loop (init) where

    loop = forever do
        initialPosition <- S.get
        let pause = (^. the @(Set Key) . contains (SpecialKey KeySpace))
        stateAtPause <- until pause (sf initialPosition)
        lift $ try (stateAtPause <$ timer 1 )
        stateAtTimeofDownCollision <- until (pause . (\userInput -> (Debug.Trace.trace $ show $ userInput ^. the @(Set Key)) userInput ) :: UserInput -> Bool) (constM $ pure stateAtPause)
        error "foo"
        S.put stateAtTimeofDownCollision


traceIt x = Debug.Trace.trace (show x) x
until cond signal = lift $ try proc input -> do
    output <- signal -< input
    _ <- throwOn' -< (cond input, output)
    returnA -< output

-- pause :: SignalFunction m b c -> SignalFunction m UserInput c

gui :: SignalFunction Stochastic UserInput Picture
gui = proc userInput -> do
  (sliderPic, radius) <- slider (V2 (-300) 300) 60 -< userInput
  (buttonPic, showImage) <- button buttonParams {buttonPos = V2 (-350) 300} -< userInput
  angle <- (*100) <$> sinceStart -< ()
  let picture = scale (into @Float radius) (into @Float radius) (rotate (into @Float angle) (translate 100 0 (circleSolid 10)))
  returnA -< (if showImage then picture else mempty) <> sliderPic <> buttonPic

gui2 :: SignalFunction Deterministic UserInput Picture
gui2 = proc userInput -> do
  let radius = 30
  let mouseInCircle = norm (userInput ^. the @"_mouse") < radius
  i <- edge -< mouseInCircle
  let click = userInput ^. the @(Set Key) . contains (MouseButton LeftButton)
  let col
        | click && mouseInCircle = yellow
        | not click && mouseInCircle = red
        | otherwise = black
  returnA -< translate 50 50 (text (show i)) <> color col (circle 30)

gui3 :: System Deterministic UserInput Picture
gui3 = proc userInput -> do

  mousePos <- mouse -< userInput
  bool <- isInside -< mousePos
  int <- edge -< bool
  bool2 <- isMouseDown -< userInput
  col <- color' -< (bool2, bool)
  picture <- render -< (int, col)
  returnA -< picture

  where
    mouse = arr (\x -> x ^. the @"_mouse")
    isInside = arr (\x -> norm x < 30)
    isMouseDown = arr (\x -> x ^. the @(Set Key) . contains (MouseButton LeftButton))
    col (click, mouseInCircle)
        | click && mouseInCircle = yellow
        | not click && mouseInCircle = red
        | otherwise = black
    color' = arr col
    render = arr (\(i, c) -> translate 50 50 (text (show i)) <> color c (circle 30))


-- switch :: (Monad m, Double ~ Time cl, Num b) =>
--   (b -> ClSFExcept m cl a b b)
--   ->
--   (b -> ClSFExcept m cl a b b)
--   -> ClSF m cl a b
switch firstSignal secondSignal = safely $ loop 0
  where
    loop v = do
      pos <- firstSignal v
      try $ pos <$ timer 0.01
      pos2 <- secondSignal pos
      try $ pos2 <$ timer 0.01
      loop pos2


-- multipleSwitch :: (Monad m, Double ~ Time cl, Num b) =>
--   (Int
--   -> b
--   -> ClSF (ExceptT (b, Int) m) cl
--         UserInput
--         b)
--   -> Int
--   -> ClSF m cl UserInput b
multipleSwitch signals = safely . loop 0
  where
    loop v i = do
      (pos, j) <- withFailure' signals i v
      try $ pos <$ timer 0.01
      loop pos j

multipleSwitch' signals init = safely . loop init
  where
    loop v i = do
      (pos, j) <- withFailure'' signals i v
      try $ pos <$ timer 0.01
      loop pos j

withFailure'' b i pos = try proc (userInput, inp) -> do
  pos2 <- b i pos -< inp
  case multipleChoice userInput of
    Just i' -> throwOn' -< (True, (pos2, i'))
    Nothing -> returnA -< ()
  returnA -< pos2


multipleChoice :: UserInput -> Maybe Int
multipleChoice = (^? the @[Event] . ix 0 . _As @"EventKey" . _1 . _As @"Char" . to pure . _Show @Int)


-- signals :: SignalFunction Deterministic UserInput (V2 Double)

-- slider :: V2 Float -> Float -> SignalFunction Deterministic UserInput (Picture, Double)
slider pos@(V2 p1 p2) range =
  let cond u = (case u ^.. the @[Event] . ix 0 of
          [EventKey (MouseButton LeftButton) _ _ _] -> True
          _ -> False)
          && abs (u ^. the @(V2 Double) . _x - into @Double p1) < 10
          && abs (u ^. the @(V2 Double) . _y - into @Double p2) < into @Double range
      toPicture v@(V2 x y) =
        let r = v ^. _2 . to (into @Double . (/ range) . (+ range / 2))
         in (
                translate (x + p1) (y + p2) (circleSolid 10)
                <> line [(p1, p2-range/2), (p1, p2+range/2)]
                -- <> translate p1 (p2 + 30) (F.scale 0.1 0.1 (text (show r)))
                ,
              r
            )
   in toPicture
        <$> switch
          (withFailureGen cond stayStill)
          (withFailureGen cond (slide pos range))

stayStill :: Monad m => b -> MSF m a b
stayStill pos = constM $ pure pos


slide :: Arrow t => V2 Float -> Float -> p -> t UserInput (V2 Float)
slide pos range _ = proc userInput -> do
  let (V2 _ y) = userInput ^. the @(V2 Double) . to (into @(V2 Float)) - pos
      (upper, lower) = (range / 2,- range / 2)
  returnA -< V2 0 (min upper $ max lower y)

data ButtonConfig = ButtonConfig {
  buttonSize :: Float,
  buttonPos :: V2 Float,
  buttonColor :: Color,
  buttonInitialVal :: Bool,
  buttonText :: String}




  -- lc <- loopCond
  -- if lc then loopBody >>= (\newBody -> while loopCond loopBody) else loopBody

buttonParams :: ButtonConfig
buttonParams = ButtonConfig {buttonSize = 20, buttonPos = 0, buttonColor = red, buttonInitialVal = False, buttonText = ""}

-- button :: ButtonConfig -> SignalFunction Deterministic UserInput (Picture, Bool)
button config = let ButtonConfig size pos@(V2 xPos yPos) col initialVal txt = config
  in proc userInput -> do

    let mousePos = userInput ^. the @(V2 Double) . to (into @(V2 Float))
    let hover = norm (mousePos - pos) <= size
    let click = userInput ^. the @[Event] . to (any (\case (EventKey (MouseButton LeftButton) Down _ _) -> True; _ -> False))
    buttonOn <- toggle initialVal -< (hover && click)
    let sqr colOutline colSolid
          | colSolid = color col $ rectangleSolid size size
          | colOutline = color col $ rectangleWire size size
          | otherwise = rectangleWire size size
    returnA -< (translate xPos yPos (sqr hover buttonOn <> translate (-20) (-20) (scale 0.1 0.1 (text txt))), buttonOn)




withFailure' b i pos = try proc userInput -> do
  pos2 <- b i pos -< userInput
  case multipleChoice userInput of
    Just i' -> throwOn' -< (True, (pos2, i'))
    Nothing -> returnA -< ()
  returnA -< pos2


withFailureGen cond b pos = try proc userInput -> do
  pos2 <- b pos -< userInput
  let stop = cond userInput
  throwOn' -< (stop, pos2)
  returnA -< pos2






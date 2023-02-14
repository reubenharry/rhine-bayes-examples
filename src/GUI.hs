-- exports some useful GUI related widgets, like buttons and sliders

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module GUI where

import Concurrent
import Control.Lens hiding (from)
import Control.Monad (forever)
import FRP.Rhine.Gloss hiding (loop, norm, scale)
import Linear
import Witch (into)
import Util

-- macros, feel free to ignore
makeLenses ''Event
makePrisms ''Event
makeLenses ''Key
makePrisms ''Key
makeLenses ''KeyState
makePrisms ''KeyState




gui :: SignalFunction Stochastic UserInput Picture
gui = proc userInput -> do
  (picture, _) <- button buttonParams {buttonPos=100} -< userInput
  returnA -< picture





switch :: (Monad m, Double ~ Time cl, Num b) =>
  (b -> ClSFExcept m cl a b b)
  -> 
  (b -> ClSFExcept m cl a b b)
  -> ClSF m cl a b
switch firstSignal secondSignal = safely $ loop 0
  where
    loop v = do
      pos <- firstSignal v
      try $ pos <$ timer 0.01
      pos2 <- secondSignal pos
      try $ pos2 <$ timer 0.01
      loop pos2


multipleSwitch :: (Monad m, Double ~ Time cl, Num b) =>
  (Int
  -> b
  -> ClSF (ExceptT (b, Int) m) cl
        UserInput
        b)
  -> Int
  -> ClSF m cl UserInput b
multipleSwitch signals = safely . loop 0
  where
    loop v i = do
      (pos, j) <- withFailure' signals i v
      try $ pos <$ timer 0.01
      loop pos j


multipleChoice :: UserInput -> Maybe Int
multipleChoice = (^? events . ix 0 . _EventKey . _1 . _Char . to pure . _Show @Int)




-- signals :: SignalFunction Deterministic UserInput (V2 Double)

slider :: V2 Float -> Float -> SignalFunction Deterministic UserInput (Picture, Double)
slider pos@(V2 p1 p2) range =
  let cond u = (case u ^.. events . ix 0 of 
          [(EventKey (MouseButton LeftButton) _ _ _)] -> True
          _ -> False) && (abs (u ^. mouse . _x - into @Double p1) < 10)
      toPicture v@(V2 x y) =
        let r = v ^. _2 . to (into @Double . (/ range) . (+ range / 2))
         in (
                translate (x + p1) (y + p2) (circleSolid 10)
                <> line [(p1, p2-(range/2)), (p1, p2+(range/2))]
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
  let (V2 _ y) = (userInput ^. mouse . to (into @(V2 Float))) - pos
      (upper, lower) = (range / 2, - range / 2)
  returnA -< V2 0 (min upper $ max lower y)

data ButtonConfig = ButtonConfig {buttonSize :: Float, buttonPos :: V2 Float, buttonColor :: Color, buttonInitialVal :: Bool}

buttonParams :: ButtonConfig
buttonParams = ButtonConfig {buttonSize = 20, buttonPos = 0, buttonColor = red, buttonInitialVal = False}

button :: ButtonConfig -> SignalFunction Deterministic UserInput (Picture, Bool)
button config = let ButtonConfig size pos@(V2 xPos yPos) col initialVal = config 
  in proc userInput -> do
  
    let mousePos = userInput ^. mouse . to (into @(V2 Float))
    let hover = norm (mousePos - pos) <= size
    let click = userInput ^. events . to (any (\case (EventKey (MouseButton LeftButton) Down _ _) -> True; _ -> False))
    buttonOn <- toggle initialVal -< (hover && click)
    let circ colOutline colSolid
          | colSolid = color col $ rectangleSolid size size
          | colOutline = color col $ rectangleWire size size
          | otherwise = rectangleWire size size
    returnA -< (translate xPos yPos $ circ hover buttonOn, buttonOn)




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






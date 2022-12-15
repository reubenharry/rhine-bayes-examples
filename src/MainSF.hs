


{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module MainSF where
import FRP.Rhine.Gloss hiding (Up, Down)
import Prelude hiding (until)
import qualified Example
import Control.Monad.Morph
import Control.Lens
import Control.Monad (forever)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import qualified Data.Text as T
import Graphics.Gloss
    ( loadBMP,
      bitmapOfByteString,
      BitmapFormat(BitmapFormat),
      PixelFormat(PxRGBA) )
import Text.Megaparsec (Parsec, MonadParsec (eof), runParser)
import Data.Void (Void)
import Control.Applicative (optional)
import Data.Either (isRight)
import qualified Active
import qualified BetaBern
import Inference (SignalFunction, Stochastic, InputOutput, type (&), Feedback, hold)
import Data.Text (Text)
import qualified Loop
import qualified Circular
import qualified Bearing
import qualified HarderObservation
import qualified Switch
import Text.Megaparsec.Char.Lexer (decimal)

import Data.ByteString as B hiding (foldr)
import Data.Word

import Codec.Picture.Png
import Codec.Picture.Repa
import qualified Data.ByteString.Internal as BI
import Data.Array.Repa ((:.)(..), Z, Z(..), extent, DIM3, Array)
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as F
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap (RowOrder(TopToBottom))
import GHC.IO (unsafePerformIO)
import qualified Pong
import qualified Future
import qualified Control
import qualified Deterministic
import qualified Mutual
import qualified Communication
import qualified MutualStoch
import Linear (V2)
import Data.Set ( Set, toList, fromList )
import FRP.Rhine.Gloss (KeyState(Down))
import Linear.V2 (V2(V2))
import Witch (into)
import qualified Data.Text.IO as T
import Control.Concurrent
import Data.Functor (void)
import Control.Monad.Bayes.Sampler (sampleIO)
import qualified Data.Maybe as M
import Control.Monad.Fix (MonadFix)
import qualified Debug.Trace as Debug
import qualified Concurrent
import Concurrent (GlossInput (_mouse, _keys, GlossInput), getTextFromGloss, handle, keys, noInput, mouse)
import Debug.Trace (traceM)






glossClock :: RescaledClock GlossEventClockIO Double
glossClock = RescaledClock
  { unscaledClock = GlossEventClockIO
  , rescale = into @Double
  }


toGloss :: SignalFunction (Stochastic & InputOutput & Feedback) (GlossInput, T.Text) Picture -> IO ()
toGloss sf = do
    mvar <- newMVar ""
    void $ forkIO $ forever do
        x <- T.getLine
        swapMVar mvar x

    flowGlossIO defaultSettings { display = InWindow "rhine-bayes" (1724, 1260) (10, 10) }
        $ tagS @@ glossClock >--
            collect -@- glossConcurrently -->
                morphS (hoist (lift . sampleIO)) (proc events -> do
                    inputText <- (constM (liftIO (swapMVar mvar ""))) -< ()
                    letters <- arr ((^. keys) . flip handle noInput)  -< events
                    gl <- accumulateWith handle noInput -< events
                    let glossInput = GlossInput {_keys = letters, _mouse = gl ^. mouse}
                    out <- sf -< (glossInput, inputText)
                    returnA -< out
                    )
                >-> arrMCl paintAllIO

                @@ Example.glossClock

mainSF :: SignalFunction (Stochastic & InputOutput & Feedback) (GlossInput, Text) Picture
mainSF = safely loop

  where

  loop = forever do
    inp <- try $ proc (glossInput, _) -> do
        inputText <- getTextFromGloss -< glossInput
        -- arrM (liftIO . print) -< show (glossInput ^. keys)
        -- if isJust inputText then 
        --   else returnA -< ()
        x <- hold (Left undefined)  -< runParser (getCommand <* eof) "" <$> inputText
        throwOn' -< (isRight x, x)
        returnA -< displayOptions
    fromMaybe (pure mempty) case inp of
      Right (Right i) -> withQuitting <$> options ^? ix i . _1
      Right (Left i) -> do
        let x = constM . pure . unsafePerformIO . fmap pngToPic . B.readFile <$> options ^? ix i . _3
        withQuitting <$> x
      Left _ -> Nothing



  getCommand :: Parsec Void Text (Either Int Int)
  getCommand = do
    isPicture <- isJust <$> optional ":p "
    (if isPicture then Left else Right) <$> decimal

  options = [

              (constM $ pure mempty, "Definitions", "img/slide.png"),
              (constM $ pure mempty, "Particle filter", "img/pf.png"),
              (getTextFromGloss >>> hold "" >>> Deterministic.main, "Deterministic", "img/deterministic.png"),

              (getTextFromGloss >>> hold "" >>> Example.dot, "Moving particle", "img/dot.png"),
              (getTextFromGloss >>> hold "" >>> Example.gloss, "Particle tracking", "img/gloss.png"),
              (getTextFromGloss >>> hold "" >>> Example.weakPrior, "Weak prior", "img/weakPrior.png"),
              (getTextFromGloss >>> hold "" >>> Example.noObservations, "No observations", "img/noObservations.png"),
              (getTextFromGloss >>> hold "" >>> Circular.gloss, "Pendulum", "img/circular.png"),
              (getTextFromGloss >>> hold "" >>> Bearing.gloss, "Bearing", "img/bearing.png"),
              (getTextFromGloss >>> hold "" >>> HarderObservation.gloss, "Acceleration observation", "img/acceleration.png"),
              (getTextFromGloss >>> hold "" >>> BetaBern.gloss, "Beta bernoulli", "img/betabern.png"),
              (getTextFromGloss >>> hold "" >>> Loop.gloss, "Teleportation", "todo"),
              (getTextFromGloss >>> hold "" >>> Switch.gloss, "Hybrid system", "todo"),

              -- user input
              (Concurrent.gloss, "Follow mouse", "todo"),

              (getTextFromGloss >>> hold "" >>> Future.past, "Past smoothed", "todo"),
              (getTextFromGloss >>> hold "" >>> Future.pastFilter, "Past unsmoothed", "todo"),
              (getTextFromGloss >>> hold "" >>> Future.allPast, "All past", "todo"),

              -- transform the posterior stream
              (getTextFromGloss >>> hold "" >>> Example.main, "Posterior predictive", "img/predictive.png"),

              -- mutual recursion
              (getTextFromGloss >>> hold "" >>> Mutual.main, "Two deterministic agents", "img/mutual.png"),
              (getTextFromGloss >>> hold "" >>> MutualStoch.mainSimple, "Two stochastic agents", "img/mutualstoch.png"),

              -- inference in the loop
              (getTextFromGloss >>> hold "" >>> MutualStoch.selfBelief, "Self justification", "img/mutualstoch.png"),
              (MutualStoch.followWhenCertain, "Follow when position is known", "img/follow.png"),
              (getTextFromGloss >>> hold "" >>> MutualStoch.main, "Two stochastic uncertain agents", "img/mutualstoch.png"),
              (getTextFromGloss >>> hold "" >>> MutualStoch.mainComplex, "Two stochastic uncertain agents: rank 2", "img/mutualstoch.png"),
              (getTextFromGloss >>> hold "" >>> Active.mainSignal, "Choose observation", "img/active.png"),
              (getTextFromGloss >>> hold "" >>> Pong.mainSignal, "Pong", "img/pong.png"),
              (Control.gloss, "One agent control", "img/control.png"),

              (getTextFromGloss >>> hold "" >>> MutualStoch.convention, "Language", "img/control.png")
              ]

  displayOptions = translate (-400) 400 $ ifoldMap
      (\i str -> translate 0 (-30* fromIntegral i) $ scale 0.2 0.2 $ text (show i <> ": " <> str)) $
      options ^.. traverse . _2

  withQuitting sf = try proc (glossInput, t) -> do
        out <- morphS (hoist lift) sf -< glossInput 
        throwOn' -< (glossInput ^. keys . contains (Char 'q'), text "")
        returnA -< out





pngToPic :: ByteString -> Picture
pngToPic png
    = let
        Right img -- unsafe
            = decodePng png
        repa
            = imgData (convertImage img :: Img RGBA)
    in repaToPicture True repa

-- repaToPicture :: Bool -> Array F.F DIM3 Word8 -> Picture
repaToPicture b arr
    = bitmapOfByteString row col (BitmapFormat TopToBottom PxRGBA) bs b
    where
        bs
            = BI.fromForeignPtr fptr 0 (R.size sh)
        fptr
            = F.toForeignPtr arr'
        sh@(Z :. col :. row :. depth)
            = extent arr'
        arr'
            = flipVert arr

-- flipVert :: Array F.F DIM3 Word8 -> Array F.F DIM3 Word8
flipVert g
    = R.computeS $ R.backpermute e flop g
    where
        e@(Z :. x :. y :. _)
            = extent g
        flop (Z :. i         :. j         :. k)
            = Z :. x - i - 1 :. j :. k
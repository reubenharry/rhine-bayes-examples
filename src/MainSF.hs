


{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeOperators #-}

module MainSF where
import FRP.Rhine.Gloss hiding (Up, Down)
import Prelude hiding (until)
import qualified Example
import Control.Monad.Morph
import Control.Lens
import Control.Monad (forever)
import Data.Maybe (isJust, fromMaybe)
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
import Inference (SignalFunction, Stochastic, InputOutput, type (&), Feedback)
import Data.Text (Text)
import qualified Loop
import qualified Circular
import qualified Bearing
import qualified HarderObservation
import qualified Switch
import Text.Megaparsec.Char.Lexer (decimal)

import Data.ByteString as B
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


mainSF :: SignalFunction (Stochastic & InputOutput & Feedback) Text Picture
mainSF = safely loop

  where

  loop = forever do
    inp <- try $ proc inputText -> do
        x <- arr (runParser (getCommand <* eof) "") -< inputText
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
              (Deterministic.main, "Deterministic", "img/deterministic.png"),

              (Example.dot, "Moving particle", "img/dot.png"),
              (Example.gloss, "Particle tracking", "img/gloss.png"),
              (Example.weakPrior, "Weak prior", "img/weakPrior.png"),
              (Example.noObservations, "No observations", "img/noObservations.png"),
              (Circular.gloss, "Pendulum", "img/circular.png"),
              (Bearing.gloss, "Bearing", "img/bearing.png"),
              (HarderObservation.gloss, "Acceleration observation", "img/acceleration.png"),
              (BetaBern.gloss, "Beta bernoulli", "img/betabern.png"),
              (Loop.gloss, "Teleportation", "todo"),
              (Switch.gloss, "Hybrid system", "todo"),

              (Future.past, "Past smoothed", "todo"),
              (Future.pastFilter, "Past unsmoothed", "todo"),
              (Future.allPast, "All past", "todo"),

              -- transform the posterior stream
              (Example.main, "Posterior predictive", "img/predictive.png"),

              -- mutual recursion
              (Mutual.main, "Two deterministic agents", "img/mutual.png"),
              (MutualStoch.mainSimple, "Two stochastic agents", "img/mutualstoch.png"),
              
              -- inference in the loop
              (MutualStoch.selfBelief, "Self justification", "img/mutualstoch.png"),
              (MutualStoch.main, "Two stochastic uncertain agents", "img/mutualstoch.png"),
              (MutualStoch.mainComplex, "Two stochastic uncertain agents: rank 2", "img/mutualstoch.png"),
              (Active.mainSignal, "Choose observation", "img/active.png"),
              (Pong.mainSignal, "Pong", "img/pong.png"),
              (Control.gloss, "One agent control", "img/control.png"),

              (Communication.main, "Language", "img/control.png")
              ]

  displayOptions = translate (-400) 400 $ ifoldMap
      (\i str -> translate 0 (-30* fromIntegral i) $ scale 0.2 0.2 $ text (show i <> ": " <> str)) $
      options ^.. traverse . _2

  withQuitting sf = try proc t -> do
        out <- morphS (hoist lift) sf -< t
        throwOn' -< (t=="q", text "")
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
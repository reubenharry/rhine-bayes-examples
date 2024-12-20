{-# OPTIONS_GHC -Wno-missing-signatures #-}

module MainSF where

import Concurrent (UserInput, getTextFromGloss, handle, noInput)
import Control.Applicative (optional)
import Control.Concurrent
import Control.Lens
import Control.Monad (forever)
import Control.Monad.Bayes.Sampler.Strict (sampleIO, SamplerIO)
import Control.Monad.Morph
import Data.Either (isRight)
import Data.Functor (void)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import FRP.Rhine.Gloss hiding (loop, Down, Up)
import Text.Megaparsec (MonadParsec (eof), Parsec, runParser)
import Text.Megaparsec.Char.Lexer (decimal)
import Witch (into)
import Prelude hiding (until)
import Util
import Data.Set (Set)
import Data.Generics.Product (the)

import FRP.Rhine.Gloss hiding (loop, Down, Up)
import SDE (sdeRhine, feR, attempt, attemptR)
import Control.Monad.Bayes.Class
import qualified Data.MonadicStreamFunction.InternalCore as A
import qualified Control.Category as C
import Control.Monad.Trans.MSF (ReaderT)
import Example (stochasticOscillator)

-- main = SDE.main



main2 :: IO ()
main2 = flowGlossIO defaultSettings {display = InWindow "rhine-bayes" (1200, 1000) (10, 10)}  $
  feR
  
  -- Rhine ( 
  --   Postcompose 
  --   (Feedback (undefined ) (undefined :: SN
  -- (GlossConcT IO)
  -- (RescaledClock GlossSimClockIO Double)
  -- ((), (Double,Int))
  -- ((Double,Int), (Double,Int)) )) undefined
  -- ) glossClock

  -- tagS
  --     @@ eventClock
  --       >-- collect 
  --       --> 
  --         undefined @@ glossClock



eventClock :: RescaledClock GlossEventClockIO Double
eventClock = RescaledClock
  { unscaledClock = GlossEventClockIO
  , rescale = into @Double
  }

glossClock :: RescaledClock GlossSimClockIO Double
glossClock = RescaledClock
  { unscaledClock = GlossSimClockIO
  , rescale = into @Double
  }

glossClockFast :: RescaledClock GlossSimClockIO Double
glossClockFast = RescaledClock
  { unscaledClock = GlossSimClockIO
  , rescale = \x -> 2 * x & into @Double
  }

-- Internal plumbing code. Quite complicated, handles concurrency. Shouldn't need to be changed or inspected.
-- toGloss :: SignalFunction (Stochastic & InputOutput & Feedback) (UserInput, T.Text) Picture -> IO ()
toGloss sf = do
  mvar <- newMVar ""
  void $
    forkIO $ forever do
      x <- T.getLine
      swapMVar mvar x

  flowGlossIO defaultSettings {display = InWindow "rhine-bayes" (1200, 1000) (10, 10)} $
    tagS
      @@ eventClock
        >-- collect
        --> morphS
          (hoist (lift . sampleIO))
          ( proc events -> do
              inputText <- (constM (liftIO (swapMVar mvar ""))) -< ()
              gl <- accumulateWith handle noInput -< events
              out <- sf -< (gl, inputText)
              returnA -< out
          )
      >-> arrMCl paintAllIO
      @@ glossClock

mainSF options = safely loop
  where
    loop = forever do
      inp <- try $ proc (userInput, _) -> do
        inputText <- getTextFromGloss -< userInput
        x <- hold (Left undefined) -< runParser (getCommand <* eof) "" <$> inputText
        throwOn' -< (isRight x, x)
        returnA -< displayOptions
      fromMaybe (pure mempty) case inp of
        Right (Right i) -> withQuitting <$> options ^? ix i . _1
        Right (Left _) -> error ""
        Left _ -> Nothing

    getCommand :: Parsec Void Text (Either Int Int)
    getCommand = do
      isPicture <- isJust <$> optional ":p "
      (if isPicture then Left else Right) <$> decimal

    displayOptions =
      translate (-400) 300 $
        ifoldMap
          (\i str -> translate 0 (-30 * fromIntegral i) $ scale 0.2 0.2 $ text (show i <> ": " <> str))
          $ options ^.. traverse . _2

    withQuitting sf = try proc (userInput, _) -> do
      out <- morphS (hoist lift) sf -< userInput
      throwOn' -< (userInput ^. the @(Set Key) . contains (Char 'q'), text "")
      returnA -< out

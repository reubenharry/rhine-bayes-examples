


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
import Control.Monad.Bayes.Sampler
import Control.Monad.Morph
import Control.Lens
import Control.Monad (forever)
import Data.Maybe (isJust, fromMaybe)
import Control.Monad.Trans.MSF.Reader (ReaderT)
import qualified Data.Text as T
import Graphics.Gloss (loadBMP)
import Text.Megaparsec (Parsec, MonadParsec (eof), runParser)
import Data.Void (Void)
import Control.Applicative (optional)
import Text.Megaparsec.Char (digitChar)
import Data.Char (digitToInt)
import Data.Either (isRight)
import qualified Active
import qualified BetaBern
import Inference (SignalFunction, Stochastic, InputOutput, type (&), Feedback)
import Data.Text (Text)
import qualified Loop

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
      Right (Left i) -> withQuitting . constM . liftIO . fmap (scale 0.5 0.5) . loadBMP <$> options ^? ix i . _3
      Left _ -> Nothing



  getCommand :: Parsec Void T.Text (Either Int Int)
  getCommand = do
    isPicture <- isJust <$> optional ":p "
    i <- digitToInt <$> digitChar
    return $ (if isPicture then Left else Right) i

  options = [
              (Example.dot, "Moving particle", "img/dot.bmp"),
              (Example.gloss, "Particle tracking", "img/gloss.bmp"),
              (Example.weakPrior, "Weak prior", "img/weakPrior.bmp"),
              (Example.noObservations, "No observations", "img/noObservations.bmp"),
              (Example.main, "Posterior predictive", "img/predictive.bmp"),
              (Active.chooseObservation, "Choose observation", "todo"),
              (BetaBern.gloss, "Beta bernoulli", "todo"),
              (Loop.gloss, "Teleportation", "todo")
              ]

  displayOptions = ifoldMap
      (\i str -> translate (-100) (-20* fromIntegral i) $ scale 0.2 0.2 $ text (show i <> ": " <> str)) $
      options ^.. traverse . _2

  withQuitting sf = try proc t -> do
        out <- morphS (hoist lift) sf -< t
        throwOn' -< (t=="q", text "")
        returnA -< out



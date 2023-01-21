{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}




{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TemplateHaskell #-}


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

-- base
module Concurrent where

import Data.Maybe (maybeToList)

-- rhine-gloss
import FRP.Rhine.Gloss
import Witch (into)
import Data.Set ( Set, toList, fromList, (\\) )

import Control.Monad.Bayes.Sampler (sampleIOfixed, sampleSTfixed, SamplerST, sampleIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Bayes.Class (MonadSample(random))
import Control.Monad.Trans.Class (MonadTrans(lift))
import GlossInput hiding (latent, measured, particles)
import Inference (SMCSettings(n), hold, particleFilter, params, Stochastic, SignalFunction, InputOutput, type (&), Feedback)
import Linear (V2(..))
import Example (renderObjects, Result (..), glossClock)
import qualified Data.Text.IO as T
import Control.Concurrent.MVar
import Control.Monad (void, forever)
import Control.Concurrent (forkIO)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Set (Set)
import Control.Lens
import qualified Data.Maybe as M
import qualified Data.Set as S
import Debug.Trace (traceM)
import qualified Debug.Trace as Debug

data GlossInput = GlossInput
  { _mouse :: V2 Double
  , _keys  :: Set Key
  , _events :: [Event]
  }

-- Some nice lenses to go with it
makeLenses ''GlossInput




ls :: [Key] -> T.Text
ls = T.pack . M.mapMaybe \case
  Char char -> Just char
  SpecialKey KeySpace -> Just ' '
  _ -> Nothing

traceIt x = Debug.trace (show x) x

getTextFromGloss :: Monad m => ClSF m cl GlossInput (Maybe Text)
getTextFromGloss = safely foo
  where
    foo = forever $ do 
      try proc glossInput -> do
          chars <- getCharsFromGloss -< glossInput
          arrM traceM -< show (glossInput ^. events) <> " Debug 4"
          let predicateUp x = any (==True) $ map (\case (EventKey (SpecialKey KeyEnter) Up _ _) -> True; _ -> False) x
          let predicateDown x = any (==True) $ map (\case (EventKey (SpecialKey KeyEnter) Down _ _) -> True; _ -> False) x
          throwOn' -< (predicateUp $ glossInput ^. events, ())
          arrM traceM -< (show $ predicateDown $ glossInput ^. events ) <> " debug"
          returnA -< traceIt $ if predicateDown $ glossInput ^. events
            then 
              Just $ T.concat $ map (\case Char c -> T.singleton c; _ -> "") chars
            else 
              Nothing 
      step (const $ pure (Nothing, ()))

getCharsFromGloss :: Monad m => MSF m GlossInput [Key]
getCharsFromGloss = feedback mempty proc (glossInput, oldText) -> do
  let newLetters = (glossInput ^. keys) \\ (S.fromList oldText)
  let newText = oldText <> S.toList newLetters
  -- arrM traceM -< show newText <> " Debug 2"
  -- arrM traceM -< show oldText <> " Debug 3"
  returnA -< (newText, newText)


    -- letters <- accumulateWith handle noInput  -< glossInput ^. events
    -- let pressed x = glossInput ^. keys . contains x
    -- outputText  <- iPre [] >>> accumulateWith (foldr (.) id) "" -<
    --     (<> letters ^. keys . to (ls . toList) )
    --         :
    --     [const "" | pressed  (SpecialKey KeyEnter) || pressed (Char 'q') ]
    -- returnA -< if pressed (SpecialKey KeyEnter) then Just outputText else Nothing


noInput :: GlossInput
noInput = GlossInput {_mouse = 0, _keys = mempty, _events = []}

handle :: [Event] -> GlossInput -> GlossInput
handle es = (& events .~ es) . (foldr (.) id . fmap \case
  (EventKey key upOrDown _ _) -> (keys.contains key .~ (upOrDown == Down))
  (EventMotion (x,y)) -> mouse .~ V2 (into @Double x) (into @Double y)
  _ -> id) es


gloss :: SignalFunction Stochastic GlossInput Picture
gloss = proc glossInput -> do
            let actualPosition = (glossInput ^. mouse) / 150
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter params {n = 50} posterior -< measuredPosition
            renderObjects -< Example.Result {
                                particles = samples
                                , measured = measuredPosition
                                , latent =  actualPosition
                                }


isMotionEvent :: Event -> Bool
isMotionEvent (EventMotion _) = True
isMotionEvent _ = False


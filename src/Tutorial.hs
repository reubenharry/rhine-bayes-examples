{-# LANGUAGE TypeApplications #-}
module Tutorial where

import Inference
import Concurrent (GlossInput, mouse)
import FRP.Rhine.Gloss
import Control.Lens
import Linear (V2(..))
import Witch (into)
import Control.Monad.Bayes.Class (MonadSample(..), factor, MonadInfer)
import GUI (toggle)
import Numeric.Log (Log)
import MutualStoch (expected)
import Example (empirical)

type UserInput = GlossInput 


sim :: SignalFunction Stochastic UserInput Picture
sim = proc userInput -> do
    let pos@(V2 x y) = userInput ^. mouse
    coinFlip <- constM (bernoulli 0.01) -< ()
    toggleOutput <- toggle True -< coinFlip
    let c = if toggleOutput then red else black
    returnA -< color c (translate (into @Float x) (into @Float y) (circleSolid 30 ))



-- sim2 :: SignalFunction MonadInfer GlossInput Picture
-- sim2 = proc glossInput -> do
--   let (V2 x y) = glossInput ^. mouse 
--   flip <- constM (bernoulli 0.5) -< ()
--   f <- toggle True -< flip
--   let c = if f then red else black
--   arrM factor -< (if f then 1000 else 1)
--   returnA -< color c $ translate (into @Float x) (into @Float y) $ circleSolid  30

-- sim :: SignalFunction MonadSample GlossInput Picture
-- sim = proc glossInput -> do 
--     pics <- particleFilter params{n=20} sim2 -< glossInput
--     arrM empirical -< pics
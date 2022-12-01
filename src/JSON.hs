{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module JSON where



import Inference
import FRP.Rhine.Gloss
import qualified Example
import Control.Monad.Bayes.Sampler
import Control.Monad.Trans.MSF (ReaderT)
import Data.Functor.Foldable (Recursive(cata))
import Witch hiding (over)
import Control.Monad.Free
import Data.Void (Void, absurd)
import qualified Control.Monad.Trans.Free as F
import Control.Monad.Morph (MonadTrans(lift))
import Control.Monad.Bayes.Class (MonadSample (normal))
import Control.Lens (transformM, makeLenses)
import qualified Data.Map.Lazy as M
import Control.Lens.Getter ((^.))
import Control.Lens.Combinators (makePrisms)
import Control.Lens.Fold ((^?))
import Control.Lens
import Data.Monoid (Ap(..))
import Control.Applicative ((<|>))

data Node a where
  Leaf :: Node a
  Branches :: (a, Double) -> (a, Double) -> Node a
  deriving (Show, Functor, Foldable, Traversable)

-- a = makePrisms

$(makePrisms ''Node)

-- foobar :: MonadSample m => (Double -> m Double) -> Free Node a -> m (Free Node a)
foobar = set (_Free . _Branches . ix 2 . _2)


test = ([1],[2]) & (_1 <> _2) %~ (fmap (+1))
-- pointer = (_Free . _Branches . _2 . _2)

-- type family Index Tree where
--     Index Tree = Int

updatedTree :: M.Map [Int] Double -> Tree -> Tree
updatedTree dictionary = \case
    Free Leaf -> Free Leaf
    Free (Branches (x, d1) (y, d2)) -> undefined

-- & traversed %~ (+1) 

type Tree = Free Node Void

-- instance Plated Tree where

baz :: Maybe Tree
baz = traverse absurd tree

foo :: MonadSample m => Tree -> m Tree
foo = transformM \case
    Free (Branches (x, d1) (y, d2)) -> do
        scale1 <- normal 1 0.01
        scale2 <- normal 1 0.01
        pure $ Free (Branches (x, d1*scale1) (y, d2*scale2))
    x -> pure x

convert bs i = 
    let bs' = [_Free . _Branches . ix (if i then 1 else 2) . _1 | i <- bs ]
    in (foldr (.) (_Free . _Branches . ix i . _2) bs') 

exam = tree ^.. cosmos . _Free . _Branches . _2 . _2

-- convert [False] 2

m :: SignalFunction Stochastic () Tree 
m = feedback tree proc (x, oldTree) -> do
    d <- (+1) <$> Example.walk1D -< ()
    d2 <- (+1) <$> Example.walk1D -< ()
    let newTree' = transform (over (_Free . _Branches . _2 . _2) (const d)) tree
    let newTree = transform (over (_Free . _Branches . _1 . _2) (const d2)) newTree'
            -- & convert [True] 1 .~ d
            -- & convert [] 1 .~ d2
            -- & convert [] 2 .~ d2
    -- newTree <- arrM foo -< oldTree
    returnA -< (newTree, newTree)

tree :: Tree
tree = let x = Free $ Branches (Free Leaf, 1) (Free (Branches (Free Leaf, 1) (Free Leaf, 2)), 2)
    in Free $ Branches (x , 3) (x , -2)

gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl Example.glossClock proc () -> do
            tree <- m -< ()
            rotation <- Example.walk1D -< ()
            withSideEffect_ (lift clearIO) >>> visualize -< (tree, into @Float rotation)
            -- measuredPosition <- observationModel -< actualPosition
            -- samples <- particleFilter 200 resampleMultinomial posterior -< measuredPosition
            -- (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
            --                     particles = samples
            --                     , measured = measuredPosition
            --                     , latent = actualPosition
            --                     }

-- visualize :: SignalFunction InputOutput Tree ()
-- visualize :: MSF
--   (ReaderT
--      (TimeInfo cl) (GlossConcT SamplerIO))
--   Tree
--   ()
visualize = proc (tree, rotation) -> do 
    picture <- arr toPicture -< tree
    arrMCl paintIO -< rotate (rotation * 100) picture 

toPicture :: Tree -> Picture
toPicture = scale 50 50 . cata \case
    F.Pure a -> absurd a
    F.Free Leaf -> circle 1
    
    F.Free (Branches (x,d1) (y,d2)) -> 
        let d1' = into @Float d1
            d2' = into @Float d2
        in line [(0,0), (-d1', d1')] <> line [(0,0), (d2', d2')] <> translate (-d1') d1' x <> translate d2' d2' y
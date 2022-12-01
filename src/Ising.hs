{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Ising where
-- import Control.Comonad.Store
import Control.Monad.Bayes.Class
import Data.Distributive
import Data.Key (Indexable (), Key)
import Control.Comonad.Identity (IdentityT, Identity (Identity), Comonad (extend, extract))
import Data.Functor.Compose
import qualified Data.Vector as V
import Control.Arrow (Arrow(first), returnA)
import Control.Comonad.Store (ComonadStore (seeks), Comonad (duplicate))
import Control.Comonad.Store.Class (ComonadStore(peek, pos, peeks, seek))
import Data.MonadicStreamFunction.Util (unfold)
import Control.Monad.Bayes.Sampler (SamplerIO, sampleIO)
import Data.MonadicStreamFunction (MSF, feedback, arrM)
import FRP.Rhine.Gloss hiding (step)
import Example (glossClock)
import Data.Foldable (Foldable(fold))
import Witch (from)
import Control.Monad.Morph (MonadTrans(lift))
-- import Control.Comonad.Store hiding (StoreT, Store)




type Grid a = Store (Compose VBounded VBounded) a

-- | VBounded a ~ [a]
newtype VBounded a = VBounded (V.Vector a)
  deriving (Eq, Show, Functor, Foldable, Traversable)


instance Distributive VBounded where
  distribute = distributeRep


-- | size of the Ising grid
gridSize :: Int
gridSize = 40

-- | you can view a grid as a function from indices to square values
-- or as an array of square values.
-- These two structures are isomorphic, as witnessed by: 
-- index . tabulate === id === tabulate . index
instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v V.! (i `mod` gridSize)
  tabulate desc = VBounded $ V.generate gridSize desc

-- | make a grid
mkGrid :: [(Int, Int)] -> Grid Bool
mkGrid xs = store ( (`elem` xs)) (0, 0)

-- type Table = Compose VBounded VBounded


step :: MonadSample m =>  Grid Bool -> m (Grid Bool)
step = sequence . extend rule

rule :: MonadSample m => Grid Bool -> m Bool
rule = experiment (uniformD . neighbours)
  where
    neighbours s = addCoords s <$> neighbourCoords
    addCoords (x, y) (x', y') = (x + x', y + y')
    neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]


sf :: MonadSample m => MSF m () (Grid Bool)
sf = unfoldM (mkGrid [(m,n) | m <- [0..5], n <- [0..20]] ) step --  uniformD [(mkGrid [(m,n) | m <- [0..5], n <- [0..20]] ), (mkGrid [(m,n) | m <- [0..20], n <- [0..5]] )]

unfoldM :: Monad m => i -> (i -> m i) -> MSF m () i
unfoldM init step = feedback init proc ((), b) -> do
    
    -- x <- arr (const init) -< ()
    -- a <- iPre () -< ()
    -- let
    newB <- arrM step -< b
    returnA -< (newB, newB)



gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            grid <- sf -< ()
            constM (lift clearIO) -< ()
            arrMCl paintIO -< render grid
            -- actualPosition <- prior -< ()
            -- measuredPosition <- observationModel -< actualPosition
            -- samples <- particleFilter 200 resampleMultinomial posterior -< measuredPosition
            -- (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
            --                     particles = samples
            --                     , measured = measuredPosition
            --                     , latent = actualPosition
            --                     }

render :: Grid Bool -> Picture
render (StoreT (Identity grid) _) = ifoldMapRep match grid where
    match (i1,i2) b = 
        color (if b then red else black) $ 
        scale 15 15 $ 
        translate (fromIntegral i1) (fromIntegral i2) $ 
        Polygon [(-1,-1), (-1,1), (1,1), (1,-1)]


renderS :: Grid Bool -> String
renderS (StoreT (Identity grid) _) = ifoldMapRep match grid where
    match (i1,i2) b = show (i1,i2,b)




foo = fmap renderS $ sampleIO $ step (mkGrid [(m,n) | m <- [0..10], n <- [0..20]] )




ifoldMapRep :: forall r m a. (Representable r, Foldable r, Monoid m)
            => (Rep r -> a -> m) -> (r a -> m)
ifoldMapRep ix xs = fold (tabulate (\(i :: Rep r) -> ix i $ index xs i) :: r m)



-- samples = do
--   s <- sampler $ unweighted $ mh 2 model
--   mapM_ (\x -> putStrLn ("\n\n" <> display x)) $ reverse $ take 2 s
experiment f w = fmap (`peek` w) (f (pos w))

instance (Comonad w, Representable g) => Comonad (StoreT g w) where
  duplicate (StoreT wf s) = StoreT (extend (tabulate . StoreT) wf) s
  extract (StoreT wf s) = index (extract wf) s

  
instance (Comonad w, Representable g, Rep g ~ s) => ComonadStore s (StoreT g w) where
  pos (StoreT _ s) = s
  peek s (StoreT w _) = extract w `index` s
  peeks f (StoreT w s) = extract w `index` f s
  seek s (StoreT w _) = StoreT w s
  seeks f (StoreT w s) = StoreT w (f s)


class (Distributive f) => Representable f where
  -- | If no definition is provided, this will default to 'GRep'.
  type Rep f :: *
--   type Rep f = GRep f

  -- |
  -- @
  -- 'fmap' f . 'tabulate' ≡ 'tabulate' . 'fmap' f
  -- @
  --
  -- If no definition is provided, this will default to 'gtabulate'.
  tabulate :: (Rep f -> a) -> f a
  index :: forall a . f a -> Rep f -> a
--   fa -> (Rep f -> a)

-- newtype Rep f a = Rep { unrep :: f a }

-- build an ising model
-- build a percolation model

data StoreT g w a = StoreT (w (g a)) (Rep g) deriving Functor

type Store g = StoreT g Identity

store :: Representable g => (Rep g -> a)
    -> Rep g
    -> StoreT g Identity a
store = StoreT . fmap tabulate . Identity -- StoreT . Identity
runStore (StoreT (Identity ga) k) = (index ga, k)

distributeRep :: (Representable f, Functor w) => w (f a) -> f (w a)
distributeRep wf = tabulate (\k -> fmap (`index` k) wf)

instance (Representable f, Representable g) => Representable (Compose f g) where
  type Rep (Compose f g) = (Rep f, Rep g)
  index (Compose fg) (i,j) = index (index fg i) j
  tabulate = Compose . tabulate . fmap tabulate . curry

instance Foldable (Store g)
instance (Traversable g, Representable g, Functor g) => Traversable (Store g) where
  sequenceA g = fmap (uncurry store . first index) (distributed g) where
    distributed :: (Representable g, Traversable g, Applicative m) => Store g (m a) -> m (g a, Rep g)
    distributed  g = grab $
      first (sequenceA . tabulate) $
      runStore g
    grab (ma, b) = (,b) <$> ma
{-# LANGUAGE ScopedTypeVariables #-}

module OED where
import Control.Monad.Bayes.Class hiding (posterior)
import Control.Monad.Bayes.Enumerator
import FixedConvention (categorical')
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.IO.Class
import Data.Functor.Base (ListF)
import Data.Functor.Foldable
import Control.Monad.Trans.Free
import Data.Void (Void, absurd)
import Data.Functor.Compose
import Control.Monad.Bayes.Population
import Data.Functor.Identity (Identity(..))
import Control.Monad (join, (>=>))
import Data.Distributive (Distributive(..))
import Control.Arrow (Arrow(..))
import Data.Functor (($>))

data Action = ViewL | ViewR deriving (Show, Eq, Ord)
newtype Observation = O Bool deriving (Show, Eq, Ord)
newtype State = S (Bool, Bool) deriving (Show, Eq, Ord)

observationModel :: MonadDistribution m => State -> Action -> m Observation
observationModel state@(S (b1, b2)) action = do
    let obs = case action of
            ViewL -> b1
            ViewR -> b2
    noise <- bernoulli 0.1
    return $ O if noise then not obs else obs

world :: MonadDistribution m => Action -> m Observation
world action = do
    state <- statePrior
    observationModel state action

statePrior :: MonadDistribution m => m State
statePrior = do
    left <- bernoulli 0.9
    right <- bernoulli 0.5
    return $ S (left, right)

    -- categorical' [(S (True,True), 0.4), (S (True,False), 0.4),
    -- (S (False,True), 0.2), (S (False,False), 0.2)]

kl :: Ord a => Enumerator a -> Enumerator a -> Double
kl d1 d2 = sum ((\(p1,p2) -> p1 * log (p1/p2)) <$> zip (snd <$> enumerate d1) (snd <$> enumerate d2))

entropy :: Ord a => Enumerator a -> Double
entropy dist = negate $ sum $ (\x -> x * log x) . snd <$> enumerate dist

posterior :: (MonadDistribution m, MonadFactor m) =>
    m State -> Observation -> Action -> m State
posterior prior observation action = do
    state <- prior
    predObs <- observationModel state action
    condition (observation == predObs)
    return state

eig :: (MonadMeasure m) => (MonadMeasure m => m State) -> Action -> m State
eig prior action = do
    observation <- world ViewL
    posterior prior observation action

m = sampleIO do
    liftIO $ print $ enumerate statePrior

    -- liftIO $ print $ sum [kl (posterior statePrior obs ViewL) statePrior * mass (world ViewL) obs | obs <- [O True, O False]]
    -- liftIO $ print $ sum [kl (posterior statePrior obs ViewR) statePrior * mass (world ViewR) obs | obs <- [O True, O False]]

    liftIO $ print $ sum [(entropy (posterior statePrior obs ViewL) -  entropy statePrior) * mass (world ViewL) obs | obs <- [O True, O False]]
    liftIO $ print $ sum [(entropy (posterior statePrior obs ViewR) -  entropy statePrior) * mass (world ViewR) obs | obs <- [O True, O False]]

    -- liftIO $ print $ kl (eig statePrior ViewL) statePrior
    -- liftIO $ print $ kl (eig statePrior ViewR) statePrior

    -- obs <- world ViewR
    -- liftIO $ print obs
    -- let belief = enumerate $ posterior statePrior obs ViewR
    -- liftIO $ print belief

    -- -- liftIO $ print $ enumerate statePrior
    -- liftIO $ print $ kl (eig (categorical' belief) ViewL) (categorical' belief)
    -- liftIO $ print $ kl (eig (categorical' belief) ViewR) (categorical' belief)

    -- obs2 <- world ViewR
    -- liftIO $ print obs2
    -- let belief2 = enumerate $ posterior (categorical' belief) obs2 ViewR
    -- liftIO $ print belief2
    -- -- print $ kl (world ViewR) (uniformD [O True, O False])

type CoAlgebra f a = a -> Base f a

coalgebra :: MonadMeasure m => CoAlgebra (FreeT (ListF Int) m Void) Bool
coalgebra = Compose . (\x -> uniformD [Free (if not x then Nil else Cons i (even i)) | i <- [1,2,3]])

coalgebra' :: forall m . MonadDistribution m => CoAlgebra (FreeT (ListF Int) m Void) (PopulationT m Bool)
coalgebra' x =
    let y = ((getCompose <$> coalgebra @(PopulationT m)) =<< x)
        -- z = runPopulationT y 
    in Compose (Free <$> undefined)
    -- in Compose (Free <$> undefined z)
-- coalgebra = undefined

algebra :: MonadDistribution m => Base (FreeT (ListF Int) m Void) (m [Int]) -> m [Int]
algebra (Compose f) = do
    f' <- f
    let f'' = helper f'
    case f'' of
        Nil -> pure []
        Cons a b -> (a:) <$> b

foo :: MonadDistribution m => PopulationT m Bool -> FreeT (ListF Int) m Void
foo = ana coalgebra'


bar :: MonadDistribution m => m [Int]
bar = cata algebra (foo (pure True))


helper :: FreeF m Void a -> m a
helper (Free inp) = inp
helper (Pure b) = absurd b


-- easier

-- a -> m a 
-- f a -> m (f a)

hop :: MonadMeasure m => Bool -> m Bool
hop b = if b then bernoulli 0.5 else bernoulli 0.3

-- skip :: MonadMeasure m => Bool -> m Bool
-- skip :: m [(a, Log Double)]
skip :: MonadDistribution m => Bool -> m [(Bool, Log Double)]
skip = runPopulationT . (hop >=> hop)

jump :: forall m . MonadDistribution m => (Bool -> PopulationT m Bool) -> PopulationT Identity Bool -> m (PopulationT Identity Bool )
-- jump = fmap (fromWeightedList . Identity) . runPopulationT . hop @(PopulationT m)
-- jump = fmap (fromWeightedList . Identity) . runPopulationT . fmap hop @(PopulationT m)
jump h x = fmap (fromWeightedList . Identity . join) (sequence $  fmap  ((\(m, d) -> fmap (fmap (\(b,d2) -> (b, d*d2))) m )) $ runIdentity $ runPopulationT $ fmap (runPopulationT . h) x)

skip2 :: forall m . MonadDistribution m => Bool -> m ( PopulationT Identity Bool)
skip2 x = (jump hop >=> jump hop ) (spawn 2 $> x)
    -- x <- runPopulationT . hop @(PopulationT m) $ b
    -- let y = join <$> mapM ((\(m, d) -> fmap (fmap (\(_,d2) -> (b, d*d2))) m ) . first (fmap (runIdentity . runPopulationT) . (jump @m))) x
    -- -- let y = fmap (jump @(PopulationT m)) x
    -- y
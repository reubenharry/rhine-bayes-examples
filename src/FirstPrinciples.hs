{-# LANGUAGE DeriveFunctor #-}
module FirstPrinciples where
import Data.Fix (Fix (Fix))
import Data.Functor.Foldable (Recursive(cata), Corecursive (ana), futu)
import Control.Monad

newtype Carrier m a b c = SF (a -> m (b, c)) deriving Functor

type SF m a b = Fix (Carrier m a b)

-- reactimate :: SF m () () -> m ()
react :: Monad m => SF m () () -> m ()
react = cata (\(SF carrier) -> snd =<< carrier () )

comp :: Monad m => SF m a b -> SF m b c -> SF m a c
comp (Fix (SF x)) (Fix (SF y)) = undefined \k -> do
    (b ,rest) <- x k
    (c, rest2) <- y b
    pure c

constM :: Monad m => m a -> SF m b a
constM a = 
    -- futu undefined a
    
    Fix $ SF (const do
        a' <- a
        pure (a', constM a))
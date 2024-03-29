

import Prelude hiding ((.)) -- don't expose standard definition of `.`
import Control.Category
import Control.Arrow (Arrow(arr, first), returnA)
import GHC.Base (Type, join)
import Control.Monad.Bayes.Population (PopulationT)
import Control.Monad.Bayes.Sampler.Strict (SamplerIO)


-- I've left gaps in various parts of the code, marked by the word `undefined` (which just throws a runtime error).  
-- See `Answers.hs` for the correct solutions.

-- here is the implementation of a discrete deterministic system.  
-- Read this as saying: for any `a` and `b` that are Types,
-- a `SimpleSystem a b` is constructed by passing a function of 
-- type `(a -> (b, SimpleSystem a b))` (note the recursion) as an argument to `S`
-- `S` is known as a "constructor", and has type: 
    -- S :: forall a b .  (a -> (b, SimpleSystem a b)) -> SimpleSystem a b
newtype SimpleSystem (a :: Type) (b :: Type) = S (a -> (b, SimpleSystem a b))

type TimeInterval = Double 

newtype ContinuousSystem a b = SC (a -> TimeInterval -> (b, ContinuousSystem a b))


newtype MonadicSystem m a b = MS (a -> m (b, MonadicSystem m a b))


runMonadicSystem :: forall a b m . Monad m => MonadicSystem m a b -> [a] -> m [b]
runMonadicSystem _ [] = return []
runMonadicSystem (MS step) (x:xs) = do
    (b, nextSystem) <- step x
    next <- runMonadicSystem nextSystem xs
    pure (b:next)


monadicCompose :: Monad m => MonadicSystem m b c -> MonadicSystem m a b -> MonadicSystem m a c
monadicCompose (MS g) (MS f)  = MS (\x -> do 
    (fVal, nextFStep) <- f x
    (gVal, nextGStep) <- g fVal
    return (gVal, monadicCompose nextGStep nextFStep) )

instance Monad m => Category (MonadicSystem m) where
  (.) = monadicCompose
  id = undefined




-- instance Functor [] where 
--   fmap :: (a -> b) -> [a] -> [b]
--   fmap _ [] = []
--   fmap f (x:xs) = (f x) : fmap f xs

-- f >>= x
-- bind f x 

returnList a = [a]

bind :: [a] -> (a -> [b]) -> [b]
bind ls monadicFunction = let blah = fmap monadicFunction ls 
  in join blah


-- runMonadicSystem (MS step) (x:xs) = step x >>= (\(b, nextSystem) -> (runMonadicSystem nextSystem xs >>= (\next -> pure (b:next))) )
  
  -- do
  --   (b, nextSystem) <- step x
  --   next <- runMonadicSystem nextSystem xs
  --   pure (b:next)



-- note: there is another encoding of a system that is perhaps more useful if we're porting outside haskell, shown below
-- but in Haskell it's somewhat complicated for technical reasons
-- data System m a b = forall s . S
--   { initial :: s
--   , step  :: (s, a) -> m (s, b)
--   }

runSystemSimple :: forall (a :: Type) (b :: Type) . SimpleSystem a b -> ([a] -> [b]) -- i.e. for any `a` and `b` which are types, take a value of type `SimpleSystem a b` and return a function from a list of values of type a to a list of values of type b.
-- or equivalently:
-- runSystemSimple :: SimpleSystem a b -> [a] -> [b] -- (no need to write `forall` explicitly)
runSystemSimple _ [] = [] -- handle the base case of the empty list as input
runSystemSimple (S f) (x:xs) = let (output, nextSystem) = f x in undefined
-- Hint: (:) is list prefixing, so e.g. (1:[2,3]) ==== [1,2,3]

simpleCompose :: SimpleSystem b c -> SimpleSystem a b -> SimpleSystem a c
simpleCompose (S g) (S f)  = S (\x ->
    let (fVal, nextFStep) = f x
        (gVal, nextGStep) = undefined 
    in undefined
  )

simpleIdentity :: SimpleSystem a a
simpleIdentity = S undefined

feedback :: c -> SimpleSystem (a, c) (b, c) -> SimpleSystem a b
feedback initialVal (S f) = S (\aVal -> 
  let ((nextBVal, nextC), nextACBCSystem) = f (aVal, initialVal)
  in (undefined, undefined))

-- this is a typeclass, which is a set of methods for a given type.  
-- For example, the following states that `SimpleSystem` has the appropriate `(.)` and `id` functions
-- associated with it
instance Category SimpleSystem where
  id :: SimpleSystem a a
  id = simpleIdentity
  -- note that we can write (f . g) which is convenient notation equivalent to ((.) f g)
  (.) :: SimpleSystem b c -> SimpleSystem a b -> SimpleSystem a c
  (.) = simpleCompose



-- `Arrow` is the typeclass required for the "proc ... do" notation used to manipulate systems.
-- To be an instance of `Arrow`, a type must first be an instance of `Category` (as above), and also implement `arr` and `first`.
instance Arrow SimpleSystem where
  arr :: (b -> c) -> SimpleSystem b c
  arr f = S (\x -> undefined)
  first :: SimpleSystem b c -> SimpleSystem (b, d) (c, d)
  first (S f) = S undefined





-- ghci> runSystemSimple boringExample [1,2,3]
-- [(2,4),(3,6),(4,8)]
boringExample :: SimpleSystem Int (Int, Int)
boringExample = proc x -> do
  y <- arr (\x -> x + 1) -< x
  z <- arr (\x -> x*2) -< y
  returnA -< (y,z)

-- ghci> runSystemSimple statefulExample [1,2,3, 4, 3, 6, 1]
-- [1,2,3,4,3,0,0]
-- ghci> runSystemSimple statefulExample [1,6,3, 5, 3, 6, 1]
-- [1,0,0,0,0,0,0]
statefulExample :: SimpleSystem Int Int 
statefulExample = feedback True (proc (i, switchVal) -> do
    newSwitchVal <- arr (\(x, y) -> (x > 5) || (not y))  -< (i, switchVal)
    returnA -< (if newSwitchVal then 0 else i, not newSwitchVal)
    )


-- there are various combinators which make it easy to construct arbitrary stateful systems like:

-- this shows that the more intuitive stateful notion of a system can be translated to the present implementation
constructExplicitly :: (a -> s -> (b, s)) -> s -> SimpleSystem a b
constructExplicitly f s0 = undefined

accumulateWith :: (a -> s -> s) -> s -> SimpleSystem a s
accumulateWith f s0 = undefined


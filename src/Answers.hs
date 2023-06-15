

import Prelude hiding ((.)) -- don't expose standard definition of `.`
import Control.Category
import Control.Arrow (Arrow(arr, first), returnA)
import GHC.Base (Type)
import Control.Monad.Bayes.Class (MonadDistribution, MonadMeasure, Log)
import Control.Monad.Bayes.Weighted (runWeighted)
import Control.Monad.Bayes.Population (runPopulation, extractEvidence, spawn, fromWeightedList, Population, resampleSystematic)
import Control.Monad.Bayes.Inference.SMC (SMCConfig(resampler))
import Data.MonadicStreamFunction.InternalCore (MSF(unMSF, MSF))
import Data.Functor (($>))
import Inference (normalize)


-- I've left gaps in various parts of the code, marked by the word `undefined` (which just throws a runtime error).  
-- See `Answers.hs` for the correct solutions.

-- here is the implementation of a discrete deterministic system.  
-- Read this as saying: for any `a` and `b` that are Types,
-- a `SimpleSystem a b` is constructed by passing a function of 
-- type `(a -> (b, SimpleSystem a b))` (note the recursion) as an argument to `S`
-- `S` is known as a "constructor", and has type: 
    -- S :: forall a b .  (a -> (b, SimpleSystem a b)) -> SimpleSystem a b
newtype SimpleSystem (a :: Type) (b :: Type) = S (a -> (b, SimpleSystem a b))



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
runSystemSimple (S f) (x:xs) = let (output, nextSystem) = f x in output : runSystemSimple nextSystem xs


simpleCompose :: SimpleSystem b c -> SimpleSystem a b -> SimpleSystem a c
simpleCompose (S g) (S f)  = S (\x ->
    let (fVal, nextFStep) = f x
        (gVal, nextGStep) = g fVal
    in (gVal, simpleCompose nextGStep nextFStep) )

simpleIdentity :: SimpleSystem a a
simpleIdentity = S (\x -> (x, simpleIdentity))

feedback :: c -> SimpleSystem (a, c) (b, c) -> SimpleSystem a b
feedback initialVal (S f) = S (\aVal ->
  let ((nextBVal, nextC), nextACBCSystem) = f (aVal, initialVal)
  in (nextBVal, feedback nextC nextACBCSystem))

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
  arr f = S (\x -> (f x, arr f))
  first :: SimpleSystem b c -> SimpleSystem (b, d) (c, d)
  first (S f) = S (\(x, dVal) ->
      let (outVal, nextStep) = f x
      in ((outVal, dVal), first nextStep)
      )


-- ghci> runSystemSimple boringExample [1,2,3]
-- [(2,4),(3,6),(4,8)]
boringExample :: SimpleSystem Int (Int, Int)
boringExample = proc x -> do
  y <- arr (+ 1) -< x
  z <- arr (* 2) -< y
  returnA -< (y,z)

-- ghci> runSystemSimple statefulExample [1,2,3, 4, 3, 6, 1]
-- [1,2,3,4,3,0,0]
-- ghci> runSystemSimple statefulExample [1,6,3, 5, 3, 6, 1]
-- [1,0,0,0,0,0,0]
statefulExample :: SimpleSystem Int Int
statefulExample = feedback True (proc (i, switchVal) -> do
    newSwitchVal <- arr (\(x, y) -> x > 5 || not y)  -< (i, switchVal)
    returnA -< (if newSwitchVal then 0 else i, not newSwitchVal)
    )


-- there are various combinators which make it easy to construct arbitrary stateful systems like:

-- this shows that the more intuitive stateful notion of a system can be translated to the present implementation
constructExplicitly :: (a -> s -> (b, s)) -> s -> SimpleSystem a b
constructExplicitly f s0 = feedback s0 (arr (uncurry f))

accumulateWith :: (a -> s -> s) -> s -> SimpleSystem a s
accumulateWith f s0 = feedback s0 (arr g)
  where
    g (a, s) = let s' = f a s in (s', s')








------------





-- WIP

-- -- to generalize beyond a discrete deterministic system, we generalize in the following way.  
-- -- the idea is that `m` is any function from types to types
newtype System (m :: Type -> Type) (a :: Type) (b :: Type) = M {unM :: a -> m (b, System m a b)}
--                                                        -- ^ arbitrary name

type a >--> b = forall m. MonadDistribution m => System m a b

type a >-/-> b = forall m. MonadMeasure m => System m a b

type Particles a = [(a, Log Double)]

sys :: a >--> Particles b
sys = particleFilter undefined

-- particleFilter :: MonadDistribution m => System (Population m) a b -> System m a (Particles b)
particleFilter :: (a >-/-> b) -> (a >--> Particles b)
particleFilter msf = step initial

  where

  initial = spawn 100 $> msf
  step msfs = M \a -> do
    let afterKernel = msfs >>= (\x -> unM x a)
    bAndMSFs <- runPopulation $ normalize $ resampleSystematic afterKernel
    let (currentPopulation, continuations) =
          unzip $ (\((b, sf), weight) -> ((b, weight), (sf, weight))) <$> bAndMSFs
    return (currentPopulation, step $ fromWeightedList $ pure continuations)

-- instance Category (System m) where
--   id :: System m a a
--   id = undefined
--   -- note that we can write (f . g) which is convenient notation equivalent to ((.) f g)
--   (.) :: System m b c -> System m a b -> System m a c
--   (.) = undefined

-- -- `Arrow` is the typeclass required for the "proc ... do" notation used to manipulate systems.
-- -- To be an instance of `Arrow`, a type must first be an instance of `Category` (as above), and also implement `arr` and `first`.
-- instance Arrow (System m) where
--   arr :: (b -> c) -> System m b c
--   arr f = undefined
--   first :: System m b c -> System m (b, d) (c, d)
--   first (M f) = undefined


-- -------------------
-- -- Probability
-- -------------------

-- -- what does it mean to be an implementation of probability distributions?

-- -- Dist a 

-- -- we should have a function of type `a -> Dist a` which assigns all probability to the given value of type a. 

-- -- we should have:
-- -- (a -> b) -> (Dist a -> Dist b)

-- -- todo bind

-- -- for continuous distributions, we should have a uniform distribution of type `Dist Double`


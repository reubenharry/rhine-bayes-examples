
{-# LANGUAGE ScopedTypeVariables #-}

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

simpleIdentity :: forall a. SimpleSystem a a
simpleIdentity = S (\x -> (x, simpleIdentity))

feedback :: forall a b c.  c -> SimpleSystem (a, c) (b, c) -> SimpleSystem a b
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


-- -- to generalize beyond a discrete deterministic system, we generalize in the following way.  
-- -- the idea is that `m` is any function from types to types (but to be useful in practice, a monad)
newtype MonadicSystem (m :: Type -> Type) (a :: Type) (b :: Type) = 
  MS                              {unM :: a -> m (b, MonadicSystem m a b)}
-- ^ arbitrary constructor name     ^ accessor

-- some type aliases that I use elsewhere in the code base
type a >--> b = forall m. MonadDistribution m => MonadicSystem m a b
type a >-/-> b = forall m. MonadMeasure m => MonadicSystem m a b

-- another convenient alias
type Particles a = [(a, Log Double)]

-- helpful note: `Population m a` is isomorphic to `m [(a, Log Double)]`


-- particleFilter takes an unnormalized stochastic system and returns a normalized stochastic system


-- Input system is of type `MonadicSystem (Population SamplerIO) a b`

-- unpacking the definitions, this is effectively of type:
-- a -> SamplerIO [((b, ...), Log Double)]

-- Output system is of type `MonadicSystem SamplerIO a (Particles b)` 
-- Unpacking the definitions, this is effectively of type:
-- a -> SamplerIO ([(b, Log Double)], ...)

-- (Note carefully where the square brackets and `Log Double` tuple are in the input system vs output system)


-- I've added type signatures quite verbosely throughout
-- As usual with Haskell, there's really only one way this definition can go if you follow the types
-- note that this code doesn't need to be specialized to `SamplerIO`, but really any `MonadDistribution` instance. 
particleFilter :: forall a b . MonadicSystem (Population SamplerIO) a b -> MonadicSystem SamplerIO a (Particles b)
particleFilter (msf :: MonadicSystem (Population SamplerIO) a b) = step initial

  where

  -- start by making one hundred copies of the input system
  initial :: Population SamplerIO (MonadicSystem (Population SamplerIO) a b)
  initial = spawn 100 $> msf


  step :: Population SamplerIO (MonadicSystem (Population SamplerIO) a b) -> MonadicSystem SamplerIO a [(b, Log Double)]
  step msfs = MS \(inputVal :: a) -> do -- this do-notation takes place in the monad SamplerIO 
           -- ^ we now have to define the output system, which is a function `a -> SamplerIO ([(b, Log Double)], ...)`

    -- for each copy of the input system (defined in `initial`), apply it to the input value (of the output system that we are in the process of defining)
    let afterKernel :: Population SamplerIO (b, MonadicSystem (Population SamplerIO) a b)
        afterKernel = msfs >>= (\x -> unM x inputVal) -- this bind (i.e. `>>=`) takes place in the (Population SamplerIO) monad
    
    -- this is the key step where we do resampling. note that we currently do resampling at every single step of the system, not adaptively
    -- `runPopulation` is basically the accessor for a `Population SamplerIO`, which is under the hood just a `SamplerIO [(a, Log Double)]
    (bAndMSFs :: [((b, MonadicSystem (Population m) a b), Log Double)] ) <- runPopulation (normalize (resampleSystematic afterKernel))
    
    let (currentPopulation :: [(b, Log Double)], continuations :: [(MonadicSystem (Population SamplerIO) a b, Log Double)]) =
          unzip $ (\((b, sf), weight) -> ((b, weight), (sf, weight))) <$> bAndMSFs
    
    let output :: [(b, Log Double)]
        output = currentPopulation

    -- some recursion needed here
    -- `fromWeightedList` constructs a Population, mouseover for its type
    let theRest :: MonadicSystem SamplerIO a [(b, Log Double)]
        theRest = step (fromWeightedList (pure continuations))

    return (output, theRest)





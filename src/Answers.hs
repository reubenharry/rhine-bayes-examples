
{-# LANGUAGE ScopedTypeVariables #-}


import Prelude hiding ((.)) -- don't expose standard definition of `.`
import Control.Category
import Control.Arrow (Arrow(arr, first), returnA)
import GHC.Base (Type)
import Control.Monad.Bayes.Class (MonadDistribution, MonadMeasure, Log, uniformD)
import Control.Monad.Bayes.Weighted (runWeightedT)
import Control.Monad.Bayes.Population (runPopulationT, extractEvidence, spawn, fromWeightedList, PopulationT, resampleSystematic)
import Control.Monad.Bayes.Inference.SMC (SMCConfig(resampler))
import Data.MonadicStreamFunction.InternalCore (MSF(unMSF, MSF))
import Data.Functor (($>))
import Inference (normalize)
import Control.Monad.Bayes.Sampler.Strict (SamplerIO)
import Control.Monad.Bayes.Enumerator (Enumerator, fromList)
import Control.Monad.Trans.MSF (Writer, writer)
import Control.Lens ((^.))
import Data.Fix (Fix(..))
import Data.Functor.Foldable
import Control.Monad.Trans.Free
import Data.Void (Void, absurd)
import Data.Functor.Compose
import Data.Functor.Identity (Identity(..))


-- I've left gaps in various parts of the code, marked by the word `undefined` (which just throws a runtime error).  
-- See `Answers.hs` for the correct solutions.

-- here is the implementation of a discrete deterministic system.  
-- Read this as saying: for any `a` and `b` that are Types,
-- a `SimpleSystem a b` is constructed by passing a function of 
-- type `(a -> (b, SimpleSystem a b))` (note the recursion) as an argument to `S`
-- `S` is known as a "constructor", and has type: 
    -- S :: forall a b .  (a -> (b, SimpleSystem a b)) -> SimpleSystem a b
newtype SimpleSystem (a :: Type) (b :: Type) = S (a -> (b, SimpleSystem a b))

newtype Simple a b c = SS {unS :: (a -> (b, c))} deriving Functor

type Syst m a b = FreeT (Simple a b) m Void

foo :: Monad m => Syst m Bool Bool
foo = FreeT (pure (Free (SS \b -> (not b, foo))))

bar :: MonadDistribution m => Syst m Double Int
bar = ana (\i -> Compose (pure (Free (SS \_ -> (3, i)))) ) True


r :: Monad m => Syst m a b -> [a] -> m [b]
r = cata (\(Compose f) -> \case
  [] -> pure []
  a:as -> do
                            f' <- f
                            case f' of
                              Pure x -> absurd x 
                              Free (SS x) -> do
                                let (b, newF) = x a 
                                bs <- newF as
                                pure $ b : bs
                            
  )

et :: SamplerIO [Int]
et = r bar []
  -- [] -> pure []
  -- (a:as) -> let (b, fn) = f a in do
  --     fn'' <- helper fn
  --     (b :) <$> fn'' as
  -- )

helper :: FreeF m Void a -> m a
helper (Free inp) = inp
helper (Pure b) = absurd b

-- pf :: forall a b . Syst (PopulationT SamplerIO) a b
--     -> Syst SamplerIO a (PopulationT Identity b)
-- pf (msf :: Syst (PopulationT SamplerIO) a b) = step initial

--   where

--   -- start by making one hundred copies of the input system
--   initial :: (PopulationT SamplerIO) (Syst (PopulationT SamplerIO) a b)
--   initial = spawn 100 $> msf


  -- step :: (PopulationT SamplerIO) (Syst (PopulationT SamplerIO) a b) -> Syst SamplerIO a (PopulationT Identity b)
  -- step msfs = ana (\(_) -> Compose (SS (\inputVal ->

  --   let afterKernel :: PopulationT SamplerIO (b, Syst (PopulationT SamplerIO) a b)
  --       afterKernel = msfs >>= (\x -> undefined $ (unS $ runFreeT x) inputVal ) -- this bind (i.e. `>>=`) takes place in the (PopulationT SamplerIO) monad
  --   in undefined
  --   ))) undefined

    -- FreeT \(inputVal :: a) -> do -- this do-notation takes place in the monad SamplerIO 
            -- ^ we now have to define the output system, which is a function `a -> SamplerIO ([(b, Log Double)], ...)`

    -- -- for each copy of the input system (defined in `initial`), apply it to the input value (of the output system that we are in the process of defining)

    -- -- this is the key step where we do resampling. note that we currently do resampling at every single step of the system, not adaptively
    -- -- `runPopulationT` is basically the accessor for a `PopulationT SamplerIO`, which is under the hood just a `SamplerIO [(a, Log Double)]
    -- (bAndMSFs :: [((b, Syst (PopulationT m) a b), Log Double)] ) <- runPopulationT (normalize (resampleSystematic afterKernel))

    -- let (currentPopulationT :: [(b, Log Double)], continuations :: [(Syst (PopulationT SamplerIO) a b, Log Double)]) =
    --       unzip (fmap (\((b, cont), weight) -> ((b, weight), (cont, weight))) bAndMSFs)


    -- -- some recursion needed here
    -- -- `fromWeightedList` constructs a PopulationT, mouseover for its type
    -- let theRest :: MonadicSystem SamplerIO a (PopulationT Identity b)
    --     theRest = step (fromWeightedList (pure continuations))

    -- return (fromWeightedList $ Identity currentPopulationT, theRest)
    -- undefined


ex :: MonadMeasure m => FreeT (ListF Int) m Void
ex = uncurry ana ex'

ex' :: MonadMeasure m => ( () -> Base (FreeT (ListF Int) m Void) (), ())
ex' = (undefined, ())

  -- (pure b, undefined))

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










-- -- to generalize beyond a discrete deterministic system, we generalize in the following way.  
-- -- the idea is that `m` is any function from types to types (but to be useful in practice, a monad)
newtype MonadicSystem (m :: Type -> Type) (a :: Type) (b :: Type) =
  MS                              {unM :: a -> m (b, MonadicSystem m a b)}
-- ^ arbitrary constructor name     ^ accessor

-- some type aliases that I use elsewhere in the code base
type a >--> b = forall m. MonadDistribution m => MonadicSystem m a b
type a >-/-> b = forall m. MonadMeasure m => MonadicSystem m a b

-- another convenient alias
type Particles = PopulationT Identity -- [(a, Log Double)]

-- helpful note: `PopulationT m a` is isomorphic to `m [(a, Log Double)]`


-- particleFilter takes an unnormalized stochastic system and returns a normalized stochastic system


-- Input system is of type `MonadicSystem (PopulationT SamplerIO) a b`

-- unpacking the definitions, this is effectively of type:
-- a -> SamplerIO [((b, ...), Log Double)]

-- Output system is of type `MonadicSystem SamplerIO a (Particles b)` 
-- Unpacking the definitions, this is effectively of type:
-- a -> SamplerIO ([(b, Log Double)], ...)

-- (Note carefully where the square brackets and `Log Double` tuple are in the input system vs output system)


-- PopulationT(SamplerIO)(a) ~ SamplerIO ([(a, Log(Double)])


-- I've added type signatures quite verbosely throughout
-- As usual with Haskell, there's really only one way this definition can go if you follow the types
-- note that this code doesn't need to be specialized to `SamplerIO`, but really any `MonadDistribution` instance. 
particleFilter :: forall a b .
  MonadicSystem (PopulationT SamplerIO) a b
  -> MonadicSystem SamplerIO a (PopulationT Identity b)
particleFilter (msf :: MonadicSystem (PopulationT SamplerIO) a b) = step initial

  where

  -- start by making one hundred copies of the input system
  initial :: (PopulationT SamplerIO) (MonadicSystem (PopulationT SamplerIO) a b)
  initial = spawn 100 $> msf


  step :: (PopulationT SamplerIO) (MonadicSystem (PopulationT SamplerIO) a b) -> MonadicSystem SamplerIO a (PopulationT Identity b)
  step msfs = MS \(inputVal :: a) -> do -- this do-notation takes place in the monad SamplerIO 
            -- ^ we now have to define the output system, which is a function `a -> SamplerIO ([(b, Log Double)], ...)`

    -- for each copy of the input system (defined in `initial`), apply it to the input value (of the output system that we are in the process of defining)
    let afterKernel :: PopulationT SamplerIO (b, MonadicSystem (PopulationT SamplerIO) a b)
        afterKernel = msfs >>= (`unM` inputVal) -- this bind (i.e. `>>=`) takes place in the (PopulationT SamplerIO) monad

    -- this is the key step where we do resampling. note that we currently do resampling at every single step of the system, not adaptively
    -- `runPopulationT` is basically the accessor for a `PopulationT SamplerIO`, which is under the hood just a `SamplerIO [(a, Log Double)]
    (bAndMSFs :: [((b, MonadicSystem (PopulationT m) a b), Log Double)] ) <- runPopulationT (normalize (resampleSystematic afterKernel))

    let (currentPopulationT :: [(b, Log Double)], continuations :: [(MonadicSystem (PopulationT SamplerIO) a b, Log Double)]) =
          unzip (fmap (\((b, cont), weight) -> ((b, weight), (cont, weight))) bAndMSFs)


    -- some recursion needed here
    -- `fromWeightedList` constructs a PopulationT, mouseover for its type
    let theRest :: MonadicSystem SamplerIO a (PopulationT Identity b)
        theRest = step (fromWeightedList (pure continuations))

    return (fromWeightedList $ Identity currentPopulationT, theRest)

type Pop = PopulationT Identity

pfGen ::
  (input >-/-> output)
  ->
  (input >--> Pop output)
pfGen msf = step initial

  where

  -- start by making one hundred copies of the input system
  -- initial :: (PopulationT SamplerIO) (MonadicSystem (PopulationT SamplerIO) a b)
  initial = spawn 100 $> msf



  step msfs = MS \(inputVal :: a) -> do -- this do-notation takes place in the monad SamplerIO 
           -- ^ we now have to define the output system, which is a function `a -> SamplerIO ([(b, Log Double)], ...)`

    -- for each copy of the input system (defined in `initial`), apply it to the input value (of the output system that we are in the process of defining)
    let afterKernel = msfs >>= (`unM` inputVal) -- this bind (i.e. `>>=`) takes place in the (PopulationT SamplerIO) monad

    -- this is the key step where we do resampling. note that we currently do resampling at every single step of the system, not adaptively
    -- `runPopulationT` is basically the accessor for a `PopulationT SamplerIO`, which is under the hood just a `SamplerIO [(a, Log Double)]
    bAndMSFs <- runPopulationT (normalize (resampleSystematic afterKernel))

    let (currentPopulationT, continuations ) =
          unzip (fmap (\((b, cont), weight) -> ((b, weight), (cont, weight))) bAndMSFs)



    -- some recursion needed here
    -- `fromWeightedList` constructs a PopulationT, mouseover for its type
    let theRest = step (fromWeightedList (pure continuations))

    return (fromWeightedList $ Identity currentPopulationT, theRest)





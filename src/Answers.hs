

import Prelude hiding ((.)) -- don't expose standard definition of `.`
import Control.Category
import Control.Arrow (Arrow, returnA)
import FRP.Rhine (Arrow(arr, first), accumulateWith)
import GHC.Base (Type)


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
constructExplicitly f s0 = feedback s0 (arr (\(x, y) -> f x y))

accumulateWith :: (a -> s -> s) -> s -> SimpleSystem a s
accumulateWith f s0 = feedback s0 (arr g)
  where
    g (a, s) = let s' = f a s in (s', s')


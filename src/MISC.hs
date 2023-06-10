
-- WIP

-- -- to generalize beyond a discrete deterministic system, we generalize in the following way.  
-- -- the idea is that `m` is any function from types to types
-- newtype System (m :: Type -> Type) (a :: Type) (b :: Type) = M (a -> m (b, System m a b))
--                                                           -- ^ arbitrary name



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


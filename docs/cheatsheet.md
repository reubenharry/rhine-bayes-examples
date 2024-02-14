## Rhine

!!! Note

    This document assumes intermediate level familiarity with Haskell.

This page serves as a reference to the core concepts behind Rhine, which is a beautiful library.

## Mealy machines for FRP

## Monadic FRP

The second idea is to represent it not as an ordinary Mealy machine, but a monadic Mealy machine:

example 

## Continuous time FRP

The third idea is to choose the monad to be `Reader Double` (or `ReaderT Double m` for an underlying monad `m`), where the `Double` represents intervals of time. Spelling this out:

example todo 

```haskell
type ClSF m cl a b = MSF (ReaderT (TimeInfo cl) m) a b
```


This means that we are no longer describing a discrete signal, but a **continuous** one: we don't say what the signal should be at the next step, but rather what it should be after a time interval $t$.

## Clocked FRP

The fourth idea is to think of the time intervals as being produced by a **clock**. Concretely, a clock is a discrete signal, so:

example

We can then run a signal function on a clock.

example: 

Rhine
flow

## Type-safe FRP

The fifth idea is to use type-level programming to ensure *clock safety*. For example, we might want the type of a clock to guarantee that it will tick every millisecond.


```haskell
class (TimeDomain (Time cl)) => Clock m cl where
  -- | The time domain, i.e. type of the time stamps the clock creates.
  type Time cl

  -- | Additional information that the clock may output at each tick,
  --   e.g. if a realtime promise was met, if an event occurred,
  --   if one of its subclocks (if any) ticked.
  type Tag cl

  -- | The method that produces to a clock value a running clock,
  --   i.e. an effectful stream of tagged time stamps together with an initialisation time.
  initClock ::
    -- | The clock value, containing e.g. settings or device parameters
    cl ->
    -- | The stream of time stamps, and the initial time
    m (MSF m () (Time cl) (Tag cl), (Time cl))
```


```haskell
data TimeInfo cl = TimeInfo
  { sinceLast :: Diff (Time cl)
  -- ^ Time passed since the last tick
  , sinceInit :: Diff (Time cl)
  -- ^ Time passed since the initialisation of the clock
  , absolute :: Time cl
  -- ^ The absolute time of the current tick
  , tag :: Tag cl
  -- ^ The tag annotation of the current tick
  }
```



### Asynchronous FRP

The goal of Rhine is to (safely) compose signal functions that run on different clocks.

Clocked signal functions are combined by means of a **resampling buffer**, which amounts to a function:

todo 


# Real-time Bayesian agents

![Particle filter](notebooks/basic-tracker.gif)

In the above gif, the green dot represents a latent variable, the unseen but true position of a particle, which is moving stochastically around a 2D environment. 
What the system actually sees are the noisy observations shown in red.
The purple swarm represents the system's guess as to the true position of the particle at the current time.

Both the simulation and the inference run **in real time**, so this gif is just a short snippet of a live demo.


# How to run

Install Haskell, using the installer, [GHCup](https://www.haskell.org/ghcup/).

Build and run with `cabal run demo`. This will open a window with numbered options, which you can choose by entering a number on your keyboard and pressing Enter.

# The code

The code is written in a *probabilistic programming library* in the functional programming language Haskell. The model for the first example looks like this:

```haskell
prior :: SignalFunction Stochastic () Position
prior = proc _ -> do
  x <- brownianMotion1D -< ()
  y <- brownianMotion1D -< ()
  returnA -< V2 x y

  where 

    brownianMotion1D :: SignalFunction Stochastic () Double
    brownianMotion1D = proc _ -> do
      dacceleration <- constM (normal 0 8 ) -< ()
      acceleration <- decayingIntegral 1 -< dacceleration
      velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
      position <- decayingIntegral 1 -< velocity
      returnA -< position
```


The `prior` describes the system's prior knowledge of how the green particle moves. Note that `prior` is a *time-varying* distribution, i.e. a stochastic process. This is reflected in the type of `prior`. Next, the generative model:

```haskell
observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
    (x,y) <- (noise &&& noise) -< ()
    returnA -< p + V2 x y
    where noise = constM (normal 0 std)
```

`observationModel` generates a process describing observations *given* the process describing the true position. Again, this is reflected in its type. Then the posterior:

```haskell
posterior :: SignalFunction (Stochastic & Unnormalized) Observation Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- prior -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent
```

Given a process representing incoming observations, `posterior` is a process representing the inferred position of the particle. We cannot sample from it yet, because it is unnormalized.

```haskell
inference :: SignalFunction Stochastic Observation [(Position, Weight)]
inference =  particleFilter params {n = 100} posterior
```

The `particleFilter` inference method takes an unnormalized signal function (here the posterior), and produces a (normalized) signal function representing the position of a set of particles and their corresponding weights, given the observations. This is what we sample from to obtain the purple particles shown in the first gif above.

Finally, we wrap the whole system in a signal function that expresses the behavior to be displayed to screen:

```haskell
main :: SignalFunction Stochastic Text Picture
main = proc message -> do
  actualPosition <- prior -< ()
  measuredPosition <- observationModel -< actualPosition
  samples <- particleFilter params posterior -< measuredPosition
  renderObjects -< Result measuredPosition actualPosition samples
```

# How to read the code

Start out with `Example.hs`, which has a simple example. If you are adding your own new example (which should have type `SignalFunction Stochastic UserInput Picture`), add it to the list of examples in `app/Main.hs`.

Sharp bits:
- the code in `MainSF.hs` is probably the least user friendly, but there's no reason to need to inspect or modify it.
- `Demo.hs` has some slightly more complex examples than `Example.hs`.



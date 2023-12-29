# Real-time Bayesian inference

[Rhine-Bayes](https://hackage.haskell.org/package/rhine-bayes) is a library in Haskell for simulating real-time Bayesian inference. An example of real time inference is a state space model, where observations arrive over time, and beliefs are updated accordingly.

![Particle filter](img/basic-tracker.gif)

In the above gif, the green dot represents a latent variable, the unseen but true position of a particle, which is moving stochastically around a 2D environment. 
What the system actually sees are the noisy observations shown in red.
The purple swarm represents the system's guess as to the true position of the particle at the current time.

Both the simulation and the inference run **in real time**, so this gif is just a short snippet of a live demo.

# The code

The code is written in a *probabilistic programming library* in the functional programming language Haskell. The model for the first example looks like this:

```haskell
prior :: () >--> Position
prior = proc _ -> do
  x <-brownianMotion1D-< ()
  y <-brownianMotion1D-< ()
  returnA -< V2 x y
```

`a >--> b` is roughly the type `(Time -> a) -> (Time -> b)`, so the type of stochastic dynamical systems, which take an `a`-valued function as input and return a distribution over `b`-valued functions as output.

!!! Formalism

    More formally, a dynamical system $S_{A,B}$ with an input of type $A$ and an output of type $B$ has the form:

    $$ S : (Time \to A) \to (Time \to B) $$

    with the additional requirement (known as causality) that $S(f)(t)$ is only a function of $S(t')$ for $t' \leq t$.

    This is also called a filter, or a causal signal function, depending on the area of study.

So the type `() >--> Position` of `prior` means that it takes nothing (or rather the trivial function $\lambda x : ()$ ) as input, and returns a signal as output with the value `Position` at each time point.

`prior` describes the system's prior knowledge of how the green particle moves. Note that `prior` is a *time-varying* distribution, i.e. a stochastic process. This is reflected in the type of `prior`. Next, the generative model:

```haskell
observationModel :: Position >--> Observation
observationModel = proc p -> do
  xNoise <-(uncorrelated (normal 0 std))-< ()
  yNoise <-(uncorrelated (normal 0 std))-< ()
  returnA -< p + V2 xNoise yNoise
```

`observationModel` generates a process describing observations *given* the process describing the true position. The is what we see in the red dots in the above gif. In this case, the observations are just the positions perturbed by uncorrelated Gaussian noise. Again, this is reflected in its type. Then the posterior:

```haskell
posterior :: Observation >-/-> Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <-prior-< ()
  () <-observe-< normalPdf oY std trueY * normalPdf oX std trueX
  returnA-< latent
```

Given a process representing incoming observations, `posterior` is a process representing the inferred position of the particle. We cannot sample from it yet, because it is unnormalized. This is reflected in the fact that they type has `>-/->`, not `>-->`.

```haskell
inference :: Observation >--> [(Position, Log Double)]
inference = particleFilter params posterior
```

The `particleFilter` inference method takes an unnormalized signal function (here the posterior), and produces a (normalized) signal function representing the position of a set of particles and their corresponding weights, given the observations. This is what we sample from to obtain the purple particles shown in the first gif above.

Finally, we wrap the whole system in a signal function that expresses the behavior to be displayed to screen:

```haskell
main :: Text >--> Picture
main = proc message -> do
  actualPosition <-prior-< ()
  measuredPosition <-observationModel-< actualPosition
  samples <-inference-< measuredPosition
  (showObs, showParts) <-interpretMessage-< message
  renderObjects-< Result 
    (if showObs then measuredPosition else 0) 
    actualPosition 
    (if showParts then samples else [])
```


# The approach

This is all implemented using a combination of (1) a paradigm for representing Bayesian probability and inference known as **probabilistic programming**, and (2) a paradigm for representing real-time interactive systems known as (functional) **reactive programming**.

There are several appealing aspects to this approach:

- it offers a natural representation for continuous time stochastic processes. Time is continuous in the sense that you write code that is agnostic to the final sampling rate of the whole system.

- it allows for these processes to be run indefinitely long without concerns about time or space leaks.

- it decouples the specification of the prior and posterior from the inference algorithm. For example, in the code for the above gif, the latent variable is described by a prior (which is a stochastic process) and the posterior is described by a (causal) stochastic function from the stream of observations to the latent variable.

- it allows for data to come from multiple streams, at different rates, or even from user input.

- it allows for Bayesian decision problems to be formulated. For example, one might want to take actions in real time, based on the current belief state, which would in turn influence incoming data.

# Functional reactive probabilistic programming: the details

Monad-bayes is great at handling **distributions** in a pure and functional manner (probabilistic programming), but doesn't have a good model of **time**. However, there are packages which can handle **time** in a pure and functional way (reactive programming), but don't know about **distributions**.

Because Haskell values extremely compositional code, it turns out that combining two such libraries seamlessly yields a library for time varying distributions.

# Details

todo
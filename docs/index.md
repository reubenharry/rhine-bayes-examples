This is a page documenting a [project](https://hackage.haskell.org/package/rhine-bayes) fusing probabilistic programming (a way to express probability distributions in code) with reactive programming (a way to express time-varying processes in code).

This approach makes is substantially easier to write stochastic dynamics systems with feedback and online Bayesian inference. In particular, in makes it much easier to write nested SMC, that is, running SMC on a model which includes a component that is the result of running SMC.

These are precisely the elements that make it tractable to build models of real-time Bayesian agents interacting. For that reason, I've been getting interested in models of communication, to get some theoretical handles on the questions of how and when conventions emerge and evolve.

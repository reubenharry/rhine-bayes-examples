[]{#markdown-mermaid aria-hidden="true" dark-mode-theme="dark"
light-mode-theme="default"}

# Convention emergence and change

I\'m interested in how natural communication systems emerge and change.
I think it\'s useful to explore (runnable) models in order to iron out
the details.

I have been experimenting with a framework for real-time inference that
I think is well suited for this, described in diagrammatic form below.
The code pretty closely mirrors the diagrams.

# A dynamical systems perspective on convention

The core idea is to model agents and the world as *dynamical systems*.
More formally, a dynamical system [[$S_{A,B}$]{.katex-mathml}[[[]{.strut
style="height:0.9694em;vertical-align:-0.2861em;"}[[S]{.mord .mathnormal
style="margin-right:0.05764em;"}[[[[[[]{.pstrut
style="height:2.7em;"}[[[A]{.mord .mathnormal .mtight}[,]{.mpunct
.mtight}[B]{.mord .mathnormal .mtight
style="margin-right:0.05017em;"}]{.mord .mtight}]{.sizing .reset-size6
.size3
.mtight}]{style="top:-2.55em;margin-left:-0.0576em;margin-right:0.05em;"}]{.vlist
style="height:0.3283em;"}[​]{.vlist-s}]{.vlist-r}[[]{.vlist
style="height:0.2861em;"}]{.vlist-r}]{.vlist-t
.vlist-t2}]{.msupsub}]{.mord}]{.base}]{.katex-html
aria-hidden="true"}]{.katex} with an input of type
[[$A$]{.katex-mathml}[[[]{.strut style="height:0.6833em;"}[A]{.mord
.mathnormal}]{.base}]{.katex-html aria-hidden="true"}]{.katex} and an
output of type [[$B$]{.katex-mathml}[[[]{.strut
style="height:0.6833em;"}[B]{.mord .mathnormal
style="margin-right:0.05017em;"}]{.base}]{.katex-html
aria-hidden="true"}]{.katex} has the form:

[[[$$\left. S:(Time\rightarrow A)\rightarrow(Time\rightarrow B) \right.$$]{.katex-mathml}[[[]{.strut
style="height:0.6833em;"}[S]{.mord .mathnormal
style="margin-right:0.05764em;"}[]{.mspace
style="margin-right:0.2778em;"}[:]{.mrel}[]{.mspace
style="margin-right:0.2778em;"}]{.base}[[]{.strut
style="height:1em;vertical-align:-0.25em;"}[(]{.mopen}[T]{.mord
.mathnormal style="margin-right:0.13889em;"}[im]{.mord
.mathnormal}[e]{.mord .mathnormal}[]{.mspace
style="margin-right:0.2778em;"}[→]{.mrel}[]{.mspace
style="margin-right:0.2778em;"}]{.base}[[]{.strut
style="height:1em;vertical-align:-0.25em;"}[A]{.mord
.mathnormal}[)]{.mclose}[]{.mspace
style="margin-right:0.2778em;"}[→]{.mrel}[]{.mspace
style="margin-right:0.2778em;"}]{.base}[[]{.strut
style="height:1em;vertical-align:-0.25em;"}[(]{.mopen}[T]{.mord
.mathnormal style="margin-right:0.13889em;"}[im]{.mord
.mathnormal}[e]{.mord .mathnormal}[]{.mspace
style="margin-right:0.2778em;"}[→]{.mrel}[]{.mspace
style="margin-right:0.2778em;"}]{.base}[[]{.strut
style="height:1em;vertical-align:-0.25em;"}[B]{.mord .mathnormal
style="margin-right:0.05017em;"}[)]{.mclose}]{.base}]{.katex-html
aria-hidden="true"}]{.katex}]{.katex-display}

With the additional requirement (known as causality) that
[[$S(f)(t)$]{.katex-mathml}[[[]{.strut
style="height:1em;vertical-align:-0.25em;"}[S]{.mord .mathnormal
style="margin-right:0.05764em;"}[(]{.mopen}[f]{.mord .mathnormal
style="margin-right:0.10764em;"}[)]{.mclose}[(]{.mopen}[t]{.mord
.mathnormal}[)]{.mclose}]{.base}]{.katex-html
aria-hidden="true"}]{.katex} is only a function of
[[$S(t^{\prime})$]{.katex-mathml}[[[]{.strut
style="height:1.0019em;vertical-align:-0.25em;"}[S]{.mord .mathnormal
style="margin-right:0.05764em;"}[(]{.mopen}[[t]{.mord
.mathnormal}[[[[[[]{.pstrut style="height:2.7em;"}[[[′]{.mord
.mtight}]{.mord .mtight}]{.sizing .reset-size6 .size3
.mtight}]{style="top:-3.063em;margin-right:0.05em;"}]{.vlist
style="height:0.7519em;"}]{.vlist-r}]{.vlist-t}]{.msupsub}]{.mord}[)]{.mclose}]{.base}]{.katex-html
aria-hidden="true"}]{.katex} for
[[$t^{\prime} \leq t$]{.katex-mathml}[[[]{.strut
style="height:0.8879em;vertical-align:-0.136em;"}[[t]{.mord
.mathnormal}[[[[[[]{.pstrut style="height:2.7em;"}[[[′]{.mord
.mtight}]{.mord .mtight}]{.sizing .reset-size6 .size3
.mtight}]{style="top:-3.063em;margin-right:0.05em;"}]{.vlist
style="height:0.7519em;"}]{.vlist-r}]{.vlist-t}]{.msupsub}]{.mord}[]{.mspace
style="margin-right:0.2778em;"}[≤]{.mrel}[]{.mspace
style="margin-right:0.2778em;"}]{.base}[[]{.strut
style="height:0.6151em;"}[t]{.mord .mathnormal}]{.base}]{.katex-html
aria-hidden="true"}]{.katex}.

This is also called a filter, or a causal signal function, depending on
the area of study.

## Agents and the world

Suppose we have a space of observations [[$O$]{.katex-mathml}[[[]{.strut
style="height:0.6833em;"}[O]{.mord .mathnormal
style="margin-right:0.02778em;"}]{.base}]{.katex-html
aria-hidden="true"}]{.katex} and a space of actions
[[$A$]{.katex-mathml}[[[]{.strut style="height:0.6833em;"}[A]{.mord
.mathnormal}]{.base}]{.katex-html aria-hidden="true"}]{.katex}.

An agent is a dynamical system [[$S_{O,A}$]{.katex-mathml}[[[]{.strut
style="height:0.9694em;vertical-align:-0.2861em;"}[[S]{.mord .mathnormal
style="margin-right:0.05764em;"}[[[[[[]{.pstrut
style="height:2.7em;"}[[[O]{.mord .mathnormal .mtight
style="margin-right:0.02778em;"}[,]{.mpunct .mtight}[A]{.mord
.mathnormal .mtight}]{.mord .mtight}]{.sizing .reset-size6 .size3
.mtight}]{style="top:-2.55em;margin-left:-0.0576em;margin-right:0.05em;"}]{.vlist
style="height:0.3283em;"}[​]{.vlist-s}]{.vlist-r}[[]{.vlist
style="height:0.2861em;"}]{.vlist-r}]{.vlist-t
.vlist-t2}]{.msupsub}]{.mord}]{.base}]{.katex-html
aria-hidden="true"}]{.katex}.

``` {style="all:unset;"}
flowchart LR
  Observation(( )) -->|Observation| Agent -->|Action| Action(( ))
```

The world (outside of the agent) is a dynamical system
[[$S_{A,O}$]{.katex-mathml}[[[]{.strut
style="height:0.9694em;vertical-align:-0.2861em;"}[[S]{.mord .mathnormal
style="margin-right:0.05764em;"}[[[[[[]{.pstrut
style="height:2.7em;"}[[[A]{.mord .mathnormal .mtight}[,]{.mpunct
.mtight}[O]{.mord .mathnormal .mtight
style="margin-right:0.02778em;"}]{.mord .mtight}]{.sizing .reset-size6
.size3
.mtight}]{style="top:-2.55em;margin-left:-0.0576em;margin-right:0.05em;"}]{.vlist
style="height:0.3283em;"}[​]{.vlist-s}]{.vlist-r}[[]{.vlist
style="height:0.2861em;"}]{.vlist-r}]{.vlist-t
.vlist-t2}]{.msupsub}]{.mord}]{.base}]{.katex-html
aria-hidden="true"}]{.katex}.

``` {style="all:unset;"}
flowchart LR
  Action(( )) -->|Action| World -->|Observation| Observation(( ))
```

The whole system can be expressed as a feedback loop:

``` {style="all:unset;"}
flowchart BT
  
  subgraph Agent[ Agent ]
    direction TB

  end

  subgraph World
  end 

  World -->|Observation| Agent
  Agent -->|Action| World
```

# More about the world

Assume the world consists of two parts, in sequence:

1.  A dynamical system [[${Evolve}_{A,Z}$]{.katex-mathml}[[[]{.strut
    style="height:0.9805em;vertical-align:-0.2861em;"}[[[Evolve]{.mord
    .mathit}]{.mord}[[[[[[]{.pstrut style="height:2.7em;"}[[[A]{.mord
    .mathnormal .mtight}[,]{.mpunct .mtight}[Z]{.mord .mathnormal
    .mtight style="margin-right:0.07153em;"}]{.mord .mtight}]{.sizing
    .reset-size6 .size3
    .mtight}]{style="top:-2.55em;margin-right:0.05em;"}]{.vlist
    style="height:0.3283em;"}[​]{.vlist-s}]{.vlist-r}[[]{.vlist
    style="height:0.2861em;"}]{.vlist-r}]{.vlist-t
    .vlist-t2}]{.msupsub}]{.mord}]{.base}]{.katex-html
    aria-hidden="true"}]{.katex} which describes the (time varying)
    latent state of the world (potentially as a function of the agent\'s
    actions)
2.  A dynamical system [[${Render}_{Z,O}$]{.katex-mathml}[[[]{.strut
    style="height:0.9805em;vertical-align:-0.2861em;"}[[[Render]{.mord
    .mathit}]{.mord}[[[[[[]{.pstrut style="height:2.7em;"}[[[Z]{.mord
    .mathnormal .mtight style="margin-right:0.07153em;"}[,]{.mpunct
    .mtight}[O]{.mord .mathnormal .mtight
    style="margin-right:0.02778em;"}]{.mord .mtight}]{.sizing
    .reset-size6 .size3
    .mtight}]{style="top:-2.55em;margin-right:0.05em;"}]{.vlist
    style="height:0.3283em;"}[​]{.vlist-s}]{.vlist-r}[[]{.vlist
    style="height:0.2861em;"}]{.vlist-r}]{.vlist-t
    .vlist-t2}]{.msupsub}]{.mord}]{.base}]{.katex-html
    aria-hidden="true"}]{.katex} which describes the (time varying)
    observations in terms of the latent state

``` {style="all:unset;"}
flowchart LR
  
  subgraph Agent[ Agent ]
    direction TB

  end

  subgraph World
    direction BT
    Evolve -->|State| Render((Render))
  end 

  Render -->|Observation| Agent
  Agent -->|Action| Evolve((Evolve))
```

# More about the agent

Suppose that the agent also consists of two parts:

1.  An inference enginge [[${Infer}_{O,Z}$]{.katex-mathml}[[[]{.strut
    style="height:1.0747em;vertical-align:-0.3802em;"}[[[Infer]{.mord
    .mathit}]{.mord}[[[[[[]{.pstrut style="height:2.7em;"}[[[O]{.mord
    .mathnormal .mtight style="margin-right:0.02778em;"}[,]{.mpunct
    .mtight}[Z]{.mord .mathnormal .mtight
    style="margin-right:0.07153em;"}]{.mord .mtight}]{.sizing
    .reset-size6 .size3
    .mtight}]{style="top:-2.4559em;margin-right:0.05em;"}]{.vlist
    style="height:0.2342em;"}[​]{.vlist-s}]{.vlist-r}[[]{.vlist
    style="height:0.3802em;"}]{.vlist-r}]{.vlist-t
    .vlist-t2}]{.msupsub}]{.mord}]{.base}]{.katex-html
    aria-hidden="true"}]{.katex} which produces a (time varying) belief
    about the latent state given the (time varying) observation.
2.  A decision model which converts this belief into an action

Part (1) is Bayesian: it uses a model of the world as a prior, and
conditions on the observations. Inference is done by Sequential Monte
Carlo (particle filtering), which delivers an unbiased estimate of the
true posterior.

Part (2) could be anything, depending on the agent\'s goal.

``` {style="all:unset;"}
flowchart LR
  
  subgraph Agent[ Agent ]
    direction TB
    subgraph PF[Particle Filter]
        direction LR
        Infer((Model of world))
    end
    PF -->|Belief about State | CA((Decide))
  end

  subgraph World
    direction BT
    Evolve -->|State| Render((Render))
  end 

  Render -->|Observation| PF
  CA -->|Action| Evolve((Evolve))
```

# Multiple agents

We really want 2 or more agents interacting with the world, like so:

``` {style="all:unset;"}
flowchart LR
  
  subgraph Agent[ Agent 1 ]
    direction TB
    subgraph PF[Particle Filter]
        direction LR
        Infer((Model of world))
    end
    PF -->|Belief about State | CA((Decide))
  end
  
  
  subgraph Agent2[ Agent 2 ]
    direction TB
    subgraph PF2[Particle Filter]
        direction LR
        Infer2((Model of world))
    end
    PF2 -->|Belief about State | CA2((Decide))
  end

  subgraph World
    direction BT
    Evolve -->|State| Render((Render))
  end 

  Render -->|Agent 1 Observation| PF
  CA -->|Agent 1 Action| Evolve((Evolve))
  
  Render -->|Agent 2 Observation| PF2
  CA2 -->|Agent 2 Action| Evolve((Evolve))
```

But this is not quite right, because now the model of the world that
each agent must have needs to include the other agent:

``` {style="all:unset;"}
flowchart LR
  
  subgraph AgentSub[ Agent 1 ]
    direction TB
    subgraph PFSub[Particle Filter]
        direction LR
        InferSub((Model of world))
    end
    PFSub -->|Belief about State | CASub((Decide))
  end
  
  subgraph AgentSub2[ Agent 2 ]
    direction TB
    subgraph PFSub2[Particle Filter]
        direction LR
        InferSub2((Model of world))
    end
    PFSub2 -->|Belief about State | CASub2((Decide))
  end
  
  subgraph Agent[ Agent 1 ]
    direction TB
    subgraph PF[Particle Filter]
        direction LR
        AgentSub2((Model of world))
    end
    PF -->|Belief about State | CA((Decide))
  end
  
  
  subgraph Agent2[ Agent 2 ]
    direction TB
    subgraph PF2[Particle Filter]
        direction LR
        AgentSub((Model of world))
    end
    PF2 -->|Belief about State | CA2((Decide))
  end

  subgraph World
    direction BT
    Evolve -->|State| Render((Render))
  end 

  Render -->|Observation| PF
  CA -->|Action| Evolve((Evolve))
  
  Render -->|Observation| PF2
  CA2 -->|Action| Evolve((Evolve))
```

In fact, this causes an infinite regress, since really, each agent\'s
model of the other must include the other agent\'s model of it, which in
turn\...

# Grounding in a base case

To break this cycle, let\'s assume that `Agent 1` models the world
without knowing about `Agent 2`. That is, from `Agent 1`\'s perspective,
the world looks like:

``` {style="all:unset;"}
flowchart LR


  subgraph World[World From Perspective of Agent 1 ]
    direction BT
    Evolve -->|State| Render((Render))
  end 

  Render -->|Observation| A(( ))
  B(( )) -->|Action| Evolve((Evolve))
```

Or, at least, `Agent 2` models `Agent 1` in this way, regardless of
whether this is what `Agent 1` truly does.

``` {style="all:unset;"}
flowchart LR
  
  subgraph AgentSub[ Agent 1 ]
    direction TB
    subgraph PFSub[Particle Filter]
        direction LR
        InferSub((Model of world))
    end
    PFSub -->|Belief about State | CASub((Decide))
  end
  
  subgraph AgentSub2[ World From Perspective of Agent 1 ]
    direction TB
    subgraph Word[ ]
        direction LR
        Evolv((Evolve)) -->|State| Rende((Render))
    end 

 
  end
  
  subgraph Agent[ Agent 1 ]
    direction TB
    subgraph PF[Particle Filter]
        direction LR
        AgentSub2((Model of world))
    end
    PF -->|Belief about State | CA((Decide))
  end
  
  
  subgraph Agent2[ Agent 2 ]
    direction TB
    subgraph PF2[Particle Filter]
        direction LR
        AgentSub((Model of world))
    end
    PF2 -->|Belief about State | CA2((Decide))
  end

  subgraph World
    direction BT
    Evolve -->|State| Render((Render))
  end 

  Render -->|Observation| PF
  CA -->|Action| Evolve((Evolve))
  
  Render -->|Observation| PF2
  CA2 -->|Action| Evolve((Evolve))
```

We also assume that `Agent 1`, in actual fact, does model `Agent 2`, in
the same way, i.e. by assuming that `Agent 2` doesn\'t model `Agent 1`.

# Uncertainty over the base case

The final step is for each agent to jointly infer (some parameters of)
their model of the other agent, in addition to their usual inference
about the state of the world.

For example, if `Agent 2` believes that `Agent 1` produces actions by
some process `p`, they will invert `p` to learn information about
`Agent 1`\'s belief state and in turn, the state of the world.

We can think of the parameters of each agent\'s model of the other as a
form of language/convention, and the inference about it as a form of
language change.

This reduces the problem of language change/emergence to: can we specify
priors over languages, and inference algorithms in a way which a) is
cognitively plausible, and b) gives rise to recognizable dynamics of
language change?

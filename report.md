# Process Algebraic Grammatical Parser

In the implementation of this parser, we show that Haskell allows for intuitive and elegant code representation of reduction systems.

## Core Components

### Channel

```haskell
data Channel = Output Name | Input Name | Empty
```

### Process

Each process represents a transition system

* Parallel: several processes can run in parallel, "bonding" i.e: exchanging channels with each other
* No ordering between processes: parallel processes are not ordered

```haskell
data Process =
  Saturated Name
  | Stable Name
  | Prefix [Channel] Process
  | Parallel [Process]
```

## Reduction System

So, the *reduction system* is, more appropriately, a transition system, as nothing gets reduced- rather, processes transition to other processes on communication.

Initial: a set of processes running in parallel
Final: Also a set of processes running in parallel


### Transition Rule

The only transition rule for this simple system involves processes offering/accepting channels.

Two processes running in parallel can communicate like below:

P1 = subject? P1'
P2 = subject! P2'

c? P1 | c! P2 -> P1' | P2'

Note that the parallel operator `|` is commutative and associative.


### Reaction

Or: simulating a parallel processes model


## Parsing As Reduction: Interactive

If we want to model natural language parsing as a process algebra, we need to define the end result of the parse- i.e: at what point 

If we restrict channels to only mandatory (?) arguments and nothing more, then we can define a stable process as a `Null` process, as in, a process without any channels

### Optional Channels

If we extend our model of natural language to account for the possibility of optional arguments, then we need to change our definition of stable process.

We add a new final state to the `Process` type, and do some renaming to reflect the fact that a `Null` process now represents a "saturated" word- one with no arguments left to fulfil or offer: 

```haskell
data Process =
  Lexeme Name
  | Prefix [Channel] [Channel] Process
  | Parallel [Process]
```

## Parsing As Reduction: Automatic

## Dictionary and Populator

## Problems

### Local Word Grouping Through Transitions

How can we account for local word groups, or for parsing hierarchical syntax in general? (I don't think there's a non-hierarchical syntactic theory)

## Future Work

* Making Reduction Stochastic - bonding probabilies on channel pairs

* Making Transitions Probabilistic Instead of deterministic

* Changing Reduction Probabilities: "Molecular Attraction" : if Px in set, increase bonding probabilities on channels [a...n]

* Learning Dynamics - So how do we learn the probabilies and increases?
# mop
middle-out programming<sup>[1](#middle-out-def)</sup><a name="middle-out"></a> in Haskell. 

## Argument

We have failed to approach our programming languages and library implementations with the knowledge and results gained from the first language abstraction experiments: that by building upon a series of linearly-declared instructions with increased granularity increases productivity and eases analysis, implementation, composition, extensibility, modularity, generation, optimization, and execution. I hope to show that complex libraries should always be written in a style whose functionality can be subsumed and optionally overwritten. 

## Solution

Towards this goal, the middle-out approach attempts to reify the concept of designing programs as the granular composition of DSLs. `mop` declares the machinery for defining DSLs as free monadic algebras paired with cofree comonadic coalgebras. This library exists as a utility layer for modules written in this modular style as well as as a guide for the generic approach to library design that this style entails. Thus, the style of development implied by middle-out programming is the production of a problem-specific DSL built as the composition of new and existing DSLs. 

For modularity, the pairing of instructions with interpreters is class-based for overloading. That is, if you want to swap out an interpreter for an individual instruction in your domain you may easily overlap its existing instance with a custom-designed interpreter and pair them up.

For performance, DSLs may be optimized before being paired with a coalgebraic interpreter for execution. The chances for optimization of a domain should be unbounded and guaranteed as opposed to that of RULES pragmas. 

For purity and optimizability, effectful operations should be pushed to the bottom of the interpreter - instead of being effectful, non-effectfully produce a series of instructions that can be executed in an effectful context. This can maintain the purity of your stack which improves chances for optimization. 

## Past

This library came about as a realization of the need for, and capabilities implied by, the production of libraries as DSLs with instances of the free monad. This realization came shortly before finding Dave Laing's excellent series of articles titled 'Coproducts for free and products for cofree' which cemented the desire to jump in and the recognition of the beautiful duality between free monadic algebras and cofree comonadic coalgebras.

## Future

The plan for now is:

1. The production of a set of base libraries in this style.
1. Experiments in composing interpreters.
1. Experiments in combining instruction sets. 
1. Experiments in styles of testing.
1. Experiments in optimization of free monadic instruction sets.

## Naming

While `mop` is meant as a solution to this problem, the name is simply an homage to MP Ward's work on Language-oriented programming. Thus, this library could similarly be named `lomop` or language-oriented middle-out programming.

### References

<a name="middle-out-def">[<sup>1</sup>](#middle-out):</a> MP Ward - Language Oriented Programming: <a href="http://www.cse.dmu.ac.uk/~mward/martin/papers/middle-out-t.ps.gz">PDF</a>

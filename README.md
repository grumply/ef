# mop
middle-out programming<sup>[1](#middle-out-def)</sup><a name="middle-out"></a> in Haskell. 

This library and its dependents mainly exist as a playground for paired, extensible, composable DSL/interpreter implementations. 

## Mission Statement

We have failed to approach our programming languages and library implementations with the knowledge and results gained from the first language abstraction experiments: that by building upon a series of linearly-declared instructions with increased granularity increases productivity and eases analysis, implementation, composition, extensibility, modularity, generation, optimization, and execution; that complex libraries should always be written in a style whose functionality can be subsumed and optionally overwritten. 

## Solution

Towards this goal, the middle-out approach attempts to reify the concept of designing programs as the granular composition of DSLs. `mop` declares the machinery for defining DSLs as free monadic algebras paired with cofree comonadic coalgebras. This library exists as a utility layer for modules written in this modular style as well as as a guide for the generic approach to library design that this style entails. Thus, the style of development implied by middle-out programming is the production of a problem-specific DSL built as the composition of new and existing DSLs as well as a problem-specific interpreter built as the composition of new and existing interpreters.

For modularity, the pairing of instructions with interpreters is class-based for overloading. That is, if you want to swap out an interpreter for an individual instruction in your domain you may easily overlap its existing instance with a custom-designed interpreter and pair them up.

For performance, DSLs may be optimized before being paired with a coalgebraic interpreter for execution. The chances for optimization of a domain should be unbounded and guaranteed as opposed to that of RULES pragmas. That is, custom optimization passes for a free algebra that is defined as the composition of two other algebras is possible.

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

While `mop` is meant as a solution to this problem, the name is simply an homage to MP Ward's work on Language-oriented programming. Thus, this library could similarly be named `lomop` or `language-oriented middle-out programming`.

## Standard Conventions

### Nomenclature

There are a few fundamental pieces of this DSL-based design. They include:

1. Instruction: A data declaration with a continuation parameter.
    Variants:
      1. Open: Has only one constructor. 
      1. Closed: Has multiple constructors.
1. Interpreter: A data declaration that pairs an Instruction. Derivable.
    Variants:
      1. Open: Single record data declaration.
      1. Closed: Multiple record data declaration.
1. Pairing: A type-classed link between Instruction and Interpreter. Derivable.
1. Interface: A type used to guide development and include a set of functionality in a goal implementation. Derivable.
1. Implementation: A type used to implement a group of interpreters that pairs with an algebra. Skeletally-derivable. 

### Usage

The following will lay out what open and closed means and how various styles of interface and implementation interact. These are largely just good-practice suggestions rather than compiler-enforced invariants. While some of these suggestions may be ignored, it can break modularity.

Instructions may be combined in a sum-of-functors style to create sets of instructions that may be paired with interpreters combined in a product-of-functors style. As long as pairing exists between individual instruction and individual interpreter, the pairing propagates to the pairing of sum-of-instructions and product-of-interpreters.

Open variants of instructions implies that some functionality exists independent from the rest of a system. That no other instructions are required to implement the functionality implied by the instruction.

Example open instruction: tag something as full
```Haskell
data Full k = Full k

-- Use like this to ease and automate derivation of implementation
type SomeInterface = Full :+: RestOfInterface
```

A closed instruction set implies that some functionality exists in an always-coupled fashion; that one instruction may not be implemented without implementation of the others.

Example of closed instruction: validity
```Haskell
data Validity k = Valid k | Invalid k

type SomeInterface = Validity :+: RestOfInterface
```

Any implementation must pair both instructions. Since Validity is a single type with multiple constructors, we must pair it with a record of interpreters.

```Haskell
data CoValidity k = CoValidity
  { coValid :: k
  , coInvalid :: k
  }
```

TODO: 
1. expand on pairings
1. demonstrate concrete instruction set
1. expand on implementation, including derivation

### References

<a name="middle-out-def">[<sup>1</sup>](#middle-out):</a> MP Ward - Language Oriented Programming: <a href="http://www.cse.dmu.ac.uk/~mward/martin/papers/middle-out-t.ps.gz">PDF</a>

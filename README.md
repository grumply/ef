# mop
middle-out programming<sup>[1](#middle-out-def)</sup><a name="middle-out"></a> in Haskell. 

This file exists purely as a development environment for the mop ecosystem including requirements and specifications. This repository is a work in progress, please do not share yet. Discussion and contributions welcome.

## Mission Statement

We have failed to approach our programming languages and library implementations with the knowledge and results gained from the first language abstraction experiments: that by building upon a series of linearly-declared instructions with increased granularity increases productivity and eases analysis, implementation, composition, extensibility, modularity, generation, optimization, and execution; that complex libraries should always be written in a style whose functionality can be subsumed and optionally overwritten. 

The goal of the mop ecosystem is a reasoned approach to software design including automatic derivation of skeletal implementations from abstract instruction representations, automated versioning based on interplay between abstract interfaces and concrete implementations.

## Approach

Design:

<img src="https://github.com/grumply/mop/blob/master/doc/Design.jpg" width="500">

Semantic Versioning:


<img src="https://github.com/grumply/mop/blob/master/doc/VersioningAlgCoalg.jpg" width="500">

<img src="https://github.com/grumply/mop/blob/master/doc/VersioningInstrInterp.jpg" width="500">


<add image>

TODO: 
  Comment on:
    1. Algebra easier to implement than coalgebra
    2. Coalgebra derivable from algebra.
    3. Instructions derivable from algebra.
    4. Interpreter derivable from coalgebra, thus transitively derivable from algebra.
    5. Algebra, theoretically, derivable from instructions.
    6. Easiest approach is to implement an algebra representing instruction set and derive coalgebra, instructions, and interpreters and then use the skeletal implementation of interpreters to write a library. So the approach to library design is 'Write a specification as an algebra' and 'Implement derived interpreter'.
    7. Type signatures are largely unneccesary.
    

## Past

This library came about as the realization of the need for, and capabilities implied by, the duality between monadic free algebras and comonadic cofree coalebgras plus the duality between code and data that these constructs have strongly suggested. Dave Laing has a nice [series](http://dlaing.org/cofun/) which was part of the impetus to start this project. If you haven't read it, I strongly suggest reading the wonderful paper by Wouter Swierstra: [Data types à la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) (Warning: PDF)

## Naming

The name `mop` is an homage to MP Ward's work on Language-oriented programming. Thus, this library could similarly be named `lomop` or `language-oriented middle-out programming`.

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

Examples of shapes of instructions and their corresponding interpreter shapes. Interpreter shapes are automatically derivable from instructions. I am unsure about instructions defined as the product of continuations. Is the proper pairing to unify them as in G/CoG below, or should records be used for closed interpreters/data?
```Haskell
data A k = A k
data CoA k = CoA k

data B s k = B s k
data CoB s k = CoB (s -> k)

data C s k = C (s -> k)
data CoC s k = CoC (s,k)

data D s k = D (s,k)
data CoD s k = CoD (s -> k)

data EF k = E (Int -> k) | F k
data CoEF k = CoEF
  { coE :: (Int,k)
  , coF :: k
  }

data G k = Magma k => G
  { g1 :: Int -> k
  , g2 :: Bool -> k
  }
data CoG k = Magma k => CoG (Int,k) (Bool,k)
instance Pairing CoG G where
  pair p (CoG (i,k1) (b,k2)) (G ik bk) 
    = p (k1 <> k2) (ik i <> bk b)
```

TODO: 
1. expand on pairings
1. demonstrate concrete instruction set
1. expand on implementation, including derivation

### References

<a name="middle-out-def">[<sup>1</sup>](#middle-out):</a> MP Ward - Language Oriented Programming: <a href="http://www.cse.dmu.ac.uk/~mward/martin/papers/middle-out-t.ps.gz">PDF</a>

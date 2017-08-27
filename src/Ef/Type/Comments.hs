{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
module Ef.Type.Comments where

import GHC.TypeLits
import GHC.Exts

import Data.Proxy

-- An expiriment in type-level comments. Benefits of this approach are seen in
-- an environment with type inspection or when retrieving type information in
-- ghci. These comments are trivially removed at compile time. Downsides are
-- excessively long error messages in compilation.

-- | Type-level inline annoation.
--
-- > divide :: Int -> Int ::: "Must be non-zero." -> Int
--
-- Note that comments added with (:::) are visible when using underscore for
-- wildcard hole-completion. Thus, in an environment that supports it, like
-- ghci, and given the following code:
--
-- > divide 3 _
--
-- You could see: Found hole: _ :: Int ::: "Must be non-zero." ...
--
-- This annoation is useful even when a comment constraint context is supplied
-- because the annoation propagates to completion tooltips in editors, wild-card
-- compiler completions and compilation errors.
type (:::) (a :: k) (comment :: Symbol) = a
infixl 0 :::

type Using (u :: k) (x :: k') = (() :: Constraint)
type Describe (d :: k) (describe :: k') = (() :: Constraint)

type Given (givens :: k) = (() :: Constraint)

type Invariant (i :: k) = (() :: Constraint)
type Assumption (a :: k) = (() :: Constraint)
type Assume (x :: k) (assume :: k') = (() :: Constraint)
type Assumptions (assumptions :: k') = (() :: Constraint)

type Define (d :: k) = (() :: Constraint)
type Defines (d :: k) = (() :: Constraint)
type Parameter (param :: k) = (() :: Constraint)
type Variable (v :: k) = (() :: Constraint)
type Environment (environment :: k') = (() :: Constraint)

type It (it :: k) = (() :: Constraint)
type Should (should :: k) = (() :: Constraint)
type Then (t :: k) = (() :: Constraint)
type May (t :: k) = (() :: Constraint)

type Produce (p :: k) = (() :: Constraint)
type Consume (c :: k) = (() :: Constraint)

type Produces (p :: k) = p
type Consumes (c :: k) = c

type Input (input :: k) = (() :: Constraint)
type Output (output :: k) = (() :: Constraint)
type Finally (finally :: k) = (() :: Constraint)

type Projects (p :: k) = (() :: Constraint)

type Transforms (p :: k) = (() :: Constraint)
type Transformed (t :: k) = (() :: Constraint)

-- type Manage (manages :: k) = (() :: Constraint)
type Manages (manages :: k) = (() :: Constraint)
type Construct (constrcuts :: k) = (() :: Constraint)
type Constructs (constrcuts :: k) = (() :: Constraint)
type Destroy (destroys :: k) = (() :: Constraint)
type Destroys (destroys :: k) = (() :: Constraint)
type Mutate (mutate :: k) = (() :: Constraint)
type Mutates (mutates :: k) = (() :: Constraint)
type Return (return :: k) = (() :: Constraint)
type Returns (returns :: k) = (() :: Constraint)
type Remove (remove :: k) = (() :: Constraint)
type Removes (removes :: k) = (() :: Constraint)
type Add (add :: k) = (() :: Constraint)
type AddS (adds :: k) = (() :: Constraint)
type Update (update :: k) = (() :: Constraint)
type Updates (updates :: k) = (() :: Constraint)

-- Tags
type New (new :: k) = new
type Old (old :: k) = old
type Intermediate (imd :: k) = imd
type Updated (updated :: k) = updated
type Added (added :: k) = added
-- type Removed (removed :: k) = removed
type Returned (returned :: k) = returned
type Mutated (mutated :: k) = mutated
type Destroyed (destroyed :: k) = destroyed
type Constructed (constructed :: k) = constructed
type Managed (managed :: k) = managed

type Mutable (x :: k) = x
type Immutable (x :: k) = x


type From (x :: k) (y :: k') = y
type In (x :: k') (y :: k') = y

type Let (x :: k) = (() :: Constraint)

type An (a :: k) = (() :: Constraint)

type Result (result :: k') = (() :: Constraint)
type Results (results :: k') = (() :: Constraint)

type Class (c :: k) = (() :: Constraint)

type Constructor (constructor :: k') = (() :: Constraint)
type Method (f :: k) = (() :: Constraint)
type Function (f :: k) = (() :: Constraint)
type Field (f :: k) = (() :: Constraint)
type Selector (s :: k) = (() :: Constraint)
type Pattern (p :: k) = (() :: Constraint)
type Implicit (i :: k) = (() :: Constraint)

type Example (ex :: k') = (() :: Constraint)

type Usage (x :: k) = (() :: Constraint)
type About (x :: k) = (() :: Constraint)
type Use (x :: k) = (() :: Constraint)
type Misuse (x :: k) = (() :: Constraint)
type Permits (x :: k) = (() :: Constraint)
type Forbids (x :: k) = (() :: Constraint)
type Allows (x :: k) = (() :: Constraint)
type Disallows (x :: k) = (() :: Constraint)
type Dos (x :: k) = (() :: Constraint)
type Don'ts (x :: k) = (() :: Constraint)
type Errors (x :: k) = (() :: Constraint)

type TODO (x :: k) = (() :: Constraint)
type FIXME (x :: k) = (() :: Constraint)
type NOTE (x :: k) = (() :: Constraint)
type BUG (x :: k) = (() :: Constraint)
type IDEA (x :: k) = (() :: Constraint)
type HACK (x :: k) = (() :: Constraint)

type Thus (a :: k) (a' :: k) = a' ~ a

type Reconciles (a :: k) (b :: k) = a ~ b

type Compares (a :: k) (b :: k) = a ~ b
type Simultaneously (a :: k) (b :: k') = (() :: Constraint)

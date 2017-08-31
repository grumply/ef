{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
module Ef.Type.Comments where

import GHC.TypeLits
import GHC.Exts

import Data.Char

import Language.Haskell.TH
import Language.Haskell.TH.Quote

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

c :: QuasiQuoter
c = QuasiQuoter { quoteType = commentExprType }

commentExprType :: String -> TypeQ
commentExprType = foldr ((\x xs -> appT (appT promotedConsT x) xs) . litT . strTyLit) promotedNilT . lines . unindent

type Using (u :: k) (x :: k') = (() :: Constraint)
type Describe (d :: k) (describe :: k') = (() :: Constraint)

type Parameters (ps :: k) = (() :: Constraint)
type Param    (p :: k) (x :: k') = (() :: Constraint)

type Returns (returns :: k) = (() :: Constraint)
type Return (v :: k) (return :: k') = (() :: Constraint)

type Value (v :: k) (x :: k') = (() :: Constraint)

type Implicits (is :: k) = (() :: Constraint)
type Implicit (i :: k) (x :: k') = (() :: Constraint)
type Variable (v :: k) (x :: k') = (() :: Constraint)

type Given (givens :: k) = (() :: Constraint)

type Invariant (i :: k) = (() :: Constraint)
type Assumption (a :: k) = (() :: Constraint)
type Assume (x :: k) (assume :: k') = (() :: Constraint)
type Assumptions (assumptions :: k') = (() :: Constraint)

type Define (d :: k) = (() :: Constraint)
type Defines (d :: k) = (() :: Constraint)
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

type Result (r :: k) (x :: k') = (() :: Constraint)
type Results (rs :: k) = (() :: Constraint)

type Errors (es :: k) = (() :: Constraint)
type Error (e :: k) (x :: k') = (() :: Constraint)

type Class (c :: k) = (() :: Constraint)

type Constructor (nm :: Symbol) (c :: k) = (() :: Constraint)
type Method (nm :: Symbol) (m :: k) = (() :: Constraint)
type Function (nm :: Symbol) (f :: k) = (() :: Constraint)
type Field (nm :: Symbol) (f :: k) = (() :: Constraint)
type Selector (nm :: Symbol) (s :: k) = (() :: Constraint)
type Pattern (nm :: Symbol) (p :: k) = (() :: Constraint)

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

-- unindent is from the great interpolate package by Simon Hengel.
--
-- Originally licensed as MIT, sublicensing as BSD3.
--
-- Original License:
--
{-
Copyright (c) 2013-2015 Simon Hengel <sol@typeful.net>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
unindent :: String -> String
unindent = concat . removeIndentation . trimLastLine . removeLeadingEmptyLine . lines_
  where
    isEmptyLine = all isSpace

    lines_ s
      | null s = []
      | otherwise =
          case span (/= '\n') s of
            (first,'\n':rest) -> (first ++ "\n") : lines_ rest
            (first,rest)      -> first : lines_ rest

    removeLeadingEmptyLine xs =
      case xs of
        y:ys | isEmptyLine y -> ys
        _                    -> xs

    trimLastLine [] = []
    trimLastLine (a : b : r) = a : trimLastLine (b : r)
    trimLastLine [a]
      | all (== ' ') a = []
      | otherwise = [a]

    removeIndentation ys = map (dropSpaces indentation) ys
      where
        dropSpaces 0 s = s
        dropSpaces n s =
          case s of
            (' ':r) -> dropSpaces (n - 1) r
            _       -> s

        indentation = minimalIndentation ys

        minimalIndentation =
            safeMinimum 0
          . map (length . takeWhile (== ' '))
          . removeEmptyLines

        removeEmptyLines = filter (not . isEmptyLine)

        safeMinimum x [] = x
        safeMinimum _ xs = minimum xs

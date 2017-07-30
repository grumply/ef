module Ef.Type.Comments where

import GHC.TypeLits
import Data.Kind

-- An expiriment in type-level comments. Benefits of this approach are seen in
-- an environment with type inspection or when retrieving type information in
-- ghci. These comments are trivially removed at compile time.

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
type (:::) (a :: k) (comment :: Symbol) = a
infixl 1 :::

-- | Type-level constraint annoation. Reduces to a unital constraint.
--
-- > double :: Comment "Double the given integer." => Int -> Int
--
-- These constraint comments are only visible when retrieving type information.
-- In environments with support for function type inspection, like ghci, these
-- comments are visible.
type Comment (comment :: Symbol) = (() :: Constraint)

-- | Type-level constraint annoation. Reduces to a unital constraint.
--
-- > adminLogin :: ( credentials ~ [UserPrivilege]
-- >               , Param credentials "Must contain AdminPrivilege"
-- >               , adminSession ~ AdminSession
-- >               , Param adminSession "Admin sessions expire after 30 minutes"
-- >               )
-- >            => credentials
-- >            -> IO (Maybe adminSession)
--
-- As with `Comment`, these variable constraint comments are only visible when
-- retrieving type information. In environments with support for function type
-- inspection, like ghci, these comments are visible.
type Param (a :: k) (comment :: Symbol) = Comment comment

type Result (comment :: Symbol) = Comment comment

type Constructor (comment :: Symbol) = Comment comment

type Function (comment :: Symbol) = Comment comment

type Field (comment :: Symbol) = Comment comment

type Selector (comment :: Symbol) = Comment comment

type TODO (comment :: Symbol) = Comment comment

type Note (comment :: Symbol) = Comment comment

type Permits (comment :: Symbol) = Comment comment

type Forbids (comment :: Symbol) = Comment comment

type Allows (comment :: Symbol) = Comment comment

type Disallows (comment :: Symbol) = Comment comment

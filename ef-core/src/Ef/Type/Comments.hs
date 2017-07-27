module Ef.Type.Comments where

import GHC.TypeLits
import Data.Kind

-- An expiriment in type-level comments. Benefits of this approach are seen in
-- an environment with type inspection or when retrieving type information in
-- ghci. These comments are trivially removed at compile time.

-- | Type-level inline variable comment.
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
type (:::) a (comment :: Symbol) = a

-- | Type-level comment constraint. Reduces to a unital constraint.
--
-- > double :: Comment "Double the given integer." => Int -> Int
--
-- These constraint comments are only visible when retrieving type information.
-- In environments with support for function type inspection, like ghci, these
-- comments are visible.
type Comment (comment :: Symbol) = (() :: Constraint)

-- | Type-level variable comment constraint. Reduces to a unital constraint.
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
type Param a (comment :: Symbol) = (() :: Constraint)

type Constructor (comment :: Symbol) = (() :: Constraint)

type Function (comment :: Symbol) = (() :: Constraint)

type Field (comment :: Symbol) = (() :: Constraint)

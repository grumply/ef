{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Ef.Set where


-- | De-duplicating append as a type family; `Union` and `∪` uses this.
-- Note: consider removing `∪` since it just duplicates this functionality.
type family (:+:) a xs
  where

    a :+: '[] =
        '[a]

    a :+: (a ': xs) =
        (a ': xs)

    a :+: (x ': xs) =
        x ': a :+: xs



-- | Synonym for `∪`
type Union xs ys = xs ∪ ys



-- | Union two sets-as-lists. Used in a type-level fashion: capabilities are represented
-- as lists of higher-kinded types whose elements are guaranteed to be unique and this
-- type family will permit the representation of two sets of capabilities unioned with
-- deduplication.
type family (∪) {- emacs: c-x 8 <enter> 222a <enter> -} xs ys
  where

    '[] ∪ ys =
        ys

    (x ': xs) ∪ ys =
        x :+: (xs ∪ ys)



-- | In is a type family that determines membership of an element in a list of elements.
type family In (x :: k) (xs :: [k]) :: Bool
  where

    In x '[] =
        'False

    In x (x ': xs) =
        'True

    In x (y ': xs) =
        In x xs



-- -- | Subset is a class that guarantees that one list is a subset of another.
-- -- Note: check uses of this as it may be extraneous since we use Can/Has.
-- class Subset xs ys



-- instance Subset '[] ys



-- instance ( In x ys ~ 'True
--          , Subset xs ys
--          )
--     => Subset (x ': xs) ys



-- | A type family that determines if two types are /not/ equal.
type family (/==) (x :: k) (y :: k) :: Bool
  where

    (/==) x x =
        'False

    (/==) x y =
        'True


-- | A class that specifies that an element does not appear in a list.
class Denies (x :: k) (ys :: [k])



instance Denies x '[]



instance ( (/==) x y ~ 'True
         , Denies x ys
         )
    => Denies x (y ': ys)

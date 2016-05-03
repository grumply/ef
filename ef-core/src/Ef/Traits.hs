{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
module Ef.Traits
    ( Traits(..)
    , Has'(..)
    , Has(..)
    , stretch
    ) where



import Ef.Type.Set
import Ef.Type.Nat

import Data.Typeable
import Control.DeepSeq


-- | Traits are the abstract components of 'Object's. A Traits construction
-- consists of zero or more `Trait`s in a de-duplicated singly-linked. There
-- are negative performance implications to this approach, but it represents
-- exactly what we're looking for in a set of attributes. Refer to the
-- documentation for a discussion of the performance implications of this
-- approach.
data Traits (traits :: [* -> *]) x
  where

    Empty
        :: Traits '[] x

    Trait
        :: trait x
        -> Traits traits x
        -> Traits (trait ': traits) x

instance NFData (Traits '[] x) where
    rnf _ = ()

instance (NFData (trait x), NFData (Traits traits x))=> NFData (Traits (trait ': traits) x) where
    rnf (Trait trait traits) = rnf trait `seq` rnf traits


instance Eq (Traits '[] x) where
    _ == _ = True



instance (Eq (context x),Eq (Traits contexts x))
        => Eq (Traits (context ': contexts) x)
    where

        (Trait trait0 contexts0) == (Trait trait1 contexts1) =
            trait0 == trait1 && contexts0 == contexts1



instance Ord (Traits '[] x) where
    _ <= _ = True



instance (Ord (context x),Ord (Traits contexts x))
        => Ord (Traits (context ': contexts) x)
    where

        (Trait trait0 contexts0) <= (Trait trait1 contexts1) =
            trait0 <= trait1 && contexts0 <= contexts1



-- instance Binary (Traits '[] x)
--   where

--     get =
--        pure Empty



--     put _ =
--        pure ()



-- instance ( Binary (Traits traits x)
--          , Denies trait traits
--          , Binary (trait x)
--          )
--     => Binary (Traits (trait ': traits) x)
--   where

--     get =
--         Trait <$> get <*> get



--     put (Trait x as) =
--         put x >> put as



instance Functor (Traits '[])
  where

    fmap _ _ =
        Empty



instance ( Functor trait
         , Functor (Traits traits)
         )
    => Functor (Traits (trait ': traits))
  where

    fmap f (Trait trait traits) =
        Trait
            (fmap f trait)
            (fmap f traits)



instance {-# OVERLAPPING #-} Show (Traits '[] traits)
    where

        show _ = ""



instance {-# OVERLAPS #-} 
         ( traits ~ (trait ': traits')
         , Typeable (trait ())
         , Show (trait m)
         , Show (Traits traits' m)
         )
         => Show (Traits traits m)
    where

        show (Trait trait traits) =
            let
                traitType =
                    typeOf (undefined :: trait ())

                traitString =
                    reverse . (" :" ++) . drop 3 . reverse . show $ traitType
            in
               case traits of

                   Empty ->
                       traitString ++ show trait

                   _ ->
                       traitString ++ show trait ++ ", " ++ show traits



-- | Has is a class representing (1) the ability to push a trait into a set of
-- traits and (2) the ability to pull a trait out of a set of traits. If an
-- object witnesses a trait, these exist automatically. There does appear to be
-- some room here for dynamic extension of object capabilities, like pulling
-- components out of an object and constructing a desired component that doesn't
-- exist naturally inside that object, but I have yet to explore that.
class Has (trait :: * -> *) (traits :: [* -> *])
  where

    push
        :: trait x
        -> Traits traits x
        -> Traits traits x



    pull
        :: Traits traits x
        -> trait x



instance ( index ~ Offset trait traits
         , Has' trait traits index
         )
    => Has trait traits
  where

    push trait =
        let
          index =
              Index :: Index (Offset trait traits)

        in
          push' index trait



    pull traits =
        let
          index =
              Index :: Index (Offset trait traits)

        in
          pull' index traits


-- | stretch is a 'pull' followed by a transformation by way of @f@ followed by a 'push'.
stretch
    :: ( index ~ Offset trait traits
       , Has' trait traits index
       )
    => (forall z. trait z -> trait z)
    -> Traits traits x
    -> Traits traits x
stretch f traits =
    let
      trait =
          pull traits

    in
      push (f trait) traits



class Has' (trait :: * -> *) (traits :: [* -> *]) (n :: Nat)
  where

    push'
        :: Index n
        -> trait x
        -> Traits traits x
        -> Traits traits x



    pull'
        :: Index n
        -> Traits traits x
        -> trait x



instance traits ~ (trait ': xs)
    => Has' trait traits 'Z
  where

    push' _ trait (Trait _ traits) =
        Trait trait traits



    pull' _ (Trait trait _) =
        trait



instance ( index ~ Offset trait traits
         , Has' trait traits index
         )
    => Has' trait (trait' ': traits) ('S n)
  where

    push' _ trait (Trait trait' traits) =
        let
          index =
              Index :: Index (Offset trait traits)

        in
          Trait trait' (push' index trait traits)



    pull' _ (Trait _ traits) =
        let
          index =
              Index :: Index (Offset trait traits)

        in
          pull' index traits


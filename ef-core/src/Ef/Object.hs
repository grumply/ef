{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Ef.Object
    ( Trait
    , Method
    , Subclass
    , Superclass
    , type (.>)
    , type (<.)
    , Has(..)
    , stretch
    , Traits(..)
    , Object(..)
    , (*:*)
    , singleton
    , trait
    ) where

import Ef.Type.Set
import Ef.Traits

import Ef.Type.Nat
import Data.Coerce

type Trait trait traits super =
    trait (Method traits super)

type Method traits super =
    Object traits super -> super (Object traits super)


type Subclass s t = (<.) s t
type family (<.) (traits :: [* -> *]) traits' where

    (<.) (trait ': '[]) traits' =
        (Has' trait traits' (Offset trait traits'))

    (<.) (trait ': traits) traits' =
        ( Has' trait traits' (Offset trait traits')
        , traits <. traits'
        )



type Superclass t s = (.>) t s
type family (.>) traits traits' where

    (.>) traits traits' =
        traits' <. traits



newtype Object traits super =
      Object
          {
            deconstruct
                :: Traits traits (Object traits super -> super (Object traits super))
          }

instance (Eq (Traits traits (Object traits super -> super (Object traits super))))
        => Eq (Object traits super)
    where

        (Object o1) == (Object o2) =
            o1 == o2


instance (Ord (Traits traits (Object traits super -> super (Object traits super))))
        => Ord (Object traits super)
     where

         (Object o1) <= (Object o2) =
             o1 <= o2



instance Show (Traits traits (Object traits super -> super (Object traits super)))
         => Show (Object traits super)
    where

        show (Object traits) =
            "{ " ++ show traits ++ " }"



infixr 6 *:*

(*:*)
    :: Denies trait traits
    => trait a
    -> Traits traits a
    -> Traits (trait ': traits) a

(*:*) = Trait

singleton :: Trait trait '[trait] super -> Object '[trait] super
singleton t = coerce $ t *:* Empty

trait :: (Has trait traits, Monad super, Functor f)
      => (Trait trait traits super -> f (Trait trait traits super))
      -> Object traits super
      -> f (Object traits super)
trait f obj = fmap (\t -> coerce $ push t (coerce obj)) (f (pull $ coerce obj))

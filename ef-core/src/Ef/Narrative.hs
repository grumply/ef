{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Ef.Narrative
     ( Narrative(..)
     , type (<:)
     , type (:>)
     , self
     , super
     , transform
     , Supertype
     , Subtype
     , Lift(..)
     , Can(..)
     , Upcast(..)
     ) where



import Ef.Messages
import Ef.Type.Nat

import Control.Applicative
import Control.Exception (Exception(..),SomeException)
import Control.Exception.Base (PatternMatchFail(..))
import Control.Monad

import GHC.Generics


data Narrative self super result
    = forall intermediate.
      Say (Messages self intermediate)
          (    intermediate
             -> Narrative self super result
          )

    | Super (super (Narrative self super result))

    | Return result

    | Fail SomeException


instance ( Upcast (Messages small) (Messages large)
         , Functor super
         )
    => Upcast (Narrative small super) (Narrative large super)
    where

        upcast (Fail e) =
            Fail e

        upcast (Return result) =
            Return result

        upcast (Super sup) =
            Super (fmap upcast sup)

        upcast (Say message k) =
            Say (upcast message) (upcast . k)



class Functor m'
    => Lift m m'
  where

    lift :: m a -> m' a



instance Functor m
    => Lift m m
  where

    lift =
        id



instance Functor super
    => Lift super (Narrative self super)
  where

    lift =
        super


instance Lift newSuper super
    => Lift newSuper (Narrative self super)
  where

    lift =
        super . lift


super
    :: Functor super
    => super result
    -> Narrative self super result

super m =
    Super (fmap Return m)


self
    :: (Monad super, '[message] <: self)
    => message result -> Narrative self super result
self message = Say (inj message) return

{-# RULES
  "self/bind" forall m k. self m >>= k = Say (inj m) k
  #-}


type Subtype s t = (<:) s t
-- bounded parametric subtype polymorphism implemented as statically guaranteed set containment.
-- Given S <: T, guarantee that for all elements t of T, there is an element s of S where t is equal to s.
type family (:>) messages (messages' :: [* -> *]) where

    messages :> (message ': '[]) =
      (Can' message messages (Offset message messages))

    messages :> (message ': messages') =
      ( Can' message messages (Offset message messages)
      , messages :> messages'
      )



type Supertype t s = (:>) t s
-- bounded parametric supertype polymorphism implemented as statically guaranteed set containment.
-- Given T :> S, guarantee that for all elements t of T, there is an element s of S where t is equal to s.
type family (<:) (messages' :: [* -> *]) messages where

    (message ': '[]) <: messages =
        (Can' message messages (Offset message messages))

    (message ': messages') <: messages =
        ( Can' message messages (Offset message messages)
        , messages' <: messages 
        )



instance Functor super
    => Functor (Narrative self super)
  where

    fmap =
        _fmap



instance Monad super
    => Applicative (Narrative self super)
  where

    pure =
        return



    (<*>) =
        ap



    (*>) =
        (>>)



instance Monad super
    => Monad (Narrative self super)
  where
    return =
        Return


    (>>=) =
        _bind



    fail =
        Fail . toException . PatternMatchFail


{-# NOINLINE [1] _fmap #-}
_fmap
    :: Functor super
    => (a -> b)
    -> Narrative self super a
    -> Narrative self super b

_fmap f =
    go
  where

    go (Fail e) =
        Fail e

    go (Return a) =
        Return (f a)

    go (Super m) =
        Super (fmap go m)

    go (Say message k) =
        Say message (go . k)


{-# NOINLINE [1] _bind #-}
_bind
    :: Monad super
    => Narrative self super intermediate
    -> (intermediate -> Narrative self super result)
    -> Narrative self super result

p0 `_bind` f =
    go p0
  where

    go (Fail e) =
        Fail e

    go (Say message k) =
        Say message (go . k)

    go (Return res) =
        f res

    go (Super m) =
        Super (fmap go m)



{-# RULES

    "_bind (Fail e) f"
        forall e f .
            _bind (Fail e) f =
                Fail e
    ;

    "_bind (Say message k) f"
        forall message k f .
            _bind (Say message k) f =
                Say message (\a -> _bind (k a) f)
    ;

    "_bind (Super m) f"
        forall m f .
            _bind (Super m) f =
                Super (m >>= \p -> return (_bind p f))
    ;

    "_bind (Return result) f"
        forall result f .
            _bind (Return result) f =
                f result
    ;

  #-}



instance MonadPlus super
    => Alternative (Narrative self super)
  where

    empty =
        mzero



    (<|>) =
        mplus



instance MonadPlus super
    => MonadPlus (Narrative self super)
  where

    mzero =
        super mzero



    mplus =
        _mplus

{-# NOINLINE [1] _mplus #-}
_mplus
    :: MonadPlus super
    => Narrative self super result
    -> Narrative self super result
    -> Narrative self super result

_mplus p0 p1 =
    go p0
  where

    go (Super m) =
        Super (fmap go m)

    go (Say message k) =
        Say message (go . k)

    go (Fail _) =
        p1

    go result =
        result



instance ( Monad super
         , Monoid result
         )
    => Monoid (Narrative self super result)
  where

    mempty =
        pure mempty



    mappend =
        _mappend

{-# NOINLINE [1] _mappend #-}
_mappend
    :: ( Monad super
       , Monoid result
       )
    => Narrative self super result
    -> Narrative self super result
    -> Narrative self super result

_mappend p0 p1 =
    go p0
  where

    go (Fail e) =
        Fail e

    go (Return result) =
        fmap (mappend result) p1

    go (Super m) =
        Super (fmap go m)

    go (Say message k) =
        Say message (go . k)



-- This approach to transformations tries to maintain that
--     lift (x >>= y) = lift x >>= (lift . y)
-- as well as
--     throw e >> _ = throw e
{-# INLINE transform #-}
transform
    :: Monad super
    => (forall x. Messages self x -> (x -> Narrative self super result) -> Narrative self super result)
    -> Narrative self super result
    -> Narrative self super result

transform = _transform


{-# NOINLINE [1] _transform #-}
_transform
    :: Monad super
    => (forall x. Messages self x -> (x -> Narrative self super result) -> Narrative self super result)
    -> Narrative self super result
    -> Narrative self super result

_transform t =
    go
    where

        go (Say message k) =
           t message k

        go (Super m) =
           Super (m >>= return . go)

        go (Fail e) =
            Fail e

        go (Return r) =
            Return r

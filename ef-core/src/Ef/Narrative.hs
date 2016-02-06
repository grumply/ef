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
{-# LANGUAGE Trustworthy #-}
module Ef.Narrative
     ( Narrative(..)
     , Subtype
     , Invoke
     , Invokes
     , self
     , super
     , transform
     , Lift(..)
     , Can(..)
     , Upcast(..)
     ) where



import Ef.Messages

import Control.Applicative
import Control.Exception (Exception(..),SomeException)
import Control.Exception.Base (PatternMatchFail(..))
import Control.Monad

import GHC.Exts (Constraint)



-- | Narrative is a monad for composing method invocations.
-- This is a transition from the standard OOP design to MOP, or message-oriented
-- programming. Development can still be done via the standard object-style
-- dependency-oriented approach, but I believe development by way of a Narrative
-- leads to cleaner, naturally arising, object hierarchies, as well as easy
-- prototyping and testing as well as more dynamic approaches to development,
-- including DSL-, or language-, oriented design. Examples of various approaches
-- and their implications can be found in the project documentation (todo).
data Narrative self super result
  where

    Say
        :: Messages self intermediate
        -> (    intermediate
             -> Narrative self super result
           )
        -> Narrative self super result

    Super
        :: super (Narrative self super result)
        -> Narrative self super result

    Return
        :: result
        -> Narrative self super result

    Fail
        :: SomeException
        -> Narrative self super result



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



-- -- | don't use this, just rewrite your Object constructors for consistency
-- rearrangeMessages
--     :: ( Upcast (Messages small) (Messages large)
--        , Functor super
--        )
--     => Narrative small super result
--     -> Narrative large super result

-- rearrangeMessages =
--     upcast



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
    :: message result
    -> Invoke message self super result

self message =
    Say (inj message) return



type Invoke message self super result =
    ( Subtype '[message] self
    , Monad super
    )
    => Narrative self super result



type Invokes messages self super result =
    ( Subtype messages self
    , Monad super
    )
    => Narrative self super result



type family Subtype (messages :: [* -> *]) messages' :: Constraint where

    Subtype (message ': '[]) messages' =
        (Can message messages')

    Subtype (message ': messages) messages' =
        ( Can message messages'
        , Subtype messages messages' 
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



{-# NOINLINE _fmap #-}
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



{-# RULES

    "_fmap f (Fail e)"
        forall f e.
            _fmap f (Fail e) =
                Fail e
    ;

    "_fmap f (Say message k)"
        forall message k f.
            _fmap f (Say message k) =
                Say message (_fmap f . k)
    ;

    "_fmap f (Super m)"
        forall f m.
            _fmap f (Super m) =
                let
                  continue =
                      _fmap f

                in
                  Super (fmap continue m)
    ;

    "_fmap f (Return result)"
        forall f result.
            _fmap f (Return result) =
                Return (f result)
    ;

  #-}



{-# NOINLINE _bind #-}
_bind
    :: Functor super
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
                Say message (flip _bind f . k)
    ;

    "_bind (Super m) f"
        forall m f .
            _bind (Super m) f =
                let
                  continue =
                      flip _bind f

                in
                  Super (fmap continue m)
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
    :: Functor super
    => (forall x. Messages self x -> (x -> Narrative self super result) -> Narrative self super result)
    -> Narrative self super result
    -> Narrative self super result

transform f = transform_ (Transform f)



{-# NOINLINE transform_ #-}
transform_
    :: Functor super
    => Transform self super result
    -> Narrative self super result
    -> Narrative self super result

transform_ (Transform t) =
    go
    where

        go (Say message k) =
           t message k

        go (Super sup) =
            Super (fmap go sup)

        go (Fail e) =
            Fail e

        go (Return r) =
            Return r



{-# INLINE applyTransform #-}
applyTransform
    :: Transform self super result
    -> Messages self x
    -> (x -> Narrative self super result)
    -> Narrative self super result

applyTransform (Transform t) = t

data Transform (self :: [* -> *]) (super :: * -> *) (result :: *)
    where

        Transform
            :: (forall x. Messages self x -> (x -> Narrative self super result) -> Narrative self super result)
            -> Transform self super result

{-# RULES

    "transform_ t (Fail e) == Fail e"
        forall t e.
            transform_ t (Fail e) =
                Fail e
    ;

    "transform_ t (Super sup) == Super (fmap (transform_ t) sup)"
        forall t sup.
            transform_ t (Super sup) =
                Super (fmap (transform_ t) sup)
    ;

    "transform_ t (Return r) == Return r"
        forall t r.
            transform_ t (Return r) =
                Return r
     ;

    "transform_ t (Say message k)"
        forall t message k.
            transform_ t (Say message k) =
                applyTransform t message k
    ;

  #-}


-- Let's move these to an external package; they are narrative transformations for which
-- there shouldn't be default implementations as arbitrary cutoffs can leave objects in
-- invalid states.

-- -- | cutoffSteps limits the number of Step constructors in a 'Narrative'. To limit
-- -- the number of (Step constructors + M constructors), use 'cutoff'.
-- --
-- -- >>> import Ef.Core
-- -- >>> import Effect.State
-- -- >>> newtype St = St Int
-- -- >>> :{
-- --  do
-- --    let
-- --      inc (St n) =
-- --          St (n + 1)
-- --
-- --      newStore =
-- --          store (St 0)
-- --
-- --      obj =
-- --          Object (newStore *:* Empty)
-- --
-- --      test =
-- --          replicateM_ 5 (modify inc)
-- --
-- --    result0 <- delta obj (cutoffSteps 3 test)
-- --    let
-- --      (o,_) =
-- --          result0
-- --
-- --    result1 <- delta o get
-- --    let
-- --      (_,St i) =
-- --          result1
-- --
-- --    print i
-- -- :}
-- --3



-- cutoffSteps
--     :: Monad super
--     => Integer
--     -> Narrative self super result
--     -> Narrative self super (Maybe result)

-- cutoffSteps _ (Fail e) =
--     Fail e

-- cutoffSteps ((<= 0) -> True) _ =
--     return Nothing

-- cutoffSteps _ (Return result) =
--     Return (Just result)

-- cutoffSteps stepsRemaining (Super m) =
--     let
--       newCutoff =
--           cutoffSteps (stepsRemaining - 1)

--     in
--       Super (fmap newCutoff m)

-- cutoffSteps stepsRemaining (Say message k) =
--     let
--       newCutoff =
--           cutoffSteps (stepsRemaining - 1)

--     in
--       Say message (newCutoff . k)



-- cutoff
--     :: Monad super
--     => Integer
--     -> Narrative self super result
--     -> Narrative self super (Maybe result)

-- cutoff _ (Fail e) =
--     Fail e

-- cutoff ((<= 0) -> True) _ =
--     return Nothing

-- cutoff _ (Return result) =
--     Return (Just result)

-- cutoff stepsRemaining (Super m) =
--     let
--       newCutoff =
--           cutoff (stepsRemaining - 1)

--     in
--       Super (fmap newCutoff m)

-- cutoff stepsRemaining (Say message k) =
--     let
--       newCutoff =
--           cutoff (stepsRemaining - 1)

--     in
--       Say message (newCutoff . k)

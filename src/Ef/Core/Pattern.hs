{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE IncoherentInstances #-}
module Ef.Core.Pattern where



import Ef.Core.Pattern.Symbols

import Control.Applicative
import Control.Exception (Exception(..),SomeException)
import Control.Monad



data MonadFailString
  where

    MonadFailString
        :: String
        -> MonadFailString
  deriving Show



instance Exception MonadFailString



data Pattern scope parent result
  where

    Send
        :: Symbol scope intermediate
        -> (    intermediate
             -> Pattern scope parent result
           )
        -> Pattern scope parent result

    Super
        :: parent (Pattern scope parent result)
        -> Pattern scope parent result

    Pure
        :: result
        -> Pattern scope parent result

    Fail
        :: SomeException
        -> Pattern scope parent result



rearrange
    :: ( Functor parent
       , As (Symbol scope) (Symbol scope')
       )
    => Pattern scope parent a
    -> Pattern scope' parent a

rearrange (Fail e) =
    Fail e

rearrange (Pure r) =
    Pure r

rearrange (Super m) =
    Super (fmap rearrange m)

rearrange (Send symbol k) =
    Send (conv symbol) (rearrange . k)



upcast
    :: ( Functor parent
       , Cast scopeSmall scopeLarge
       )
    => Pattern scopeSmall parent result
    -> Pattern scopeLarge parent result

upcast (Fail e) =
    Fail e

upcast (Pure result) =
    Pure result

upcast (Super p) =
    Super (fmap upcast p)

upcast (Send symbol k) =
    Send (cast symbol) (upcast . k)



-- | As is an internal convenience utility used to reorder
--   a set of symbols.
class x `As` y
  where

    conv
        :: x a
        -> y a



instance x `As` x
  where

    conv =
        id



instance (Symbol '[]) `As` (Symbol '[])



instance ( Allows x ys
         , (Symbol xs) `As` (Symbol ys)
         )
    => (Symbol (x ': xs)) `As` (Symbol ys)
  where

    conv (Symbol sa) =
        inj sa

    conv (Further ss) =
        conv ss



class Functor m'
    => Lift m m'
  where

    lift
        :: m a
        -> m' a



instance Functor m
    => Lift m m
  where

    lift =
        id



instance Functor parent
    => Lift parent (Pattern scope parent)
  where

    lift =
        lift_



instance Lift newParent parent
    => Lift newParent (Pattern scope parent)
  where

    lift =
        lift_ . lift



lift_
    :: Functor parent
    => parent result
    -> Pattern scope parent result 

lift_ m =
    Super (fmap Pure m)



super
    :: Functor parent
    => parent result
    -> Pattern scope parent result

super =
    lift_



self
    :: Is symbol scope parent
    => symbol result
    -> Pattern scope parent result

self symbol =
    Send (inj symbol) return



instance Functor parent
    => Functor (Pattern scope parent)
  where

    fmap =
        _fmap



instance Monad parent
    => Applicative (Pattern scope parent)
  where

    pure =
        return



    (<*>) =
        ap



    (*>) =
        (>>)



instance Monad parent
    => Monad (Pattern scope parent)
  where

#ifdef TRANSFORMERS_SAFE
    return =
        Super . return . Pure
#else
    return =
        Pure
#endif



    (>>=) =
        _bind



    fail =
        Fail . toException . MonadFailString



{-# NOINLINE _fmap #-}
_fmap
    :: Functor parent
    => (a -> b)
    -> Pattern scope parent a
    -> Pattern scope parent b

_fmap f =
    go
  where

    go (Fail e) =
        Fail e

    go (Pure a) =
        Pure (f a)

    go (Super m) =
        Super (fmap go m)

    go (Send symbol k) =
        Send symbol (go . k)



{-# RULES

    "_fmap f (Fail e)"
        forall f e.
            _fmap f (Fail e) =
                Fail e
    ;

    "_fmap f (Send symbol k)"
        forall symbol k f.
            _fmap f (Send symbol k) =
                Send symbol (_fmap f . k)
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

    "_fmap f (Pure result)"
        forall f result.
            _fmap f (Pure result) =
                Pure (f result)
    ;

  #-}



{-# NOINLINE _bind #-}
_bind
    :: Functor parent
    => Pattern scope parent intermediate
    -> (intermediate-> Pattern scope parent result)
    -> Pattern scope parent result

p0 `_bind` f =
    go p0
  where

    go (Fail e) =
        Fail e

    go (Send symbol k) =
        Send symbol (go . k)

    go (Pure res) =
        f res

    go (Super m) =
        Super (fmap go m)



{-# RULES

    "_bind (Fail e) f"
        forall e f .
            _bind (Fail e) f =
                Fail e
    ;

    "_bind (Send symbol k) f"
        forall symbol k f .
            _bind (Send symbol k) f =
                Send symbol (flip _bind f . k)
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

    "_bind (Pure result) f"
        forall result f .
            _bind (Pure result) f =
                f result
    ;

  #-}



instance MonadPlus parent
    => Alternative (Pattern scope parent)
  where

    empty =
        mzero



    (<|>) =
        mplus



instance MonadPlus parent
    => MonadPlus (Pattern scope parent)
  where

    mzero =
        lift_ mzero



    mplus =
        _mplus



_mplus
    :: MonadPlus parent
    => Pattern scope parent result
    -> Pattern scope parent result
    -> Pattern scope parent result

_mplus p0 p1 =
    go p0
  where

    go (Super m) =
        Super (fmap go m `mplus` return p1)

    go (Send symbol k) =
        Send symbol (go . k)

    go x =
        x



instance ( Monad parent
         , Monoid result
         )
    => Monoid (Pattern scope parent result)
  where

    mempty =
        pure mempty



    mappend =
        _mappend



_mappend
    :: ( Monad parent
       , Monoid result
       )
    => Pattern scope parent result
    -> Pattern scope parent result
    -> Pattern scope parent result

_mappend p0 p1 =
    go p0
  where

    go (Fail e) =
        Fail e

    go (Pure result) =
        fmap (mappend result) p1

    go (Super m) =
        Super (fmap go m)

    go (Send symbol k) =
        Send symbol (go . k)




-- | cutoffSteps limits the number of Step constructors in a 'Pattern'. To limit
-- the number of (Step constructors + M constructors), use 'cutoff'.
--
-- >>> import Ef.Core
-- >>> import Effect.State
-- >>> newtype St = St Int
-- >>> :{
--  do
--    let
--      inc (St n) =
--          St (n + 1)
--
--      newStore =
--          store (St 0)
--
--      obj =
--          Object (newStore *:* Empty)
--
--      test =
--          replicateM_ 5 (modify inc)
--
--    result0 <- delta obj (cutoffSteps 3 test)
--    let
--      (o,_) =
--          result0
--
--    result1 <- delta o get
--    let
--      (_,St i) =
--          result1
--
--    print i
-- :}
--3



cutoffSteps
    :: Monad parent
    => Integer
    -> Pattern scope parent result
    -> Pattern scope parent (Maybe result)

cutoffSteps _ (Fail e) =
    Fail e

cutoffSteps ((<= 0) -> True) _ =
    return Nothing

cutoffSteps _ (Pure result) =
    Pure (Just result)

cutoffSteps stepsRemaining (Super m) =
    let
      newCutoff =
          cutoffSteps (stepsRemaining - 1)

    in
      Super (fmap newCutoff m)

cutoffSteps stepsRemaining (Send symbol k) =
    let
      newCutoff =
          cutoffSteps (stepsRemaining - 1)

    in
      Send symbol (newCutoff . k)



cutoff
    :: Monad parent
    => Integer
    -> Pattern scope parent result
    -> Pattern scope parent (Maybe result)

cutoff _ (Fail e) =
    Fail e

cutoff ((<= 0) -> True) _ =
    return Nothing

cutoff _ (Pure result) =
    Pure (Just result)

cutoff stepsRemaining (Super m) =
    let
      newCutoff =
          cutoff (stepsRemaining - 1)

    in
      Super (fmap newCutoff m)

cutoff stepsRemaining (Send symbol k) =
    let
      newCutoff =
          cutoff (stepsRemaining - 1)

    in
      Send symbol (newCutoff . k)

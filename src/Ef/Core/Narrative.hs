{-# LANGUAGE RankNTypes #-}
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
module Ef.Core.Narrative where


import Ef.Core.Type.Nat
import Ef.Core.Narrative.Lexeme

import Control.Applicative
import Control.Exception (Exception(..),SomeException)
import Control.Exception.Base (PatternMatchFail(..))
import Control.Monad



data Sentence lexicon environment result
  where

    Say
        :: Word lexicon intermediate
        -> (    intermediate
             -> Sentence lexicon environment result
           )
        -> Sentence lexicon environment result

    Super
        :: environment (Sentence lexicon environment result)
        -> Sentence lexicon environment result

    Pure
        :: result
        -> Sentence lexicon environment result

    -- for convenience
    Fail
        :: SomeException
        -> Sentence lexicon environment result



rearrange
    :: ( Functor parent
       , As (Lexemes scope) (Lexemes scope')
       )
    => Sentence scope parent a
    -> Sentence scope' parent a

rearrange (Fail e) =
    Fail e

rearrange (Pure r) =
    Pure r

rearrange (Super m) =
    Super (fmap rearrange m)

rearrange (Say symbol k) =
    Say (conv symbol) (rearrange . k)



upembed
    :: ( Functor parent
       , Embed scopeSmall scopeLarge
       )
    => Sentence scopeSmall parent result
    -> Sentence scopeLarge parent result

upembed (Fail e) =
    Fail e

upembed (Pure result) =
    Pure result

upembed (Super p) =
    Super (fmap upembed p)

upembed (Say symbol k) =
    Say (embed symbol) (upembed . k)



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



instance (Lexemes '[]) `As` (Lexemes '[])



instance ( Allows x ys
         , (Lexemes xs) `As` (Lexemes ys)
         )
    => (Lexemes (x ': xs)) `As` (Lexemes ys)
  where

    conv (Lexeme sa) =
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
    => Lift parent (Sentence scope parent)
  where

    lift =
        lift_



instance Lift newParent parent
    => Lift newParent (Sentence scope parent)
  where

    lift =
        lift_ . lift



lift_
    :: Functor parent
    => parent result
    -> Sentence scope parent result

lift_ m =
    Super (fmap Pure m)



super
    :: Functor parent
    => parent result
    -> Sentence scope parent result

super =
    lift_



say
    :: Can lexeme lexicon environment
    => lexeme result
    -> Sentence lexicon environment result

say symbol =
    Say (inj symbol) return



type Method method scope parent result =
    (Monad parent
    , Allows' method scope (IndexOf method scope))
    => Sentence scope parent result


instance Functor parent
    => Functor (Sentence scope parent)
  where

    fmap =
        _fmap



instance Monad parent
    => Applicative (Sentence scope parent)
  where

    pure =
        return



    (<*>) =
        ap



    (*>) =
        (>>)



instance Monad parent
    => Monad (Sentence scope parent)
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
        Fail . toException . PatternMatchFailure



{-# NOINLINE _fmap #-}
_fmap
    :: Functor parent
    => (a -> b)
    -> Sentence scope parent a
    -> Sentence scope parent b

_fmap f =
    go
  where

    go (Fail e) =
        Fail e

    go (Pure a) =
        Pure (f a)

    go (Super m) =
        Super (fmap go m)

    go (Say symbol k) =
        Say symbol (go . k)



{-# RULES

    "_fmap f (Fail e)"
        forall f e.
            _fmap f (Fail e) =
                Fail e
    ;

    "_fmap f (Say symbol k)"
        forall symbol k f.
            _fmap f (Say symbol k) =
                Say symbol (_fmap f . k)
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
    => Sentence scope parent intermediate
    -> (intermediate -> Sentence scope parent result)
    -> Sentence scope parent result

p0 `_bind` f =
    go p0
  where

    go (Fail e) =
        Fail e

    go (Say symbol k) =
        Say symbol (go . k)

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

    "_bind (Say symbol k) f"
        forall symbol k f .
            _bind (Say symbol k) f =
                Say symbol (flip _bind f . k)
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
    => Alternative (Sentence scope parent)
  where

    empty =
        mzero



    (<|>) =
        mplus



instance MonadPlus parent
    => MonadPlus (Sentence scope parent)
  where

    mzero =
        lift_ mzero



    mplus =
        _mplus



_mplus
    :: MonadPlus parent
    => Sentence scope parent result
    -> Sentence scope parent result
    -> Sentence scope parent result

_mplus p0 p1 =
    go p0
  where

    go (Super m) =
        Super (fmap go m)

    go (Say symbol k) =
        Say symbol (go . k)

    go (Fail _) =
        p1

    go result =
        result



instance ( Monad parent
         , Monoid result
         )
    => Monoid (Sentence scope parent result)
  where

    mempty =
        pure mempty



    mappend =
        _mappend



_mappend
    :: ( Monad parent
       , Monoid result
       )
    => Sentence scope parent result
    -> Sentence scope parent result
    -> Sentence scope parent result

_mappend p0 p1 =
    go p0
  where

    go (Fail e) =
        Fail e

    go (Pure result) =
        fmap (mappend result) p1

    go (Super m) =
        Super (fmap go m)

    go (Say symbol k) =
        Say symbol (go . k)




-- | cutoffSteps limits the number of Step constructors in a 'Sentence'. To limit
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
    -> Sentence scope parent result
    -> Sentence scope parent (Maybe result)

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

cutoffSteps stepsRemaining (Say symbol k) =
    let
      newCutoff =
          cutoffSteps (stepsRemaining - 1)

    in
      Say symbol (newCutoff . k)



cutoff
    :: Monad parent
    => Integer
    -> Sentence scope parent result
    -> Sentence scope parent (Maybe result)

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

cutoff stepsRemaining (Say symbol k) =
    let
      newCutoff =
          cutoff (stepsRemaining - 1)

    in
      Say symbol (newCutoff . k)

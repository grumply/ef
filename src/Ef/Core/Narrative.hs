{-# LANGUAGE ConstraintKinds #-}
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



data Narrative lexicon environment result
  where

    Say
        :: Lexeme lexicon intermediate
        -> (    intermediate
             -> Narrative lexicon environment result
           )
        -> Narrative lexicon environment result

    Super
        :: environment (Narrative lexicon environment result)
        -> Narrative lexicon environment result

    Return
        :: result
        -> Narrative lexicon environment result

    -- for convenience
    Fail
        :: SomeException
        -> Narrative lexicon environment result



rearrange
    :: ( Functor environment
       , As (Lexeme lexicon) (Lexeme lexicon')
       )
    => Narrative lexicon environment a
    -> Narrative lexicon' environment a

rearrange (Fail e) =
    Fail e

rearrange (Return r) =
    Return r

rearrange (Super m) =
    Super (fmap rearrange m)

rearrange (Say symbol k) =
    Say (conv symbol) (rearrange . k)



upembed
    :: ( Functor environment
       , Grow lexiconSmall lexiconLarge
       )
    => Narrative lexiconSmall environment result
    -> Narrative lexiconLarge environment result

upembed (Fail e) =
    Fail e

upembed (Return result) =
    Return result

upembed (Super p) =
    Super (fmap upembed p)

upembed (Say symbol k) =
    Say (grow symbol) (upembed . k)



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



instance (Lexeme '[]) `As` (Lexeme '[])



instance ( Allows x ys
         , (Lexeme xs) `As` (Lexeme ys)
         )
    => (Lexeme (x ': xs)) `As` (Lexeme ys)
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



instance Functor environment
    => Lift environment (Narrative lexicon environment)
  where

    lift =
        lift_



instance Lift newEnvironment environment
    => Lift newEnvironment (Narrative lexicon environment)
  where

    lift =
        lift_ . lift



lift_
    :: Functor environment
    => environment result
    -> Narrative lexicon environment result

lift_ m =
    Super (fmap Return m)



super
    :: Functor environment
    => environment result
    -> Narrative lexicon environment result

super =
    lift_



say
    :: lexeme result
    -> Say lexeme lexicon environment result

say symbol =
    Say (inj symbol) return


type Can lexeme lexemes environment =
      ( Allows' lexeme lexemes (IndexOf lexeme lexemes)
      , Monad environment
      )

type Say lexeme lexicon environment result =
    Can lexeme lexicon environment
    => Narrative lexicon environment result


instance Functor environment
    => Functor (Narrative lexicon environment)
  where

    fmap =
        _fmap



instance Monad environment
    => Applicative (Narrative lexicon environment)
  where

    pure =
        return



    (<*>) =
        ap



    (*>) =
        (>>)



instance Monad environment
    => Monad (Narrative lexicon environment)
  where

#ifdef TRANSFORMERS_SAFE
    return =
        Super . return . Return
#else
    return =
        Return
#endif



    (>>=) =
        _bind



    fail =
        Fail . toException . PatternMatchFail



{-# NOINLINE _fmap #-}
_fmap
    :: Functor environment
    => (a -> b)
    -> Narrative lexicon environment a
    -> Narrative lexicon environment b

_fmap f =
    go
  where

    go (Fail e) =
        Fail e

    go (Return a) =
        Return (f a)

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

    "_fmap f (Return result)"
        forall f result.
            _fmap f (Return result) =
                Return (f result)
    ;

  #-}



{-# NOINLINE _bind #-}
_bind
    :: Functor environment
    => Narrative lexicon environment intermediate
    -> (intermediate -> Narrative lexicon environment result)
    -> Narrative lexicon environment result

p0 `_bind` f =
    go p0
  where

    go (Fail e) =
        Fail e

    go (Say symbol k) =
        Say symbol (go . k)

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

    "_bind (Return result) f"
        forall result f .
            _bind (Return result) f =
                f result
    ;

  #-}



instance MonadPlus environment
    => Alternative (Narrative lexicon environment)
  where

    empty =
        mzero



    (<|>) =
        mplus



instance MonadPlus environment
    => MonadPlus (Narrative lexicon environment)
  where

    mzero =
        lift_ mzero



    mplus =
        _mplus



_mplus
    :: MonadPlus environment
    => Narrative lexicon environment result
    -> Narrative lexicon environment result
    -> Narrative lexicon environment result

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



instance ( Monad environment
         , Monoid result
         )
    => Monoid (Narrative lexicon environment result)
  where

    mempty =
        pure mempty



    mappend =
        _mappend



_mappend
    :: ( Monad environment
       , Monoid result
       )
    => Narrative lexicon environment result
    -> Narrative lexicon environment result
    -> Narrative lexicon environment result

_mappend p0 p1 =
    go p0
  where

    go (Fail e) =
        Fail e

    go (Return result) =
        fmap (mappend result) p1

    go (Super m) =
        Super (fmap go m)

    go (Say symbol k) =
        Say symbol (go . k)




-- | cutoffSteps limits the number of Step constructors in a 'Narrative'. To limit
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
    :: Monad environment
    => Integer
    -> Narrative lexicon environment result
    -> Narrative lexicon environment (Maybe result)

cutoffSteps _ (Fail e) =
    Fail e

cutoffSteps ((<= 0) -> True) _ =
    return Nothing

cutoffSteps _ (Return result) =
    Return (Just result)

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
    :: Monad environment
    => Integer
    -> Narrative lexicon environment result
    -> Narrative lexicon environment (Maybe result)

cutoff _ (Fail e) =
    Fail e

cutoff ((<= 0) -> True) _ =
    return Nothing

cutoff _ (Return result) =
    Return (Just result)

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

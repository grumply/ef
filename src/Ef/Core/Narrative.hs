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
module Ef.Core.Narrative
     ( Narrative(..)
     , Say
     , say
     , super
     , grow
     , rearrangeLexicon
     , Lift(..)
     , Knows
     , Grow(..)
     , cutoff
     , cutoffSteps
     ) where


import Ef.Core.Type.Nat
import Ef.Core.Narrative.Lexicon

import Control.Applicative
import Control.Exception (Exception(..),SomeException)
import Control.Exception.Base (PatternMatchFail(..))
import Control.Monad



data Narrative lexicon environment result
  where

    Say
        :: Lexicon lexicon intermediate
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

    Fail
        :: SomeException
        -> Narrative lexicon environment result



instance ( Grow (Lexicon small) (Lexicon large)
         , Functor environment
         )
    => Grow (Narrative small environment) (Narrative large environment)
    where

        grow (Fail e) =
            Fail e

        grow (Return result) =
            Return result

        grow (Super sup) =
            Super (fmap grow sup)

        grow (Say symbol k) =
            Say (grow symbol) (grow . k)



-- | don't use this, just rewrite your Object constructors for consistency
rearrangeLexicon
    :: ( Grow (Lexicon small) (Lexicon large)
       , Functor environment
       )
    => Narrative small environment result
    -> Narrative large environment result

rearrangeLexicon =
    grow



class Functor m'
    => Lift m m'
  where

    lift :: m a -> m' a



instance Functor m
    => Lift m m
  where

    lift =
        id



instance Functor environment
    => Lift environment (Narrative lexicon environment)
  where

    lift =
        super



instance Lift newEnvironment environment
    => Lift newEnvironment (Narrative lexicon environment)
  where

    lift =
        super . lift



super
    :: Functor environment
    => environment result
    -> Narrative lexicon environment result

super m =
    Super (fmap Return m)



say
    :: lexeme result
    -> Say lexeme lexicon environment result

say symbol =
    Say (inj symbol) return



type Say lexeme lexicon environment result =
    Knows lexeme lexicon environment
    => Narrative lexicon environment result



type Knows lexeme lexicon environment =
    ( Can' lexeme lexicon (IndexOf lexeme lexicon)
    , Monad environment
    )



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
        super mzero



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

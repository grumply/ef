{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE IncoherentInstances #-}
module Ef.Core.Pattern where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat
import Ef.Core.Pattern.Symbols


import Control.Applicative
import Control.Monad
import Unsafe.Coerce



data Pattern symbols m a

  = forall b. Step (Symbol symbols b) (b -> Pattern symbols m a)

  | M (m (Pattern symbols m a))

  | Pure a



cast
    :: forall fs gs m a.
       ( Functor m
       , As (Symbol fs) (Symbol gs)
       )
    => Pattern fs m a
    -> Pattern gs m a
cast (Pure r) =
    Pure r

cast (M m) =
    M (fmap cast m)

cast (Step sym bp) =
    Step (conv sym) (unsafeCoerce bp)



rearrange
    :: ( Functor m
       , i ~ IndexOf (Symbol s) ss
       , Allows' (Symbol s) ss i
       )
    => Pattern s m a
    -> Pattern ss m a
rearrange (Pure r) =
    Pure r

rearrange (M m) =
    M (fmap rearrange m)

rearrange (Step sym bp) =
    Step (inj sym) (unsafeCoerce bp)



class As x y
  where

    conv
        :: x a
        -> y a



instance As x x
  where

    conv =
        id



instance As (Symbol '[]) (Symbol '[])



instance ( As x y
         , As (Symbol xs) (Symbol ys)
         , Denies y ys
         )
    => As (Symbol (x ': xs)) (Symbol (y ': ys))
  where

    conv (Symbol sa) =
        Symbol (conv sa)

    conv (Further ss) =
        Further (conv ss)



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



instance Functor m
    => Lift m (Pattern fs m)
  where

    lift =
        lift_



instance Lift m m'
    => Lift m (Pattern fs m')
  where

    lift =
        lift_ . lift



lift_
    :: Functor m
    => m a
    -> Pattern symbols m a
lift_ m =
    M (fmap Pure m)



super
    :: Functor m
    => Pattern fs m a
    -> Pattern gs (Pattern fs m) a
super =
    lift



self
    :: Is x symbols m
    => x a
    -> Pattern symbols m a
self xa =
    Step (inj xa) return



instance Functor m
    => Functor (Pattern symbols m)
  where
    fmap f p0 =
        _fmap f p0



instance Monad m
    => Applicative (Pattern symbols m)
  where

    pure = return



    (<*>) = ap



    (*>) = (>>)



instance Monad m
    => Monad (Pattern symbols m)
  where

#ifdef TRANSFORMERS_SAFE
    return = M . return . Pure
#else
    return = Pure
#endif



    (>>=) = _bind



{-# NOINLINE _fmap #-}
_fmap
    :: Functor m
    => (a -> b)
    -> Pattern symbols m a
    -> Pattern symbols m b
_fmap f =
    go
  where

    go (Pure a) =
        Pure (f a)

    go (M m) =
        M (fmap go m)

    go (Step sym bp) =
        Step sym (go . bp)



{-# RULES

    "_fmap f (Step syms k)"
        forall syms k f.
            _fmap f (Step syms k) =
                Step syms (_fmap f . k)
    ;

    "_fmap f (M m)"
        forall f m.
            _fmap f (M m) =
                let
                  continue =
                      _fmap f

                in
                  M (fmap continue m)
    ;

    "_fmap f (Pure r)"
        forall f r.
            _fmap f (Pure r) =
                Pure (f r)
    ;

  #-}



{-# NOINLINE _bind #-}
_bind
    :: Functor m
    => Pattern symbols m a
    -> (a -> Pattern symbols m a')
    -> Pattern symbols m a'
p0 `_bind` f =
    go p0
  where
    go p =
        case p of

            Step syms k ->
                Step syms (go . k)

            Pure res ->
                f res

            M m ->
                M (fmap go m)



{-# RULES

    "_bind (Step syms k) f"
        forall syms k f .
            _bind (Step syms k) f =
                Step syms (flip _bind f . k)
    ;

    "_bind (M m) f"
        forall m f.
            _bind (M m) f =
                let
                  continue =
                      flip _bind f

                in
                  M (fmap continue m)
    ;

    "_bind (Pure r) f"
        forall r f.
            _bind (Pure r) f =
                f r
    ;

  #-}

instance MonadPlus m
    => Alternative (Pattern fs m)
  where

    empty =
        mzero



    (<|>) =
        mplus



instance MonadPlus m
    => MonadPlus (Pattern fs m)
  where

    mzero =
        lift_ mzero



    mplus =
        _mplus



_mplus
    :: MonadPlus m
    => Pattern fs m a
    -> Pattern fs m a
    -> Pattern fs m a
_mplus p0 p1 =
    go p0
  where
    go p =
        case p of

            Step sym bp ->
                Step sym (go . bp)

            Pure r ->
                Pure r

            M m ->
                M (fmap go m `mplus` return p1)



instance ( Monad m
         , Monoid r
         )
    => Monoid (Pattern fs m r)
  where

    mempty =
        pure mempty



    mappend =
        _mappend

_mappend
    :: ( Monad m
       , Monoid r
       )
    => Pattern fs m r
    -> Pattern fs m r
    -> Pattern fs m r
_mappend p0 p1 =
    go p0
  where
    go p =
        case p of

            Step sym bp ->
                Step sym (go . bp)

            M m ->
                M (fmap go m)

            Pure r ->
                fmap (mappend r) p1



observe
    :: forall fs m a.
       Monad m
    => Pattern fs m a
    -> Pattern fs m a
observe = M . go
  where
    go p =
        case p of

          Step sym bp ->
              let
                continue =
                    observe . bp

              in
                return (Step sym continue)

          M m ->
              m >>= go

          Pure r ->
              return (Pure r)



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
    :: Monad m
    => Integer
    -> Pattern fs m a
    -> Pattern fs m (Maybe a)
cutoffSteps n p
    | n <= 0 =
        return Nothing

    | otherwise =
          let
            newCutoff =
                cutoff (n - 1)

          in
            case p of

                Pure a ->
                    Pure (Just a)

                M m ->
                    M (fmap newCutoff m)

                Step sym k ->
                    Step sym (newCutoff . k)



cutoff
    :: Monad m
    => Integer
    -> Pattern fs m a
    -> Pattern fs m (Maybe a)
cutoff n p
    | n <= 0 =
        return Nothing

    | otherwise =
        let
          newCutoff =
              cutoff (n - 1)

        in
          case p of

              Pure a ->
                  Pure (Just a)

              M m ->
                  M (fmap newCutoff m)

              Step sym k ->
                  Step sym (newCutoff . k)

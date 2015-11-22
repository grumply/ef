{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Core.Pattern.Symbols where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat



data Symbol symbols a
  where

    Symbol
        :: Denies s ss
        => s a
        -> Symbol (s ': ss) a

    Further
        :: Denies s ss
        => Symbol ss a
        -> Symbol (s ': ss) a



instance Functor (Symbol '[])



instance ( Functor s
         , tail ~ Symbol ss
         , Functor tail
         , syms ~ (s ': ss)
         )
    => Functor (Symbol syms)
  where

    fmap f (Symbol sb) =
        Symbol (fmap f sb)

    fmap f (Further ss) =
        Further (fmap f ss)



class Allows x xs
  where

    inj
        :: x a
        -> Symbol xs a



    prj
        :: Symbol xs a
        -> Maybe (x a)



instance ( i ~ IndexOf x xs
         , Allows' x xs i
         )
    => Allows x xs
  where

    inj xa =
        let
          i =
              Index :: Index (IndexOf x xs)

        in
          inj' i xa



    prj xs =
        let
          i =
              Index :: Index (IndexOf x xs)

        in
          prj' i xs



class Allows' x xs (n :: Nat)
  where

    inj'
        :: Index n
        -> x a
        -> Symbol xs a

    prj'
        :: Index n
        -> Symbol xs a
        -> Maybe (x a)



instance ( Denies x' xs'
         , xs ~ (x' ': xs')
         , i ~ IndexOf x xs'
         , Allows' x xs' i
         )
    => Allows' x xs ('S n)
  where

    inj' _ xa =
        let
          i =
              Index :: Index (IndexOf x xs')

        in
          Further (inj' i xa)



    prj' _ (Further ss) =
        let
          i =
              Index :: Index (IndexOf x xs')

        in
          prj' i ss

    prj' _ _ =
        Nothing



instance ( Denies x xs'
         , xs ~ (x ': xs')
         )
    => Allows' x xs 'Z
  where

    inj' _ =
        Symbol



    prj' _ (Symbol sa) =
        Just sa

    prj' _ (Further _) =
        Nothing



type Is f fs m =
    ( Monad m
    , Allows' f fs (IndexOf f fs)
    )



type (f :< fs) m =
    Is f fs m



type Invokes fs' fs m =
    ( Monad m
    , AllowsSubset fs' fs
    )



class AllowsSubset (fs' :: [* -> *]) (fs :: [* -> *])



instance AllowsSubset '[] fs



instance ( i ~ IndexOf f fs
         , Allows' f fs i
         , AllowsSubset fs' fs
         )
  => AllowsSubset (f ': fs') fs

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Core.Object.Attributes where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat


import Unsafe.Coerce
import Data.Binary



data Attrs (as :: [* -> *]) a
  where

    Empty
        :: Attrs '[] a

    Attr
        :: Denies f fs
        => f a
        -> Attrs fs a
        -> Attrs (f ': fs) a



instance Binary (Attrs '[] x)
  where

    get =
       pure Empty

    put _ =
       pure ()



instance ( Binary (Attrs as x)
         , Denies a as
         , Binary (a x)
         )
    => Binary (Attrs (a ': as) x)
  where

    get =
        Attr <$> get <*> get

    put (Attr x as) =
        put x >> put as



instance Functor (Attrs '[])
  where

    fmap _ =
        unsafeCoerce



instance ( Functor f
         , attrs ~ Attrs fs
         , Functor attrs
         )
    => Functor (Attrs (f ': fs))
  where

    fmap f (Attr fa fs) =
        Attr
            (fmap f fa)
            (fmap f fs)



class Admits (x :: * -> *) (xs :: [* -> *])
  where

    push
        :: x a -> Attrs xs a -> Attrs xs a



    pull
        :: Attrs xs a -> x a



instance ( i ~ IndexOf x xs
         , Admits' x xs i
         )
    => Admits x xs
  where

    push xa =
        let
          i =
              Index :: Index (IndexOf x xs)

        in
          push' i xa



    pull xs =
        let
          i =
              Index :: Index (IndexOf x xs)

        in
          pull' i xs



stretch
    :: ( i ~ IndexOf x xs
       , Admits' x xs i
       )
    => (forall z. x z -> x z)
    -> Attrs xs a
    -> Attrs xs a
stretch f xs =
    let
      ys =
          pull xs

    in
      push (f ys) xs



class Admits' (x :: * -> *) (xs :: [* -> *]) (n :: Nat)
  where

    push'
        :: Index n
        -> x a
        -> Attrs xs a
        -> Attrs xs a



    pull'
        :: Index n
        -> Attrs xs a
        -> x a



instance xs ~ (x ': xs')
    => Admits' x xs 'Z
  where

    push' _ xa (Attr _ fs) =
        Attr xa fs



    pull' _ (Attr fa _) =
        fa



instance ( i ~ IndexOf x xs'
         , Admits' x xs' i
         )
    => Admits' x (x' ': xs') ('S n)
  where

    push' _ xa (Attr fa xs) =
        let
          i =
              Index :: Index (IndexOf x xs')

        in
          Attr fa (push' i xa xs)



    pull' _ (Attr _ xs) =
        let
          i =
              Index :: Index (IndexOf x xs')

        in
          pull' i xs



class AdmitsSubset (xs :: [* -> *]) (ys :: [* -> *])
  where

    pushSubset
        :: Attrs xs a
        -> Attrs ys a
        -> Attrs ys a



    pullSubset
        :: Attrs ys a
        -> Attrs xs a



instance AdmitsSubset '[] ys
  where

    pushSubset _ ys =
        ys



    pullSubset _ =
        Empty



instance ( Denies x xs
         , i ~ IndexOf x ys
         , Admits' x ys i
         , AdmitsSubset xs ys
         )
    => AdmitsSubset (x ': xs) ys
  where

    pushSubset (Attr xa xs) ys =
        pushSubset xs (push xa ys)



    pullSubset ys =
        Attr (pull ys) (pullSubset ys)

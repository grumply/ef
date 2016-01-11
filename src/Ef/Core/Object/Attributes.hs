{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Core.Object.Attributes where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat


import Data.Binary
import Data.Typeable
import Unsafe.Coerce



data Attrs (attrs :: [* -> *]) x
  where

    Empty
        :: Attrs '[] x

    Attr
        :: Denies attr attrs
        => attr x
        -> Attrs attrs x
        -> Attrs (attr ': attrs) x



instance Binary (Attrs '[] x)
  where

    get =
       pure Empty



    put _ =
       pure ()



instance ( Binary (Attrs attrs x)
         , Denies attr attrs
         , Binary (attr x)
         )
    => Binary (Attrs (attr ': attrs) x)
  where

    get =
        Attr <$> get <*> get



    put (Attr x as) =
        put x >> put as



instance Functor (Attrs '[])
  where

    fmap _ _ =
        Empty



instance ( Functor attr
         , Functor (Attrs attrs)
         )
    => Functor (Attrs (attr ': attrs))
  where

    fmap f (Attr attr attrs) =
        Attr
            (fmap f attr)
            (fmap f attrs)



instance {-# OVERLAPPING #-} Show (Attrs '[] methods)
    where

        show _ = ""



instance {-# OVERLAPS #-} 
         ( attrs ~ (attr ': attrs')
         , Typeable (attr ())
         , Show (attr method)
         , Show (Attrs attrs' method)
         )
         => Show (Attrs attrs method)
    where

        show (Attr attr attrs) =
            let
                attrType =
                    typeOf (undefined :: attr ())

                attrString =
                    reverse . (" :" ++) . drop 3 . reverse . show $ attrType
            in
               case attrs of
                 
                   Empty -> 
                       attrString ++ show attr
                       
                   _ ->
                       attrString ++ show attr ++ ", " ++ show attrs


class Admits (attr :: * -> *) (attrs :: [* -> *])
  where

    push
        :: attr x -> Attrs attrs x -> Attrs attrs x



    pull
        :: Attrs attrs x -> attr x



instance ( index ~ IndexOf attr attrs
         , Admits' attr attrs index
         )
    => Admits attr attrs
  where

    push attr =
        let
          index =
              Index :: Index (IndexOf attr attrs)

        in
          push' index attr



    pull attrs =
        let
          index =
              Index :: Index (IndexOf attr attrs)

        in
          pull' index attrs



stretch
    :: ( index ~ IndexOf attr attrs
       , Admits' attr attrs index
       )
    => (forall z. attr z -> attr z)
    -> Attrs attrs x
    -> Attrs attrs x
stretch f attrs =
    let
      attr =
          pull attrs

    in
      push (f attr) attrs



class Admits' (attr :: * -> *) (attrs :: [* -> *]) (n :: Nat)
  where

    push'
        :: Index n
        -> attr x
        -> Attrs attrs x
        -> Attrs attrs x



    pull'
        :: Index n
        -> Attrs attrs x
        -> attr x



instance attrs ~ (attr ': xs)
    => Admits' attr attrs 'Z
  where

    push' _ attr (Attr _ attrs) =
        Attr attr attrs



    pull' _ (Attr attr _) =
        attr



instance ( index ~ IndexOf attr attrs
         , Admits' attr attrs index
         )
    => Admits' attr (attr' ': attrs) ('S n)
  where

    push' _ attr (Attr attr' attrs) =
        let
          index =
              Index :: Index (IndexOf attr attrs)

        in
          Attr attr' (push' index attr attrs)



    pull' _ (Attr _ attrs) =
        let
          index =
              Index :: Index (IndexOf attr attrs)

        in
          pull' index attrs



class AdmitsSubset (small :: [* -> *]) (large :: [* -> *])
  where

    pushSubset
        :: Attrs small x
        -> Attrs large x
        -> Attrs large x



    pullSubset
        :: Attrs large x
        -> Attrs small x



instance AdmitsSubset '[] largeAttrs
  where

    pushSubset _ large =
        large



    pullSubset _ =
        Empty



instance ( Denies attr small
         , index ~ IndexOf attr large
         , Admits' attr large index
         , AdmitsSubset small large
         )
    => AdmitsSubset (attr ': small) large
  where

    pushSubset (Attr attr small) large=
        pushSubset small(push attr large)



    pullSubset large =
        Attr (pull large) (pullSubset large)

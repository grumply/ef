{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
module Ef.Core.Object where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat
import Ef.Core.Object.Attributes

import Data.Binary
import Data.Typeable



type Method fs m =
       Object fs m
    -> m (Object fs m)



type Attribute f fs m =
    f (Method fs m)



newtype Object fs m =
    Object
        {
          deconstruct
              :: Attrs fs (Method fs m)
        }



instance Binary (Attrs fs (Method fs m))
    => Binary (Object fs m)
  where

    get =
        Object <$> get

    put (Object as) =
        put as



data Purpose =
    forall fs m.
       ( Typeable fs
       , Typeable m
       )
    => Purpose (Object fs m)



type Uses f fs m =
    ( Monad m
    , Admits' f fs (IndexOf f fs)
    )



type Extends extended orig m =
    ( Monad m
    , AdmitsSubset orig extended
    )



type (extended :=> orig) m =
    Extends extended orig m



simple
    :: Monad m
    => Object '[] m

simple =
    Object Empty



class UnsafeBuild fs
  where

    unsafeBuild
        :: Attrs fs a



instance UnsafeBuild '[]
  where

    unsafeBuild =
        Empty



instance ( Typeable f
         , Denies f fs
         , UnsafeBuild fs
         )
    => UnsafeBuild (f ': fs)
  where

    unsafeBuild =
        let
          attr =
              show (typeOf1 (undefined :: forall a. f a))

          msg =
              "Attribute (" ++ attr ++ ") uninitialized."

        in
          Attr (error msg) unsafeBuild



build
    :: UnsafeBuild attrs
    => (    Attrs attrs (Method attrs m)
         -> Attrs attrs (Method attrs m)
       )
    -> Object attrs m

build =
    Object . ($ unsafeBuild)



infixr 6 *:*

(*:*)
    :: Denies f fs
    => f a
    -> Attrs fs a
    -> Attrs (f ': fs) a

(*:*) fa Empty =
    Attr fa Empty

(*:*) fa i =
    Attr fa i



view
    :: Admits f fs
    => Object fs m
    -> Attribute f fs m

view =
    pull . deconstruct



infixl 5 .=

(.=)
    :: Uses f fs m
    => Object fs m
    -> Attribute f fs m
    -> Object fs m

is .= x =
    let
        deconstructed =
            deconstruct is

    in
      Object (push x deconstructed)

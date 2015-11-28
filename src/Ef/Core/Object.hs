{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Core.Object where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat
import Ef.Core.Object.Attributes

import Control.Monad
import Data.Binary
import Data.Typeable
import Data.Typeable.Internal
import GHC.Generics

import qualified Data.ByteString.Lazy as BSL
import Codec.Compression.Zlib.Raw hiding (Method)



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



instance Binary TyCon



instance Binary TypeRep



deriving instance Generic TyCon



deriving instance Generic TypeRep



instance ( Typeable (Object fs m)
         , Binary (Attrs fs (Method fs m))
         )
    => Binary (Object fs m)
  where

    get =
        do
          tr <- get
          if tr == typeOf (undefined :: Object fs m) then
              Object <$> get
          else
              mzero



    put o@(Object as) =
        do
          put (typeOf o)
          put as



serialize
    :: Binary (Object fs m)
    => Object fs m
    -> BSL.ByteString

serialize =
    compress . encode



deserialize
    :: Binary (Object fs m)
    => BSL.ByteString
    -> Object fs m

deserialize =
    decode . decompress



data Purpose =
    forall fs m.
    Purpose (Object fs m)



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

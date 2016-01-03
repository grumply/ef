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



type Method attrs parent =
       Object attrs parent
    -> parent (Object attrs parent)



type Attribute attr attrs parent =
    attr (Method attrs parent)



newtype Object attrs parent =
    Object
        {
          deconstruct
              :: Attrs attrs (Method attrs parent)
        }



instance Binary TyCon



instance Binary TypeRep



deriving instance Generic TyCon



deriving instance Generic TypeRep



instance ( Typeable (Object attrs parent)
         , Binary (Attrs attrs (Method attrs parent))
         )
    => Binary (Object attrs parent)
  where

    get =
        do
          typeRep <- get
          if typeRep == typeOf (undefined :: Object attrs parent) then
              Object <$> get
          else
              mzero



    put o@(Object as) =
        do
          put (typeOf o)
          put as



type Uses attr attrs parent =
    ( Monad parent
    , Admits' attr attrs (IndexOf attr attrs)
    )



type Extends extended orig parent =
    ( Monad parent
    , AdmitsSubset orig extended
    )



simple
    :: Monad parent
    => Object '[] parent

simple =
    Object Empty



class UnsafeBuild attrs
  where

    unsafeBuild
        :: Attrs attrs a



instance UnsafeBuild '[]
  where

    unsafeBuild =
        Empty



instance ( Typeable attr
         , Denies attr attrs
         , UnsafeBuild attrs
         )
    => UnsafeBuild (attr ': attrs)
  where

    unsafeBuild =
        let
          attr =
              show (typeOf1 (undefined :: forall a. attr a))

          msg =
              "Attribute (" ++ attr ++ ") uninitialized."

        in
          Attr (error msg) unsafeBuild



build
    :: UnsafeBuild attrs
    => (    Attrs attrs (Method attrs parent)
         -> Attrs attrs (Method attrs parent)
       )
    -> Object attrs parent

build =
    Object . ($ unsafeBuild)



infixr 6 *:*

(*:*)
    :: Denies attr attrs
    => attr a
    -> Attrs attrs a
    -> Attrs (attr ': attrs) a

(*:*) attr Empty =
    Attr attr Empty

(*:*) attr attrs =
    Attr attr attrs



view
    :: Admits attr attrs
    => Object attrs parent
    -> Attribute attr attrs parent

view =
    pull . deconstruct



infixl 5 .=

(.=)
    :: Uses attr attrs parent
    => Object attrs parent
    -> Attribute attr attrs parent
    -> Object attrs parent

is .= x =
    let
        deconstructed =
            deconstruct is

    in
      Object (push x deconstructed)

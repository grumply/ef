{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Core.Object
    ( Morphism
    , Use
    , Does(..)
    , Extend(..)
    , stretch
    , Context(..)
    , Object(..)
    , (*:*)
    , view
    , (.=)
    ) where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat
import Ef.Core.Object.Context

import Control.Monad
import Data.Binary
import Data.Typeable
import Data.Typeable.Internal
import GHC.Generics

import qualified Data.ByteString.Lazy as BSL



type Morphism contexts environment =
    Object contexts environment
    -> environment (Object contexts environment)



type Use context contexts environment =
    ( Does context contexts
    , Monad environment
    ) 
    => context (Morphism contexts environment)



newtype Object contexts environment =
      Object
          {
            deconstruct
                :: Context contexts (Morphism contexts environment) 
          }



-- Orphans
instance Binary TyCon
instance Binary TypeRep
deriving instance Generic TyCon
deriving instance Generic TypeRep



instance ( Typeable (Object contexts environment)
         , Binary (Context contexts (Morphism contexts environment))
         )
    => Binary (Object contexts environment)
  where

    get =
        do
          typeRep <- get
          if typeRep == typeOf (undefined :: Object contexts environment) then
              Object <$> get
          else
              mzero



    put o@(Object as) =
        do
          put (typeOf o)
          put as



instance Show (Context contexts (Morphism contexts environment))
         => Show (Object contexts environment)
    where

        show (Object contexts) =
            "Object { " ++ show contexts ++ " }"



infixr 6 *:*

(*:*)
    :: Denies context contexts
    => context a
    -> Context contexts a
    -> Context (context ': contexts) a

(*:*) context contexts =
    Context context contexts



view
    :: Does context contexts
    => Object contexts environment
    -> context (Morphism contexts environment)

view =
    pull . deconstruct



infixl 5 .=

(.=)
    :: ( Does context contexts
       , Monad environment
       )
    => Object contexts environment
    -> context (Morphism contexts environment)
    -> Object contexts environment

is .= x =
    let
        deconstructed =
            deconstruct is

    in
      Object (push x deconstructed)

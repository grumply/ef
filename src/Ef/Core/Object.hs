{-# LANGUAGE MultiParamTypeClasses #-}
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
{-# LANGUAGE ConstraintKinds #-}
module Ef.Core.Object
    ( Morphism
    , Use
    , Does(..)
    , Has(..)
    , Extend(..)
    , stretch
    , Context(..)
    , Object(..)
    , (*:*)
    , view
    , view2
    , view3
    , view4
    , view5
    , view6
    , view7
    , view8
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
import GHC.Exts (Constraint)

import qualified Data.ByteString.Lazy as BSL



type Morphism contexts environment =
    Object contexts environment
    -> environment (Object contexts environment)



type Use context contexts environment =
    Has context contexts environment
    => context (Morphism contexts environment)



type Uses context contexts contexts' environment =
    HasAll contexts contexts' environment
    => context (Morphism contexts environment)



type Has context contexts environment =
    ( Does' context contexts (IndexOf context contexts)
    , Monad environment
    )



type family HasAll (attributes :: [* -> *]) contexts environment :: Constraint where

    HasAll (attribute ': '[]) contexts environment =
        ( Has attribute contexts environment
        , Monad environment
        )

    HasAll (attribute ': attributes) contexts environment =
        ( Does' attribute contexts (IndexOf attribute contexts)
        , HasAll attributes contexts environment
        )

type family DoesAll (attributes :: [* -> *]) contexts :: Constraint where

    DoesAll (attribute ': '[]) contexts =
        (Does attribute contexts)

    DoesAll (attribute ': attributes) contexts =
        (Does attribute contexts,DoesAll attributes contexts)



newtype Object contexts environment =
      Object
          {
            deconstruct
                :: Context contexts (Morphism contexts environment) 
          }



instance (Eq (Context contexts (Morphism contexts environment)))
        => Eq (Object contexts environment)
    where

        (Object o1) == (Object o2) =
            o1 == o2


instance (Ord (Context contexts (Morphism contexts environment)))
        => Ord (Object contexts environment)
     where

         (Object o1) <= (Object o2) =
             o1 <= o2


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
            "{ " ++ show contexts ++ " }"



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



view2
    :: DoesAll '[context1,context2] contexts
    => Object contexts environment
    -> ( context1 (Morphism contexts environment)
       , context2 (Morphism contexts environment)
       )
view2 obj =
    (view obj,view obj)



view3
    :: DoesAll '[context1,context2,context3] contexts
    => Object contexts environment
    -> ( context1 (Morphism contexts environment)
       , context2 (Morphism contexts environment)
       , context3 (Morphism contexts environment)
       )
view3 obj =
    (view obj,view obj,view obj)




view4
    :: DoesAll '[context1,context2,context3,context4] contexts
    => Object contexts environment
    -> ( context1 (Morphism contexts environment)
       , context2 (Morphism contexts environment)
       , context3 (Morphism contexts environment)
       , context4 (Morphism contexts environment)
       )
view4 obj =
    (view obj,view obj,view obj,view obj)




view5
    :: DoesAll '[context1,context2,context3,context4,context5] contexts
    => Object contexts environment
    -> ( context1 (Morphism contexts environment)
       , context2 (Morphism contexts environment)
       , context3 (Morphism contexts environment)
       , context4 (Morphism contexts environment)
       , context5 (Morphism contexts environment)
       )
view5 obj =
    (view obj,view obj,view obj,view obj,view obj)



view6
    :: DoesAll '[context1,context2,context3,context4,context5,context6] contexts
    => Object contexts environment
    -> ( context1 (Morphism contexts environment)
       , context2 (Morphism contexts environment)
       , context3 (Morphism contexts environment)
       , context4 (Morphism contexts environment)
       , context5 (Morphism contexts environment)
       , context6 (Morphism contexts environment)
       )
view6 obj =
    (view obj,view obj,view obj,view obj,view obj,view obj)



view7
    :: DoesAll '[context1,context2,context3,context4,context5,context6,context7] contexts
    => Object contexts environment
    -> ( context1 (Morphism contexts environment)
       , context2 (Morphism contexts environment)
       , context3 (Morphism contexts environment)
       , context4 (Morphism contexts environment)
       , context5 (Morphism contexts environment)
       , context6 (Morphism contexts environment)
       , context7 (Morphism contexts environment)
       )
view7 obj =
    (view obj,view obj,view obj,view obj,view obj,view obj,view obj)



view8
    :: DoesAll '[context1,context2,context3,context4,context5,context6,context7,context8] contexts
    => Object contexts environment
    -> ( context1 (Morphism contexts environment)
       , context2 (Morphism contexts environment)
       , context3 (Morphism contexts environment)
       , context4 (Morphism contexts environment)
       , context5 (Morphism contexts environment)
       , context6 (Morphism contexts environment)
       , context7 (Morphism contexts environment)
       , context8 (Morphism contexts environment)
       )
view8 obj =
    (view obj,view obj,view obj,view obj,view obj,view obj,view obj,view obj)



infixl 5 .=

(.=)
    :: ( Does' context contexts (IndexOf context contexts)
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

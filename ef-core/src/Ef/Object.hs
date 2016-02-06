{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
module Ef.Object
    ( Method
    , Implementation
    , Subclass
    , Has(..)
    , Use
    , stretch
    , Methods(..)
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



import Ef.Set
import Ef.Methods

import GHC.Exts (Constraint)

import Ef.Nat

type Method method methods supertype =
    method (Implementation methods supertype)

type Use method methods super =
    (Has' method methods (Offset method methods), Monad super)
    => method (Implementation methods super)


type Implementation methods supertype =
    Object methods supertype -> supertype (Object methods supertype)



type family Subclass (methods :: [* -> *]) methods' :: Constraint where

    Subclass (method ': '[]) methods' =
        (Has method methods')

    Subclass (method ': methods) methods' =
        (Has method methods',methods `Subclass` methods')



type family Superclass methods methods' :: Constraint where

    Superclass methods methods' =
        methods' `Subclass` methods



newtype Object methods supertype =
      Object
          {
            deconstruct
                :: Methods methods (Implementation methods supertype)
          }
          


instance (Eq (Methods methods (Implementation methods supertype)))
        => Eq (Object methods supertype)
    where

        (Object o1) == (Object o2) =
            o1 == o2


instance (Ord (Methods methods (Implementation methods supertype)))
        => Ord (Object methods supertype)
     where

         (Object o1) <= (Object o2) =
             o1 <= o2


-- -- Orphans
-- instance Binary TyCon
-- instance Binary TypeRep
-- deriving instance Generic TyCon
-- deriving instance Generic TypeRep



-- instance ( Typeable (Object methods supertype)
--          , Binary (Methods methods (Implementation methods supertype))
--          )
--     => Binary (Object methods supertype)
--   where

--     get =
--         do
--           typeRep <- get
--           if typeRep == typeOf (undefined :: Object methods supertype) then
--               Object <$> get
--           else
--               mzero



--     put o@(Object as) =
--         do
--           put (typeOf o)
--           put as



instance Show (Methods methods (Implementation methods supertype))
         => Show (Object methods supertype)
    where

        show (Object methods) =
            "{ " ++ show methods ++ " }"



infixr 6 *:*

(*:*)
    :: Denies method methods
    => method a
    -> Methods methods a
    -> Methods (method ': methods) a

(*:*) = Method



view
    :: Has method methods
    => Object methods supertype
    -> Method method methods supertype

view =
    pull . deconstruct



view2
    :: (methods `Superclass` '[method1,method2])
    => Object methods supertype
    -> ( Method method1 methods supertype
       , Method method2 methods supertype
       )
view2 obj =
    (view obj,view obj)



view3
    :: (methods `Superclass` '[method1,method2,method3])
    => Object methods supertype
    -> ( Method method1 methods supertype
       , Method method2 methods supertype
       , Method method3 methods supertype
       )
view3 obj =
    (view obj,view obj,view obj)




view4
    :: (methods `Superclass` '[method1,method2,method3,method4])
    => Object methods supertype
    -> ( Method method1 methods supertype
       , Method method2 methods supertype
       , Method method3 methods supertype
       , Method method4 methods supertype
       )
view4 obj =
    (view obj,view obj,view obj,view obj)




view5
    :: ( methods `Superclass`
            '[method1,method2,method3,method4
             ,method5]
       )
    => Object methods supertype
    -> ( Method method1 methods supertype
       , Method method2 methods supertype
       , Method method3 methods supertype
       , Method method4 methods supertype
       , Method method5 methods supertype
       )
view5 obj =
    (view obj,view obj,view obj,view obj,view obj)



view6
    :: ( methods `Superclass`
            '[method1,method2,method3,method4
             ,method5,method6]
       )
    => Object methods supertype
    -> ( Method method1 methods supertype
       , Method method2 methods supertype
       , Method method3 methods supertype
       , Method method4 methods supertype
       , Method method5 methods supertype
       , Method method6 methods supertype
       )
view6 obj =
    (view obj,view obj,view obj,view obj,view obj,view obj)



view7
    :: ( methods `Superclass`
            '[method1,method2,method3,method4
             ,method5,method6,method7]
       )
    => Object methods supertype
    -> ( Method method1 methods supertype
       , Method method2 methods supertype
       , Method method3 methods supertype
       , Method method4 methods supertype
       , Method method5 methods supertype
       , Method method6 methods supertype
       , Method method7 methods supertype
       )
view7 obj =
    (view obj,view obj,view obj,view obj,view obj,view obj,view obj)



view8
    :: ( methods `Superclass`
            '[method1,method2,method3,method4
             ,method5,method6,method7,method8]
       )
    => Object methods supertype
    -> ( Method method1 methods supertype
       , Method method2 methods supertype
       , Method method3 methods supertype
       , Method method4 methods supertype
       , Method method5 methods supertype
       , Method method6 methods supertype
       , Method method7 methods supertype
       , Method method8 methods supertype
       )
view8 obj =
    (view obj,view obj,view obj,view obj,view obj,view obj,view obj,view obj)



infixl 5 .=

(.=)
    :: ( Has method methods
       , Monad supertype
       )
    => Object methods supertype
    -> Method method methods supertype
    -> Object methods supertype

is .= x =
    let
        deconstructed =
            deconstruct is

    in
      Object (push x deconstructed)

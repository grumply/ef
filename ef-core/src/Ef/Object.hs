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
{-# LANGUAGE Safe #-}
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



import Ef.Type.Set
import Ef.Methods

import Ef.Type.Nat
import Control.DeepSeq

type Method method methods super =
    method (Implementation methods super)

type Use method methods super =
    (Has' method methods (Offset method methods), Monad super)
    => method (Implementation methods super)


type Implementation methods super =
    Object methods super -> super (Object methods super)



type family Subclass (methods :: [* -> *]) methods' where

    Subclass (method ': '[]) methods' =
        (Has' method methods' (Offset method methods'))

    Subclass (method ': methods) methods' =
        (Has' method methods' (Offset method methods'),methods `Subclass` methods')



type family Superclass methods methods' where

    Superclass methods methods' =
        methods' `Subclass` methods



newtype Object methods super =
      Object
          {
            deconstruct
                :: Methods methods (Implementation methods super)
          }
          
instance (NFData (Methods methods (Implementation methods super))) => NFData (Object methods super) where
    rnf (Object methods) = rnf methods

instance (Eq (Methods methods (Implementation methods super)))
        => Eq (Object methods super)
    where

        (Object o1) == (Object o2) =
            o1 == o2


instance (Ord (Methods methods (Implementation methods super)))
        => Ord (Object methods super)
     where

         (Object o1) <= (Object o2) =
             o1 <= o2


-- -- Orphans
-- instance Binary TyCon
-- instance Binary TypeRep
-- deriving instance Generic TyCon
-- deriving instance Generic TypeRep



-- instance ( Typeable (Object methods super)
--          , Binary (Methods methods (Implementation methods super))
--          )
--     => Binary (Object methods super)
--   where

--     get =
--         do
--           typeRep <- get
--           if typeRep == typeOf (undefined :: Object methods super) then
--               Object <$> get
--           else
--               mzero



--     put o@(Object as) =
--         do
--           put (typeOf o)
--           put as



instance Show (Methods methods (Implementation methods super))
         => Show (Object methods super)
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
    => Object methods super
    -> Method method methods super

view =
    pull . deconstruct



view2
    :: (methods `Superclass` '[method1,method2])
    => Object methods super
    -> ( Method method1 methods super
       , Method method2 methods super
       )
view2 obj =
    (view obj,view obj)



view3
    :: (methods `Superclass` '[method1,method2,method3])
    => Object methods super
    -> ( Method method1 methods super
       , Method method2 methods super
       , Method method3 methods super
       )
view3 obj =
    (view obj,view obj,view obj)




view4
    :: (methods `Superclass` '[method1,method2,method3,method4])
    => Object methods super
    -> ( Method method1 methods super
       , Method method2 methods super
       , Method method3 methods super
       , Method method4 methods super
       )
view4 obj =
    (view obj,view obj,view obj,view obj)




view5
    :: ( methods `Superclass`
            '[method1,method2,method3,method4
             ,method5]
       )
    => Object methods super
    -> ( Method method1 methods super
       , Method method2 methods super
       , Method method3 methods super
       , Method method4 methods super
       , Method method5 methods super
       )
view5 obj =
    (view obj,view obj,view obj,view obj,view obj)



view6
    :: ( methods `Superclass`
            '[method1,method2,method3,method4
             ,method5,method6]
       )
    => Object methods super
    -> ( Method method1 methods super
       , Method method2 methods super
       , Method method3 methods super
       , Method method4 methods super
       , Method method5 methods super
       , Method method6 methods super
       )
view6 obj =
    (view obj,view obj,view obj,view obj,view obj,view obj)



view7
    :: ( methods `Superclass`
            '[method1,method2,method3,method4
             ,method5,method6,method7]
       )
    => Object methods super
    -> ( Method method1 methods super
       , Method method2 methods super
       , Method method3 methods super
       , Method method4 methods super
       , Method method5 methods super
       , Method method6 methods super
       , Method method7 methods super
       )
view7 obj =
    (view obj,view obj,view obj,view obj,view obj,view obj,view obj)



view8
    :: ( methods `Superclass`
            '[method1,method2,method3,method4
             ,method5,method6,method7,method8]
       )
    => Object methods super
    -> ( Method method1 methods super
       , Method method2 methods super
       , Method method3 methods super
       , Method method4 methods super
       , Method method5 methods super
       , Method method6 methods super
       , Method method7 methods super
       , Method method8 methods super
       )
view8 obj =
    (view obj,view obj,view obj,view obj,view obj,view obj,view obj,view obj)



infixl 5 .=

(.=)
    :: ( Has method methods
       , Monad super
       )
    => Object methods super
    -> Method method methods super
    -> Object methods super

is .= x =
    let
        deconstructed =
            deconstruct is

    in
      Object (push x deconstructed)

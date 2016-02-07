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
{-# LANGUAGE Safe #-}
module Ef.Methods
    ( Methods(..)
    , Has'(..)
    , Has(..)
    , stretch
    ) where



import Ef.Type.Set
import Ef.Type.Nat

import Data.Typeable



-- | Methods are the abstract components of 'Object's. A Methods construction
-- consists of zero or more `Method`s in a de-duplicated singly-linked. There
-- are negative performance implications to this approach, but it represents
-- exactly what we're looking for in a set of attributes. Refer to the
-- documentation for a discussion of the performance implications of this
-- approach.
data Methods (methods :: [* -> *]) x
  where

    Empty
        :: Methods '[] x

    Method
        :: Denies method methods
        => method x
        -> Methods methods x
        -> Methods (method ': methods) x



instance Eq (Methods '[] x) where
    _ == _ = True



instance (Eq (context x),Eq (Methods contexts x))
        => Eq (Methods (context ': contexts) x)
    where

        (Method method0 contexts0) == (Method method1 contexts1) =
            method0 == method1 && contexts0 == contexts1



instance Ord (Methods '[] x) where
    _ <= _ = True



instance (Ord (context x),Ord (Methods contexts x))
        => Ord (Methods (context ': contexts) x)
    where

        (Method method0 contexts0) <= (Method method1 contexts1) =
            method0 <= method1 && contexts0 <= contexts1



-- instance Binary (Methods '[] x)
--   where

--     get =
--        pure Empty



--     put _ =
--        pure ()



-- instance ( Binary (Methods methods x)
--          , Denies method methods
--          , Binary (method x)
--          )
--     => Binary (Methods (method ': methods) x)
--   where

--     get =
--         Method <$> get <*> get



--     put (Method x as) =
--         put x >> put as



instance Functor (Methods '[])
  where

    fmap _ _ =
        Empty



instance ( Functor method
         , Functor (Methods methods)
         )
    => Functor (Methods (method ': methods))
  where

    fmap f (Method method methods) =
        Method
            (fmap f method)
            (fmap f methods)



instance {-# OVERLAPPING #-} Show (Methods '[] methods)
    where

        show _ = ""



instance {-# OVERLAPS #-} 
         ( methods ~ (method ': methods')
         , Typeable (method ())
         , Show (method m)
         , Show (Methods methods' m)
         )
         => Show (Methods methods m)
    where

        show (Method method methods) =
            let
                methodType =
                    typeOf (undefined :: method ())

                methodString =
                    reverse . (" :" ++) . drop 3 . reverse . show $ methodType
            in
               case methods of

                   Empty -> 
                       methodString ++ show method

                   _ ->
                       methodString ++ show method ++ ", " ++ show methods



-- | Has is a class representing (1) the ability to push a method into a set of
-- methods and (2) the ability to pull a method out of a set of methods. If an
-- object witnesses a method, these exist automatically. There does appear to be
-- some room here for dynamic extension of object capabilities, like pulling
-- components out of an object and constructing a desired component that doesn't
-- exist naturally inside that object, but I have yet to explore that.
class Has (method :: * -> *) (methods :: [* -> *])
  where

    push
        :: method x
        -> Methods methods x
        -> Methods methods x



    pull
        :: Methods methods x
        -> method x



instance ( index ~ Offset method methods
         , Has' method methods index
         )
    => Has method methods
  where

    push method =
        let
          index =
              Index :: Index (Offset method methods)

        in
          push' index method



    pull methods =
        let
          index =
              Index :: Index (Offset method methods)

        in
          pull' index methods


-- | stretch is a 'pull' followed by a transformation by way of @f@ followed by a 'push'.
stretch
    :: ( index ~ Offset method methods
       , Has' method methods index
       )
    => (forall z. method z -> method z)
    -> Methods methods x
    -> Methods methods x
stretch f methods =
    let
      method =
          pull methods

    in
      push (f method) methods



class Has' (method :: * -> *) (methods :: [* -> *]) (n :: Nat)
  where

    push'
        :: Index n
        -> method x
        -> Methods methods x
        -> Methods methods x



    pull'
        :: Index n
        -> Methods methods x
        -> method x



instance methods ~ (method ': xs)
    => Has' method methods 'Z
  where

    push' _ method (Method _ methods) =
        Method method methods



    pull' _ (Method method _) =
        method



instance ( index ~ Offset method methods
         , Has' method methods index
         )
    => Has' method (method' ': methods) ('S n)
  where

    push' _ method (Method method' methods) =
        let
          index =
              Index :: Index (Offset method methods)

        in
          Method method' (push' index method methods)



    pull' _ (Method _ methods) =
        let
          index =
              Index :: Index (Offset method methods)

        in
          pull' index methods


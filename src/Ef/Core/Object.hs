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
module Ef.Core.Object where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat
import Ef.Core.Object.Context

import Control.Monad
import Data.Binary
import Data.Typeable
import Data.Typeable.Internal
import GHC.Generics

import qualified Data.ByteString.Lazy as BSL




instance Binary TyCon



instance Binary TypeRep



deriving instance Generic TyCon



deriving instance Generic TypeRep



instance ( Typeable (Object environment contexts)
         , Binary (Context contexts (Morphism environment contexts))
         )
    => Binary (Object environment contexts)
  where

    get =
        do
          typeRep <- get
          if typeRep == typeOf (undefined :: Object environment contexts) then
              Object <$> get
          else
              mzero



    put o@(Object as) =
        do
          put (typeOf o)
          put as



-- Method
-- type Morphism contexts environment =
--        Object contexts environment
--     -> environment (Object contexts environment)


data Interpretation environment a
    where

        Pure
            :: a
            -> Interpretation environment a

        Ap
            :: environment (b -> a)
            -> Interpretation environment b
            -> Interpretation environment a

invoke x =
    (fmap const x) `Ap` (Pure ())

instance (Functor environment)
    => Functor (Interpretation environment)
    where

        fmap =
            _fmap



{-# NOINLINE _fmap #-}
_fmap
    :: Functor environment
    => (a -> b)
    -> Interpretation environment a
    -> Interpretation environment b

_fmap f =
    go
    where

        go (Pure a) =
            Pure (f a)

        go (Ap ef m) =
            Ap (fmap (f .) ef) m



{-# RULES

    "_fmap f (Pure a)"
        forall f a.
            _fmap f (Pure a) =
                Pure (f a)

    ;

    "_fmap f (Ap ef m)" 
        forall f ef m.
            _fmap f (Ap ef m) =
                Ap (fmap (f .) ef) m
    ;

  #-}



instance Functor environment
    => Applicative (Interpretation environment)
    where

        pure =
            Pure 

        (<*>) =
            flip _ap



{-# NOINLINE _ap #-}
_ap f =
    go
    where

        go (Pure g) =
            fmap g f

        go (Ap ef x) =
            Ap (fmap uncurry ef) ((,) <$> x <*> f)


            
{-# RULES

    "_ap (Pure g) f"
        forall g f.
            _ap f (Pure g) =
                fmap g f
    ;

    "_ap (Ap ef x) f"
        forall ef x f.
            _ap f (Ap ef x) =
                Ap (fmap uncurry ef) ((,) <$> x <*> f)
    ;

  #-}


interpret
    :: Monad environment
    => Object environment contexts
    -> Morphism environment contexts
    -> environment (Object environment contexts)

interpret =
    _interpret


{-# NOINLINE _interpret #-}
_interpret obj =
    go
    where

        go (Pure f) =
            pure $ f obj

        go (Ap ef x) =
            do
                boo <- ef
                go (fmap boo x)
   
{-# RULES

    "_interpret obj (Pure f)"
        forall obj f.
            _interpret obj (Pure f) =
                pure $ f obj

    ;

    "_interpret obj (Ap ef x)"
        forall obj ef x.
            _interpret obj (Ap ef x) =
                do
                    boo <- ef
                    _interpret obj (fmap boo x)
    ;

  #-}

type Morphism environment contexts =
    Interpretation environment (Object environment contexts -> Object environment contexts)


type Builder environment contexts contexts' =
    Interpretation environment (    Context contexts (Morphism environment contexts)
                                 -> Context contexts' (Morphism environment contexts')
                               )


newtype Object environment contexts =
      Object
          {
            deconstruct
                :: Context contexts (Morphism environment contexts) 
          }


type Does context contexts environment =
    ( Admits' context contexts (IndexOf context contexts)
    , Monad environment
    )



type Use context contexts environment =
    Does context contexts environment
    => context (Morphism environment contexts)




instance Show (Context contexts (Morphism environment contexts))
         => Show (Object environment contexts)
    where

        show (Object contexts) =
            "Object { " ++ show contexts ++ " }"



simple
    :: Monad environment
    => Object environment '[]

simple =
    Object Empty


class UnsafeBuild contexts
  where

    unsafeBuild
        :: Context contexts a



instance UnsafeBuild '[]
  where

    unsafeBuild =
        Empty



instance ( Typeable context
         , Denies context contexts
         , UnsafeBuild contexts
         )
    => UnsafeBuild (context ': contexts)
  where

    unsafeBuild =
        let
          context =
              show (typeOf1 (undefined :: forall a. context a))

          msg =
              "Context (" ++ context ++ ") uninitialized."

        in
          Context (error msg) unsafeBuild



build
    :: UnsafeBuild contexts
    => (    Context contexts (Morphism environment contexts)
         -> Context contexts (Morphism environment contexts)
       )
    -> Object environment contexts

build =
    Object . ($ unsafeBuild)



infixr 6 *:*

(*:*)
    :: Denies context contexts
    => context a
    -> Context contexts a
    -> Context (context ': contexts) a

(*:*) context Empty =
    Context context Empty

(*:*) context contexts =
    Context context contexts



add
    :: ( Applicative environment
       , Denies context contexts
       , Extend contexts (context ': contexts)
       )
    => context (Morphism environment (context ': contexts))
    -> Interpretation environment (Object environment contexts -> Object environment (context ': contexts))

add context =
    Ap (\x y -> pure $ Object $ context *:* (extend $ fmap _ ( (deconstruct _)))) (Pure ())



view
    :: Does context contexts environment
    => Object environment contexts
    -> Interpretation environment (context (Morphism environment contexts))
view =
    invoke . pure . pull . deconstruct




set
    :: Does context contexts environment
    => context (Morphism environment contexts)
    -> Morphism environment contexts

set x =
    _

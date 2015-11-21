{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Ef.Lang.Scoped.Diverge
   ( Diverging
   , modself

   , Divergable
   , diverger

   , Introspect
   , introspect

   , typeOfSelf
   )
   where

import Ef.Core
import Unsafe.Coerce
import Data.Typeable



-- | Symbol

data Diverging k

  = Snapshot k

  | forall gs m. Inject (Object gs m) k

  | forall gs m. Project (Object gs m -> k)



-- | Symbol Modules

data Divergable k =
    forall gs m.
        Divergable
            { current
                  :: Object gs m

            , reification
                  :: k

            , setter
                  :: Object gs m
                  -> k

            , getter
                  :: k
            }



data Introspect fs gs m =
    Introspect
        { project
              :: Pattern fs m (Object gs m)
        , inject
              :: Object gs m
              -> Pattern fs m ()
        }



-- | Attribute

diverger
    :: Uses Divergable gs m
    => Attribute Divergable gs m
diverger =
    Divergable undefined snapshot_ overwrite_ return
  where

    snapshot_ fs =
        case view fs of
            Divergable _ s _ d ->
                pure $ fs .=
                    Divergable
                        { current = fs
                        , reification = s
                        , setter = overwrite_
                        , getter = d
                        }

    overwrite_ obj fs =
        case view fs of
            Divergable _ s o d ->
                pure $ fs .=
                    Divergable
                        { current = unsafeCoerce obj
                        , reification = s
                        , setter = o
                        , getter = d
                        }



-- | Symbol/Attribute Symmetry

instance Symmetry Divergable Diverging where
  symmetry use (Divergable _ ss _ _) (Snapshot k) =
      use ss k

  symmetry use (Divergable _ _ ok _) (Inject obj k') =
      let
        k =
          ok (unsafeCoerce obj)
      in
        use k k'

  symmetry use (Divergable obj _ _ k) (Project ok) =
      let
        k' =
          ok (unsafeCoerce obj)
      in
        use k k'



-- | Local Scoping Construct

introspect
    :: ( Symmetry (Attrs gs) (Symbol fs)
       , Is Diverging fs m
       )
    => (    Introspect fs gs m
         -> Pattern fs m r
       )
    -> Pattern fs m r
introspect f =
    f Introspect
        { project =
              do
                self (Snapshot ())
                self (Project id)

        , inject =
              \o ->
                  self (Inject o ())
        }



-- | Extended API

-- NotEq here is used to help prevent misuse. It catches the simplest case only:
--   a ~ Object gs m; you can bypass it by returning ((),Object gs m)
modself
    :: ( Symmetry (Attrs gs) (Symbol fs)
       , Is Diverging fs m
       , (Object gs m /== a) ~ 'True
       )
    => (    Object gs m
         -> Pattern fs m (Object gs m,a)
       )
    -> Pattern fs m a
modself f =
    introspect $ \Introspect{..} ->
        do
          obj <- project
          (newObj,a) <- f obj
          inject newObj
          return a



typeOfSelf
    :: ( Symmetry (Attrs gs) (Symbol fs)
       , Is Diverging fs m,Typeable gs
       , Typeable m
       )
    => Pattern fs m TypeRep
typeOfSelf =
    introspect $ \i ->
        do
          obj <- project i
          return (typeOf obj)



-- | Inlines

{-# INLINE diverger #-}
{-# INLINE introspect #-}
{-# INLINE modself #-}
{-# INLINE typeOfSelf #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Lang.Scoped.Diverge
   ( modself, typeOfSelf
   , diverger
   , Divergable, Diverging
   , Introspection, introspect
   )
   where

import Mop.Core
import Unsafe.Coerce
import Data.Typeable

-- | Symbol

data Diverging k
  = Snapshot k
  | forall gs m. Inject (Object gs m) k
  | forall gs m. Project (Object gs m -> k)

-- | Symbol Modules

data Divergable k = forall gs m. Divergable
  { current :: Object gs m
  , reification :: k
  , setter :: Object gs m -> k
  , getter :: k
  }

data Introspection fs gs m = Introspection
  { project :: Pattern fs m (Object gs m)
  , inject :: Object gs m -> Pattern fs m ()
  }


-- | Attribute

{-# INLINE diverger #-}
diverger :: forall gs m. (Uses Divergable gs m)
          => Attribute Divergable gs m
diverger = Divergable undefined snapshot_ overwrite_ return
  where
    snapshot_ fs =
      case view fs of
        (Divergable _ s _ d :: Divergable (Method gs m)) ->
          pure $ fs .= Divergable { current = fs
                                 , reification = s
                                 , setter = overwrite_
                                 , getter = d
                                 }
    overwrite_ obj fs =
      case view fs of
        (Divergable _ s o d :: Divergable (Method gs m)) ->
          pure $ fs .= Divergable { current = unsafeCoerce obj
                                 , reification = s
                                 , setter = o
                                 , getter = d
                                 }

-- | Symbol/Attribute Symmetry

instance Symmetry Divergable Diverging where
  symmetry use (Divergable _ ss _ _) (Snapshot k) = use ss k
  symmetry use (Divergable _ _ ow _) (Inject obj k) = use (ow (unsafeCoerce obj)) k
  symmetry use (Divergable obj _ _ k) (Project ok) = use k (ok (unsafeCoerce obj))

-- | Local Scoping Construct

{-# INLINE introspect #-}
introspect :: forall fs gs m r. (Symmetry (Attrs gs) (Symbol fs),Is Diverging fs m)
            => (    Introspection fs gs m
                 -> Pattern fs m r
               ) -> Pattern fs m r
introspect f =  f Introspection
    { project = do
          self (Snapshot ())
          self (Project id)
    , inject = \o -> self (Inject o ())
    }

-- | Extended API

{-# INLINE modself #-}
-- NotEq here is used to help prevent misuse. It catches the simplest case only:
--   a ~ Object gs m; you can bypass it by returning (b,Object gs m)
modself :: (Monad m, Symmetry (Attrs gs) (Symbol fs),Is Diverging fs m, (Object gs m /== a) ~ 'True)
        => (Object gs m -> Pattern fs m (Object gs m,a)) -> Pattern fs m a
modself f = introspect $ \i -> do
  slf <- project i
  (slf',a) <- f slf
  inject i slf'
  return a

{-# INLINE typeOfSelf #-}
typeOfSelf :: forall fs gs m.
            (Symmetry (Attrs gs) (Symbol fs),Is Diverging fs m,Typeable gs,Typeable m)
         => Pattern fs m TypeRep
typeOfSelf = introspect $ \(i :: Introspection fs gs m) -> do
  Object slf <- project i
  return (typeOf slf)

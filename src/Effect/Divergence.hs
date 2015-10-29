{-# LANGUAGE GADTs #-}
module Effect.Divergence
   ( modself, typeOfSelf
   , divergent
   , Divergent, Diverge
   )
   where

import Mop.Core
import Unsafe.Coerce
import Control.Monad
import Data.Typeable

data Divergent k = forall gs m. Divergent
  { current :: Object gs m
  , reification :: k
  , setter :: Object gs m -> k
  , getter :: k
  }

divergent :: forall gs m. (Uses Divergent gs m)
          => Attribute Divergent gs m
divergent = Divergent undefined snapshot_ overwrite_ return
  where
    snapshot_ fs =
      case (fs&) of
        (Divergent _ s o d :: Divergent (Method gs m)) ->
          pure $ fs .= Divergent { current = fs
                                 , reification = s
                                 , setter = overwrite_
                                 , getter = d
                                 }
    overwrite_ obj fs =
      case (fs&) of
        (Divergent _ s o d :: Divergent (Method gs m)) ->
          pure $ fs .= Divergent { current = unsafeCoerce obj
                                 , reification = s
                                 , setter = o
                                 , getter = d
                                 }


instance Pair Divergent Diverge where
  pair p (Divergent _ ss _ _) (Snapshot k) = p ss k
  pair p (Divergent _ _ ow _) (Inject obj k) = p (ow (unsafeCoerce obj)) k
  pair p (Divergent obj _ _ k) (Project ok) = p k (ok (unsafeCoerce obj))

data Diverge k
  = Snapshot k
  | forall gs m. Inject (Object gs m) k
  | forall gs m. Project (Object gs m -> k)

data Introspection fs gs m = Introspection
  { project :: Plan fs m (Object gs m)
  , inject :: Object gs m -> Plan fs m ()
  }

introspect :: forall fs gs m r. (Pair (Attrs gs) (Symbol fs),Has Diverge fs m)
            => (    Introspection fs gs m
                 -> Plan fs m r
               ) -> Plan fs m r
introspect f =  f Introspection
    { project = do
          self (Snapshot ())
          self (Project id)
    , inject = \o -> self (Inject o ())
    }

-- NotEq here is used to help prevent misuse. It catches the simplest case only:
--   a ~ Object gs m; you can bypass it by returning (b,Object gs m)
modself :: (Monad m, Pair (Attrs gs) (Symbol fs),Has Diverge fs m, NotEq (Object gs m) a ~ True)
        => (Object gs m -> Plan fs m (Object gs m,a)) -> Plan fs m a
modself f = introspect $ \i -> do
  slf <- project i
  (slf',a) <- f slf
  inject i slf'
  return a

typeOfSelf :: forall fs gs m.
            (Pair (Attrs gs) (Symbol fs),Has Diverge fs m,Typeable gs,Typeable m)
         => Plan fs m TypeRep
typeOfSelf = introspect $ \(i :: Introspection fs gs m) -> do
  Object slf <- project i
  return (typeOf slf)

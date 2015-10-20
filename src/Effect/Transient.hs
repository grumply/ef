{-# LANGUAGE ImpredicativeTypes #-}
module Effect.Transient
  ( Transient, Transience, transience
  , Token
  , deallocate, register, unregister, onEnd
  ) where

import Mop
import Mop.IO
import Effect.Exception

import Control.Monad
import Data.Either

import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

-- this module does not have the same sort of asynchronous exception safety
-- as in resourcet, but still admits a nice interface for allocate/deallocate
-- as well as onEnd/finalize and register/unregister.

-- I believe the only approach that will fix the asynchronous exception safety
-- issues is attaching transient to the top-level scope by embedding it inside
-- delta. But as soon as you embed it in delta, performance will drop
-- tremendously. Please, if you have any ideas, send them to me at:
-- sean@grump.ly

newtype Token a = Token Integer

data Transient k
  = FreshScope (Integer -> k)
  | forall a fs m. Allocate Integer (PlanT fs m a) (a -> PlanT fs m ()) (Token a -> k)
  | forall a. Deallocate (Token a) k
  | forall fs m a. Register (Token a) (PlanT fs m ()) k
  | forall a. Unregister (Token a) k
  | forall fs m a. OnEnd (PlanT fs m ()) (Token () -> k)

data Transience k = Transience (IORef Integer) k k
transience :: Uses Transience fs m => Instruction Transience fs m
transience = flip (Transience (unsafePerformIO (newIORef 0))) return $ \fs ->
  let Transience i non me = view fs
      next = unsafePerformIO (modifyIORef i succ)
  in next `seq` return fs

instance Pair Transience Transient where
  pair p (Transience _ _ k) (Deallocate _ k') = p k k'
  pair p (Transience i k _) (FreshScope ik)   =
    let n = unsafePerformIO $ readIORef i
    in n `seq` p k (ik n)
  pair p _ _ = error "Transient misuse"

freshScope :: Has Transient fs m => PlanT fs m Integer
freshScope = symbol (FreshScope id)

deallocate :: Has Transient fs m => Token a -> PlanT fs m ()
deallocate rsrc = symbol (Deallocate rsrc ())

register :: Has Transient fs m => Token a -> PlanT fs m () -> PlanT fs m ()
register rsrc onEnd = symbol (Register rsrc onEnd ())

unregister :: Has Transient fs m => Token a -> PlanT fs m ()
unregister rsrc = symbol (Unregister rsrc ())

onEnd :: Has Transient fs m => PlanT fs m () -> PlanT fs m (Token ())
onEnd oE = symbol (OnEnd oE id)

-- use: transiently $ \allocate ->
transiently :: forall fs m r.
               (Has Transient fs m,Has Throw fs m,MIO m)
            => (    (forall a. PlanT fs m a -> (a -> PlanT fs m ()) -> PlanT fs m (Token a))
                 -> PlanT fs m r
               ) -> PlanT fs m r
transiently x = do
  scope <- freshScope
  let alloc create onEnd = symbol (Allocate scope create onEnd id)
  transform scope [] $ x alloc
  where
    transform scope store = go store
      where
        go store = go'
          where
            go' :: PlanT fs m r -> PlanT fs m r
            go' p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Allocate i create oE _ ->
                          if i == scope
                          then do
                            n <- freshScope
                            let t = Token n
                            a <- unsafeCoerce create
                            go ((n,unsafeCoerce (oE a)):store)
                               (bp (unsafeCoerce t))
                          else Step sym (\b -> go' (bp b))
                        Deallocate (Token t) _ -> do
                          case extract t store of
                            Just (store',cleanup) ->
                              Step sym (\b -> go store' (cleanup >> bp b))
                            Nothing -> Step sym (\b -> go' (bp b))
                        Register (Token t) p _ ->
                          go (unsafeCoerce (t,p):store) (bp (unsafeCoerce ()))
                        Unregister (Token t) _ ->
                          go (filter ((/= t) . fst) store)
                             (bp (unsafeCoerce ()))
                        OnEnd oE _ -> do
                          n <- freshScope
                          go (unsafeCoerce (n,oE):store)
                             (bp (unsafeCoerce (Token n :: Token ())))
                        _ -> Step sym (\b -> go' (bp b))
                    Nothing -> Step sym (\b -> go' (bp b))
                M m -> M (fmap go' m)
                Pure r ->
                  case store of
                    [] -> Pure r
                    ((_,x):xs) -> go xs (x >> return r)

    extract n = finish . partitionEithers . map go
      where
        finish (xs,[]) = Nothing
        finish (xs,[a]) = Just (xs,a)
        finish (xs,rs) = Just (xs,foldr1 (>>) rs :: PlanT fs m ())
        go (t,x) = if t == n then Right x else Left (t,x)

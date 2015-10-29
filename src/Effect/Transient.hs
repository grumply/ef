{- |
-}
{-
TODO: Spell out the exact semantics of this module. Notably, the approach
      with deallocate not being removed by a child scope but unregister
      being removed could be confusing.
-}
module Effect.Transient
  ( Transient, Transience, transience, transiently
  , TransientScope
  , Token
  , deallocate, allocate, register, unregister, onEnd
  ) where

import Mop.Core
import Mop.IO
import Effect.Exception

import Control.Monad
import Data.Either

import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

-- this module does not have the same sort of asynchronous exception safety
-- as in resourcet, but still admits a nice interface for allocate/deallocate
-- as well as onEnd/register/unregister.

-- I believe the only approach that will fix the asynchronous exception safety
-- issue is attaching transient to the top-level scope by embedding it inside
-- delta. But as soon as you embed it in delta, performance will drop
-- tremendously. Please, if you have any ideas, send them to me at:
-- sean@grump.ly

-- For now, exception safety can be guaranteed through internal use of mio for
-- finalizers. However, this will not permit exception safety /between/
-- executions of finalizers, sadly. Thus, to achieve an acceptable level of
-- exception safety, it is imperative not to conflate resource management
-- with asynchronous exception safety; we must, by current design, be careful
-- when using resources and make thoughtful use of exception handling.


newtype Token a = Token Integer

data Transient k
    = FreshScope (Integer -> k)

    | forall fs m a . Allocate   Integer (Plan fs m a) (a -> Plan fs m ()) ((a,Token a) -> k)

    | forall fs m a . OnEnd      Integer           (Plan fs m ()) (Token () -> k)
    | forall fs m a . Register   Integer (Token a) (Plan fs m ()) k

    | forall      a . Unregister Integer (Token a) k
    | forall      a . Deallocate         (Token a) k

data Transience k = Transience Integer k k

{-# INLINE transience #-}
transience :: Uses Transience fs m => Attribute Transience fs m
transience = Transience 0 return $ \fs ->
    let Transience i non me = (fs&)
    in pure (fs .= Transience (succ i) non me)

transientMisuse :: String -> a
transientMisuse method = error $
  "Transient misuse: " ++ method ++ " used outside of a 'transiently' block. \
  \Do not return a TransientScope or its internal fields from its instantiation\
  \ block."

instance Pair Transience Transient where
    pair p (Transience _ _ k) (Deallocate _ k') = p k k'
    pair p (Transience i k _) (FreshScope ik)   = p k (ik i)
    pair p _ (OnEnd _ _ _)      = transientMisuse "OnEnd"
    pair p _ (Register _ _ _ _) = transientMisuse "Register"
    pair p _ (Unregister _ _ _) = transientMisuse "Unregister"

{-# INLINE freshScope #-}
freshScope :: Has Transient fs m => Plan fs m Integer
freshScope = self (FreshScope id)

{-# INLINE deallocate #-}
deallocate :: Has Transient fs m => Token a -> Plan fs m ()
deallocate rsrc = self (Deallocate rsrc ())

data TransientScope fs m = TransientScope
    { allocate   :: forall a. Plan fs m a -> (a -> Plan fs m ()) -> Plan fs m (a,Token a)
    , register   :: forall a. Token a -> Plan fs m () -> Plan fs m ()
    , unregister :: forall a. Token a -> Plan fs m ()
    , onEnd      ::           Plan fs m () -> Plan fs m (Token ())
    }

{-# INLINE transiently #-}
-- use: transiently $ \transient -> do
--        (a,key) <- allocate transient _ _
--        transient&unregister key
--        onEnd transient _
--        deallocate key
transiently :: forall fs m r.
               (Has Transient fs m,Has Throw fs m,MIO m)
            => (    TransientScope fs m
                 -> Plan fs m r
               ) -> Plan fs m r
transiently x = do
    scope <- freshScope
    let alloc create onEnd = self (Allocate scope create onEnd id)
        register token onEnd = self (Register scope token onEnd ())
        unregister token = self (Unregister scope token ())
        onEnd oE = self (OnEnd scope oE id)
    transform scope [] $ x $ TransientScope alloc register unregister onEnd
  where
    transform scope store = go store
      where
        go store = go'
          where
            go' :: Plan fs m r -> Plan fs m r
            go' p = case p of
                Step sym bp -> case prj sym of
                    Just x  -> case x of
                        Allocate i create oE _ ->
                            if i == scope
                            then do
                              n <- freshScope
                              let t = Token n
                              a <- unsafeCoerce create
                              go ((n,unsafeCoerce (oE a)):store)
                                 (bp (unsafeCoerce (a,t)))
                            else Step sym (\b -> go' (bp b))
                        Deallocate (Token t) _ -> case extract t store of
                            Just (store',cleanup) ->
                                Step sym (\b -> go store' (cleanup >> bp b))
                            Nothing -> Step sym (\b -> go' (bp b))
                        Register i (Token t) p _ ->
                            if i == scope
                            then go (unsafeCoerce (t,p):store)
                                    (bp (unsafeCoerce ()))
                            else Step sym (\b -> go' (bp b))
                        Unregister i (Token t) _ ->
                            if i == scope
                            then go (filter ((/= t) . fst) store)
                                    (bp (unsafeCoerce ()))
                            else Step sym (\b -> go' (bp b))
                        OnEnd i oE _ -> do
                            if i == scope
                            then do
                                n <- freshScope
                                go (unsafeCoerce (n,oE):store)
                                   (bp (unsafeCoerce (Token n :: Token ())))
                            else Step sym (\b -> go' (bp b))
                        _ -> Step sym (\b -> go' (bp b))
                    Nothing -> Step sym (\b -> go' (bp b))
                M m -> M (fmap go' m)
                Pure r -> case store of
                    [] -> Pure r
                    ((_,x):xs) -> go xs (x >> return r)

    extract n = finish . partitionEithers . map go
      where
        finish (xs,[]) = Nothing
        finish (xs,[a]) = Just (xs,a)
        finish (xs,rs) = Just (xs,foldr1 (>>) rs :: Plan fs m ())
        go (t,x) = if t == n then Right x else Left (t,x)

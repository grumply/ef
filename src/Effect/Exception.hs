{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE IncoherentInstances #-}
module Effect.Exception
  ( Throw, throw,  catch, handle, catchJust, handleJust, try, tryJust
         , catches, Handler(..)
         , onException, finally, bracket, bracket_, bracketOnError, mapException
  , exceptions, Exceptions
  , Throws, catchChecked, throwChecked
  , Exception,SomeException(..)
  ) where

import Mop
import Control.Exception (SomeException(..),Exception(..))
import qualified Control.Exception as Exc

import Data.Coerce
import Data.Proxy

data Throw k = Throw SomeException k
data Exceptions k = Exceptions (SomeException -> k)

throw :: (Has Throw symbols m, Exception e) => e -> Plan symbols m a
throw e = symbol (Throw (toException e) undefined)

class Throws e where
newtype Catch e = Catch e
instance Throws (Catch e) where
type role Throws representational

throwChecked :: (Exception e,Throws e,Has Throw fs m) => e -> Plan fs m a
throwChecked = throw

catchChecked :: forall e fs m a. (Exception e,Has Throw fs m)
             => (Throws e => Plan fs m a)
             -> (e -> Plan fs m a)
             -> Plan fs m a
catchChecked act = catch (unthrow (Proxy :: Proxy e) (act :: Throws e => Plan fs m a))

unthrow :: proxy e -> (Throws e => a) -> a
unthrow _ = unWrap . coerceWrap . Wrap

newtype Wrap e a = Wrap { unWrap :: Throws e => a }

coerceWrap :: Wrap e a -> Wrap (Catch e) a
coerceWrap = coerce

data Handler fs m a = forall e. Exception e => Handler (e -> Plan fs m a)

instance Functor m => Functor (Handler fs m) where
    fmap f (Handler h) = Handler (fmap f . h)

catches :: Has Throw fs m => Plan fs m a -> [Handler fs m a] -> Plan fs m a
catches p handlers = p `catch` catchesHandler handlers

catchesHandler :: Has Throw fs m => [Handler fs m a] -> SomeException -> Plan fs m a
catchesHandler handlers e = foldr tryHandler (throw e) handlers
  where
    tryHandler (Handler handler) res = case fromException e of
       Just e' -> handler e'
       Nothing -> res

exceptions :: Attribute Exceptions gs k
exceptions = Exceptions (\se -> error $ "Uncaught exception: " ++ show se)

catch :: (Has Throw symbols m, Exception e)
      => Plan symbols m a -> (e -> Plan symbols m a) -> Plan symbols m a
catch plan handler = go plan
  where
    go p = case p of
        Step sym bp -> case prj sym of
            Just (Throw se _) -> case fromException se of
                Just e -> handler e
                Nothing -> Step sym (\b -> go (bp b))
            _ -> Step sym (\b -> go (bp b))
        M m -> M (fmap go m)
        Pure r -> Pure r

handle :: (Has Throw symbols m,Exception e)
       => (e -> Plan symbols m a)
       -> Plan symbols m a
       -> Plan symbols m a
handle = flip catch

catchJust :: (Exception e,Has Throw symbols m)
          => (e -> Maybe b)
          -> Plan symbols m a
          -> (b -> Plan symbols m a)
          -> Plan symbols m a
catchJust p a handler = catch a handler'
  where
    handler' e = case p e of
        Nothing -> throw e
        Just b -> handler b

handleJust :: (Has Throw symbols m,Exception e)
           => (e -> Maybe b)
           -> (b -> Plan symbols m a)
           -> Plan symbols m a
           -> Plan symbols m a
handleJust p = flip (catchJust p)

mapException :: (Exception e, Exception e', Has Throw symbols m)
             => (e -> e') -> Plan symbols m a -> Plan symbols m a
mapException f p = catch p (\e -> throw (f e))

try :: (Exception e,Has Throw symbols m) => Plan symbols m a -> Plan symbols m (Either e a)
try a = catch (a >>= \v -> return (Right v)) (\e -> return (Left e))

tryJust :: (Exception e,Has Throw symbols m)
        => (e -> Maybe b)
        -> Plan symbols m a
        -> Plan symbols m (Either b a)
tryJust p a = do
    r <- try a
    case r of
        Right v -> return (Right v)
        Left e ->
            case p e of
                Nothing -> throw e
                Just b -> return (Left b)

onException :: Has Throw symbols m
            => Plan symbols m a
            -> Plan symbols m b
            -> Plan symbols m a
onException p what = p `catch` \e -> do
    _ <- what
    throw (e :: SomeException)

-- this seems incorrect; sequel might actually run twice
finally :: Has Throw symbols m
        => Plan symbols m a
        -> Plan symbols m b
        -> Plan symbols m a
finally a sequel = do
    r <- a `onException` sequel
    _ <- sequel
    return r

bracket :: Has Throw symbols m
        => Plan symbols m a
        -> (a -> Plan symbols m b)
        -> (a -> Plan symbols m c)
        -> Plan symbols m c
bracket before after thing = do
    a <- before
    r <- (thing a) `onException` after a
    _ <- after a
    return r

bracket_ :: Has Throw symbols m
         => Plan symbols m a
         -> Plan symbols m b
         -> Plan symbols m c
         -> Plan symbols m c
bracket_ before after thing = bracket before (const after) (const thing)

bracketOnError :: Has Throw symbols m
               => Plan symbols m a
               -> (a -> Plan symbols m b)
               -> (a -> Plan symbols m c)
               -> Plan symbols m c
bracketOnError before after thing = do
    a <- before
    (thing a) `onException` (after a)

instance Pair Exceptions Throw where
    pair p (Exceptions k) (Throw e k') = p (k e) k'

{-# LANGUAGE RankNTypes #-}
module Effect.Exception
  ( Throw, throw, catch, handle, catchJust, handleJust, try, tryJust
         , onException, finally, bracket, bracket_, bracketOnError
         , mapException, mask, masked, unmasked
  , throws, Throws
  , Exception,SomeException(..)
  ) where

import Mop
import Control.Exception (SomeException(..),Exception(..))
import qualified Control.Exception as Exc

data Masking = Masked | Unmasked

data Throw k = Throw Masking SomeException k
data Throws k = Throws (SomeException -> k)

masked Masked = True
masked _ = False

unmasked Unmasked = True
unmasked _ = False

throw :: (Has Throw symbols m, Exception e) => e -> PlanT symbols m a
throw e = symbol (Throw Unmasked (toException e) undefined)

throws :: Throws k
throws = Throws (\se -> error $ "Uncaught exception: " ++ show se)

mask :: Has Throw fs m => ((forall a. PlanT fs m a -> PlanT fs m a) -> PlanT fs m b) -> PlanT fs m b
mask x = x unmask
  where
    unmask p =
      case p of
        Step sym bp ->
          case prj sym of
            Just (Throw _ se k) -> Step (inj (Throw Unmasked se k)) (\b -> unmask (bp b))
            Nothing -> Step sym (\b -> unmask (bp b))
        M m -> M (fmap unmask m)
        Pure r -> Pure r

catch :: (Has Throw symbols m, Exception e)
      => PlanT symbols m a -> (e -> PlanT symbols m a) -> PlanT symbols m a
catch plan handler = go plan
  where
    go p =
      case p of
        Step sym bp ->
          case prj sym of
            Just (Throw Unmasked se _) ->
                case fromException se of
                  Just e -> handler e
                  Nothing -> Step sym (\b -> go (bp b))
            _ -> Step sym (\b -> go (bp b))
        M m -> M (fmap go m)
        Pure r -> Pure r

handle :: (Has Throw symbols m,Exception e)
       => (e -> PlanT symbols m a)
       -> PlanT symbols m a
       -> PlanT symbols m a
handle = flip catch

catchJust :: (Exception e,Has Throw symbols m)
          => (e -> Maybe b)
          -> PlanT symbols m a
          -> (b -> PlanT symbols m a)
          -> PlanT symbols m a
catchJust p a handler = catch a handler'
  where
    handler' e =
      case p e of
        Nothing -> throw e
        Just b -> handler b

handleJust :: (Has Throw symbols m,Exception e)
           => (e -> Maybe b)
           -> (b -> PlanT symbols m a)
           -> PlanT symbols m a
           -> PlanT symbols m a
handleJust p = flip (catchJust p)

mapException :: (Exception e, Exception e', Has Throw symbols m)
             => (e -> e') -> PlanT symbols m a -> PlanT symbols m a
mapException f p = catch p (\e -> throw (f e))

try :: (Exception e,Has Throw symbols m) => PlanT symbols m a -> PlanT symbols m (Either e a)
try a = catch (a >>= \v -> return (Right v)) (\e -> return (Left e))

tryJust :: (Exception e,Has Throw symbols m)
        => (e -> Maybe b)
        -> PlanT symbols m a
        -> PlanT symbols m (Either b a)
tryJust p a = do
  r <- try a
  case r of
    Right v -> return (Right v)
    Left e -> case p e of
                Nothing -> throw e
                Just b -> return (Left b)

onException :: Has Throw symbols m
            => PlanT symbols m a
            -> PlanT symbols m b
            -> PlanT symbols m a
onException p what = p `catch` \e -> do _ <- what
                                        throw (e :: SomeException)

-- this seems incorrect; sequel might actually run twice
finally :: Has Throw symbols m
        => PlanT symbols m a
        -> PlanT symbols m b
        -> PlanT symbols m a
finally a sequel = do
  r <- a `onException` sequel
  _ <- sequel
  return r

bracket :: Has Throw symbols m
        => PlanT symbols m a
        -> (a -> PlanT symbols m b)
        -> (a -> PlanT symbols m c)
        -> PlanT symbols m c
bracket before after thing = do
  a <- before
  r <- (thing a) `onException` after a
  _ <- after a
  return r

bracket_ :: Has Throw symbols m
         => PlanT symbols m a
         -> PlanT symbols m b
         -> PlanT symbols m c
         -> PlanT symbols m c
bracket_ before after thing = bracket before (const after) (const thing)

bracketOnError :: Has Throw symbols m
               => PlanT symbols m a
               -> (a -> PlanT symbols m b)
               -> (a -> PlanT symbols m c)
               -> PlanT symbols m c
bracketOnError before after thing = do
  a <- before
  (thing a) `onException` (after a)

instance Pair Throws Throw where
  pair p (Throws k) (Throw _ e k') = p (k e) k'

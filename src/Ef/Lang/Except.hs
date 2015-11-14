{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Except
  ( Excepting
         , throw,  catch, handle, catchJust, handleJust, try, tryJust
         , catches, Handler(..)
         , onException, finally, bracket, bracket_, bracketOnError, mapException
  , excepter, Exceptable
  , Exception(..),SomeException(..),assert
  ) where

import Ef.Core
import Control.Exception (SomeException(..),Exception(..),assert)

-- | Symbol

data Excepting k = Throw SomeException k

-- | Global Symbol Construct

{-# INLINE throw #-}
throw :: (Is Excepting fs m, Exception e) => e -> Pattern fs m a
throw e = self (Throw (toException e) undefined)

-- | Attribute

data Exceptable k = Exceptable (SomeException -> k)

-- | Attribute Construct

{-# INLINE excepter #-}
excepter :: Attribute Exceptable gs k
excepter = Exceptable (\se -> error $ "Uncaught exception: " ++ show se)

-- | Symbol/Attribute Symmetry

instance Symmetry Exceptable Excepting where
    symmetry p (Exceptable k) (Throw e k') = p (k e) k'

-- | Symbol Substitution Scope

{-# INLINE catch #-}
catch :: (Is Excepting fs m, Exception e)
      => Pattern fs m a -> (e -> Pattern fs m a) -> Pattern fs m a
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

-- | Extended API

{-# INLINE handle #-}
handle :: (Is Excepting fs m,Exception e)
       => (e -> Pattern fs m a)
       -> Pattern fs m a
       -> Pattern fs m a
handle = flip catch

{-# INLINE catchJust #-}
catchJust :: (Exception e,Is Excepting fs m)
          => (e -> Maybe b)
          -> Pattern fs m a
          -> (b -> Pattern fs m a)
          -> Pattern fs m a
catchJust p a handler = catch a handler'
  where
    handler' e = case p e of
        Nothing -> throw e
        Just b -> handler b

{-# INLINE handleJust #-}
handleJust :: (Is Excepting fs m,Exception e)
           => (e -> Maybe b)
           -> (b -> Pattern fs m a)
           -> Pattern fs m a
           -> Pattern fs m a
handleJust p = flip (catchJust p)

{-# INLINE mapException #-}
mapException :: (Exception e, Exception e', Is Excepting fs m)
             => (e -> e') -> Pattern fs m a -> Pattern fs m a
mapException f p = catch p (\e -> throw (f e))

{-# INLINE try #-}
try :: (Exception e,Is Excepting fs m) => Pattern fs m a -> Pattern fs m (Either e a)
try a = catch (a >>= \v -> return (Right v)) (\e -> return (Left e))

{-# INLINE tryJust #-}
tryJust :: (Exception e,Is Excepting fs m)
        => (e -> Maybe b)
        -> Pattern fs m a
        -> Pattern fs m (Either b a)
tryJust p a = do
    r <- try a
    case r of
        Right v -> return (Right v)
        Left e ->
            case p e of
                Nothing -> throw e
                Just b -> return (Left b)

{-# INLINE onException #-}
onException :: Is Excepting fs m
            => Pattern fs m a
            -> Pattern fs m b
            -> Pattern fs m a
onException p what = p `catch` \e -> do
    _ <- what
    throw (e :: SomeException)

{-# INLINE finally #-}
finally :: Is Excepting fs m
        => Pattern fs m a
        -> Pattern fs m b
        -> Pattern fs m a
finally a sequel = do
    r <- a `onException` sequel
    _ <- sequel
    return r

{-# INLINE bracket #-}
bracket :: Is Excepting fs m
        => Pattern fs m a
        -> (a -> Pattern fs m b)
        -> (a -> Pattern fs m c)
        -> Pattern fs m c
bracket before after thing = do
    a <- before
    r <- (thing a) `onException` after a
    _ <- after a
    return r

{-# INLINE bracket_ #-}
bracket_ :: Is Excepting fs m
         => Pattern fs m a
         -> Pattern fs m b
         -> Pattern fs m c
         -> Pattern fs m c
bracket_ before after thing = bracket before (const after) (const thing)

{-# INLINE bracketOnError #-}
bracketOnError :: Is Excepting fs m
               => Pattern fs m a
               -> (a -> Pattern fs m b)
               -> (a -> Pattern fs m c)
               -> Pattern fs m c
bracketOnError before after thing = do
    a <- before
    (thing a) `onException` (after a)

data Handler fs m a = forall e. Exception e => Handler (e -> Pattern fs m a)

instance Functor m => Functor (Handler fs m) where
    fmap f (Handler h) = Handler (fmap f . h)

{-# INLINE catches #-}
catches :: Is Excepting fs m => Pattern fs m a -> [Handler fs m a] -> Pattern fs m a
catches p handlers = p `catch` catchesHandler handlers

catchesHandler :: Is Excepting fs m => [Handler fs m a] -> SomeException -> Pattern fs m a
catchesHandler handlers e = foldr tryHandler (throw e) handlers
  where
    tryHandler (Handler handler) res = case fromException e of
       Just e' -> handler e'
       Nothing -> res

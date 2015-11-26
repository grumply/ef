{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Core.Pattern.Exception where



import Ef.Core.Pattern

import Control.Exception
    ( Exception(..)
    , SomeException
    )



throw
    :: Exception e
    => e
    -> Pattern fs m a

throw e =
    Fail (toException e)



catch
    :: Exception e
    => Pattern fs m a
    -> (e -> Pattern fs m a)
    -> Pattern fs m a

catch plan handler =
    rewrite plan
  where

    rewrite (Fail se) =
        case fromException se of

            Just e ->
                handler e

            Nothing ->
                Fail se

    rewrite x =
        x



handle
    :: Exception e
    => (e -> Pattern fs m a)
    -> Pattern fs m a
    -> Pattern fs m a

handle = flip catch



catchJust
    :: Exception e
    => (e -> Maybe b)
    -> Pattern fs m a
    -> (b -> Pattern fs m a)
    -> Pattern fs m a

catchJust p a handler =
    catch a handler'
  where

    handler' e =
        case p e of

            Just x ->
                handler x

            Nothing ->
                throw e


handleJust
    :: Exception e
    => (e -> Maybe b)
    -> (b -> Pattern fs m a)
    -> Pattern fs m a
    -> Pattern fs m a

handleJust p =
    flip (catchJust p)



mapException
    :: ( Exception e
       , Exception e'
       )
    => (e -> e')
    -> Pattern fs m a
    -> Pattern fs m a

mapException f p =
    let
      rethrow err =
        throw (f err)

    in
      handle rethrow p



try
    :: ( Monad m
       , Exception e
       )
    => Pattern fs m a
    -> Pattern fs m (Either e a)

try p =
    let
      bad exception =
          return (Left exception)

      analyze =
          do
            value <- p
            return (Right value)

    in
      catch analyze bad



tryJust
    :: ( Exception e
       , Monad m
       )
    => (    e
         -> Maybe b
       )
    -> Pattern fs m a
    -> Pattern fs m (Either b a)

tryJust analyze p =
    do
      result <- try p
      let
        good value =
            return (Right value)

        bad exception =
            return (Left exception)

        handler e =
            let
              rethrow =
                  throw e

              analyzed =
                  analyze e

            in
              maybe rethrow bad analyzed

      either handler good result



onException
    :: Monad m
    => Pattern fs m a
    -> Pattern fs m b
    -> Pattern fs m a

onException p sequel =
    let
       onFailure e =
           do
             _ <- sequel
             throw (e :: SomeException)

    in
      p `catch` onFailure



finally
    :: Monad m
    => Pattern fs m a
    -> Pattern fs m b
    -> Pattern fs m a

finally p sequel =
    do
      result <- p `onException` sequel
      _ <- sequel
      return result



bracket
    :: forall fs m a b c.
       Monad m
    => Pattern fs m a
    -> (a -> Pattern fs m b)
    -> (a -> Pattern fs m c)
    -> Pattern fs m c

bracket acquire cleanup p =
    do
      resource <- acquire
      let
        computation =
            p resource

        after =
            cleanup resource

        handled =
            computation `onException` after

      result <- handled
      _ <- after
      return result



bracket_
    :: Monad m
    => Pattern fs m a
    -> Pattern fs m b
    -> Pattern fs m c
    -> Pattern fs m c

bracket_ acquire after computation =
    bracket
        acquire
        (const after)
        (const computation)



bracketOnError
    :: Monad m
    => Pattern fs m a
    -> (a -> Pattern fs m b)
    -> (a -> Pattern fs m c)
    -> Pattern fs m c

bracketOnError acquire cleanup p =
    do
      resource <- acquire
      let
        computation =
            p resource

        after =
            cleanup resource

      computation `onException` after



data Handler fs m a
  where

    Handler
        :: Exception e
        => (    e
             -> Pattern fs m a
           )
        -> Handler fs m a



instance Functor m
    => Functor (Handler fs m)
  where

    fmap f (Handler h) =
        Handler (fmap f . h)



catches
    :: Pattern fs m a
    -> [Handler fs m a]
    -> Pattern fs m a

catches p handlers =
    p `catch` catchesHandler handlers


handles
    :: [Handler fs m a]
    -> Pattern fs m a
    -> Pattern fs m a
handles =
    flip catches


catchesHandler
    :: [Handler fs m a]
    -> SomeException
    -> Pattern fs m a

catchesHandler handlers e =
    let
      rethrow =
          throw e

    in
      foldr tryHandler rethrow handlers
  where

    tryHandler (Handler handler) rest =
        case fromException e of

            Just err ->
                handler err

            Nothing ->
                rest

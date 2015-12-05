{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Core.Pattern.Exception
    ( throw
    , catch
    , handle
    , catchJust
    , handleJust
    , mapException
    , try
    , tryJust
    , onException
    , finally
    , bracket
    , bracket_
    , bracketOnError

    , Handler(..)
    , catches
    , handles
    ) where



import Ef.Core.Pattern

import Control.Exception
    ( Exception(..)
    , SomeException
    )



throw
    :: Exception e
    => e
    -> Pattern scope parent a

throw e =
    Fail (toException e)



catch
    :: ( Functor parent
       , Exception e
       )
    => Pattern scope parent a
    -> (e -> Pattern scope parent a)
    -> Pattern scope parent a

catch plan handler =
    rewrite plan
  where
    rewrite (Pure r) =
        Pure r

    rewrite (Super m) =
        Super (fmap rewrite m)

    rewrite (Send sym bp) =
        Send sym (rewrite . bp)


    rewrite (Fail se) =
        case fromException se of

            Just e ->
                handler e

            Nothing ->
                Fail se



handle
    :: ( Functor parent
       , Exception e
       )
    => (e -> Pattern scope parent a)
    -> Pattern scope parent a
    -> Pattern scope parent a

handle = flip catch



catchJust
    :: ( Functor parent
       , Exception e
       )
    => (e -> Maybe b)
    -> Pattern scope parent a
    -> (b -> Pattern scope parent a)
    -> Pattern scope parent a

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
    :: ( Functor parent
       , Exception e
       )
    => (e -> Maybe b)
    -> (b -> Pattern scope parent a)
    -> Pattern scope parent a
    -> Pattern scope parent a

handleJust p =
    flip (catchJust p)



mapException
    :: ( Functor parent
       , Exception e
       , Exception e'
       )
    => (e -> e')
    -> Pattern scope parent a
    -> Pattern scope parent a

mapException f p =
    let
      rethrow err =
        throw (f err)

    in
      handle rethrow p



try
    :: ( Monad parent
       , Exception e
       )
    => Pattern scope parent a
    -> Pattern scope parent (Either e a)

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
       , Monad parent
       )
    => (    e
         -> Maybe b
       )
    -> Pattern scope parent a
    -> Pattern scope parent (Either b a)

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
    :: Monad parent
    => Pattern scope parent a
    -> Pattern scope parent b
    -> Pattern scope parent a

onException p sequel =
    let
       onFailure e =
           do
             _ <- sequel
             throw (e :: SomeException)

    in
      p `catch` onFailure



finally
    :: Monad parent
    => Pattern scope parent a
    -> Pattern scope parent b
    -> Pattern scope parent a

finally p sequel =
    do
      result <- p `onException` sequel
      _ <- sequel
      return result



bracket
    :: forall scope parent a b c.
       Monad parent
    => Pattern scope parent a
    -> (a -> Pattern scope parent b)
    -> (a -> Pattern scope parent c)
    -> Pattern scope parent c

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
    :: Monad parent
    => Pattern scope parent a
    -> Pattern scope parent b
    -> Pattern scope parent c
    -> Pattern scope parent c

bracket_ acquire after computation =
    bracket
        acquire
        (const after)
        (const computation)



bracketOnError
    :: Monad parent
    => Pattern scope parent a
    -> (a -> Pattern scope parent b)
    -> (a -> Pattern scope parent c)
    -> Pattern scope parent c

bracketOnError acquire cleanup p =
    do
      resource <- acquire
      let
        computation =
            p resource

        after =
            cleanup resource

      computation `onException` after



data Handler scope parent a
  where

    Handler
        :: Exception e
        => (    e
             -> Pattern scope parent a
           )
        -> Handler scope parent a



instance Functor parent
    => Functor (Handler scope parent)
  where

    fmap f (Handler h) =
        Handler (fmap f . h)



catches
    :: Functor parent
    => Pattern scope parent a
    -> [Handler scope parent a]
    -> Pattern scope parent a

catches p handlers =
    p `catch` catchesHandler handlers


handles
    :: Functor parent
    => [Handler scope parent a]
    -> Pattern scope parent a
    -> Pattern scope parent a
handles =
    flip catches


catchesHandler
    :: [Handler scope parent a]
    -> SomeException
    -> Pattern scope parent a

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

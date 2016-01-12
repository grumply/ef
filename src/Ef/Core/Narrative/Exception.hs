{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Core.Narrative.Exception
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



import Ef.Core.Narrative

import Control.Exception
    ( Exception(..)
    , SomeException
    )



throw
    :: Exception e
    => e
    -> Narrative lexicon environment a

throw e =
    Fail (toException e)



catch
    :: ( Functor environment
       , Exception e
       )
    => Narrative lexicon environment a
    -> (e -> Narrative lexicon environment a)
    -> Narrative lexicon environment a

catch plan handler =
    rewrite plan
  where
    rewrite (Return r) =
        Return r

    rewrite (Super m) =
        Super (fmap rewrite m)

    rewrite (Say sym bp) =
        Say sym (rewrite . bp)


    rewrite (Fail se) =
        case fromException se of

            Just e ->
                handler e

            Nothing ->
                Fail se



handle
    :: ( Functor environment
       , Exception e
       )
    => (e -> Narrative lexicon environment a)
    -> Narrative lexicon environment a
    -> Narrative lexicon environment a

handle = flip catch



catchJust
    :: ( Functor environment
       , Exception e
       )
    => (e -> Maybe b)
    -> Narrative lexicon environment a
    -> (b -> Narrative lexicon environment a)
    -> Narrative lexicon environment a

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
    :: ( Functor environment
       , Exception e
       )
    => (e -> Maybe b)
    -> (b -> Narrative lexicon environment a)
    -> Narrative lexicon environment a
    -> Narrative lexicon environment a

handleJust p =
    flip (catchJust p)



mapException
    :: ( Functor environment
       , Exception e
       , Exception e'
       )
    => (e -> e')
    -> Narrative lexicon environment a
    -> Narrative lexicon environment a

mapException f p =
    let
      rethrow err =
        throw (f err)

    in
      handle rethrow p



try
    :: ( Monad environment
       , Exception e
       )
    => Narrative lexicon environment a
    -> Narrative lexicon environment (Either e a)

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
       , Monad environment
       )
    => (    e
         -> Maybe b
       )
    -> Narrative lexicon environment a
    -> Narrative lexicon environment (Either b a)

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
    :: Monad environment
    => Narrative lexicon environment a
    -> Narrative lexicon environment b
    -> Narrative lexicon environment a

onException p sequel =
    let
       onFailure e =
           do
             _ <- sequel
             throw (e :: SomeException)

    in
      p `catch` onFailure



finally
    :: Monad environment
    => Narrative lexicon environment a
    -> Narrative lexicon environment b
    -> Narrative lexicon environment a

finally p sequel =
    do
      result <- p `onException` sequel
      _ <- sequel
      return result



bracket
    :: forall lexicon environment a b c.
       Monad environment
    => Narrative lexicon environment a
    -> (a -> Narrative lexicon environment b)
    -> (a -> Narrative lexicon environment c)
    -> Narrative lexicon environment c

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
    :: Monad environment
    => Narrative lexicon environment a
    -> Narrative lexicon environment b
    -> Narrative lexicon environment c
    -> Narrative lexicon environment c

bracket_ acquire after computation =
    bracket
        acquire
        (const after)
        (const computation)



bracketOnError
    :: Monad environment
    => Narrative lexicon environment a
    -> (a -> Narrative lexicon environment b)
    -> (a -> Narrative lexicon environment c)
    -> Narrative lexicon environment c

bracketOnError acquire cleanup p =
    do
      resource <- acquire
      let
        computation =
            p resource

        after =
            cleanup resource

      computation `onException` after



data Handler lexicon environment a
  where

    Handler
        :: Exception e
        => (    e
             -> Narrative lexicon environment a
           )
        -> Handler lexicon environment a



instance Functor environment
    => Functor (Handler lexicon environment)
  where

    fmap f (Handler h) =
        Handler (fmap f . h)



catches
    :: Functor environment
    => Narrative lexicon environment a
    -> [Handler lexicon environment a]
    -> Narrative lexicon environment a

catches p handlers =
    p `catch` catchesHandler handlers


handles
    :: Functor environment
    => [Handler lexicon environment a]
    -> Narrative lexicon environment a
    -> Narrative lexicon environment a
handles =
    flip catches


catchesHandler
    :: [Handler lexicon environment a]
    -> SomeException
    -> Narrative lexicon environment a

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

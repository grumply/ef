{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
module Ef.Exception
    ( throw
    , catch
    , handle
    , catchJust
    , handleJust
    , mapException
    , try
    , tryJust
    , tryAny
    , onException
    , finally
    , bracket
    , bracket_
    , bracketOnError
      
    , SomeException(..)
    , Exception(..)

    , Handler(..)
    , catches
    , handles
    ) where



import Ef.Narrative

import Control.Exception
    ( Exception(..)
    , SomeException
    )



throw
    :: Exception e
    => e
    -> Narrative self super a

throw e =
    Fail (toException e)



catch
    :: ( Functor super
       , Exception e
       )
    => Narrative self super a
    -> (e -> Narrative self super a)
    -> Narrative self super a

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
    :: ( Functor super
       , Exception e
       )
    => (e -> Narrative self super a)
    -> Narrative self super a
    -> Narrative self super a

handle = flip catch



catchJust
    :: ( Functor super
       , Exception e
       )
    => (e -> Maybe b)
    -> Narrative self super a
    -> (b -> Narrative self super a)
    -> Narrative self super a

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
    :: ( Functor super
       , Exception e
       )
    => (e -> Maybe b)
    -> (b -> Narrative self super a)
    -> Narrative self super a
    -> Narrative self super a

handleJust p =
    flip (catchJust p)



mapException
    :: ( Functor super
       , Exception e
       , Exception e'
       )
    => (e -> e')
    -> Narrative self super a
    -> Narrative self super a

mapException f p =
    let
      rethrow err =
        throw (f err)

    in
      handle rethrow p



try
    :: ( Monad super
       , Exception e
       )
    => Narrative self super a
    -> Narrative self super (Either e a)

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


tryAny :: ( Monad super
          )
       => Narrative self super a
       -> Narrative self super (Either SomeException a)

tryAny = try



tryJust
    :: ( Exception e
       , Monad super
       )
    => (    e
         -> Maybe b
       )
    -> Narrative self super a
    -> Narrative self super (Either b a)

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
    :: Monad super
    => Narrative self super a
    -> Narrative self super b
    -> Narrative self super a

onException p sequel =
    let
       onFailure e =
           do
             _ <- sequel
             throw (e :: SomeException)

    in
      p `catch` onFailure



finally
    :: Monad super
    => Narrative self super a
    -> Narrative self super b
    -> Narrative self super a

finally p sequel =
    do
      result <- p `onException` sequel
      _ <- sequel
      return result



bracket
    :: forall self super a b c.
       Monad super
    => Narrative self super a
    -> (a -> Narrative self super b)
    -> (a -> Narrative self super c)
    -> Narrative self super c

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
    :: Monad super
    => Narrative self super a
    -> Narrative self super b
    -> Narrative self super c
    -> Narrative self super c

bracket_ acquire after computation =
    bracket
        acquire
        (const after)
        (const computation)



bracketOnError
    :: Monad super
    => Narrative self super a
    -> (a -> Narrative self super b)
    -> (a -> Narrative self super c)
    -> Narrative self super c

bracketOnError acquire cleanup p =
    do
      resource <- acquire
      let
        computation =
            p resource

        after =
            cleanup resource

      computation `onException` after



data Handler self super a
  where

    Handler
        :: Exception e
        => (    e
             -> Narrative self super a
           )
        -> Handler self super a



instance Functor super
    => Functor (Handler self super)
  where

    fmap f (Handler h) =
        Handler (fmap f . h)



catches
    :: Functor super
    => Narrative self super a
    -> [Handler self super a]
    -> Narrative self super a

catches p handlers =
    p `catch` catchesHandler handlers


handles
    :: Functor super
    => [Handler self super a]
    -> Narrative self super a
    -> Narrative self super a
handles =
    flip catches


catchesHandler
    :: [Handler self super a]
    -> SomeException
    -> Narrative self super a

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

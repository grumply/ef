{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Lang.Event
    where


import Ef.Core
import Ef.Core.Narrative
import Ef.Lang.IO

import Data.Typeable
import Unsafe.Coerce


data Events lexicon environment
    where

        Vacuous
            :: Events lexicon environment

        Culture
            :: Typeable a
            => [Behavior a lexicon environment]
            -> Events lexicon environment
            -> Events lexicon environment



data Behavior event lexicon environment
    where

        Behavior
            :: (Event lexicon environment -> event -> Narrative lexicon environment ())
            -> Behavior event lexicon environment


dispatch
    :: forall event lexicon environment.
       (Typeable event,Typeable environment,Typeable lexicon,Monad environment)
    => event
    -> Events lexicon environment
    -> Narrative lexicon environment ()

dispatch event =
    go
    where

        behavior = typeOf (undefined :: event -> Narrative lexicon environment ())

        go Vacuous =
            return ()

        go (Culture behaviors es) =
            case cast behaviors :: Maybe [Behavior event lexicon environment] of

                Just bs ->
                    run bs

                Nothing ->
                    go es

        run [] =
            return ()

        run (Behavior b:bs) =
            do
                b event
                run bs


main =
    let
        chb1 ch = io (putStr [ch])
        chb2 ch = io (putStrLn [ch])

        ib1 i = if i == (0 :: Int) then io (print i) else return ()
        ib2 i = if i == (1 :: Int) then io (print i) else return ()

        c1 = [Behavior chb1,Behavior chb2]

        c2 = [Behavior ib1,Behavior ib2]

        events = Culture c1 (Culture c2 Vacuous)

        obj = Object Empty

    in
        do
            obj $.
                (do
                    { dispatch 'a' events
                    ; dispatch (1 :: Int) events
                    }
                )

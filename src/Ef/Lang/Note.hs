{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Ef.Lang.Note
    ( Book(..)
    , notate

    , Action(..)
    , notated
    ) where



import Ef.Core.Narrative
import Ef.Lang.Knot

import Control.DeepSeq
import Control.Monad

import Data.Monoid

import Prelude hiding (log)



import Ef.Core.Object
import Ef.Lang.Knot.Context
import Ef.Lang.IO
import Ef.Core


data Action notes
    where

        Write
            :: notes
            -> Action notes

        Watch
            :: Action notes

        Finish
            :: Action notes



data Book notes lexicon environment =
    Book
        {
          write
              :: notes
              -> Narrative lexicon environment ()

        , watch
              :: forall result.
                 Narrative lexicon environment result
              -> Narrative lexicon environment (result,notes)

        , condense
              :: forall result.
                 Narrative lexicon environment (result,notes -> notes)
              -> Narrative lexicon environment result

        , watches
              :: forall result b.
                 (notes -> b)
              -> Narrative lexicon environment result
              -> Narrative lexicon environment (result,b)

        , edit
              :: forall result.
                 (notes -> notes)
              -> Narrative lexicon environment result
              -> Narrative lexicon environment result

       }



notated
    :: Knows Knots lexicon environment
    => (Book notes lexicon environment -> Narrative lexicon environment result)
    -> Knotted (Action notes) notes () X lexicon environment (result,notes)

notated computation =
    let
        notationInterface up =
            Book
                {
                  write =
                      \notes ->
                          do
                              _ <- up (Write notes)
                              return ()

                , watch =
                      \passage ->
                          do
                              _ <- up Watch
                              result <- passage
                              notes <- up Finish
                              _ <- up (Write notes)
                              return (result,notes)
                              
                , condense =
                      \passage ->
                          do
                              _ <- up Watch
                              (result,mod) <- passage
                              notes <- up Finish
                              let
                                  new = mod notes

                              _ <- up (Write new)
                              return result

                , watches =
                      \view passage ->
                          do
                              _ <- up Watch
                              result <- passage
                              notes <- up Finish
                              let
                                  b = view notes

                              return (result,b)

                , edit =
                      \change passage ->
                          do
                              _ <- up Watch
                              result <- passage
                              notes <- up Finish
                              let
                                  changed = change notes

                              _ <- up (Write changed)
                              return result

                }

    in
        knotted $ \up _ ->
            do
                result <- computation (notationInterface up)
                notes <- up Finish
                return (result,notes)



notate
    :: ( Knows Knots lexicon environment
       , Monoid notes
       )
    => (Book notes lexicon environment -> Narrative lexicon environment result)
    -> Narrative lexicon environment (result,notes)

notate computation =
    linearize (serve +>> notated computation)
    where

        serve firstRequest =
            knotted $ \_ dn ->
                withRespond dn mempty firstRequest
            where

                withRespond respond =
                    handle
                    where

                        handle current (Write notes) =
                            do
                                let
                                    new = current <> notes

                                next <- respond new
                                handle new next

                        handle current Finish =
                            do
                                next <- respond current
                                handle current next -- won't actually happen

                        handle current Watch =
                            do
                                let
                                    new = mempty

                                next <- respond new
                                watching (handle current) new next
                            where

                                watching continue current Watch =
                                    do
                                        let
                                            new = mempty

                                        next <- respond new
                                        watching (watching continue current) new next

                                watching continue current (Write notes) =
                                    do
                                        let
                                            new = current <> notes

                                        next <- respond new
                                        watching continue new next

                                watching continue current Finish =
                                    do
                                        next <- respond current
                                        continue next


{-# INLINE notated #-}
{-# INLINE notate #-}

main :: IO ()
main = do
    let
        obj = Object $ knots *:* Empty

    delta obj $
        do
            (result,notes) <- notate $ \Book{..} ->
                do
                    write "Message 1\n"
                    _ <- edit (const []) $
                        do
                            write "Message 2\n"
                            return ()
                    return ()
            io $ print notes
    return ()

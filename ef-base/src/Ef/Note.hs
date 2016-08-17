module Ef.Note
    ( Book(..)
    , notate

    , notated
    ) where



import Ef.Narrative
import Ef.Sync

import Control.Monad

import Data.Monoid

import Prelude hiding (log)


data Action notes
    where

        Write
            :: notes
            -> Action notes

        Watch
            :: Action notes

        Finish
            :: Action notes



data Book notes self super =
    Book
        {
          write
              :: notes
              -> Narrative self super ()

        , watch
              :: forall result.
                 Narrative self super result
              -> Narrative self super (result,notes)

        , condense
              :: forall result.
                 Narrative self super (result,notes -> notes)
              -> Narrative self super result

        , watches
              :: forall result b.
                 (notes -> b)
              -> Narrative self super result
              -> Narrative self super (result,b)

        , edit
              :: forall result.
                 (notes -> notes)
              -> Narrative self super result
              -> Narrative self super result

       }



notated
    :: ('[Sync] <: self, Monad super)
    => (Book notes self super -> Narrative self super result)
    -> Synchronized (Action notes) notes () X self super (result,notes)

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
        synchronized $ \up _ ->
            do
                result <- computation (notationInterface up)
                notes <- up Finish
                return (result,notes)



notate
    :: ( '[Sync] <: self
       , Monad super
       , Monoid notes
       )
    => (Book notes self super -> Narrative self super result)
    -> Narrative self super (result,notes)

notate computation =
    runSync (serve +>> notated computation)
    where

        serve firstRequest =
            synchronized $ \_ dn ->
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

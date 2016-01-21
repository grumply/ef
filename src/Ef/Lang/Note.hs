{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Log
    ( Log(..)
    , log
    ) where


import Ef.Core.Narrative
import Ef.Lang.Knot

import Control.DeepSeq
import Control.Monad

import Prelude hiding (log)



data Eagerness
    where

        Strict
            :: Eagerness

        Lazy
            :: Eagerness

    deriving Eq



data Action notes
    where

        Note
            :: notes
            -> Action notes

        Amend
            :: (notes -> notes)
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

        , amend
              :: (notes -> notes)
              -> Narrative lexicon environment ()

        , watch
              :: forall result.
                 Narrative lexicon environment result
              -> Narrative lexicon environment (notes,result)
        }



notated
    :: Knows Knots lexicon environment
    => (Book notes lexicon environment -> Narrative lexicon environment result)
    -> Knotted (Action notes) notes () X lexicon environment (notes,result)

notated computation =
    let
        notationInterface up =
            Book
                {
                  write =
                      \notes ->
                          _

                , amend =
                      \mod ->
                          _

                , watch =
                      \watched ->
                          _
                }

    in
        knotted $ \up _ ->
            do
                result <- computation (notationInterface up)
                notes <- up Finish
                return (result,notes)

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.Try
    ( Try(..)
    , tries
    ) where



import Ef.Core

import Ef.Lang.Scoped.Exit



data Try a scope parent =
    Try
        {
          success
              :: forall b.
                 a
              -> Pattern scope parent b
        , failure
              :: forall b.
                 Pattern scope parent b
        }



tries
    :: Is Exiting scope parent
    => (    Try result scope parent
         -> Pattern scope parent (Maybe result)
       )
    -> Pattern scope parent (Maybe result)

tries f =
    exits $ \Exit{..} -> f
        Try
            {
              success =
                  \a ->
                      exit (Just a)

            , failure =
                  exit Nothing
            }

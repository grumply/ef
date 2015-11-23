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



data Try a fs m =
    Try
        {
          success
              :: forall b.
                 a
              -> Pattern fs m b
        , failure
              :: forall b.
                 Pattern fs m b
        }



tries
    :: Is Exiting fs m
    => (    Try a fs m
         -> Pattern fs m (Maybe a)
       )
    -> Pattern fs m (Maybe a)

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

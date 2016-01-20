{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Lang.Variable
    ( Variable(..)
    ) where



import Ef.Core.Narrative
import Ef.Lang.IO
import Ef.Lang.Knot

import Control.Monad
import Data.IORef
import Unsafe.Coerce



data Eagerness
    where

        Strict
            :: Eagerness

        Lazy
            :: Eagerness

    deriving Eq



data Modify
    where

        Modify
            :: Eagerness
            -> (state -> state)
            -> Modify

data Variable var lexicon environment =
    Variable
        {
          modify
              :: (var -> var)
              -> Narrative lexicon environment ()

        , modify'
              :: (var -> var)
              -> Narrative lexicon environment ()

        , get
              :: Narrative lexicon environment var

        , gets
              :: forall a.
                 (var -> a)
              -> Narrative lexicon environment var

        , put
              :: var
              -> Narrative lexicon environment ()

        , puts
              :: forall a.
                 (a -> var)
              -> a
              -> Narrative lexicon environment ()

        , swap
              :: var
              -> Narrative lexicon environment var
        }



state
    :: forall lexicon environment state a.
       ( Knows Knots lexicon environment
       , Lift IO environment
       )
    => state
    -> (    Variable state lexicon environment
         -> Narrative lexicon environment a
       )
    -> Narrative lexicon environment a

state initial stateful =
        do
            ref <- io (newIORef initial)
            linearize $
                (serve ref) +>> consume

    where

        serve ref = go
            where

                go =
                    knotted $ \_ dn ->
                        do
                            current <- io $
                                do
                                    current <- readIORef ref
                                    let
                                        new =
                                            unsafeCoerce mod current

                                        force =
                                            new `seq` return ()

                                    when (strictness == Strict) force
                                    writeIORef ref new
                                    return new
                            dn 

        consume =
            let
                stateInterface
                    :: (Modify -> Narrative lexicon environment state)
                    -> Variable state lexicon environment

                stateInterface up =
                    Variable
                        {
                          modify =
                              \mod ->
                                  do
                                      _ <- up (Modify Lazy mod)
                                      return ()

                             
                        }

            in
                knotted $ \up _ -> stateful (stateInterface up)

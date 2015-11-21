{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Ef.Lang.Scoped.Guard
    ( Guarding
    , guards
    , Guardable
    , guarder
    , Guard(..)
    ) where

import Ef.Core

import Data.Foldable
import Unsafe.Coerce

-- | Symbol

data Guarding k
    = FreshScope (Int -> k)

    | forall a. Choose Int [a] (a -> k)

    | Cut Int

-- | Symbol Module

data Guard fs m =
    Guard
        {
          choose
              :: forall f a.
                 Foldable f
              => f a
              -> Pattern fs m a

        , cut :: forall b.
                 Pattern fs m b
        }

-- | Attribute

data Guardable k =
    Guardable Int k

-- | Attribute Construct

guarder
    :: Uses Guardable fs m
    => Attribute Guardable fs m
guarder =
    Guardable 0 $ \fs ->

        let Guardable i k =
                view fs

        in pure $ fs .=
               Guardable (succ i) k

-- | Symbol/Attribute Symmetry

instance Symmetry Guardable Guarding where
    symmetry use (Guardable i k) (FreshScope ik) =
        use k (ik i)

-- | Local Scoping Construct

guards
    :: forall fs m a.
       (Is Guarding fs m)
    => (    Guard fs m
         -> Pattern fs m a
       )
    -> Pattern fs m (Maybe a)
guards l = do

  scope <- self (FreshScope id)

  go scope $
      l Guard
          { choose = \as -> self (Choose scope (toList as) id)
          , cut = self (Cut scope)
          }
  where

    go scope p0 =
        start p0
      where

        start (Step sym bp) =
            let
              ignore =
                  Step sym (start . bp)

              check i scoped =
                  if i == scope then
                      scoped
                  else
                      ignore
            in
              case prj sym of

                  Just x ->
                      case x of

                          Choose i as _ ->
                              check i $
                                  try
                                      as
                                      (unsafeCoerce bp)
                                      (return Nothing)

                          _ ->
                              ignore

                  Nothing ->
                      Step sym (start . bp)

        start (M m) =
            M (fmap start m)

        start (Pure result) =
            return (Just result)

        try [] _ alt =
            alt

        try (a:as) bp alt =
            try' as alt bp (bp a)

        try' as alt bp (Step sym bp') =
            let
              ignore =
                  Step sym (try' as alt bp . bp')

              check i scoped =
                  if i == scope then
                      scoped
                  else
                      ignore
            in
              case prj sym of

                  Just x ->
                      case x of

                          Choose i as' _ ->
                              check i $
                                  try
                                      (unsafeCoerce as')
                                      (unsafeCoerce bp')
                                      (try as bp alt)

                          Cut i ->
                              check i $
                                  try as bp alt

                          _ ->
                              ignore

                  Nothing ->
                      ignore

        try' as alt bp (M m) =
            let
              nextTry = try' as alt bp
            in
              M (fmap nextTry m)

        try' _ _ _ (Pure result) =
            return result

-- | Inlines

{-# INLINE guarder #-}
{-# INLINE guards #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.Log
    ( Logging
    , logger
    , Loggable
    , logs
    , Log(..)
    , intercept
    ) where



import Ef.Core

import Data.Monoid
import Prelude hiding (log)

import Unsafe.Coerce



-- | Symbol

data Logging k
  where

    FreshScope
        :: (Int -> k)
        -> Logging k

    Logs
        :: Int
        -> a
        -> Logging k

    Eavesdrop
        :: Int
        -> w
        -> Pattern fs m a
        -> Logging k

    Reconfigure
        :: Int
        -> (a -> w -> w)
        -> Logging k




-- | Symbol Module

data Log fs m a w =
    Log
        { log
              :: a
              -> Pattern fs m ()

        , eavesdrop
              :: forall r.
                 w
              -> Pattern fs m r
              -> Pattern fs m (w,r)

        , reconfigure
              :: (a -> w -> w)
              -> Pattern fs m ()
        }



-- | Attribute

data Loggable k where
    Loggable :: Int -> k -> Loggable k



-- | Attribute Construct

logger
    :: Uses Loggable fs m
    => Attribute Loggable fs m
logger =
    Loggable 0 $ \fs ->
        let
          Loggable i k =
              view fs

          i' =
              succ i

        in
          i' `seq` pure $ fs .=
              Loggable i' k



-- | Symbol/Attribute Symmetry

instance Witnessing Loggable Logging
  where

    witness use (Loggable i k) (FreshScope ik) =
        use k (ik i)



-- | Local Scoping Construct + Substitution

logs
    :: forall fs m a w r.
       ( Is Logging fs m
       , Monoid w
       )
    => (a -> w -> w)
    -> w
    -> (    Log fs m a w
         -> Pattern fs m r
       )
    -> Pattern fs m (w,r)
logs c0 w0 f =
    do
      scope <- self (FreshScope id)
      rewrite scope c0 w0 $ f
          Log
              { log =
                    \w ->
                        self (Logs scope w)

              , eavesdrop =
                    \w' p ->
                        self (Eavesdrop scope w' p)

              , reconfigure =
                    \c' ->
                        self (Reconfigure scope c')
              }
  where
    rewrite _ _ w (Pure r) =
        return (w,r)

    rewrite scope c w (M m) =
        let
          continue = rewrite scope c w
        in
          M (fmap continue m)

    rewrite scope c w (Step sym bp) =
        let
          same =
              rewrite scope c w . bp

          ignore =
              Step sym same

          check i scoped =
              if i == scope then
                  scoped
              else
                  ignore

          continue =
              rewrite scope

        in
          case prj sym of

            Just x ->
                case x of

                    Logs i a ->
                        check i $
                            rewrite
                                 scope
                                 c
                                 (c (unsafeCoerce a) w)
                                 (bp (unsafeCoerce ()))

                    Eavesdrop i w' p' ->
                        check i $
                          do
                            ~(w'',r) <- continue
                                            c
                                            (unsafeCoerce w')
                                            (unsafeCoerce p')
                            let
                              result =
                                unsafeCoerce (w'',r)

                            continue
                                c
                                (w <> w'')
                                (bp result)

                    Reconfigure i c' ->
                        check i $
                            continue
                                (unsafeCoerce c')
                                (unsafeCoerce w)
                                (bp (unsafeCoerce ()))

                    _ ->
                        ignore

            _ ->
                ignore



-- | Extended API

intercept
    :: Monad m
    => Log fs m a w
    -> (w -> b)
    -> w
    -> Pattern fs m r
    -> Pattern fs m (b,r)
intercept Log{..} f w0 m =
    do
      ~(w, a) <- eavesdrop w0 m
      return (f w,a)

-- | Inlines

{-# INLINE logger #-}
{-# INLINE logs #-}
{-# INLINE intercept #-}

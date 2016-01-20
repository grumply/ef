{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Data.Binary
import Data.Monoid
import Prelude hiding (log)
import Unsafe.Coerce



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
        -> Pattern scope parent a
        -> Logging k

    Reconfigure
        :: Int
        -> (a -> w -> w)
        -> Logging k



data Log scope parent a w =
    Log
        { log
              :: a
              -> Pattern scope parent ()

        , eavesdrop
              :: forall r.
                 w
              -> Pattern scope parent r
              -> Pattern scope parent (w,r)

        , reconfigure
              :: (a -> w -> w)
              -> Pattern scope parent ()
        }



data Loggable k
  where

    Loggable
        :: Int
        -> k
        -> Loggable k



instance Uses Loggable attrs parent
    => Binary (Attribute Loggable attrs parent)
  where

    get =
        return logger



    put _ =
        pure ()



logger
    :: Uses Loggable attrs parent
    => Attribute Loggable attrs parent

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



instance Witnessing Loggable Logging
  where

    witness use (Loggable i k) (FreshScope ik) =
        use k (ik i)



logs
    :: forall scope parent a w r.
       ( Is Logging scope parent
       , Monoid w
       )
    => (a -> w -> w)
    -> w
    -> (    Log scope parent a w
         -> Pattern scope parent r
       )
    -> Pattern scope parent (w,r)
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

    rewrite _ _ _ (Fail e) =
        Fail e

    rewrite _ _ w (Pure r) =
        Pure (w,r)

    rewrite scope c w (Super m) =
        let
          continue = rewrite scope c w
        in
          Super (fmap continue m)

    rewrite scope c w (Send sym bp) =
        let
          same =
              rewrite scope c w . bp

          ignore =
              Send sym same

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
    :: Monad parent
    => Log scope parent a w
    -> (w -> b)
    -> w
    -> Pattern scope parent r
    -> Pattern scope parent (b,r)
intercept Log{..} f w0 m =
    do
      ~(w, a) <- eavesdrop w0 m
      return (f w,a)

-- | Inlines

{-# INLINE logger #-}
{-# INLINE logs #-}
{-# INLINE intercept #-}

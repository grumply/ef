{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.Notate
    ( Notating
    , notates
    , Notatable
    , notator
    , Notes
    , tell
    , listen
    , listens
    ) where



import Ef.Core

import Data.Binary
import Data.Monoid
import Unsafe.Coerce



data Notating k
  where

    FreshScope
        :: (    Int
             -> k
           )
        -> Notating k

    Tell
        :: Int
        -> a
        -> Notating k

    Listen
        :: Int
        -> Pattern scope parent a
        -> Notating k



data Notes notes scope parent =
    Notes
        {
          tell
              :: notes
              -> Pattern scope parent ()

        , listen
              :: forall result.
                 Pattern scope parent result
              -> Pattern scope parent (notes,result)
        }



data Notatable k
  where

    Notatable
        :: Int
        -> k
        -> Notatable k



instance Uses Notatable attrs parent
    => Binary (Attribute Notatable attrs parent)
  where

    get =
        return notator



    put _ =
        pure ()



notator
    :: Uses Notatable attrs parent
    => Attribute Notatable attrs parent

notator =
    Notatable 0 $ \fs ->
        let
          Notatable i k =
              view fs

          i' =
              succ i

        in
          i' `seq` pure $ fs .=
              Notatable i' k



instance Notatable `Witnessing` Notating
  where

    witness use (Notatable i k) (FreshScope ik) =
        use k (ik i)



notates
    :: forall scope parent notes result.
       ( Is Notating scope parent
       , Monoid notes
       )
    => (    Notes notes scope parent
         -> Pattern scope parent result
       )
    -> Pattern scope parent (notes,result)

notates f =
    do
      scope <- self (FreshScope id)
      rewrite scope mempty $ f
          Notes
              {
                tell =
                    \w ->
                        self (Tell scope w)

              , listen =
                    \p ->
                        self (Listen scope p)
              }



rewrite rewriteScope =
    withNotations
  where

    withNotations notations =
        go
      where

        go (Fail err) =
            Fail err

        go (Pure result) =
            Pure (notations,result)

        go (Super m) =
            Super (fmap go m)

        go (Send sym bp) =
            let
              check currentScope scoped =
                  if currentScope == rewriteScope then
                      scoped
                  else
                      ignore

              ignore =
                  Send sym (go . bp)

            in
              case prj sym of

                  Just x ->
                      case x of

                          Tell currentScope notation ->
                              check currentScope $
                                  let
                                    newNotations =
                                        notations <> (unsafeCoerce notation)

                                    continue =
                                        bp (unsafeCoerce ())

                                  in
                                    withNotations newNotations continue

                          Listen currentScope listenable ->
                              check currentScope $
                                  do
                                    result <-
                                        withNotations
                                            mempty
                                            (unsafeCoerce listenable)

                                    let
                                      combinedNotations =
                                          notations <> (fst result)

                                      continue =
                                          bp (unsafeCoerce result)

                                    withNotations combinedNotations continue

                          _ ->
                              ignore

                  _ ->
                      ignore




listens
    :: Monad parent
    => Notes notes scope parent
    -> (notes -> b)
    -> Pattern scope parent result
    -> Pattern scope parent (b,result)

listens Notes{..} f m =
    do
      ~(w, a) <- listen m
      return (f w,a)


{-# INLINE rewrite #-}
{-# INLINE notates #-}
{-# INLINE listens #-}
{-# INLINE notator #-}

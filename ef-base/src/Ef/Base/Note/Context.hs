{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Ef.Context.Note.Context
    ( Notes(..)
    , notator
    , writer
    , noted
    ) where



import Ef.Core.Object
import Ef.Context.Note.Attribute

import qualified Data.Binary as B
import Data.Monoid



instance ( B.Binary r
         , Monoid r
         , Has (Notes r) contexts environment
         )
    => B.Binary (Notes r (Morphism contexts environment))
  where

    get =
        do
          r <- B.get
          let
            Notes _ rk = writer

          return (Notes r rk)

    put (Notes r _) =
        B.put r



notator
    :: w
    -> (w -> w -> w)
    -> Use (Notes w) contexts environment

notator w0 f =
    Notes w0 $ \w' fs ->
        let
          Notes w k =
              view fs

        in pure $ fs .=
               Notes (f w w') k



writer
    :: Monoid w
    => Use (Notes w) attrs environment

writer =
    notator mempty (<>)



noted
    :: Has (Notes w) contexts environment
    => Object contexts environment -> w

noted fs =
    let
      Notes w _ =
          view fs

    in
      w

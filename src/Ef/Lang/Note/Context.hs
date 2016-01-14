{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Ef.Lang.Note.Context
    ( Attribute(..)
    , notator
    , writer
    , noted
    ) where



import Ef.Core.Object
import Ef.Lang.Note.Attribute

import qualified Data.Binary as B
import Data.Monoid



instance ( B.Binary r
         , Monoid r
         , Notes r contexts environment
         )
    => B.Binary (Attribute r (Morphism contexts environment))
  where

    get =
        do
          r <- B.get
          let
            Noter _ rk = writer

          return (Noter r rk)

    put (Noter r _) =
        B.put r



notator
    :: w
    -> (w -> w -> w)
    -> Use (Attribute w) contexts environment

notator w0 f =
    Noter w0 $ \w' fs ->
        let
          Noter w k =
              view fs

        in pure $ fs .=
               Noter (f w w') k



writer
    :: Monoid w
    => Use (Attribute w) attrs environment

writer =
    notator mempty (<>)




noted
    :: Notes w contexts environment
    => Object contexts environment -> w

noted fs =
    let
      Noter w _ =
          view fs

    in
      w

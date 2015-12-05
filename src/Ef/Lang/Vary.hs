{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Vary
    ( Varying
    , get
    , gets
    , put
    , puts
    , swap
    , modify
    , modify'

    , Variable
    , store
    ) where



import Ef.Core

import qualified Data.Binary as Binary



data Varying st k
  where

    Modify
        :: (st -> st)
        -> (st -> k)
        -> Varying st k


data Variable st k
  where

    Variable
        :: st
        -> (st -> k)
        -> Variable st k


instance ( Uses (Variable st) attrs parent
         , Binary.Binary st
         )
    => Binary.Binary (Attribute (Variable st) attrs parent)
  where

    get =
        do
          st <- Binary.get
          return (store st)

    put (Variable st _) =
        Binary.put st



instance (Variable st) `Witnessing` (Varying st)
  where

    witness use (Variable st k) (Modify stst stk) =
        let
          st' =
              stst st

        in
          witness use (st,k st') stk




get
    :: Is (Varying st) scope parent
    => Pattern scope parent st

get =
    self (Modify id id)



gets
    :: Is (Varying st) scope parent
    => (st -> result)
    -> Pattern scope parent result

gets f =
    self (Modify id f)



put
    :: Is (Varying st) scope parent
    => st
    -> Pattern scope parent ()

put st =
    self (Modify (const st) (const ()))



puts
    :: Is (Varying st) scope parent
    => (a -> st)
    -> a
    -> Pattern scope parent ()

puts f a =
    self (Modify (const (f a)) (const ()))



swap
    :: Is (Varying st) scope parent
    => st
    -> Pattern scope parent st

swap st =
    self (Modify (const st) id)



modify
    :: Is (Varying st) scope parent
    => (st -> st)
    -> Pattern scope parent ()

modify f =
    self (Modify f (const ()))



modify'
    :: Is (Varying st) scope parent
    => (st -> st)
    -> Pattern scope parent ()

modify' f =
    do
      st <- get
      put $! f st



store
    :: Uses (Variable st) scope parent
    => st
    -> Attribute (Variable st) scope parent

store startSt =
    Variable startSt $ \newSt fs -> pure $ fs .=
        store newSt



{-# INLINE put #-}
{-# INLINE get #-}
{-# INLINE gets #-}
{-# INLINE puts #-}
{-# INLINE swap #-}
{-# INLINE modify #-}
{-# INLINE modify' #-}
{-# INLINE store #-}

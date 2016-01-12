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


instance ( Admits' (Variable st) attrs (IndexOf (Variable st) attrs)
         , Monad environment
         , Binary.Binary st
         )
    => Binary.Binary (Attribute (Variable st) attrs environment)
  where

    get =
        do
          st <- Binary.get
          return (store st)

    put (Variable st _) =
        Binary.put st



instance (Variable st) `Inflection` (Varying st)
  where

    inflect use (Variable st k) (Modify stst stk) =
        let
          st' =
              stst st

        in
          inflect use (st,k st') stk




get
    :: Method (Varying st) lexicon environment st

get =
    say (Modify id id)



gets
    :: (st -> result)
    -> Method (Varying st) lexicon environment result

gets f =
    say (Modify id f)



put
    :: st
    -> Method (Varying st) lexicon environment ()

put st =
    say (Modify (const st) (const ()))



puts
    :: (a -> st)
    -> a
    -> Method (Varying st) lexicon environment ()

puts f a =
    say (Modify (const (f a)) (const ()))



swap
    :: st
    -> Method (Varying st) lexicon environment st

swap st =
    say (Modify (const st) id)



modify
    :: (st -> st)
    -> Method (Varying st) lexicon environment ()

modify f =
    say (Modify f (const ()))



modify'
    :: (st -> st)
    -> Method (Varying st) lexicon environment ()

modify' f =
    do
      st <- get
      put $! f st



store
    :: st
    -> Uses (Variable st) lexicon environment

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

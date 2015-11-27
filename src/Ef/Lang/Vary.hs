{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , storeAttr
    , storeObj
    ) where



import Ef.Core



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



instance (Variable st) `Witnessing` (Varying st)
  where

    witness use (Variable st k) (Modify stst stk) =
        let
          st' =
              stst st

        in
          witness use (st,k st') stk




get
    :: Is (Varying st) fs m
    => Pattern fs m st
get =
    self (Modify id id)



gets
    :: Is (Varying st) fs m
    => (st -> a)
    -> Pattern fs m a

gets f =
    self (Modify id f)



put
    :: Is (Varying st) fs m
    => st
    -> Pattern fs m ()

put st =
    self (Modify (const st) (const ()))



puts
    :: Is (Varying st) fs m
    => (a -> st)
    -> a
    -> Pattern fs m ()

puts f a =
    self (Modify (const (f a)) (const ()))



swap
    :: Is (Varying st) fs m
    => st
    -> Pattern fs m st

swap st =
    self (Modify (const st) id)



modify
    :: Is (Varying st) fs m
    => (st -> st)
    -> Pattern fs m ()

modify f =
    self (Modify f (const ()))



modify'
    :: Is (Varying st) fs m
    => (st -> st)
    -> Pattern fs m ()

modify' f =
    do
      st <- get
      put $! f st



storeAttr
    :: Uses (Variable st) fs m
    => st
    -> Attribute (Variable st) fs m

storeAttr st0 =
    Variable st0 $ \a fs -> pure $ fs .=
        storeAttr a



storeObj
    :: Monad m
    => st
    -> Object '[Variable st] m

storeObj st =
    Object (storeAttr st *:* Empty)



{-# INLINE put #-}
{-# INLINE get #-}
{-# INLINE gets #-}
{-# INLINE puts #-}
{-# INLINE swap #-}
{-# INLINE modify #-}
{-# INLINE modify' #-}
{-# INLINE storeAttr #-}
{-# INLINE storeObj #-}

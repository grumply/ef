{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AutoDeriveTypeable #-}
module Ef.Data.Queue
    ( Queue
    , Binary(..)
    , emptyQueue
    , newQueue
    , enqueue
    , dequeue
    , append
    , toList
    ) where



import Data.Binary
import Data.List



data Queue a
  where

    Queue
        :: [a]
        -> [a]
        -> Queue a

  deriving (Functor,Eq,Ord,Show)



instance Binary a
    => Binary (Queue a)
  where

    get =
        Queue <$> get <*> get

    put (Queue l r) =
        put l >> put r



emptyQueue
    :: Queue a

emptyQueue =
    Queue [] []



newQueue
    :: [a]
    -> Queue a

newQueue stack =
    Queue stack []



enqueue
    :: a
    -> Queue a
    -> Queue a

enqueue a (Queue l r) =
    Queue l (a:r)



dequeue
    :: Queue a
    -> Maybe (a,Queue a)

dequeue (Queue [] []) =
    Nothing

dequeue (Queue [] xs) =
    let
      (frst:rest) =
          foldr_reverse xs

    in
      Just (frst,Queue rest [])

dequeue (Queue (frst:rest) ys) =
    let
      newQueue =
          Queue rest ys

    in
      Just (frst,newQueue)



foldr_append
    :: [a]
    -> [a]
    -> [a]

foldr_append xs ys =
    foldr (:) ys xs



foldr_reverse
    :: [a]
    -> [a]

foldr_reverse as =
    foldr (\a cont rest -> cont (a:rest)) id as []



append
    :: Queue a
    -> Queue a
    -> Queue a

append (Queue xs ys) (Queue xs' ys') =
    let
      end =
          foldr_append xs' (foldr_reverse ys')

      begin =
          foldr_append xs (foldr_reverse ys)

      new =
          foldr_append begin end

    in
      newQueue new



toList
    :: Queue a
    -> [a]

toList =
    unfoldr dequeue



-- | Inlines

{-# INLINE newQueue #-}
{-# INLINE emptyQueue #-}
{-# INLINE enqueue #-}
{-# INLINE dequeue #-}
{-# INLINE append #-}
{-# INLINE toList #-}
{-# INLINE foldr_append #-}
{-# INLINE foldr_reverse #-}

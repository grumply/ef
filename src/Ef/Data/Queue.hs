{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AutoDeriveTypeable #-}
module Ef.Data.Queue where



import Data.Binary



data Queue a
  where

    Queue
        :: [a]
        -> [a]
        -> Queue a

  deriving (Functor,Eq,Ord)



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
    -> Maybe (Queue a,a)

dequeue (Queue [] []) =
    Nothing

dequeue (Queue [] xs) =
    let
      stack =
          reverse xs

      rest =
          tail stack

      frst =
          head stack

    in
      Just (Queue rest [],frst)

dequeue (Queue xs ys) =
    let
      rest =
          tail xs

      frst =
          head xs

    in
      Just (Queue rest ys,frst)

-- | Inlines

{-# INLINE newQueue #-}
{-# INLINE emptyQueue #-}
{-# INLINE enqueue #-}
{-# INLINE dequeue #-}

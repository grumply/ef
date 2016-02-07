{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.State.Messages
    ( State(..)
    , get
    , gets
    , put
    , puts
    , swap
    , modify
    , modify'
    ) where


import Ef.Narrative


data State st k = Modify (st -> st) (st -> k)


get :: Invoke (State st) self super st
get = self (Modify id id)


gets :: (st -> result) -> Invoke (State st) self super result
gets f = self (Modify id f)


put :: st -> Invoke (State st) self super ()
put st = self (Modify (const st) (const ()))


puts :: (a -> st) -> a -> Invoke (State st) self super ()
puts f a = self (Modify (const (f a)) (const ()))


swap :: st -> Invoke (State st) self super st
swap st = self (Modify (const st) id)


modify :: (st -> st) -> Invoke (State st) self super ()
modify f = self (Modify f (const ()))


modify' :: (st -> st) -> Invoke (State st) self super ()
modify' f = self (Modify (\x -> let x' = f x in x' `seq` x') (const ()))


{-# INLINE put #-}
{-# INLINE get #-}
{-# INLINE gets #-}
{-# INLINE puts #-}
{-# INLINE swap #-}
{-# INLINE modify #-}
{-# INLINE modify' #-}

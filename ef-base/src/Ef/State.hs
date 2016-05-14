module Ef.State
    ( State
    , state
    , get
    , gets
    , put
    , puts
    , swap
    , modify
    ) where

import Ef

data State st k
  = State !st (st -> k)
  | Modify (st -> st) (st -> k)

instance Ma (State st) (State st) where
  ma use (State st stk) (Modify stst stk') =
    use (stk (stst st)) (stk' st)

state :: (Monad super, '[State st] .> traits)
      => st -> Trait (State st) traits super
state initialState = State initialState $ \new fs ->
  return $! (fs .=) $! state new
{-# INLINE state #-}

get :: (Monad super, '[State st] :> self)
    => Narrative self super st
get = self (Modify id id)
{-# INLINE get #-}

gets :: (Monad super, '[State st] :> self)
     => (st -> result) -> Narrative self super result
gets f = self (Modify id f)
{-# INLINE gets #-}

put :: (Monad super, '[State st] :> self)
    => st -> Narrative self super ()
put st = self (Modify (const st) (const ()))
{-# INLINE put #-}

puts :: (Monad super, '[State st] :> self)
     => (a -> st) -> a -> Narrative self super ()
puts f a = self (Modify (const (f a)) (const ()))
{-# INLINE puts #-}

swap :: (Monad super, '[State st] :> self)
     => st -> Narrative self super st
swap st = self (Modify (const st) id)
{-# INLINE swap #-}

modify :: (Monad super, '[State st] :> self)
       => (st -> st) -> Narrative self super ()
modify f = self (Modify f (const ()))
{-# INLINE modify #-}

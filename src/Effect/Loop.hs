{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Loop
  ( loop
  , loops, LoopHandler(..)
  ) where

import Mop
import Data.Function
import Unsafe.Coerce

data Loop k
  = FreshScope (Integer -> k)
  | forall s. Continue Integer s
  | forall a. Break Integer a

data LoopHandler k = LoopHandler Integer k

freshScope :: Has Loop fs m => Plan fs m Integer
freshScope = symbol (FreshScope id)

loop :: forall fs m st a. Has Loop fs m => st -> ((forall b. a -> Plan fs m b) -> (forall b. st -> Plan fs m b) -> st -> Plan fs m a) -> Plan fs m a
loop s0 x = do
    scope <- freshScope
    let break a = symbol (Break scope a)
        continue st = symbol (Continue scope st)
        loopBody restart st =
          transform restart scope (x break continue st)
    fix loopBody s0
  where
    transform restart scope =
      mapStep $ \go (Step syms bp) ->
        case prj syms of
          Just l ->
            case l of
              Continue i s ->
                if i == scope
                then restart (unsafeCoerce s)
                else Step syms (\b -> go (bp b))
              Break i a ->
                if i == scope
                then Pure (unsafeCoerce a)
                else Step syms (\b -> go (bp b))
              _ ->   Step syms (\b -> go (bp b))
          Nothing -> Step syms (\b -> go (bp b))

loops :: Uses LoopHandler gs m => Instruction LoopHandler gs m
loops = LoopHandler 0 $ \fs ->
  let LoopHandler i k = view fs
  in instruction (LoopHandler (succ i) k) fs

instance Pair LoopHandler Loop where
  pair p (LoopHandler i k) (FreshScope ik) = p k (ik i)
  pair _ _ _ = error "Unscoped looping construct."

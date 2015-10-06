{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Continuation where

import Mop
import Effect.Fresh
import Unsafe.Coerce

import Debug.Trace

data Cont k = forall a. Cont Int a
data ContHandler k = ContHandler k

callCC :: forall fs m a. (Has Cont fs m,Has (Fresh Int) fs m)
       => ((forall b. a -> Plan fs m b) -> Plan fs m a) -> Plan fs m a
callCC x = do
    f <- fresh
    transform f $ x (\a -> symbol (Cont f a))
  where
    transform :: Int -> Plan fs m a -> Plan fs m a
    transform f =
      mapStep $ \go (Step syms bp) ->
        case prj syms of
          Just (Cont i a) ->
            if i == f
            then Pure (unsafeCoerce a)
            else Step syms (\b -> go (bp b))
          _ -> Step syms (\b -> go (bp b))


cont :: Monad m => ContHandler (k -> m k)
cont = ContHandler return

instance Pair ContHandler Cont

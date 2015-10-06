{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Effect.Try where

import Mop
import Effect.Fresh
import Unsafe.Coerce

-- Try implements short-circuiting plans with success and non-specific failure.

data Try k
  = forall a. Success Int a
  | Failure Int

data TryHandler k = TryHandler k

tries :: Monad m => TryHandler (k -> m k)
tries = TryHandler return

try :: forall fs m a. (Has Try fs m,Has (Fresh Int) fs m)
    => ((forall b. a -> Plan fs m b) -> (forall b. Plan fs m b) -> Plan fs m (Maybe a)) -> Plan fs m (Maybe a)
try x = do
    tries <- fresh
    transform tries $ x (\a -> symbol (Success tries a)) (symbol (Failure tries))
  where
    transform :: Int -> Plan fs m (Maybe a) -> Plan fs m (Maybe a)
    transform tries =
      mapStep $ \go stp@(Step syms bp) ->
        case prj syms of
          Just tried ->
            case tried of
              Success i a ->
                if i == tries
                then Pure (Just (unsafeCoerce a))
                else Step syms (\b -> go (bp b))
              Failure i ->
                if i == tries
                then Pure Nothing
                else Step syms (\b -> go (bp b))
          Nothing -> Step syms (\b -> go (bp b))

instance Pair TryHandler Try

module Effect.Loop
  ( loop, Loop
  , loops, Loops
  ) where

import Mop

import qualified Data.Function
import Unsafe.Coerce

-- | I believe this module is entirely subsumed by 'fix' and named functions.
-- That is:
-- @
--   loop x $ \break continue st -> do
--     something
--     if x st then continue st else break x
-- @
-- is equivalent to:
-- @
--   fix $ \go st -> do
--     something
--     if x st then go st else return x
-- @
-- which is equivalent to:
-- @
--   go st
--     where
--       go st = do
--         something
--         if x st then go st else return x
-- @
-- TODO: Test performance to find out where they differ.

data Loop k
  = FreshScope (Integer -> k)
  | forall s. Continue Integer s
  | forall a. Break Integer a

data Loops k = Loops Integer k

freshScope :: Has Loop fs m => Plan fs m Integer
freshScope = symbol (FreshScope id)

loop :: forall fs m s a. Has Loop fs m
     => s
     -> (    (forall b. a -> Plan fs m b)
          -> (forall b. s -> Plan fs m b)
          -> s
          -> Plan fs m a
        )
     -> Plan fs m a
loop s0 x = do
    scope <- freshScope
    let break a      = symbol (Break scope a)
        continue s   = symbol (Continue scope s)
        loopBody r s = transform scope r (x break continue s)
    Data.Function.fix loopBody s0
  where
    transform scope restart = go
      where
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Continue i s ->
                      if i == scope
                      then restart (unsafeCoerce s)
                      else Step sym (\b -> go (bp b))
                    Break i a ->
                      if i == scope
                      then return (unsafeCoerce a)
                      else Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                _ -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r

loops :: Uses Loops gs m => Attribute Loops gs m
loops = Loops 0 $ \fs ->
  let Loops i k = (fs&)
  in pure $ fs .= Loops (succ i) k

instance Pair Loops Loop where
  pair p (Loops i k) (FreshScope ik) = p k (ik i)
  pair _ _ _ = error "Unscoped looping construct."

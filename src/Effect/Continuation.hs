{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Continuation
  (with,callCC,Cont(..)
  --,capture
  --,recapture,recaptureM,recaptures,recapturesM
  --,iterates,iteratesM
  --,loop,foreverP
  ,cont,ContHandler(..)
  ,special,specialty,specializer,SpecialHandler(..)
  ,specialize,Special(..)
  ,recapture
  )
  where

import Mop
import Effect.Fresh
import Unsafe.Coerce
import Data.IORef
import System.IO.Unsafe
import Debug.Trace

data Cont k = forall a. Cont Integer a
data ContHandler k = ContHandler k

callCC :: (Has Cont fs m,Has (Fresh Integer) fs m)
       => ((forall b. a -> Plan fs m b) -> Plan fs m a) -> Plan fs m a
callCC = with

with :: (Has Cont fs m,Has (Fresh Integer) fs m)
     => ((forall b. a -> Plan fs m b) -> Plan fs m a) -> Plan fs m a
with x = do
    f <- fresh
    transform f $ x (\a -> symbol (Cont f a))
  where
    transform f =
      mapStep $ \go (Step syms bp) ->
        case prj syms of
          Just (Cont i a) ->
            if i == f
            then Pure (unsafeCoerce a)
            else Step syms (\b -> go (bp b))
          _ -> Step syms (\b -> go (bp b))

cont :: (Uses ContHandler gs m) => Instruction ContHandler gs m
cont = ContHandler return

instance Pair ContHandler Cont

data Special k
  = forall fs m a. Specialize (Plan fs m a -> Plan fs m a) k
  | forall fs m a. Specializer ((Plan fs m a -> Plan fs m a) -> k)

data SpecialHandler k = forall fs m a. SpecialHandler (IORef (Plan fs m a -> Plan fs m a)) ((Plan fs m a -> Plan fs m a) -> k) k

specialty :: forall gs m. (Uses SpecialHandler gs m) => Instruction SpecialHandler gs m
specialty = flip (SpecialHandler (unsafePerformIO (newIORef id))) return $ \f fs ->
  case view fs of
    (SpecialHandler iot k k' :: SpecialHandler (Transformation gs m)) ->
      let ioh' = unsafePerformIO $ modifyIORef iot ((unsafeCoerce f) .)
      in ioh' `seq` instruction (SpecialHandler iot k k') fs

specialize :: Has Special fs m => (Plan fs m a -> Plan fs m a) -> Plan fs m ()
specialize f = symbol (Specialize f ())

specializer :: Has Special fs m => Plan fs m (Plan fs m a -> Plan fs m a)
specializer = symbol (Specializer id)

special :: forall fs m a. (Has Special fs m)
        => Plan fs m a -> Plan fs m a
special p = do
  x <- specializer
  case p of
    Pure r -> Pure r
    M m -> M (m >>= \p' -> return (special p'))
    Step syms bp -> Step syms (\b -> special (bp b))

instance Pair SpecialHandler Special where
  pair p (SpecialHandler iot fk k) (Specialize f k') = p (fk $ unsafeCoerce f) k'
  pair p (SpecialHandler iot fk k) (Specializer tk) = p k (tk $ unsafeCoerce $ unsafePerformIO (readIORef iot))

-- capture :: (Has Special fs m,Has (Fresh Integer) fs m)
--         => Plan fs m (Plan fs m b)
-- capture = special $ \exit t -> return (t $ exit undefined)


specialCallCC :: (Has Cont fs m,Has (Fresh Integer) fs m,Has Special fs m)
            => ((forall b. a -> Plan fs m b) -> Plan fs m a) -> Plan fs m a
specialCallCC x = do
    f <- fresh
    specialize (transform f)
    x (\a -> symbol (Cont f a))
  where
    transform f =
      mapStep $ \go (Step syms bp) ->
        case prj syms of
          Just (Cont i a) ->
            if i == f
            then Pure (unsafeCoerce a)
            else Step syms (\b -> go (bp b))
          _ -> Step syms (\b -> go (bp b))

recapture :: (Has Special fs m,Has Cont fs m,Has (Fresh Integer) fs m)
          => a -> Plan fs m (a,a -> Plan fs m b)
recapture x0 = specialCallCC $ \exit ->
  let f x = exit (x,f)
  in do return (x0,f)

-- recaptureM :: (Has Cont fs m,Has (Fresh Integer) fs m)
--            => Plan fs m a -> Plan fs m (a,Plan fs m b)
-- recaptureM x0 = callCC $ \exit ->
--   let f = x0 >>= \x -> exit (x,f)
--   in x0 >>= \x' -> return (x',f)

-- recaptures :: (Has Cont fs m,Has (Fresh Integer) fs m)
--            => a -> Plan fs m (a,Plan fs m a -> Plan fs m b)
-- recaptures x0 = callCC $ \exit ->
--   let f x = x >>= \x' -> exit (x',f)
--   in return (x0,f)

-- recapturesM :: (Has Cont fs m,Has (Fresh Integer) fs m)
--            => Plan fs m a -> Plan fs m (a,Plan fs m a -> Plan fs m b)
-- recapturesM x0 = callCC $ \exit ->
--   let f x = x >>= \x' -> exit (x',f)
--   in x0 >>= \x0' -> return (x0',f)

-- iterates :: (Has Cont fs m,Has (Fresh Integer) fs m)
--          => (a -> b) -> a -> Plan fs m (b,a -> Plan fs m c)
-- iterates f0 a0 = callCC $ \exit ->
--   let f a = exit (f0 a,f)
--   in return (f0 a0,f)

-- iteratesM :: (Has Cont fs m,Has (Fresh Integer) fs m)
--           => (a -> Plan fs m b) -> a -> Plan fs m (b,a -> Plan fs m a)
-- iteratesM f0 a0 = with $ \exit ->
--   let f a = f0 a >>= \x -> exit (x,f)
--   in f0 a0 >>= \x -> return (x,f)

-- foreverP :: (Has Special fs m,Has (Fresh Integer) fs m)
--          => Plan fs m a -> Plan fs m ()
-- foreverP f = do
--   here <- capture
--   _ <- f
--   here

-- loop :: (Has Cont fs m,Has (Fresh Integer) fs m)
--      => ((a -> Plan fs m b) -> Plan fs m a) -> Plan fs m a
-- loop f = callCC $ \exit -> callCC $ \exit' -> do
--   x <- f exit
--   exit' x

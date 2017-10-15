{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Interpreter where

import Ef hiding (send,thread,run)
import qualified Ef

import Control.Monad.ST

{- | Interp is an interpretive monad for stateful evaluation that
     exposes join points that are not naturally available to
     Narrative interpretation via foldn.

     Given an evaluation of some functorial instructions:

     > eval :: Narrative f c a -> c a

     Simply wrap it with Ef.Interpreter.run:

     > interp :: Interp f c a -> c a
     > interp = I.run eval

     And use Ef.Interpreter.send to create instructions rather than Ef.send:

     > instr = I.send ...

     And use interp rather than eval for evaluation.

     > main = interp $ do { ...; return () }

     As an example:

     > data Prompt k where
     >   Prompt :: String -> (String -> k) -> Prompt k
     >
     > eval :: MonadIO c => Narrative Prompt c a -> c a
     > eval = 


-}

newtype Interp ctx f c a = Interp
  { interpret :: (forall x. ctx -> Narrative f c x -> c (ctx,x)) -> ctx -> c (ctx,a) }

instance Functor c => Functor (Interp ctx f c) where
  {-# INLINE fmap #-}
  fmap f (Interp k) = Interp $ \k' ctx -> fmap (fmap f) (k k' ctx)

instance (Functor f, Monad c) => Applicative (Interp ctx f c) where
  {-# INLINE pure #-}
  pure a = Interp $ \k ctx -> k ctx (pure a)
  {-# INLINE (<*>) #-}
  (<*>) fab fa = Interp $ \d ctx -> do
    (ctx',ab) <- interpret fab d ctx
    (ctx'',a) <- interpret fa  d ctx'
    return (ctx'',ab a)

instance (Functor f, Monad c) => Monad (Interp ctx f c) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  k >>= f = Interp $ \d ctx -> do
    (ctx',a) <- interpret k d ctx
    interpret (f a) d ctx'

instance (Applicative f, MonadPlus c) => MonadPlus (Interp ctx f c) where
  {-# INLINE mzero #-}
  mzero = Interp $ \k ctx -> k ctx mzero
  {-# INLINE mplus #-}
  mplus l r = Interp $ \k ctx ->
    (interpret l k ctx) `mplus` (interpret r k ctx)

instance (Applicative f, Alternative c, Monad c) => Alternative (Interp ctx f c) where
  {-# INLINE empty #-}
  empty = Interp $ \k ctx -> k ctx empty
  {-# INLINE (<|>) #-}
  (<|>) l r = Interp $ \k ctx ->
    (interpret l k ctx) <|> (interpret r k ctx)

instance (Functor f, MonadFix c) => MonadFix (Interp ctx f c) where
  {-# INLINE mfix #-}
  mfix f = Interp $ \d ctx -> mfix (\(_,a) -> interpret (f a) d ctx)

instance MonadTrans (Interp ctx f) where
  lift f = Interp $ \k ctx -> k ctx (lift f)

infixr 5 !
{-# INLINE (!) #-}
(!) :: (MonadFix c, Functor (Messages ms), Delta (Modules ts) (Messages ms)) => Object ts c -> Interp (Object ts c) (Messages ms) c a -> c (Object ts c,a)
(!) o i = interpret i (Ef.!) o

{-# INLINE run #-}
run :: (Monad c, Functor f)
       => (forall x. Narrative f c x -> c x)
       -> Interp () f c a
       -> c a
run f i =
  fmap snd $
    Ef.Interpreter.thread (\n ctx -> fmap (\a -> (ctx,a)) (f n)) i ()

{-# INLINE thread #-}
thread :: (Monad c, Functor f)
    => (forall x. Narrative f c x -> ctx -> c (ctx,x))
    -> Interp ctx f c a
    -> ctx
    -> c (ctx,a)
thread f i c = interpret i (flip f) c

{-# INLINE send #-}
send :: Functor f => f a -> Interp ctx f c a
send f = Interp $ \k ctx -> k ctx (Ef.send f)

liftInterp :: (Monad c, Functor f) => Narrative f c a -> Interp ctx f c a
liftInterp (Return a) = return a
liftInterp (Lift c)   = join $ lift (fmap liftInterp c)
liftInterp (Do d)     = join $ send (fmap liftInterp d)

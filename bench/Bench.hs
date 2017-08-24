{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Trivial
-- import Bench.Functor
import Data.Functor.Identity
-- import Bench.State
import Ef
import Control.Monad.Codensity
-- import Control.Monad.Trans.State.Strict as St
import Data.Monoid
import Data.Coerce

import qualified Control.Monad.Trans.State as T
import qualified Control.Monad.Trans.Reader as T
import qualified Control.Monad.Trans.Writer as T

newtype State s k = State { runState :: s -> (s,k) }
  deriving Functor

{-# INLINE evalStateT #-}
evalStateT :: Monad c => Narrative (State s) c a -> s -> c a
evalStateT = foldn (\a _ -> return a) (\cf a -> cf >>= ($ a)) $
  \ssk s -> let (s',k) = runState ssk s in k s'

{-# INLINE get #-}
get :: Narrative (State s) c s
get = send (State (\s -> (s,s)))

{-# INLINE put #-}
put :: s -> Narrative (State s) c ()
put x = send (State (const (x,())))

{-# INLINE modify' #-}
modify' :: (s -> s) -> Narrative (State s) c ()
modify' f = send (State (\s -> let !s' = f s in (s',())))

{-# INLINE modify #-}
modify :: (s -> s) -> Narrative (State s) c ()
modify f = send (State (\s -> (f s,())))

newtype Reader r k = Reader { runReader :: r -> k }
  deriving (Functor)

{-# INLINE runReaderT #-}
runReaderT :: Monad c => Narrative (Reader r) c a -> r -> c a
runReaderT n rd = foldn return join (`runReader` rd) n

{-# INLINE ask #-}
ask :: Narrative (Reader r) c r
ask = send (Reader id)

newtype Writer w k = Writer { runWriter :: w -> (w,k) }
  deriving Functor

{-# INLINE runWriterT #-}
runWriterT :: (Monad c, Monoid w) => Narrative (Writer w) c a -> c (w,a)
runWriterT n = foldn
  (\a w -> return (w,a))
  (\cf a -> cf >>= ($ a))
  (\wk w -> let (w',k) = runWriter wk w in k w')
  n
  mempty

{-# INLINE tell #-}
tell :: Monoid w => w -> Narrative (Writer w) c ()
tell w' = send (Writer (\w -> (w <> w',())))

{-# INLINE replicateM_' #-}
replicateM_' :: Monad m => Int -> m () -> m ()
replicateM_' n f = go n
  where
    go 0 = return ()
    go n = do
      f
      go (n - 1)

type StateT s c a = Narrative (State s) c a
type ReaderT r c a = Narrative (Reader r) c a
type WriterT w c a = Narrative (Writer w) c a
type RWS r w s m a = Narrative (Reader r) (Narrative (Writer w) (Narrative (State s) m)) a

main :: IO ()
main = void $ Trivial.run suite

suite = scope "rws" $ do
  br1 <- nfio "transformers" (run_t t_go)
  notep br1
  br2 <- nfio "ef" (run_ef ef_go)
  notep br2

{-# INLINE run_t #-}
run_t = (`T.evalStateT` 0) . T.runWriterT . (`T.runReaderT` 1)

{-# INLINE run_ef #-}
run_ef = (`evalStateT` 0) . runWriterT . (`runReaderT` 1)

{-# INLINE t_go #-}
t_go :: Monad m => T.ReaderT Int (T.WriterT (Sum Int) (T.StateT Int m)) ()
t_go = replicateM_' 100 $ do
  r :: Int <- T.ask
  s :: Int <- lift $ lift T.get
  lift $ T.tell (Sum s)
  lift $ lift $ T.put $! s + r

{-# INLINE ef_go #-}
ef_go :: Monad m => RWS Int (Sum Int) Int m ()
ef_go = replicateM_' 100 $ do
  r :: Int <- ask
  s :: Int <- lift $ lift get
  lift $ tell (Sum s)
  lift $ lift $ put $! s + r


{-# LANGUAGE ScopedTypeVariables #-}
module Bench.RWS where

import Pure.Bench
import Pure.Test

import Ef

import Ef.Reader
import Ef.Writer
import Ef.State

import Bench.Shared

import Data.Monoid

import qualified Control.Monad.Trans.Reader as T
import qualified Control.Monad.Trans.Writer as T
import qualified Control.Monad.Trans.State  as T

type RWS r w s m a = Narrative (Reader r) (Narrative (Writer w) (Narrative (State s) m)) a

rws :: Test Sync ()
rws = do
  br1 <- nfio "transformers" (run_t t_go)
  br2 <- nfio "ef" (run_ef ef_go)
  report br1 br2

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


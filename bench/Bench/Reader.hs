{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
module Bench.Reader where

import Pure.Bench
import Pure.Test

import Ef

import qualified Control.Monad.Trans.Reader as T

import Ef.Interpreter as I

import Data.Functor.Identity

suite :: Test Sync ()
suite = scope "reader" $ tests
  [ Bench.Reader.reader
  ]

reader = do
  br1 <- scope "transformer" $ nf (\n -> runIdentity $ mtl_test n) (1 :: Int)
  br2 <- scope "ef" $ nf (\n -> runIdentity $ ef_test n) (1 :: Int)
  report br1 br2

{-# INLINE mtl_test #-}
mtl_test = T.runReaderT mtl_reader

{-# INLINE ef_test #-}
ef_test = interp ef_reader

type ReaderT r c a = Interp () (Reader r) c a
newtype Reader r k = Reader { runReader :: r -> k }
  deriving (Functor)

{-# INLINE eval #-}
eval :: Monad c => Narrative (Reader r) c a -> r -> c a
eval n rd = foldn return join (`runReader` rd) n

{-# INLINE interp #-}
interp :: Monad c => ReaderT r c a -> r -> c a
interp n r = I.run (flip eval r) n

{-# INLINE ask #-}
ask :: ReaderT r c r
ask = I.send (Reader id)

{-# INLINE mtl_reader #-}
mtl_reader :: Monad c => T.ReaderT Int c Int
mtl_reader = do
  x :: Int <- T.ask
  y :: Int <- T.ask
  z :: Int <- T.ask
  return (x + y + z)

{-# INLINE ef_reader #-}
ef_reader :: Monad c => ReaderT Int c Int
ef_reader = do
  x :: Int <- ask
  y :: Int <- ask
  z :: Int <- ask
  return (x + y + z)

module Bench.Reader where

import Trivial

import Ef

import Ef.Reader

import qualified Control.Monad.Trans.Reader as T

import Data.Functor.Identity

suite :: Test Sync ()
suite = scope "reader" $ tests
  [ Bench.Reader.reader
  ]

reader = return ()

module Main where

import Trivial

import qualified Bench.Reader as Reader
import qualified Bench.Writer as Writer
import qualified Bench.State  as State
import qualified Bench.Pipes  as Pipes
import qualified Bench.Pipes.Chain as Chain
import qualified Bench.RWS    as RWS

import Control.Concurrent

main = Trivial.runOnly "state" suite

suite = tests
  [ Reader.suite
  , Writer.suite
  ,  State.suite
  ,  Pipes.suite
  ,  Chain.suite
  ]
-- main = do
--   Pipes.pipes 100000000
--   threadDelay 1000000

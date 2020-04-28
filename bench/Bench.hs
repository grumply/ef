module Main where

import           Pure.Bench

import qualified Bench.Reader as Reader
import qualified Bench.Writer as Writer
import qualified Bench.State as State
import qualified Bench.Pipes as Pipes
import qualified Bench.Pipes.Chain as Chain
import qualified Bench.RWS as RWS
import qualified Bench.Noop as Noop
import qualified Bench.Interpreter as Interpreter
import qualified Bench.AltState as AltState

import           Control.Concurrent

main = run suite

suite = tests [ AltState.suite ]

others =
  [      Reader.suite
  ,      Writer.suite
  ,       State.suite
  ,       Pipes.suite
  ,       Chain.suite
  ,        Noop.suite
  , Interpreter.suite
  ,    AltState.suite
  ]
-- main = do
--   Pipes.pipes 100000000
--   threadDelay 1000000

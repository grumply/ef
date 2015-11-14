{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.IfThenElse where

import Prelude
import Ef.Core
import Control.Monad

import System.IO.Unsafe
import Data.IORef

-- This rebound if-then-else gives access to both branches of the conditional
-- for the purposes of DSL optimization. This is extremely experimental and
-- incurs a reasonably large overhead as well as being extremely unsafe and
-- unreliable. An other, safer, way to do this would be exceptionally welcome.

-- The reasoning behind this approach is thus: When analyzing a DSL, we need
-- to create the data structure representing the Pattern without actually
-- executing any effects and binding any variables. Instead, we fill all holes
-- with undefined values and use reallyUnsafePtrEquality# to find use-sites on
-- a per-case basis. If we use the built-in if-then-else, we'll end up requiring
-- the Bool to be fully evaluated, but what if that Bool depends upon a bound
-- variable in the Pattern? It might very well be a value we initialized as
-- undefined for static analysis. That works out if we're making an optimization
-- pass of the Pattern dynamically, but what if we want to optimize the Pattern
-- statically? That's where this rebound if-then-else comes in: we put each
-- branch of the conditional in an IORef and use unsafePerformIO to allow
-- analysis of each branch independently. So the hope is for the ability to do
-- static branch prediction as well as static independent branch optimization.

data ITEing k = forall fs m a. ITE (IORef (Pattern fs m a)) (IORef (Pattern fs m a)) k

data ITEable k = ITEable k

ifThenElse :: Is ITEing fs m => Bool -> Pattern fs m a -> Pattern fs m a -> Pattern fs m a
ifThenElse i t e =
  let t' = unsafePerformIO $ newIORef t
      e' = unsafePerformIO $ newIORef e
  in join $ self (ITE t' e' $ case i of
       True -> unsafePerformIO $ readIORef t'
       False -> unsafePerformIO $ readIORef e'
     )

ifThenElser :: Uses ITEable fs m => Attribute ITEable fs m
ifThenElser = ITEable return

instance Symmetry ITEable ITEing where
  symmetry use (ITEable k) (ITE _ _ k') = use k k'

main :: IO ()
main = do
  delta (Object $ ifThenElser *:* Empty) $ do
    if True
    then lift (print "Yay!")
    else lift (print "Hmm.")
  return ()

module Ef.Writer (Writer, writerFrom, writer, noted, tell) where

import Ef

import Data.Monoid

instance Ma (Writer r) (Writer r) where
    ma use (Writer _ rk) (Tell r k) = ma use rk (r,k)

data Writer r k
  = Writer r (r -> k)
  | Tell r k

writerFrom :: w -> (w -> w -> w) -> Use (Writer w) methods super
writerFrom w0 f =
    Writer w0 $ \w' fs ->
        let Writer w k = view fs
        in pure $ fs .= Writer (f w w') k
{-# INLINE writerFrom #-}

writer :: Monoid w => Use (Writer w) methods super
writer = writerFrom mempty (<>)
{-# INLINE writer #-}

noted :: ('[Writer w] .> methods) => Object methods super -> w
noted fs =
  let Writer w _ = view fs
  in w
{-# INLINE noted #-}

tell :: w -> Invoke (Writer w) self super ()
tell w = self (Tell w ())
{-# INLINE tell #-}

module Ef.Writer (Writer, writerFrom, writer, noted, tell) where

import Ef

import Control.Lens
import Data.Monoid

data Writer r k
  = Writer r (r -> k)
  | Tell r k

instance Ma (Writer r) (Writer r) where
    ma use (Writer _ rk) (Tell r k) = ma use rk (r,k)

writerFrom :: (Monad super, '[Writer w] <. traits)
           => w -> (w -> w -> w) -> Trait (Writer w) traits super
writerFrom w0 f =
    Writer w0 $ \w' fs ->
        let Writer w k = view trait fs
        in pure $ set trait (Writer (f w w') k) fs
{-# INLINE writerFrom #-}

writer :: (Monad super, '[Writer w] <. traits, Monoid w)
       => Trait (Writer w) traits super
writer = writerFrom mempty (<>)
{-# INLINE writer #-}

noted :: (Monad super, '[Writer w] <. methods)
      => Object methods super -> w
noted fs =
  let Writer w _ = view trait fs
  in w
{-# INLINE noted #-}

tell :: (Monad super, '[Writer w] <: self)
     => w -> Narrative self super ()
tell w = self (Tell w ())
{-# INLINE tell #-}

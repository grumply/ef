{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef.Writer.Methods
    ( Writer(..)
    , writerFrom
    , writer
    , noted
    ) where


import Ef.Object

import Data.Monoid


data Writer r k = Writer r (r -> k)


writerFrom :: w -> (w -> w -> w) -> Use (Writer w) methods super
writerFrom w0 f =
    Writer w0 $ \w' fs ->
        let Writer w k = view fs
        in pure $ fs .= Writer (f w w') k


writer :: Monoid w => Use (Writer w) methods super
writer = writerFrom mempty (<>)


noted :: Subclass '[Writer w] methods => Object methods super -> w
noted fs = let Writer w _ = view fs in w

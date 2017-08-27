{-# LANGUAGE FlexibleContexts #-}
module Ef.Path where

import Ef

import Control.Comonad
import Control.Comonad.Cofree

type Path ts ms c a = Cofree c (Object ts c,Ef ms c a)

{-# INLINE lay #-}
lay :: (Monad c, Delta (Modules ts) (Messages ms)) => Object ts c -> Ef ms c a -> Path ts ms c a
lay obj nar = coiter (uncurry lay) (obj,nar)
  where
  lay o = go
    where
      go (Do m) =
        let (method,b) = delta (,) (deconstruct o) m
        in method o >>= \o' -> return (o',b)
      go (Lift m) = m >>= \c -> return (o,c)
      go (Return r) = return (o,Return r)

{-# INLINE walk #-}
walk :: (Monad c, Delta (Modules ts) (Messages ms)) => Path ts ms c a -> c (Object ts c,a)
walk = step
  where
    step machine = do
      next <- unwrap machine
      let (obj,c) = extract next
      case c of
        (Return r) -> return (obj,r)
        _          -> step next


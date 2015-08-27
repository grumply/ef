{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Has where

import Control.Comonad

class b :@: a | a -> b where
  pin :: a -> b
  rewrite :: b -> a -> a

instance (b :@: a,Comonad w) => b :@: (w a)
  where
  pin = pin . extract
  rewrite b = extend (rewrite b . extract)

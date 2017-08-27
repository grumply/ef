{-# LANGUAGE DeriveFunctor #-}
module Ef.Noop where

import Ef
import qualified Ef.Interpreter as I

newtype Noop k = Noop { noop :: k }
  deriving Functor

{-# INLINE noops #-}
noops :: Monad m => Narrative Noop m a -> m a
noops = run noop

{-# INLINE noopsI #-}
noopsI :: Monad m => I.Interp () Noop m a -> m a
noopsI = I.run noops

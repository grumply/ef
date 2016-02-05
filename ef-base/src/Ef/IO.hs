{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Ef.IO
  ( unsafe
  , io
  , masked
  , masked_
  ) where


import Ef

import qualified Control.Exception as Exc


-- | lift an IO computation; this method does not lifting of exceptions.
-- `io` is strongly advised over this method.
unsafe :: Lift IO super => IO result -> Narrative messages super result
unsafe = lift


-- | execute an exception-masked computation with no way to unmask.
masked_ :: Lift IO super => IO result -> Narrative messages super result
masked_ = lift . Exc.mask_


-- | execute an exception-masked computation with an unmasking function.
masked :: Lift IO super => ((forall a. IO a -> IO a) -> IO result)
                        -> Narrative messages super result
masked = lift . Exc.mask


-- | `io` executes actions in the root IO context. This method is strongly
-- advised for all effectful computations.
io :: (Monad super, Lift IO super) => IO result -> Narrative messages super result
io ioa = do
    ea <- lift (Exc.try ioa)
    case ea of
        Left e -> throw (e :: Exc.SomeException)
        Right r -> return r

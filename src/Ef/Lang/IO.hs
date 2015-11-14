{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.Lang.IO
  ( unsafe, io
  , masked, masked_
  ) where

import Ef.Core
import Ef.Lang.Except
import qualified Control.Exception as Exc

unsafe  :: Lift IO m => IO a -> Pattern fs m a
unsafe   = lift

masked_ :: (Is Excepting fs m,Lift IO m) => IO a -> Pattern fs m a
masked_  = lift . Exc.mask_

masked  :: (Is Excepting fs m,Lift IO m) => ((forall (a :: *). IO a -> IO a) -> IO b) -> Pattern fs m b
masked   = lift . Exc.mask

io      :: (Is Excepting fs m,Lift IO m) => IO a -> Pattern fs m a
io ioa   = do
  ea <- lift (Exc.try ioa)
  case ea of
    Left e -> throw (e :: SomeException)
    Right r -> return r

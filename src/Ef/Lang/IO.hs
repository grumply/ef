{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Ef.Lang.IO
  ( unsafe
  , io
  , masked
  , masked_
  ) where



import Ef.Core

import qualified Control.Exception as Exc



unsafe
    :: Lift IO m
    => IO a
    -> Pattern fs m a

unsafe =
    lift



masked_
    :: Lift IO m
    => IO a
    -> Pattern fs m a

masked_ =
    lift . Exc.mask_



masked
    :: Lift IO m
    => (   (forall (a :: *). IO a -> IO a)
         -> IO b
       )
    -> Pattern fs m b

masked =
    lift . Exc.mask



io
    :: ( Monad m
       , Lift IO m
       )
    => IO a
    -> Pattern fs m a

io ioa =
    do
      ea <- lift (Exc.try ioa)
      case ea of

          Left e ->
              throw (e :: Exc.SomeException)

          Right r ->
              return r

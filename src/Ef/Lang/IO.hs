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
    :: Lift IO parent
    => IO result
    -> Pattern scope parent result

unsafe =
    lift



masked_
    :: Lift IO parent
    => IO result
    -> Pattern scope parent result

masked_ =
    lift . Exc.mask_



masked
    :: Lift IO parent
    => (   (forall (a :: *). IO a -> IO a)
         -> IO result
       )
    -> Pattern scope parent result

masked =
    lift . Exc.mask



io
    :: ( Monad parent
       , Lift IO parent
       )
    => IO result
    -> Pattern scope parent result

io ioa =
    do
      ea <- lift (Exc.try ioa)
      case ea of

          Left e ->
              throw (e :: Exc.SomeException)

          Right r ->
              return r

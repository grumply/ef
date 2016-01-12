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
    :: Lift IO environment
    => IO result
    -> Narrative lexicon environment result

unsafe =
    lift



masked_
    :: Lift IO environment
    => IO result
    -> Narrative lexicon environment result

masked_ =
    lift . Exc.mask_



masked
    :: Lift IO environment
    => (   (forall (a :: *). IO a -> IO a)
         -> IO result
       )
    -> Narrative lexicon environment result

masked =
    lift . Exc.mask



io
    :: ( Monad environment
       , Lift IO environment
       )
    => IO result
    -> Narrative lexicon environment result

io ioa =
    do
      ea <- lift (Exc.try ioa)
      case ea of

          Left e ->
              throw (e :: Exc.SomeException)

          Right r ->
              return r

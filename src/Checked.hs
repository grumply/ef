{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif

-- | Lightweight checked exceptions
--
-- See <http://www.well-typed.com/blog/2015/07/checked-exceptions/>.
-- Written by Edsko de Vries. Copyright 2015 Well-Typed LLP.
--
-- Modified for use with ekmett's exceptions package.
--
-- Tested with ghc 7.2, 7.4, 7.6, 7.8 and 7.10.
module Checked where

import Control.Exception (Exception, IOException)
import Unsafe.Coerce (unsafeCoerce)
import qualified Control.Exception as E
import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Class

{-------------------------------------------------------------------------------
  Main definitions
-------------------------------------------------------------------------------}

-- | Checked exceptions
class Throws e where
  throwChecked :: MC.MonadThrow m => e -> m a

-- | Wrap an action that may throw a checked exception
--
-- This is used internally in 'rethrowUnchecked' to avoid impredicative
-- instantiation of the type of 'unsafeCoerce'.
newtype Wrap e m a = Wrap (Throws e => m a)

-- | Rethrow checked exceptions as unchecked (regular) exceptions
rethrowUnchecked :: forall e a m. MC.MonadThrow m
                               => (Throws e    => m a)
                               -> (Exception e => m a)
rethrowUnchecked act = aux act MC.throwM
  where
    aux :: (Throws e => m a) -> ((e -> m a) -> m a)
    aux = unsafeCoerce . Wrap

-- | Catch a checked exception
--
-- This is the only way to discharge a 'Throws' type class constraint.
catchChecked :: (MC.MonadThrow m,MC.MonadCatch m,Exception e) => (Throws e => m a) -> (e -> m a) -> m a
catchChecked = MC.catch . rethrowUnchecked

{-------------------------------------------------------------------------------
  Additional definitions
-------------------------------------------------------------------------------}

-- | 'catchChecked' with the arguments reversed
handleChecked :: (MC.MonadCatch m,Exception e) => (e -> m a) -> (Throws e => m a) -> m a
handleChecked act handler = catchChecked handler act

-- | Throw an unchecked exception
--
-- This is just an alias for 'throw', but makes it evident that this is a very
-- intentional use of an unchecked exception.
throwUnchecked :: (MC.MonadThrow m,Exception e) => e -> m a
throwUnchecked = MC.throwM

-- | Rethrow IO exceptions as checked exceptions
checkIO :: (MonadIO m,MC.MonadCatch m,Throws IOException) => IO a -> m a
checkIO = MC.handle (\(ex :: IOException) -> throwChecked ex) . liftIO

readFile' :: Throws IOException => FilePath -> IO String
readFile' = checkIO . readFile

readEtcPasswd :: IO String
readEtcPasswd = catchChecked (readFile' "/etc/passwd") $ \(ex :: IOException) ->
                  return "Could not read"

module Show where

import           Data.Functor.Identity

import           Control.Monad.Trans.Free
import           Control.Comonad.Trans.Cofree

import qualified Control.Monad.Free as Free

showFT :: (Show (f a),Show a,Show (f (FreeT f Identity a))) => FreeT f Identity a -> String
showFT f = show $ runIdentity $ runFreeT f

showF :: (Show (f b),Show a) => FreeF f a b -> String
showF (Free fb) = show fb
showF (Pure a) = show a

showF' :: (Show (f (Free.Free f a)),Show a) => Free.Free f a -> String
showF' (Free.Free fb) = show fb
showF' (Free.Pure a) = show a

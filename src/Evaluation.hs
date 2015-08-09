module Evaluation where

import           Pairing

import           Data.Functor.Identity

import qualified Control.Monad.Free as Free

import           Control.Monad.Trans.Free
import           Control.Comonad.Trans.Cofree

import           Control.Parallel

import           Control.Comonad

run :: (Monad m, Pairing f g, ComonadCofree f w)
     => w (m a) -> FreeT g m b -> m (CofreeT f w (m a), (a, b))
run w m = w `par` do
  a <- extract w
  mb <- runFreeT m
  a `par` mb `pseq`
    case mb of
      Free fs -> pair run (unwrap w) fs
      Pure b ->
        return
          (CofreeT $ fmap ((:<) (return a) . tailF) $ runCofreeT $ coiterT unwrap w
          ,(a,b)
          )

exec :: (Monad m, Pairing f g, ComonadCofree f w) => w (m a) -> FreeT g m b -> m a
exec w = fmap (fst . snd) . run w

eval :: (Monad m, Pairing f g, ComonadCofree f w) => w (m a) -> FreeT g m b -> m b
eval w = fmap (snd . snd) . run w

interp :: (Monad m, Pairing f g, ComonadCofree f w)
       => w (m a) -> FreeT g m b -> m (CofreeT f w (m a))
interp w = fmap fst . run w

run' :: (Monad m, Pairing f g, ComonadCofree f w)
     => w (m a) -> Free.Free g b -> m (CofreeT f w (m a),(a,b))
run' w m = w `par` do
  a <- extract w
  a `par`
    case m of
      Free.Free fs -> pair run' (unwrap w) fs
      Free.Pure b -> return
        (CofreeT $ fmap ((:<) (return a) . tailF) $ runCofreeT $ coiterT unwrap w
        ,(a,b)
        )

exec' :: (Monad m, ComonadCofree f w, Pairing f g) => w (m b) -> Free.Free g b -> m b
exec'   w = fmap (fst . snd) . run' w

eval' :: (Monad m, ComonadCofree f w, Pairing f g) => w (m a) -> Free.Free g b -> m b
eval'   w = fmap (snd . snd) . run' w

interp' :: (Monad m, ComonadCofree f w, Pairing f g)
        => w (m a) -> Free.Free g b -> m (CofreeT f w (m a))
interp' w = fmap fst . run' w

module Bench.Shared where

{-# INLINE replicateM_' #-}
replicateM_' :: Monad m => Int -> m () -> m ()
replicateM_' n f = go n
  where
    go 0 = return ()
    go n = do
      f
      go (n - 1)


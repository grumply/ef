module Mop.Trans where

class MTrans t where
  lift' :: Monad m => m a -> t m a

-- this avoids internal dependency
-- instance MTrans t => Trans t where
--   lift = lift'

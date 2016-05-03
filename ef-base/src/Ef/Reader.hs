module Ef.Reader (ask, asks, local, Reader, reader) where

import Ef

instance Ma (Reader r) (Reader r) where
    ma use (Reader r k) (Ask rk) = ma use (r,k) rk

data Reader (r :: *) (k :: *)
  = Reader r k
  | Ask (r -> k)

reader :: r -> Use (Reader r) scope environment
reader r = Reader r pure
{-# INLINE reader #-}

ask :: Invoke (Reader r) self super r
ask = self (Ask id)
{-# INLINE ask #-}

asks :: (r -> a) -> Invoke (Reader r) self super a
asks f = self (Ask f)
{-# INLINE asks #-}

local
    :: forall self super r.
       ('[Reader r] :> self, Monad super)
    => (r -> r)
    -> Narrative self super r
    -> Narrative self super r
local f =
    transform go
  where

    go :: forall x. Messages self x -> (x -> Narrative self super r) -> Narrative self super r
    go message k =
        case prj message of

            Just (Ask (r :: r -> b)) ->
                let newMessage = inj (Ask (r . f))
                in Say newMessage (transform go . k)

            Nothing -> Say message (transform go . k)
{-# INLINE local #-}

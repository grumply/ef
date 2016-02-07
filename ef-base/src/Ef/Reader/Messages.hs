{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Reader.Messages
    ( Reader(..)
    , ask
    , asks
    , local
    ) where


import Ef.Narrative
import Ef.Messages


data Reader (r :: *) (k :: *) = Ask (r -> k)


ask :: Invoke (Reader r) self super r
ask = self (Ask id)


asks :: (r -> a) -> Invoke (Reader r) self super a
asks f = self (Ask f)


local
    :: forall self super r.
       ('[Reader r] <: self, Monad super)
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



{-# INLINE ask #-}
{-# INLINE asks #-}
{-# INLINE local #-}

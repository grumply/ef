module Ef.Reader
  ( Reader
  , readerp, reader
  , pattern AskP, pattern Ask
  , askp, ask
  , asksp, asks
  , localp, local
  ) where

import Ef

data Reader (p :: x) r k where
  Reader
    :: { _reader_proxy :: Proxy p
       , asker :: (r,k)
       } -> Reader p r k

  Ask_
    :: { ask_reader_proxy :: Proxy p
       , askee :: r -> k
       } -> Reader p r k

instance Functor (Reader p s) where
  fmap f (Reader p (r,sk)) = Reader p (r,f sk)
  fmap f (Ask_ p sak) = Ask_ p (fmap f sak)

pattern AskP p f = Ask_ p f

pattern Ask f <- Ask_ (Proxy :: Proxy ()) f where
  Ask f = Ask_ (Proxy :: Proxy ()) f

instance Delta (Reader p r) (Reader p r) where
  delta eval Reader {..} Ask_ {..} = delta eval asker askee

readerp :: (Monad c, ts <. '[Reader p r]) => Proxy p -> r -> Reader p r (Action ts c)
readerp p r = Reader p (r,pure)
{-# INLINE readerp #-}

reader :: (Monad c, ts <. '[Reader () r]) => r -> Reader () r (Action ts c)
reader = readerp unit
{-# INLINE reader #-}

askp :: (Monad c, ms <: '[Reader p r]) => Proxy p -> Ef ms c r
askp p = Send (AskP p Return)
{-# INLINE askp #-}

ask :: (Monad c, ms <: '[Reader () r]) => Ef ms c r
ask = Send (Ask Return)
{-# INLINE ask #-}

asksp :: (Monad c, ms <: '[Reader p r]) => Proxy p -> (r -> a) -> Ef ms c a
asksp p f = Send (AskP p (Return . f))
{-# INLINE asksp #-}

asks :: (Monad c, ms <: '[Reader () r]) => (r -> a) -> Ef ms c a
asks f = Send (Ask (Return . f))
{-# INLINE asks #-}

localp :: forall p ms c r. (Monad c, ms <: '[Reader p r]) => Proxy p -> (r -> r) -> Ef ms c r -> Ef ms c r
localp _ f n = buildn $ \r l d -> foldn r l (msg d) n
  where
    msg d m =
      case prj m of
        Nothing -> d m
        Just (AskP (p :: Proxy p) r) -> d (inj (AskP p (r . f)))
{-# INLINE localp #-}

local :: (Monad c, ms <: '[Reader () r]) => (r -> r) -> Ef ms c r -> Ef ms c r
local = localp unit
{-# INLINE local #-}

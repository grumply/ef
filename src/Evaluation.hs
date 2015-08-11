{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module Evaluation where

import           Pairing

import           Control.Monad.Trans.Free
import           Control.Comonad.Trans.Cofree

import           Control.Comonad

import           Data.Bifunctor
import           Data.Coerce


newtype Tape symbols actions result
  = Tape { s :: FreeT symbols actions result
         } deriving (Functor,Applicative,Monad)

newtype Computer instructions context actions state
  = Computer { q0 :: CofreeT instructions context (actions state)
             } deriving (Functor)

newtype Machine symbols instructions context state actions result
  = Machine { m :: ( Tape     symbols              actions result
                   , Computer instructions context actions state
                   )
            } deriving (Functor)

instance (Pairing instructions symbols
         ,Monad actions
         ,ComonadCofree symbols context
         ,Monoid (context (actions state))
         )
  => Applicative (Machine instructions symbols context state actions)
  where
    pure a = Machine (Tape $ return a
                     ,Computer $ coiterT unwrap mempty)
    (<*>) _ _ = _

instance (Pairing instructions symbols
         ,Monad actions
         ,ComonadCofree symbols context
         ,Monoid (context (actions state))
         )
  => Monad (Machine instructions symbols context state actions)
  where
    return = pure


delta :: forall instructions symbols context actions state result.
         (Pairing instructions  symbols
         ,Comonad context,Monad actions
         )
      => Computer instructions context actions state
      -> Tape     symbols              actions result
      -> actions (Computer instructions context actions state,result)
delta computer tape = do
  let from = coerce :: CofreeT f w a -> w (CofreeF f a (CofreeT f w a))
      to = coerce :: w (CofreeF f (m a) (CofreeT f w (m a))) -> Computer f w m a

  state <- extract (q0 computer)
  symbols <- runFreeT $ s tape
  case symbols of
    Free symbols' -> pair delta (fmap coerce $ unwrap $ q0 computer)
                                (fmap coerce symbols')
    Pure result -> return
      (to $ fmap (bimap (const (return state)) id) $ from $ q0 computer,result)

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Evaluation where

import           Pairing

import           Control.Category

import           Control.Comonad
import           Control.Comonad.Trans.Cofree

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans
import           Control.Monad.Trans.Free

import           Data.Bifunctor
import           Data.Coerce
import           Data.Monoid

import           Prelude hiding (id,(.))

import           Data.Functor.Identity

instance ComonadCofree Identity Identity where
  unwrap = Identity

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
            } deriving Functor

-- applicative instance threads a computer through a series of tapes,
-- keeping any changes to the computer upon the evaluation of a tape
-- and ignoring the machine associated with that tape completely.
instance forall instructions symbols context actions state.
         ( Pairing instructions symbols
         , Monad actions
         , ComonadCofree instructions context
         ) => Applicative (Machine symbols instructions context state actions)
  where
    pure :: a -> Machine symbols instructions context state actions a
    pure a = Machine (Tape tape,Computer computer)
      where
        tape = return a
        computer = coiterT unwrap (cfix extract)

    (<*>) :: Machine symbols instructions context state actions (a -> b)
          -> Machine symbols instructions context state actions a
          -> Machine symbols instructions context state actions b
    (<*>) (Machine (Tape t,Computer c))
          (Machine (Tape t',Computer _))
            = Machine (Tape (ap t t'),Computer c)

-- monad instance uses the same initial computer to execute every tape,
-- ignoring any updates to the computer upon the evaluation of a tape.
instance forall instructions symbols context actions state.
         ( Pairing instructions symbols
         , Monad actions
         , ComonadCofree instructions context
         ) => Monad (Machine symbols instructions context state actions)
  where
    return = pure
    Machine (t,c) >>= f
      = let t' = Tape $ do (Computer c',a) <- lift $ delta c t
                           let Machine (t',_) = f a
                           s t'
        in Machine (t',c)

delta :: forall instructions symbols context actions state result.
         (Pairing instructions  symbols
         ,Comonad context,Monad actions
         )
      => Computer instructions context actions state
      -> Tape     symbols              actions result
      -> actions (Computer instructions context actions state,result)
delta computer tape = do

  let from = coerce :: CofreeT f w (m a)
                    -> w (CofreeF f (m a) (CofreeT f w (m a)))
      to   = coerce :: w (CofreeF f (m a) (CofreeT f w (m a)))
                    -> Computer f w m a

  state <- extract (q0 computer)
  symbols <- runFreeT $ s tape
  case symbols of
    Free symbols' -> pair delta (fmap coerce $ unwrap $ q0 computer)
                                (fmap coerce symbols')
    Pure result -> return
      (to $ fmap (bimap (const (return state)) id) $ from $ q0 computer,result)

runMachine (Machine (t,c)) = delta c t

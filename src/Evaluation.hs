{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Evaluation where

import           Pairing

import           Control.Category

import           Control.Comonad
import           Control.Comonad.Trans.Cofree

import           Control.Monad
import           Control.Monad.Trans.Free

import           Data.Bifunctor
import           Data.Coerce

import           Prelude hiding (id,(.))

type Computer instructions context actions state
    = CofreeT instructions context (actions state)

type Tape symbols actions result
  = FreeT symbols actions result

delta :: forall instructions symbols context actions state result.
         (Pairing instructions  symbols
         ,Comonad context,Monad actions
         )
      => Computer instructions context actions state
      -> Tape     symbols              actions result
      -> actions (Computer instructions context actions state,result)
delta computer tape = do

  -- coercions to guarantee unwrapping/wrapping performance for computer
  let from = coerce :: CofreeT f w (m a)
                    -> w (CofreeF f (m a) (CofreeT f w (m a)))
      to   = coerce :: w (CofreeF f (m a) (CofreeT f w (m a)))
                    -> CofreeT f w (m a)

  state   <- extract computer -- effectfully get current state of computer
  current <- runFreeT tape    -- effectfully get current symbol on tape

  case current of             -- checking for stop symbol

    Free symbol ->            -- continue symbol

      pair delta              -- use delta to pair instruction table with symbol
           (unwrap computer)  -- get instruction table
           symbol             -- continue symbol

    Pure result ->            -- stop symbol

      return                  -- return computer with effects removed and result
        (to $ fmap (bimap (const (return state)) id) $ from computer,result)

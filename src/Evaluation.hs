{-# LANGUAGE ScopedTypeVariables #-}
module Evaluation where

import           Pairing
import           Instruction

import           Control.Comonad
import           Control.Comonad.Trans.Cofree

import           Control.Monad
import           Control.Monad.Trans.Free

import           Data.Bifunctor
import           Data.Coerce
import           Data.Proxy

type Computer instructions context  actions state
    = CofreeT instructions context (actions state)

type Tape = FreeT

-- | delta executes a Computer and Tape in their corresponding actions monad
-- by using the pairing instances for their instructions and symbols,
-- respectively.
--
-- Partial application represents a computer awaiting a tape.
-- Flipped partial application represents a tape awaiting a computer.
delta :: (Pairing instructions symbols
         ,Buildable instructions context
         ,Comonad context
         ,Monad actions
         )
      => Computer instructions context actions state
      -> Tape     symbols              actions result
      -> actions (Computer instructions context actions state,result)
delta comp tape = do

  -- coercions to guarantee unwrapping/wrapping performance for computer
  let from = coerce :: CofreeT f w (m a)
                    -> w (CofreeF f (m a) (CofreeT f w (m a)))
      to   = coerce :: w (CofreeF f (m a) (CofreeT f w (m a)))
                    -> CofreeT f w (m a)

  state   <- extract comp     -- get current state of computer effectfully

  current <- runFreeT tape    -- get next symbol on tape effectfully

  case current of

    Free symbol ->            -- continue symbol

      pair delta              -- use delta to pair instruction table with symbol
           (unwrap comp)      -- get instruction table
           symbol             -- continue symbol

    Pure result ->            -- stop symbol

      return                  -- return computer with effects removed + result.
        (to $ fmap (bimap (const (return state)) id) $ from comp,result)

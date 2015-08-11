module Evaluation where

import           Pairing

import           Control.Comonad
import           Control.Comonad.Trans.Cofree

import           Control.Monad
import           Control.Monad.Trans.Free

import           Data.Bifunctor
import           Data.Coerce

type Computer instructions context  actions state
    = CofreeT instructions context (actions state)

type Tape symbols actions result
  = FreeT symbols actions result

-- | delta executes a Computer and Tape in their corresponding actions monad
-- by using the pairing instances for their instructions and symbols,
-- respectively.
--
-- Partial application represents a computer awaiting a tape.
-- Flipped partial application represents a tape awaiting a computer.
delta :: (Pairing instructions  symbols
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

  state   <- extract computer -- get current state of computer
                              -- and execute effects from previous
                              -- instruction and symbol pairing

  current <- runFreeT tape    -- get next symbol on tape and execute
                              -- effects linked to the production of
                              -- and observation of that symbol

  case current of             -- checking for stop symbol

    Free symbol ->            -- continue symbol

      pair delta              -- use delta to pair instruction table with symbol
           (unwrap computer)  -- get instruction table
           symbol             -- continue symbol

    Pure result ->            -- stop symbol

      return                  -- return computer with effects removed + result.
                              -- I believe this is a safe thing to do since
                              -- `return a >>= k = k a` and `m >>= return = m`
        (to $ fmap (bimap (const (return state)) id) $ from computer,result)

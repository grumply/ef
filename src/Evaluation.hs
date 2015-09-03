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

{-
This implementation uses an approach that ties the computer and tape together
in a way that allows the computer to place bounds on the tape/symbols it is
willing to interpret. That is, at every iteration, a translation is extracted
from the computer and applied to the tape before extracting a symbol from the
tape. This allows the computer to affect the rest of the computation.

In terms of Turing's 'computer,' this would be equivalent to a person
manipulating the tape at the command of the instructions.
-}

type Tape = FreeT

type Translation symbols actions result
  = Tape symbols actions result -> actions (Tape symbols actions result)

type Computer instructions symbols context actions result
    = CofreeT instructions context (actions (Translation symbols actions result))

delta :: ( Pairing instructions symbols
         , Comonad context
         , Monad actions
         ) => Computer instructions symbols context actions result
           -> Tape symbols actions result
           -> actions (Computer instructions symbols context actions result,result)
delta comp tape = do

  translate <- extract comp

  current <- runFreeT (joinFree (translate tape))

  case current of

    Free symbol ->

      pair delta
           (unwrap comp)
           symbol

    Pure result ->

      return
        (toComp $ fmap (bimap (const (return translate)) id) $ fromComp comp,result)

fromComp = coerce :: CofreeT f w (m a) -> w (CofreeF f (m a) (CofreeT f w (m a)))
toComp = coerce :: w (CofreeF f (m a) (CofreeT f w (m a))) -> CofreeT f w (m a)

joinFree :: (Monad m) => m (FreeT f m a) -> FreeT f m a
joinFree = FreeT . join . fmap runFreeT

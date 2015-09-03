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

      -- pair with delta after removing the head translation value in context.
      pair delta (unwrap comp) symbol

    Pure result ->

      return -- remove the effects from the translation value in context
        (toComp $ fmap (bimap (const (return translate)) id) $ fromComp comp,result)

fromComp = coerce :: CofreeT f w (m a) -> w (CofreeF f (m a) (CofreeT f w (m a)))
toComp = coerce :: w (CofreeF f (m a) (CofreeT f w (m a))) -> CofreeT f w (m a)

joinFree :: (Monad m) => m (FreeT f m a) -> FreeT f m a
joinFree = FreeT . join . fmap runFreeT

-- reset the translation value in context contained within a computer.
-- This is a more restricted version of convert to guarantee that a
-- Computer will not be converted to work with a different symbol set.
reset :: ( Functor context
         , Functor instructions
         , Monad actions
         , Pairing instructions symbols
         ) => Computer instructions symbols context actions result
           -> Computer instructions symbols context actions result
reset = convert

-- Given a pairing between instructions and symbols and instructions and
-- symbols', 'convert' a computer, specifically the translation value in
-- context, to work over the new symbol set. The default is simply
-- 'return return' which is an identity.
convert :: forall instructions symbols symbols' context actions result.
           ( Pairing instructions symbols
           , Pairing instructions symbols'
           , Functor context
           , Monad actions
           ) => CofreeT instructions context
                  (actions (Translation symbols actions result))
             -> CofreeT instructions context
                  (actions (Translation symbols' actions result))
convert = fmap (const (return return))

{-# LANGUAGE DeriveFunctor #-}
module Ef.Reader where

import Ef

type ReaderT r c a = Narrative (Reader r) c a
newtype Reader r k = Reader { runReader :: r -> k }
  deriving (Functor)

{-# INLINE runReaderT #-}
runReaderT :: Monad c => Narrative (Reader r) c a -> r -> c a
runReaderT n rd = foldn return join (`runReader` rd) n

{-# INLINE ask #-}
ask :: Narrative (Reader r) c r
ask = send (Reader id)

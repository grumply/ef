{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.Reader.Methods
    ( Reader(..)
    , reader
    ) where


import Ef.Object


data Reader r k = Reader r k


reader :: r -> Use (Reader r) scope environment
reader r = Reader r pure

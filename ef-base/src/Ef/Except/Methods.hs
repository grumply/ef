{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef.Except.Methods
    ( Excepts(..)
    , excepter
    ) where

import Ef.Object

import Control.Exception (SomeException(..))


data Excepts k = Excepts (SomeException -> k)

excepter :: Method Excepts contexts environment
excepter =
    let uncaught err = "Impossible uncaught checked exception: " ++ show err
    in Excepts (error . uncaught)

{-# INLINE excepter #-}

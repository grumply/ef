{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Except (module Ef.Except.Messages) where

import Ef.Ma

import Ef.Except.Messages
import Ef.Except.Methods

instance Ma Excepts Except where
    ma use (Excepts k) (Throw e k') = use (k e) k'

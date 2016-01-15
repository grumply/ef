{-# LANGUAGE GADTs #-}
module Ef.Lang.Get.Attribute
    ( Gets(..)
    ) where



import Ef.Core.Object



data Gets k
  where

    Gets
        :: (Object attrs parent,k)
        -> k
        -> k
        -> Gets k

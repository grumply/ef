{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ef.Lang.Get.Attribute
    ( Attribute(..)
    , Gets
    ) where



import Ef.Core.Object



data Attribute k
  where

    Getter
        :: (Object attrs parent,k)
        -> k
        -> k
        -> Attribute k



type Gets contexts environment =
    Has Attribute contexts environment


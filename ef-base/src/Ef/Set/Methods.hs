{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Ef.Set.Methods
    ( Set(..)
    , set
    ) where


import Ef.Object


data Set k where
    Set :: (Object contexts environment -> k) -> Set k


set :: Use Set contexts environment
set = Set (const . pure)

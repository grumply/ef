{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Set.Messages
    ( Set(..)
    , become
    ) where


import Ef.Narrative
import Ef.Messages
import Ef.Ma
import Ef.Object


data Set k where
    Set :: Object methods super -> k -> Set k


become :: Ma (Methods methods) (Messages self)
       => Object methods super
       -> Invoke Set self super ()

become = self . flip Set ()

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef.Fiber.Methods
    ( Fiber(..)
    , fiber
    ) where


import Ef


data Fiber k = Fiber Int k


fiber :: Use Fiber attrs parent
fiber =
    Fiber 0 $ \fs ->
        let Fiber i k = view fs
            i' = succ i
        in i' `seq` pure $ fs .= Fiber i' k

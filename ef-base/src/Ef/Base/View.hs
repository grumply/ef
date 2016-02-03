{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Context.View
    ( module Ef.Context.View.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Context.View.Lexemes
import Ef.Context.View.Context



instance Inflection (Views r) (View r)
  where

    inflect use (Views r k) (View rk) =
        inflect use (r,k) rk

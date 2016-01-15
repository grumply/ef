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
module Ef.Lang.View
    ( module Ef.Lang.View.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Lang.View.Lexemes
import Ef.Lang.View.Context



instance Inflection (Views r) (View r)
  where

    inflect use (Views r k) (View rk) =
        inflect use (r,k) rk

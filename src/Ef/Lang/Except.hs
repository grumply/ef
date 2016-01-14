{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Checked
    ( module Ef.Lang.Except.Lexemes
    ) where



import Ef.Core.Inflect

import Ef.Lang.Except.Lexemes
import Ef.Lang.Except.Context



instance Inflection Attribute Lexicon
  where

    inflect use (Except k) (Throw e k') =
        use (k e) k'


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
module Ef.Except
    ( module Ef.Except.Lexemes
    ) where



import Ef.Ma

import Ef.Except.Lexemes
import Ef.Except.Context



instance Ma Excepts Except
    where

        ma use (Excepts k) (Throw e k') =
            use (k e) k'


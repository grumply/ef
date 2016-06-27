{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Ef.Ma where

import Ef.Traits
import Ef.Messages

class Ma f g | f -> g, g -> f where
  ma :: (a -> b -> r) -> f a -> g b -> r

instance Ma ((->) a) ((,) a) where
  ma use f = uncurry (use . f)

instance Ma ((,) a) ((->) a) where
  ma use ~(l,r) g = use r (g l)

instance Ma (Traits '[]) (Messages '[])

instance ( trait `Ma` message, Traits traits `Ma` Messages messages)
     => Ma (Traits (trait ': traits)) (Messages (message ': messages))
    where

        ma use (Trait trait _) (Message message) =
            ma use trait message

        ma use (Trait _ traits) (Further messages) =
            ma use traits messages

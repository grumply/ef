{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Ef.Core.Inflect where



import Ef.Core.Object.Attributes
import Ef.Core.Pattern.Symbols



-- Inflect a pairing between f and g with a specific use case
class Inflection f g
    | f -> g
    , g -> f
  where

    inflect
        :: (a -> b -> r)
        -> f a
        -> g b
        -> r



instance Inflection ((->) a) ((,) a)
  where

    inflect use f g =
        uncurry (use . f) g



instance Inflection ((,) a) ((->) a)
  where

    inflect use ~(l,r) g =
        use r (g l)



instance Inflection (Attrs '[]) (Symbol '[])



instance ( Inflection attr symbol
         , Inflection (Attrs attrs) (Symbol symbols)
         )
    => Inflection (Attrs (attr ': attrs)) (Symbol (symbol ': symbols))
  where

    inflect use (Attr attr _) (Symbol symbol) =
        inflect use attr symbol

    inflect use (Attr _ attrs) (Further symbols) =
        inflect use attrs symbols

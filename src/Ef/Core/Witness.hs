{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Ef.Core.Witness where



import Ef.Core.Object.Attributes
import Ef.Core.Pattern.Symbols



-- Witness a pairing between f and g with a specific use case
class Witnessing f g
    | f -> g
    , g -> f
  where

    witness
        :: (a -> b -> r)
        -> f a
        -> g b
        -> r



instance Witnessing ((->) a) ((,) a)
  where

    witness use f g =
        uncurry (use . f) g



instance Witnessing ((,) a) ((->) a)
  where

    witness use ~(l,r) g =
        use r (g l)



instance Witnessing (Attrs '[]) (Symbol '[])



instance ( Witnessing attr symbol
         , Witnessing (Attrs attrs) (Symbol symbols)
         )
    => Witnessing (Attrs (attr ': attrs)) (Symbol (symbol ': symbols))
  where

    witness use (Attr attr _) (Symbol symbol) =
        witness use attr symbol

    witness use (Attr _ attrs) (Further symbols) =
        witness use attrs symbols

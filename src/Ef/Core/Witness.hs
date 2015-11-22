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



instance ( Witnessing i s
         , Witnessing (Attrs is) (Symbol ss)
         )
    => Witnessing (Attrs (i ': is)) (Symbol (s ': ss))
  where

    witness use (Attr ia _) (Symbol  sa) =
        witness use ia sa

    witness use (Attr _ is) (Further ss) =
        witness use is ss

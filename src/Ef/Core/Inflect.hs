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



import Ef.Core.Object.Context
import Ef.Core.Narrative.Lexeme



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



instance Inflection (Context '[]) (Lexeme '[])



instance ( Inflection context lexeme
         , Inflection (Context contexts) (Lexeme lexicon)
         )
    => Inflection (Context (context ': contexts)) (Lexeme (lexeme ': lexicon))
  where

    inflect use (Context context _) (Lexeme lexeme) =
        inflect use context lexeme

    inflect use (Context _ contexts) (Further lexicon) =
        inflect use contexts lexicon

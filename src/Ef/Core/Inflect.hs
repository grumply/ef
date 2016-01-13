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
import Ef.Core.Narrative.Lexicon



-- An inflection that links f and g; a Day convolution. This is the 'why' for
-- Narrative's 'what' and Object's 'how'; axiomatics.
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



instance Inflection (Context '[]) (Lexicon '[])



instance ( Inflection context lexeme
         , Inflection (Context contexts) (Lexicon lexicon)
         )
    => Inflection (Context (context ': contexts)) (Lexicon (lexeme ': lexicon))
  where

    inflect use (Context context _) (Lexeme lexeme) =
        inflect use context lexeme

    inflect use (Context _ contexts) (Further lexicon) =
        inflect use contexts lexicon

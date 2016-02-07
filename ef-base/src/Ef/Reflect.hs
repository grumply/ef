{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.Reflect where


import Ef

import Ef.Get
import Ef.Set


data Reflection self methods super =
    Reflection
        { project :: Narrative self super (Object methods super)
        , inject :: Object methods super -> Narrative self super ()
        }


withReflection
    :: ('[Get,Set] <: self, Monad super, Methods methods `Ma` Messages self)
    => (Reflection self methods super -> Narrative self super result) -> Narrative self super result

withReflection f =
    f Reflection
          { project = introspect
          , inject = become
          }

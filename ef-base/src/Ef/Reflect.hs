module Ef.Reflect where

import Ef

import Ef.Get
import Ef.Set

data Reflection ms ts c =
    Reflection
        { project :: Code ms c (Object ts c)
        , inject :: Object ts c -> Code ms c ()
        }

withReflection :: ( '[Get,Set] <: ms
                  , '[Get,Set] <. ts
                  , Delta (Modules ts) (Messages ms)
                  , Monad c
                  )
               => (Reflection ms ts c -> Code ms c result) -> Code ms c result
withReflection f =
    f Reflection
          { project = introspect
          , inject = become
          }

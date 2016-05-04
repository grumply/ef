module Ef.Reflect where


import Ef

import Ef.Get
import Ef.Set

data Reflection self traits super =
    Reflection
        { project :: Narrative self super (Object traits super)
        , inject :: Object traits super -> Narrative self super ()
        }

withReflection
    :: ('[Get,Set] :> self, Monad super, Traits traits `Ma` Messages self)
    => (Reflection self traits super -> Narrative self super result) -> Narrative self super result

withReflection f =
    f Reflection
          { project = introspect
          , inject = become
          }

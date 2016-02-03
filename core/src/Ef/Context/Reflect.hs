{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Context.Reflect where



import Ef.Core

import Ef.Context.Get
import Ef.Context.Set



data Reflection lexicon contexts environment =
    Reflection
        {
          project
              :: Narrative lexicon environment (Object contexts environment)

        , inject
              :: Object contexts environment
              -> Narrative lexicon environment ()
        }



withReflection
    :: ( Knows Get lexicon environment
       , Knows Set lexicon environment
       , Inflections contexts lexicon
       )
    => (    Reflection lexicon contexts environment
         -> Narrative lexicon environment result
       )
    -> Narrative lexicon environment result

withReflection f =
    f Reflection
          {
            project =
                introspect

          , inject =
                become
          }

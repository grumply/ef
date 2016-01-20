{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.Reflect where



import Ef.Core

import Ef.Lang.Get
import Ef.Lang.Set



data Reflection scope attrs parent =
    Reflection
        {
          project
              :: Pattern scope parent (Object attrs parent)

        , inject
              :: Object attrs parent
              -> Pattern scope parent ()
        }



withReflection
    :: ( Is Getting scope parent
       , Is Setting scope parent
       , (Attrs attrs) `Witnessing` (Symbol scope)
       )
    => (    Reflection scope attrs parent
         -> Pattern scope parent result
       )
    -> Pattern scope parent result

withReflection f =
    f Reflection
          {
            project =
                introspect

          , inject =
                become
          }

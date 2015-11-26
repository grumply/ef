{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.Reflect where



import Ef.Core

import Ef.Lang.Get
import Ef.Lang.Set



data Reflection fs gs m =
    Reflection
        {
          inspect
              :: Pattern fs m (Object gs m)

        , infect
              :: Object gs m
              -> Pattern fs m ()
        }



withReflection
    :: ( Is Getting fs m
       , Is Setting fs m
       , (Attrs gs) `Witnessing` (Symbol fs)
       )
    => (    Reflection fs gs m
         -> Pattern fs m a
       )
    -> Pattern fs m a

withReflection f =
    f Reflection
          {
            inspect =
                introspect

          , infect =
                become
          }

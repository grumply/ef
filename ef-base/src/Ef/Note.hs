module Ef.Note (Book(..), notate) where

import Ef
import Ef.Sync
import Data.Monoid
import Prelude hiding (log)

data NoteTaking ns where
  Write :: ns -> NoteTaking ns
  Watch :: NoteTaking ns
  Finish :: NoteTaking ns

data Book ns ms c = Book
  { write :: ns -> Ef ms c ()
  , watch :: forall r. Ef ms c r -> Ef ms c (r, ns)
  , condense :: forall r. Ef ms c (r, ns -> ns) -> Ef ms c r
  , watches :: forall r b. (ns -> b) -> Ef ms c r -> Ef ms c (r, b)
  , edit :: forall r. (ns -> ns) -> Ef ms c r -> Ef ms c r
  }

notated :: (ms <: '[Sync], Monad c) => (Book ns ms c -> Ef ms c r) -> Synchronized (NoteTaking ns) ns () X ms c (r, ns)
notated computation =
    let notationInterface up =
          Book
            { write = \ns ->
                           do _ <- up (Write ns)
                              return ()
            , watch = \passage ->
                           do _ <- up Watch
                              r <- passage
                              ns <- up Finish
                              _ <- up (Write ns)
                              return (r, ns)
            , condense = \passage ->
                              do _ <- up Watch
                                 (r,mod) <- passage
                                 ns <- up Finish
                                 let new = mod ns
                                 _ <- up (Write new)
                                 return r
            , watches = \view passage ->
                             do _ <- up Watch
                                r <- passage
                                ns <- up Finish
                                let b = view ns
                                return (r, b)
            , edit = \change passage ->
                          do _ <- up Watch
                             r <- passage
                             ns <- up Finish
                             let changed = change ns
                             _ <- up (Write changed)
                             return r
            }
    in synchronized $
         \up _ -> do
           r <- computation (notationInterface up)
           ns <- up Finish
           return (r, ns)

notate :: (ms <: '[Sync], Monad c, Monoid ns) => (Book ns ms c -> Ef ms c r) -> Ef ms c (r, ns)
notate computation = runSync (serve +>> notated computation)
  where
    serve firstRequest =
        synchronized $
        \_ dn ->
             withRespond dn mempty firstRequest
      where
        withRespond respond = handle
          where
            handle current (Write ns) = do
                let new = current <> ns
                next <- respond new
                handle new next
            handle current Finish = do
                next <- respond current
                handle current next -- won't actually happen
            handle current Watch = do
                let new = mempty
                next <- respond new
                watching (handle current) new next
              where
                watching continue current Watch = do
                    let new = mempty
                    next <- respond new
                    watching (watching continue current) new next
                watching continue current (Write ns) = do
                    let new = current <> ns
                    next <- respond new
                    watching continue new next
                watching continue current Finish = do
                    next <- respond current
                    continue next

{-# INLINE notated #-}

{-# INLINE notate #-}

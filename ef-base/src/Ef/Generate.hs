{-# language UndecidableInstances #-}
module Ef.Generate (Generator(..), generate, each, discard, every) where

import Ef
import Ef.Sync
import qualified Data.Foldable as F
import Unsafe.Coerce

newtype Generator ms c a = Select
    { enumerate :: ('[Sync] <: ms, Monad c) => Producer a ms c ()
    }

instance Functor (Generator ms c) where
    fmap f (Select p) = Select (p //> (producer . flip id . f))

instance Applicative (Generator ms c) where
    pure a = Select (producer ($ a))
    mf <*> mx =
        let produce f x = producer ($ f x)
        in Select $ for (enumerate mf) $ for (enumerate mx) . produce

instance (Functor (Messages ms), Monad c) => Monad (Generator ms c) where
    return a =
        let yields yield = yield a
        in Select (producer yields)
    m >>= f = Select $ for (enumerate m) (enumerate . f)
    fail _ = mzero

instance (Functor (Messages ms), Monad c) => Alternative (Generator ms c) where
    empty =
        let ignore = const (return ())
        in Select (producer ignore)
    p1 <|> p2 =
        Select $
        synchronized $
        \up dn ->
             let run xs =
                     runSynchronized
                         (enumerate xs)
                         (unsafeCoerce up)
                         (unsafeCoerce dn)
             in do run p1
                   run p2

instance (Functor (Messages ms), Monad c) => MonadPlus (Generator ms c) where
    mzero = empty
    mplus = (<|>)

instance (Functor (Messages ms), Monad c) => Monoid (Generator ms c a) where
    mempty = empty
    mappend = (<|>)

generate
    :: ('[Sync] <: ms, Monad c)
    => Generator ms c a -> Ef ms c ()
generate l = runSync (enumerate (l >> mzero))

each
    :: ('[Sync] <: ms, Monad c, F.Foldable f)
    => f a -> Producer' a ms c ()
each xs =
    let yields yield = F.foldr (const . yield) (return ()) xs
    in producer yields

discard
    :: (Functor (Messages ms), Monad c)
    => t -> Synchronized a' a b' b ms c ()
discard _ =
    let ignore _ _ = return ()
    in Synchronized ignore

every
    :: ('[Sync] <: ms, Monad c)
    => Generator ms c a -> Producer' a ms c ()
every it = discard >\\ enumerate it

{-# INLINE generate #-}

{-# INLINE each #-}

{-# INLINE discard #-}

{-# INLINE every #-}

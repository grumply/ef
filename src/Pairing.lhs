> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE UndecidableInstances #-}
> module Pairing where

We'll need Comonad/Comonad.Cofree and Monad/Monad.Free for pairing.

> import           Control.Comonad

> import qualified Control.Comonad.Cofree as Cofree
> import           Control.Comonad.Trans.Cofree

> import qualified Control.Monad.Free as Free
> import           Control.Monad.Trans.Free

Identity acts as a monad and a comonad, so we'll use it as a base functor in a
monadic/comonadic stack. We'll pair Identities.

> import Data.Functor.Identity

The pairing class comes with a method pair that takes as its first argument a
witness to the values contained within the two functors given as its second and
third arguments. Without the witness, we're just talking about two functors and
a result. With the witness, there is an implication that a's and b's are able to
be pulled from the functors. That is the key we'll use to pair our instruction
set with our interpreter. When those instructions form a free monad and the
compiler forms a cofree comonad, we can accomplish some automatic pairing as
long as each instruction has a pairing instance with an interpreter.

Nota bene:
At the moment the pairing requires that the algebra matches linearly with the
coalgebra, but this isn't necessary and will eventually change. Things will only
improve in this domain.

> class (Functor f,Functor g) => Pairing f g where
>   pair :: (a -> b -> r) -> f a -> g b -> r

> instance {-# OVERLAPPABLE #-} Pairing f g => Pairing g f where
>   pair p f g = pair (flip p) g f

> instance Pairing Identity Identity where
>   pair f (Identity a) (Identity b) = f a b

> instance {-# OVERLAPPABLE #-} Pairing ((->) a) ((,) a) where
>   pair p f = uncurry (p . f)

> instance Pairing f g => Pairing (Cofree.Cofree f) (Free.Free g) where
>   pair p (a Cofree.:< _) (Free.Pure x) = p a x
>   pair p (_ Cofree.:< fs) (Free.Free gs) = pair (pair p) fs gs

> pairEffect :: (Pairing f g, Comonad w, Monad m)
>            => (a -> b -> r) -> CofreeT f w a -> FreeT g m b -> m r
> pairEffect p s c = do
>   mb <- runFreeT c
>   case mb of
>     Pure x -> return $ p (extract s) x
>     Free gs -> pair (pairEffect p) (unwrap s) gs

> pairEffect' :: (Pairing f g, Comonad w, Monad m)
>            => (a -> b -> r) -> CofreeT f w (m a) -> FreeT g m b -> m r
> pairEffect' p s c = do
>   a  <- extract s
>   mb <- runFreeT c
>   case mb of
>     Pure x -> return $ p a x
>     Free gs -> pair (pairEffect' p) (unwrap s) gs

> class (Functor m, Functor f, Functor g) => PairingM f g m | f g -> m where
>   pairM :: (a -> b -> m r) -> f a -> g b -> m r

> pairEffectM :: (PairingM f g m, Comonad w, Monad m)
>             => (a -> b -> m r) -> CofreeT f w a -> FreeT g m b -> m r
> pairEffectM p s c = do
>   mb <- runFreeT c
>   case mb of
>     Pure x -> p (extract s) x
>     Free gs -> pairM (pairEffectM p) (unwrap s) gs

> pairEffectM' :: (PairingM f g m,Comonad w,Monad m)
>              => (a -> b -> m r) -> CofreeT f w (m a) -> FreeT g m b -> m r
> pairEffectM' p s c = do
>   a  <- extract s
>   mb <- runFreeT c
>   case mb of
>     Pure x -> p a x
>     Free gs -> pairM (pairEffectM' p) (unwrap s) gs

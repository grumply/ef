{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Ef.Narrative
     ( Narrative(..)
     , type (<:)
     , type (:>)
     , self
     , super
     , transform
     , tryAny
     , mapException
     , forException
     , observe
     , unsafeHoist
     , Supertype
     , Subtype
     , Can(..)
     , Upcast(..)

     , Arrative(..)
     , art
     , getA
     , localA
     , constA
     , voidA
     ) where

import Ef.Messages
import Ef.Type.Nat

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Writer (MonadWriter(..))

import Prelude hiding (id,(.))

import Data.IORef

import GHC.Generics

data Narrative self super result
    = forall intermediate.
      Say (Messages self intermediate)
          (    intermediate
             -> Narrative self super result
          )

    | Super (super (Narrative self super result))

    | Return result

    | Fail SomeException

instance ( Upcast (Messages small) (Messages large)
         , Functor super
         )
    => Upcast (Narrative small super) (Narrative large super)
    where

        upcast (Fail e) =
            Fail e

        upcast (Return result) =
            Return result

        upcast (Super sup) =
            Super (fmap upcast sup)

        upcast (Say message k) =
            Say (upcast message) (upcast . k)

instance MonadTrans (Narrative self) where
    lift m = Super (fmap Return m)

instance (Monad super, MonadIO super) => MonadIO (Narrative self super) where
    liftIO m = Super (liftIO (fmap Return m))

instance MonadState s super => MonadState s (Narrative self super) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadReader r super => MonadReader r (Narrative self super) where
  ask = lift ask
  local f = go
    where
      go n =
        case n of
          Return r -> Return r
          Super sup -> Super (local f sup >>= \r -> return (go r))
          Fail e -> Fail e
          Say msg k -> Say msg (go . k)
  reader = lift . reader

instance MonadWriter w super => MonadWriter w (Narrative self super) where
  writer = lift . writer
  tell = lift . tell
  listen n0 = go n0 mempty
    where
      go p w =
        case p of
          Return r -> Return (r,w)
          Super sup -> Super (listen sup >>= \(n',w') -> return (go n' $! mappend w w'))
          Fail e -> Fail e
          Say msg k -> Say msg (flip go w . k)
  pass n0 = go n0 mempty
    where
      go n w =
        case n of
          Return (r,f) -> Super (pass (return (Return r, const (f w))))
          Super sup -> Super (listen sup >>= \(n',w') -> return (go n' $! mappend w w'))
          Fail e -> Fail e
          Say msg k -> Say msg (flip go w . k)


instance Monad super => MonadThrow (Narrative self super) where
  throwM = Fail . toException

instance Monad super => MonadCatch (Narrative self super) where
  catch = catch_
    where
      catch_
          :: ( Functor super
            , Exception e
            )
          => Narrative self super a
          -> (e -> Narrative self super a)
          -> Narrative self super a

      catch_ plan handler =
          rewrite plan
        where
          rewrite (Return r) =
              Return r

          rewrite (Super m) =
              Super (fmap rewrite m)

          rewrite (Say sym bp) =
              Say sym (rewrite . bp)


          rewrite (Fail se) =
              case fromException se of

                  Just e ->
                      handler e

                  Nothing ->
                      Fail se

instance (MonadError e super) => MonadError e (Narrative self super) where
  throwError = lift . throwError
  catchError n f = go n
    where
      go n =
        case n of
          Return r -> Return r
          Super sup -> Super ((sup >>= \n' -> return (go n')) `catchError` (\e -> return (f e)))
          Fail e -> Fail e
          Say msg k -> Say msg (go . k)

unsafeHoist :: (Monad super)
            => (forall x. super x -> super' x) -> Narrative self super r -> Narrative self super' r
unsafeHoist nat = go
  where
    go n =
      case n of
        Return r -> Return r
        Super sup -> Super (nat (sup >>= \n' -> return (go n')))
        Fail e -> Fail e
        Say msg k -> Say msg (go . k)


instance MFunctor (Narrative self) where
  hoist nat n0 = go (observe n0)
    where
      go p =
        case p of
          Return r -> Return r
          Super sup -> Super (nat (sup >>= \n' -> return (go n')))
          Fail e -> Fail e
          Say msg k -> Say msg (go . k)

instance MMonad (Narrative self) where
  embed f = go
    where
      go n =
        case n of
          Return r -> Return r
          Super sup -> f sup >>= go
          Fail e -> Fail e
          Say msg k -> Say msg (go . k)

observe :: Monad super => Narrative self super r -> Narrative self super r
observe n0 = Super (go n0)
  where
    go n =
      case n of
        Return r -> return (Return r)
        Super sup -> sup >>= go
        Say msg k -> return (Say msg $ observe . k)


tryAny :: ( Monad super
          )
       => Narrative self super a
       -> Narrative self super (Either SomeException a)
tryAny = try

mapException :: (Monad super, Exception e, Exception e')
             => (e -> e')
             -> Narrative self super a
             -> Narrative self super a
mapException f n = handle (\e -> throwM (f e)) n

forException  :: (Monad super, Exception e, Exception e')
             => Narrative self super a
             -> (e -> e')
             -> Narrative self super a
forException = flip mapException

data Restore m = Unmasked | Masked (forall x. m x -> m x)

liftMask :: forall super self r. (MonadIO super, MonadCatch super)
         => (forall s . ((forall x . super x -> super x) -> super s) -> super s)
         -> ((forall x . Narrative self super x -> Narrative self super x)
              -> Narrative self super r)
         -> Narrative self super r
liftMask maskVariant k = do
  ioref <- liftIO $ newIORef Unmasked
  let loop :: Narrative self super r -> Narrative self super r
      loop n =
        case n of
          Return r -> Return r
          Fail e -> Fail e
          Super sup -> Super $ maskVariant $ \unmaskVariant -> do
            liftIO $ writeIORef ioref $ Masked unmaskVariant
            sup >>= chunk >>= return . loop
          Say msg k -> Say msg (loop . k)
      unmask :: forall q. Narrative self super q -> Narrative self super q
      unmask n =
        case n of
          Return r -> Return r
          Fail e -> Fail e
          Super sup -> Super $ do
            Masked unmaskVariant <- liftIO $ readIORef ioref
            unmaskVariant (sup >>= chunk >>= return . unmask)
          Say msg k -> Say msg (unmask . k)
      chunk :: forall s. Narrative self super s -> super (Narrative self super s)
      chunk (Super sup) = sup >>= chunk
      chunk s           = return s
  loop $ k unmask

instance (MonadMask super, MonadCatch (Narrative self super), MonadIO super, Monad super) => MonadMask (Narrative self super) where
  mask = liftMask mask
  uninterruptibleMask = liftMask uninterruptibleMask

-- Note super is strictly more powerful than lift; lift can be implemented in
-- terms of super.
super :: Functor super => super (Narrative self super result) -> Narrative self super result
super = Super

{-# INLINE [2] self #-}
self
    :: (Monad super, '[message] <: self)
    => message result -> Narrative self super result
self message = Say (inj message) return

{-# RULES
  "self/bind" [~3] forall m k. self m >>= k = Say (inj m) k;
  "self/then" [~3] forall m k. self m >>  k = Say (inj m) (\_ -> k)
  #-}


type Subtype s t = (<:) s t
-- bounded parametric subtype polymorphism implemented as statically guaranteed set containment.
-- Given S <: T, guarantee that for all elements t of T, there is an element s of S where t is equal to s.
type family (:>) messages (messages' :: [* -> *]) where

    messages :> (message ': '[]) =
      (Can' message messages (Offset message messages))

    messages :> (message ': messages') =
      ( Can' message messages (Offset message messages)
      , messages :> messages'
      )



type Supertype t s = (:>) t s
-- bounded parametric supertype polymorphism implemented as statically guaranteed set containment.
-- Given T :> S, guarantee that for all elements t of T, there is an element s of S where t is equal to s.
type family (<:) (messages' :: [* -> *]) messages where

    (message ': '[]) <: messages =
        (Can' message messages (Offset message messages))

    (message ': messages') <: messages =
        ( Can' message messages (Offset message messages)
        , messages' <: messages
        )



instance Functor super
    => Functor (Narrative self super)
  where

    fmap =
        _fmap

{-# NOINLINE [2] _fmap #-}
_fmap
    :: Functor super
    => (a -> b)
    -> Narrative self super a
    -> Narrative self super b

_fmap f =
    go
  where

    go (Fail e) =
        Fail e

    go (Return a) =
        Return (f a)

    go (Super m) =
        Super (fmap go m)

    go (Say message k) =
        Say message (go . k)

{-# RULES

    "_fmap f (Fail e)"
        forall e f .
            _fmap f (Fail e) =
                Fail e
    ;

    "_fmap f (Say message k)"
        forall message k f .
            _fmap f (Say message k) =
                Say message (_fmap f . k)
    ;

    "_fmap f (Super m)"
        forall m f .
            _fmap f (Super m) =
                Super (fmap (_fmap f) m)
    ;

    "_fmap f (Return result)"
        forall result f .
            _fmap f (Return result) =
                Return (f result)
    ;

  #-}

instance Monad super
    => Applicative (Narrative self super)
  where

    pure =
        return



    (<*>) =
        ap



    (*>) =
        (>>)



instance Monad super
    => Monad (Narrative self super)
  where

    {-# INLINE return #-}
    return =
        Return

    {-# INLINE [2] (>>=) #-}
    (>>=) =
        _bind

    {-# INLINE [2] (>>) #-}
    (>>) =
        _then

{-# NOINLINE [2] _bind #-}
_bind
    :: Monad super
    => Narrative self super intermediate
    -> (intermediate -> Narrative self super result)
    -> Narrative self super result

p0 `_bind` f =
    go p0
  where

    go (Fail e) =
        Fail e

    go (Say message k) =
        Say message (go . k)

    go (Return res) =
        f res

    go (Super m) =
        Super (fmap go m)

{-# RULES

    "_bind (Fail e) f"
        forall e f .
            _bind (Fail e) f =
                Fail e
    ;

    "_bind (Say message k) f"
        forall message k f .
            _bind (Say message k) f =
                Say message (\a -> _bind (k a) f)
    ;

    "_bind (Super m) f"
        forall m f .
            _bind (Super m) f =
                Super (m >>= \p -> return (_bind p f))
    ;

    "_bind (Return result) f"
        forall result f .
            _bind (Return result) f =
                f result
    ;

  #-}

{-# NOINLINE [2] _then #-}
_then
    :: Monad super
    => Narrative self super intermediate
    -> Narrative self super result
    -> Narrative self super result

p0 `_then` f =
    go p0
  where

    go (Fail e) =
        Fail e

    go (Say message k) =
        Say message (go . k)

    go (Return res) =
        f

    go (Super m) =
        Super (fmap go m)

{-# RULES

    "_then (Fail e) f"
        forall e f .
            _then (Fail e) f =
                Fail e
    ;

    "_then (Say message k) f"
        forall message k f .
            _then (Say message k) f =
                Say message (\a -> _then (k a) f)
    ;

    "_then (Super m) f"
        forall m f .
            _then (Super m) f =
                Super (m >>= \p -> return (_then p f))
    ;

    "_then (Return result) f"
        forall result f .
            _then (Return result) f =
                f
    ;

  #-}

instance MonadPlus super
    => MonadPlus (Narrative self super)
  where

    mzero =
        super mzero



    mplus =
        _mplus

{-# NOINLINE [2] _mplus #-}
_mplus
    :: MonadPlus super
    => Narrative self super result
    -> Narrative self super result
    -> Narrative self super result

_mplus p0 p1 =
    go p0
  where

    go (Super m) =
        Super (fmap go m)

    go (Say message k) =
        Say message (go . k)

    go (Fail _) =
        p1

    go result =
        result

{-# RULES

    "_mplus (Fail e) r"
        forall e r.
          _mplus (Fail e) r =
            r
    ;

    "_mplus (Super sup) r"
        forall sup r.
          _mplus (Super sup) r =
            Super (fmap (\x -> _mplus x r) sup)
    ;

    "_mplus (Return res) r"
        forall r res.
          _mplus (Return res) r =
            Return res
     ;

    "_mplus (Say message k) f"
        forall r message k.
          _mplus (Say message k) r =
            Say message ((\x -> _mplus x r) . k)
    ;


  #-}

instance MonadPlus super
    => Alternative (Narrative self super)
  where

    empty =
        mzero



    (<|>) =
        mplus

instance ( Monad super
         , Monoid result
         )
    => Monoid (Narrative self super result)
  where

    mempty =
        pure mempty



    mappend =
        _mappend

{-# NOINLINE [2] _mappend #-}
_mappend
    :: ( Monad super
       , Monoid result
       )
    => Narrative self super result
    -> Narrative self super result
    -> Narrative self super result

_mappend p0 p1 =
    go p0
  where

    go (Fail e) =
        Fail e

    go (Return result) =
        fmap (mappend result) p1

    go (Super m) =
        Super (fmap go m)

    go (Say message k) =
        Say message (go . k)

{-# RULES

    "_mappend (Fail e) r"
        forall e r.
          _mappend (Fail e) r =
            Fail e
    ;

    "_mappend (Super sup) r"
        forall sup r.
          _mappend (Super sup) r =
            Super (fmap (\x -> _mappend x r) sup)
    ;

    "_mappend (Return res) r"
        forall r res.
          _mappend (Return res) r =
            fmap (mappend res) r
     ;

    "_mappend (Say message k) f"
        forall r message k.
          _mappend (Say message k) r =
            Say message ((\x -> _mappend x r) . k)
    ;


  #-}

data Transform (self :: [* -> *]) (super :: * -> *) (result :: *)
  = Transform (forall x. Messages self x -> (x -> Narrative self super result) -> Narrative self super result)

{-# INLINE applyTransform #-}
applyTransform
    :: Transform self super result
    -> Messages self x
    -> (x -> Narrative self super result)
    -> Narrative self super result

applyTransform (Transform t) = _applyTransform t

{-# NOINLINE [2] _applyTransform #-}
_applyTransform t m f = t m f

{-# RULES
  "_applyTransform (Transform t) m f"
    forall t m f.
      _applyTransform t m f =
        t m f
  #-}

-- This approach to transformations tries to maintain that
--     lift (x >>= y) = lift x >>= (lift . y)
-- as well as
--     throw e >> _ = throw e
{-# INLINE transform #-}
transform
    :: Functor super
    => (forall x. Messages self x -> (x -> Narrative self super result) -> Narrative self super result)
    -> Narrative self super result
    -> Narrative self super result

transform f = _transform (Transform f)

{-# NOINLINE [2] _transform #-}
_transform
    :: Functor super
    => Transform self super result
    -> Narrative self super result
    -> Narrative self super result

_transform (Transform t) =
    go
    where

        go (Say message k) =
           t message k

        go (Super sup) =
            Super (fmap go sup)

        go (Fail e) =
            Fail e

        go (Return r) =
            Return r

{-# RULES

    "_transform t (Fail e) == Fail e"
        forall t e.
            _transform t (Fail e) =
                Fail e
    ;

    "_transform t (Super sup) == Super (fmap (_transform t) sup)"
        forall t sup.
            _transform t (Super sup) =
                Super (fmap (_transform t) sup)
    ;

    "_transform t (Return r) == Return r"
        forall t r.
            _transform t (Return r) =
                Return r
     ;

    "_transform t (Say message k)"
        forall t message k.
            _transform t (Say message k) =
                applyTransform t message k
    ;

  #-}

localA :: b -> Arrative self super b r -> Arrative self super a r
localA a (Arrative ar) = Arrative $ \_ -> ar a

getA :: Monad super => Arrative self super b b
getA = Arrative return

-- Defined purely to avoid the existence of the invalid ArrowLoop instance.
-- This implementation is equivalent to but strictly less generic than Kleisli.
newtype Arrative self super a b = Arrative { runArrative :: a -> Narrative self super b }
instance Monad super => Category (Arrative self super) where
  id = Arrative return
  (Arrative f) . (Arrative g) = Arrative (\b -> g b >>= f)

instance Monad super => Arrow (Arrative self super) where
  arr f = Arrative (return . f)
  first (Arrative f) = Arrative (\ ~(b,d) -> f b >>= \c -> return (c,d))
  second (Arrative f) = Arrative  (\ ~(d,b) -> f b >>= \c -> return (d,c))

instance MonadPlus super => ArrowZero (Arrative self super) where
  zeroArrow = Arrative (\_ -> mzero)

instance MonadPlus super => ArrowPlus (Arrative self super) where
  Arrative f <+> Arrative g = Arrative (\x -> f x `mplus` g x)

instance Monad super => ArrowChoice (Arrative self super) where
  left f = f +++ arr id
  right f = arr id +++ f
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  Arrative f ||| Arrative g = Arrative (either f g)

instance Monad super => ArrowApply (Arrative self super) where
  app = Arrative (\(Arrative f,x) -> f x)

-- The (* -> *) classes for Arrative are reader-style arrows.
instance Functor super => Functor (Arrative self super a) where
  fmap f (Arrative n) = Arrative (fmap (fmap f) n)

instance Monad super => Applicative (Arrative self super a) where
  pure x = Arrative (arr (const (return x)))
  Arrative f <*> Arrative x = Arrative $ \r -> do
    g <- f r
    fmap g (x r)

instance Monad super => Monad (Arrative self super a) where
  (Arrative a) >>= f = Arrative $ \r -> do
    x <- a r
    let g = f x
    runArrative g r

instance MonadPlus super => Alternative (Arrative self super a) where
  empty = zeroArrow
  x <|> y = x <+> y

instance MonadPlus super => MonadPlus (Arrative self super a) where
  mzero = zeroArrow
  x `mplus` y = x <+> y

instance (Monad super, MonadIO super) => MonadIO (Arrative self super a) where
  liftIO = constA . liftIO

instance MonadState s super => MonadState s (Arrative self super a) where
  get = constA get
  put = constA . put
  state = constA . state

instance MonadReader r super => MonadReader r (Arrative self super a) where
  ask = constA ask
  local l (Arrative a) = Arrative $ local l . a
  reader = constA . reader

instance MonadWriter w super => MonadWriter w (Arrative self super a) where
  writer = constA . writer
  tell = constA . tell
  listen (Arrative a) = Arrative $ listen . a
  pass (Arrative a) = Arrative $ pass . a

instance Monad super => MonadThrow (Arrative self super a) where
  throwM = constA . throwM

instance Monad super => MonadCatch (Arrative self super a) where
  catch (Arrative a) f = Arrative $ \n ->
    catch (a n) (\e -> runArrative (f e) n)

instance (MonadError e super) => MonadError e (Arrative self super a) where
  throwError = constA . throwError
  catchError (Arrative a) f = Arrative $ \n ->
    catchError (a n) (\e -> runArrative (f e) n)

instance (MonadMask super, MonadCatch (Narrative self super), MonadIO super, Monad super) => MonadMask (Arrative self super a) where
  -- mask :: ((forall x. Arrative self super a x -> Arrative self super a x) -> Arrative self super a b) -> Arrative self super a b
  mask = liftMaskA mask
  uninterruptibleMask = liftMaskA uninterruptibleMask


liftMaskA :: forall super self a r. (MonadIO super, MonadCatch super)
         => (forall s . ((forall x . super x -> super x) -> super s) -> super s)
         -> ((forall x . Arrative self super a x -> Arrative self super a x)
              -> Arrative self super a r)
         -> Arrative self super a r
liftMaskA maskVariant k = Arrative $ \a -> do
  ioref <- liftIO $ newIORef Unmasked
  let loop :: Arrative self super a r -> Arrative self super a r
      loop n = Arrative $ \a ->
        case runArrative n a of
          Return r -> Return r
          Fail e -> Fail e
          Super sup -> Super $ maskVariant $ \unmaskVariant -> do
            liftIO $ writeIORef ioref $ Masked unmaskVariant
            sup >>= chunk . constA >>= return . flip runArrative a . loop
          Say msg k -> Say msg (flip runArrative a . loop . constA . k)
      unmask :: forall q. Arrative self super a q -> Arrative self super a q
      unmask n = Arrative $ \a ->
        case runArrative n a of
          Return r -> Return r
          Fail e -> Fail e
          Super sup -> Super $ do
            Masked unmaskVariant <- liftIO $ readIORef ioref
            unmaskVariant (sup >>= chunk . constA >>= return . flip runArrative a . unmask)
          Say msg k -> Say msg (flip runArrative a . unmask . constA . k)
      chunk :: forall s. Arrative self super a s -> super (Arrative self super a s)
      chunk ar =
        case runArrative ar a of
          (Super sup) -> sup >>= chunk . constA
          s           -> return (constA s)
  runArrative (loop $ k unmask) a

art :: (Monad super) => (a -> Narrative self super b) -> Arrative self super a b
art = Arrative

constA :: (Monad super) => Narrative self super b -> Arrative self super a b
constA = Arrative . const

voidA :: (Monad super) => Arrative self super a ()
voidA = constA (return ())

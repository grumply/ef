{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE StandaloneDeriving        #-}
module New where

import           Language.Haskell.TH.Syntax

import           Control.Applicative
import           Control.Comonad
import           Control.Comonad.Trans.Cofree
import qualified Control.Comonad.Cofree as Cofree
import           Control.Exception (Exception,IOException)
import           Control.Monad
import qualified Control.Exception as E
import qualified Control.Monad.Catch as MC
import           Control.Monad.Fix
import qualified Control.Monad.Free as Free
import qualified Control.Monad.Trans as Trans
import           Control.Monad.Trans.Free
import           Control.Monad.IO.Class
import qualified Unsafe.Coerce as UNSAFE

import           Data.Bifunctor
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Proxy
import           Data.Type.Equality

--------------------------------------------------------------------------------
-- Constraints and utilities for type-level lists

class NotIn (x :: * -> *) (ys :: [* -> *])
instance NotIn x '[]
instance ((x == y) ~ 'False,NotIn x ys) => NotIn x (y ': ys)

data Nat = Z | S Nat

data Posn (n :: Nat) = Posn

type family FindPosn (f :: * -> *) fs :: Nat where
  FindPosn f (f ': fs) = Z
  FindPosn f (any ': fs) = S (FindPosn f fs)

class Contains (xs :: [* -> *]) (ys :: [* -> *])
instance (In x ys,Contains xs ys) => Contains (x ': xs) ys

class In (x :: * -> *) (ys :: [* -> *])
instance In x (x ': ys)
instance (In x ys) => In x (y ': ys)
--------------------------------------------------------------------------------
-- Linear type-level union of functors

data Pointer (r :: [* -> *]) a where
  Exact :: (Functor f) => f a -> Pointer (f ': r) a
  Other :: (Functor any) => Pointer r a -> Pointer (any ': r) a
deriving instance Functor (Pointer r)

class PrjPointer' f r (n :: Nat) where
  prjPointer' :: Posn n -> Pointer r a -> Maybe (f a)

instance (r ~ (f ': r')) => PrjPointer' f r Z where
  prjPointer' _ (Exact fa) = Just fa

instance (r ~ (f' ': r'),PrjPointer' f r' (FindPosn f r')) => PrjPointer' f r (S n) where
  prjPointer' _ (Other loc) = prjPointer' (Posn :: Posn (FindPosn f r')) loc

class PrjPointer f r where
  prjPointer :: Pointer r a -> Maybe (f a)

instance (PrjPointer' f r (FindPosn f r)) => PrjPointer f r where
  prjPointer = prjPointer' (Posn :: Posn (FindPosn f r))

class InjPointer' f r (n :: Nat) where
  injPointer' :: Posn n -> f a -> Pointer r a

instance (Functor f,NotIn f r',r ~ (f ': r')) => InjPointer' f r Z where
  injPointer' _ = Exact

instance (Functor f',NotIn f r',NotIn f' r',r ~ (f' ': r'),InjPointer' f r' (FindPosn f r')) => InjPointer' f r (S n) where
  injPointer' _ = Other . injPointer' (Posn :: Posn (FindPosn f r'))

class InjPointer f r where
  injPointer :: f a -> Pointer r a

instance (InjPointer' f r (FindPosn f r)) => InjPointer f r where
  injPointer = injPointer' (Posn :: Posn (FindPosn f r))

data Table (r :: [* -> *]) a where
  Zero :: Table '[] a
  More :: f a -> Table r a -> Table (f ': r) a
zero :: Table '[] a
zero = Zero

class PrjTable' f r (n :: Nat) where
  prjTable' :: Posn n -> Table r a -> Maybe (f a)

instance (r ~ '[]) => PrjTable' f r n where
  prjTable' _ _ = Nothing

instance (r ~ (f ': r')) => PrjTable' f r Z where
  prjTable' _ (More x _) = Just x

instance (r ~ (f' ': r'),PrjTable' f r' (FindPosn f r')) => PrjTable' f r (S n) where
  prjTable' _ (More _ m) = prjTable' (Posn :: Posn (FindPosn f r')) m
  prjTable' _ _ = Nothing

class PrjTable f r where
  prjTable :: Table r a -> Maybe (f a)

instance (PrjTable' f r (FindPosn f r)) => PrjTable f r where
  prjTable = prjTable' (Posn :: Posn (FindPosn f r))

class InjTable' f r (n :: Nat) where
  injTable' :: Posn n -> f a -> Table r a -> Table r a

instance (r ~ (f ': r')) => InjTable' f r Z where
  injTable' _ fa (More _ m) = More fa m

instance (r ~ (f' ': r'),InjTable' f r' (FindPosn f r')) => InjTable' f r (S n) where
  injTable' _ fa (More x m) = More x (injTable' (Posn :: Posn (FindPosn f r')) fa m)

class InjTable f r where
  injTable :: f a -> Table r a -> Table r a

instance (InjTable' f r (FindPosn f r)) => InjTable f r where
  injTable = injTable' (Posn :: Posn (FindPosn f r))

class Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f g = uncurry (p . f) g

instance Pairing ((,) a) ((->) a) where
  pair p (l,r) g = p r (g l)

instance (Pairing' f gs (FindPosn g gs),Pairing f g) => Pairing f (Table gs) where
  pair = pair' (Posn :: Posn (FindPosn g gs))

class Pairing' (f :: * -> *) (gs :: [* -> *]) (n :: Nat) where
  pair' :: Posn n -> (a -> b -> r) -> f a -> Table gs b -> r

instance (Pairing f g,Pairing' f gs' (FindPosn g gs'),gs ~ (x ': gs'))
  => Pairing' f gs (S n)
  where
    pair' _ abr f (More _ m) = pair' (Posn :: Posn (FindPosn g gs')) abr f m

instance (Pairing f g,gs ~ (g ': gs'),FindPosn g gs ~ Z)
  => Pairing' f gs Z
  where
    pair' _ abr f (More x _) = pair abr f x

instance (Pairing' f gs (FindPosn g gs),Pairing'' fs gs (FindPosn g gs),Pairing f g)
  => Pairing (Pointer fs) (Table gs)
  where
    pair = pair'' (Posn :: Posn (FindPosn g gs))

class Pairing'' (fs :: [* -> *]) (gs :: [* -> *]) (n :: Nat) where
  pair'' :: Posn n -> (a -> b -> r) -> Pointer fs a -> Table gs b -> r

instance (Contains '[f] gs,Pairing f g,gs ~ (g ': gs')) => Pairing'' '[f] gs Z where
  pair'' _ p (Exact symbol) (More instruction _) = pair p symbol instruction

instance (Contains fs gs,Pairing f g,gs ~ (g' ': gs'),fs ~ (f' ': fs'),Pairing'' fs' gs' (FindPosn g gs'))
  => Pairing'' fs gs (S n)
  where
    pair'' _ p (Other m) (More _ m') = pair'' (Posn :: Posn (FindPosn g gs')) p m m'

delta cof f = do
  a <- extract cof
  s <- runFreeT f
  case s of
    Free symbol ->
      pair delta (unwrap cof) symbol
    Pure result ->
      return
        (toComp $ fmap (bimap (const (return a)) id) $ fromComp cof,result)

fromComp = coerce :: CofreeT f w (m a) -> w (CofreeF f (m a) (CofreeT f w (m a)))
toComp = coerce :: w (CofreeF f (m a) (CofreeT f w (m a))) -> CofreeT f w (m a)


--------------------------------------------------------------------------------
-- Pattern synonyms for working with free monads

pattern If fb <- (runFree -> Free fb)
pattern Result x <- (runFree -> Pure x)

pattern Case x <- (If (prjPointer -> Just x))
pattern Done <- (Result _)

--------------------------------------------------------------------------------
-- Fixpoint for free

-- This method may be built with a partially saturated iterT from free
class (Functor g,Monad m) => Fixable g m where
  fixable :: FreeT g m a -> m a

instance (MonadFix m,Fixable f m) => MonadFix (FreeT f m) where
  mfix = Trans.lift . mfix . (fixable .)

---------------------------------------------------------------------------------
-- Lift instances for FreeT/FreeF/Identity

instance (Lift (f b),Lift a) => Lift (FreeF f a b) where
  lift (Pure x) = [| Pure x |]
  lift (Free fb) = [| Free fb |]

instance (Lift (m (FreeF f a (FreeT f m a)))) => Lift (FreeT f m a) where
  lift (FreeT f) = [| FreeT f |]

instance Lift a => Lift (Identity a) where
  lift (Identity a) = [| Identity a |]

--------------------------------------------------------------------------------
-- Overridden show methods for FreeT, etc...

showFT :: (Show (f a),Show a,Show (f (FreeT f Identity a))) => FreeT f Identity a -> String
showFT f = show $ runIdentity $ runFreeT f

showF :: (Show (f b),Show a) => FreeF f a b -> String
showF (Free fb) = show fb
showF (Pure a) = show a

showF' :: (Show (f (Free.Free f a)),Show a) => Free.Free f a -> String
showF' (Free.Free fb) = show fb
showF' (Free.Pure a) = show a


-- joinFree :: (Monad m) => m (FreeT f m a) -> FreeT f m a
-- joinFree = FreeT . join . fmap runFreeT

-- -- -- reset the translation value in context contained within a computer.
-- -- -- This is a more restricted version of convert to guarantee that a
-- -- -- Computer will not be converted to work with a different symbol set.
-- -- reset :: ( Comonad context
-- --          , Functor instructions
-- --          , Monad actions
-- --          , Pairing instructions symbols
-- --          ) => Computer instructions symbols context actions result
-- --            -> Computer instructions symbols context actions result
-- -- reset = convert

-- -- -- Given a pairing between instructions and symbols and instructions and
-- -- -- symbols', 'convert' a computer, specifically the translation value in
-- -- -- context to work over the new symbol set. The default is simply
-- -- -- 'return return' which is an identity.
-- -- convert :: forall instructions symbols symbols' context actions result.
-- --            ( Pairing instructions symbols
-- --            , Pairing instructions symbols'
-- --            , Functor instructions
-- --            , Comonad context
-- --            , Monad actions
-- --            ) => CofreeT instructions context
-- --                   (actions (Translation symbols actions result))
-- --              -> CofreeT instructions context
-- --                   (actions (Translation symbols' actions result))
-- -- convert = fmap (const (return return))

--------------------------------------------------------------------------------
-- Checked exceptions

-- | Checked exceptions
class Throws e where
  throwChecked :: MC.MonadThrow m => e -> m a

-- | Wrap an action that may throw a checked exception
--
-- This is used internally in 'rethrowUnchecked' to avoid impredicative
-- instantiation of the type of 'unsafeCoerce'.
newtype Wrap e m a = Wrap (Throws e => m a)

-- | Rethrow checked exceptions as unchecked (regular) exceptions
rethrowUnchecked :: forall e a m. MC.MonadThrow m
                               => (Throws e    => m a)
                               -> (Exception e => m a)
rethrowUnchecked act = aux act MC.throwM
  where
    aux :: (Throws e => m a) -> ((e -> m a) -> m a)
    aux = UNSAFE.unsafeCoerce . Wrap

-- | Catch a checked exception
--
-- This is the only way to discharge a 'Throws' type class constraint.
catchChecked :: (MC.MonadThrow m,MC.MonadCatch m,Exception e) => (Throws e => m a) -> (e -> m a) -> m a
catchChecked = MC.catch . rethrowUnchecked

-- | 'catchChecked' with the arguments reversed
handleChecked :: (MC.MonadCatch m,Exception e) => (e -> m a) -> (Throws e => m a) -> m a
handleChecked act handler = catchChecked handler act

-- | Throw an unchecked exception
--
-- This is just an alias for 'throw', but makes it evident that this is a very
-- intentional use of an unchecked exception.
throwUnchecked :: (MC.MonadThrow m,Exception e) => e -> m a
throwUnchecked = MC.throwM

-- | Rethrow IO exceptions as checked exceptions
checkIO :: (MonadIO m,MC.MonadCatch m,Throws IOException) => IO a -> m a
checkIO = MC.handle (\(ex :: IOException) -> throwChecked ex) . liftIO

--------------------------------------------------------------------------------
-- Testing

data Get st k = Get (st -> k)
  deriving Functor
data CoGet st k = CoGet st k
  deriving Functor
instance Pairing (CoGet st) (Get st) where
  pair p (CoGet st k) (Get stk) = pair p (st,k) stk
get :: (MonadFree (Pointer r) m,InjPointer (Get st) r) => m st
get = liftF (injPointer (Get id))

data Put st k = Put st k -- k ~ (() -> k)
  deriving Functor
data CoPut st k = CoPut (st -> k)
  deriving Functor
instance Pairing (CoPut st) (Put st) where
  pair p (CoPut stk) (Put st k) = pair p stk (st,k)
put :: (MonadFree (Pointer r) m,InjPointer (Put st) r) => st -> m ()
put x = liftF (injPointer (Put x ()))


coGet :: st -> k -> CoGet st k
coGet st wa = CoGet st wa

coPut :: k -> CoPut st k
coPut wa = CoPut (const wa)

newtype TestState = TestState { runTestState :: Int }
increment :: TestState -> TestState
increment = TestState . succ . runTestState

type State st = Pointer '[Get st,Put st]

test
  :: (MonadFree (Pointer r) m,
      InjPointer' (Put TestState) r (FindPosn (Put TestState) r),
      InjPointer' (Get b) r (FindPosn (Get b) r),
      InjPointer' (Get TestState) r (FindPosn (Get TestState) r)) =>
     m b
test = do
  st <- get
  put (increment st)
  get

test' :: (MonadFree (Pointer r) m,InjPointer (Put TestState) r) => m ()
test' = put (TestState 1)

build st = injTable coPut
         $ injTable (coGet st)
         $ More undefined (More undefined Zero)

main = do
  let (comp,()) = runIdentity $ delta (coiterT build (Identity (return ()))) test'
  return ()

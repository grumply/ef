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
{-# LANGUAGE InstanceSigs              #-}
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



-- data Table (r :: [* -> *]) a where
--   Zero :: Table '[] a
--   More :: (Functor f,NotElem f r) => f a -> Table r a -> Table (f ': r) a
-- deriving instance Functor (Table r)
-- zero :: Table '[] a
-- zero = Zero

-- more :: (Functor f,NotElem f r) => f a -> Table r a -> Table (f ': r) a
-- more = More

-- class Build (xs :: [* -> *]) where
--   build :: Table xs a
-- instance Build '[] where
--   build = zero
-- instance (Build xs,Functor x,NotElem x xs) => Build (x ': xs) where
--   build = more undefined (build :: Table xs a)

-- class PrjTable' f r (n :: Nat) where
--   prjTable' :: Index n -> Table r a -> Maybe (f a)

-- instance (r ~ '[]) => PrjTable' f r n where
--   prjTable' _ _ = Nothing

-- instance (r ~ (f ': r')) => PrjTable' f r Z where
--   prjTable' _ (More x _) = Just x

-- instance (r ~ (f' ': r'),PrjTable' f r' (IndexOf f r')) => PrjTable' f r (S n) where
--   prjTable' _ (More _ m) = prjTable' (Index :: Index (IndexOf f r')) m
--   prjTable' _ _ = Nothing

-- class PrjTable f r where
--   prjTable :: Table r a -> Maybe (f a)

-- instance (PrjTable' f r (IndexOf f r)) => PrjTable f r where
--   prjTable = prjTable' (Index :: Index (IndexOf f r))

-- class InjTable' f r (n :: Nat) where
--   injTable' :: Index n -> f a -> Table r a -> Table r a

-- instance (r ~ (f ': r')) => InjTable' f r Z where
--   injTable' _ fa (More _ m) = More fa m

-- instance (r ~ (f' ': r'),InjTable' f r' (IndexOf f r')) => InjTable' f r (S n) where
--   injTable' _ fa (More x m) = More x (injTable' (Index :: Index (IndexOf f r')) fa m)

-- class InjTable f r where
--   injTable :: f a -> Table r a -> Table r a

-- instance (InjTable' f r (IndexOf f r)) => InjTable f r where
--   injTable = injTable' (Index :: Index (IndexOf f r))

-- class Pairing f g where
--   pair :: (a -> b -> r) -> f a -> g b -> r

-- instance Pairing Identity Identity where
--   pair f (Identity a) (Identity b) = f a b

-- instance Pairing ((->) a) ((,) a) where
--   pair p f g = uncurry (p . f) g

-- instance Pairing ((,) a) ((->) a) where
--   pair p (l,r) g = p r (g l)

-- instance (Pairing' f gs (IndexOf g gs),Pairing f g) => Pairing f (Table gs) where
--   pair = pair' (Index :: Index (IndexOf g gs))

-- instance (Pairing' f gs (IndexOf g gs),Pairing'' fs gs (IndexOf g gs),Pairing f g)
--   => Pairing (Table gs) (Pointer fs)
--   where
--     pair p = flip (pair'' (Index :: Index (IndexOf g gs)) (flip p))

-- class Pairing' (f :: * -> *) (gs :: [* -> *]) (n :: Nat) where
--   pair' :: Index n -> (a -> b -> r) -> f a -> Table gs b -> r

-- instance (Pairing f g,Pairing' f gs' (IndexOf g gs'),gs ~ (x ': gs'))
--   => Pairing' f gs (S n)
--   where
--     pair' _ abr f (More _ m) = pair' (Index :: Index (IndexOf g gs')) abr f m

-- instance (Pairing f g,gs ~ (g ': gs'),IndexOf g gs ~ Z)
--   => Pairing' f gs Z
--   where
--     pair' _ abr f (More x _) = pair abr f x


-- class Pairing'' (fs :: [* -> *]) (gs :: [* -> *]) (n :: Nat) | fs -> gs, gs -> fs where
--   pair'' :: Index n -> (a -> b -> r) -> Pointer fs a -> Table gs b -> r

-- instance (Pairing f g) => Pairing'' (f ': fs') (g ': gs') Z where
--   pair'' _ p (Exact symbol) (More instruction _) = pair p symbol instruction

-- instance (Pairing f g,Pairing'' fs' gs' (IndexOf g gs'))
--   => Pairing'' (f ': fs') (g' ': gs') (S n)
--   where
--     pair'' _ p (Other m) (More _ m') = pair'' (Index :: Index (IndexOf g gs')) p m m'

data Nat = Z | S Nat
data Index (n :: Nat) = Index
type family IndexOf (f :: * -> *) fs :: Nat where
  IndexOf f (f ': fs) = Z
  IndexOf f (any ': fs) = S (IndexOf f fs)

-- A table of zero or more instructions
data Instructions (is :: [* -> *]) a where
  Empty :: Instructions '[] a
  Instruction :: Functor f => f a -> Instructions is' a -> Instructions (f ': is') a
deriving instance Functor (Instructions is)
type family (:++:) (x :: [* -> *]) (y :: [* -> *]) :: [* -> *] where
  (:++:) a '[] = a
  (:++:) '[] b = b
  (:++:) (a ': as) b = a ': (as :++: b)
none :: a -> Instructions '[] a
none = const Empty
single :: Functor h => (a -> h a) -> (a -> Instructions '[h] a)
single h = (\a -> Instruction (h a) Empty)
(*:*) :: Functor h => (a -> h a) -> (a -> Instructions t a) -> (a -> Instructions (h ': t) a)
(*:*) h g = (\a -> Instruction (h a) (g a))
class Concat (t0 :: [* -> *]) (t1 :: [* -> *]) where
  (*++*) :: (ts ~ (t0 :++: t1)) => (a -> Instructions t0 a) -> (a -> Instructions t1 a) -> a -> Instructions ts a
instance Concat '[] '[] where
  (*++*) _ _ = none
instance Concat '[] t' where
  (*++*) _ g = g
instance Concat t' '[] where
  (*++*) f _ = f
instance (Concat ts (t' ': ts')) => Concat (t ': ts) (t' ': ts') where
  (*++*) f g a =
    case f a of
      Instruction (ta :: t a) (ts :: Instructions ts a) -> Instruction (ta :: t a) ((*++*) (const ts) g a)


data Symbols (symbols :: [* -> *]) a where
  Symbol :: Functor symbol => symbol a -> Symbols (symbol ': symbols) a
  Table :: Symbols symbols' a -> Symbols (symbol ': symbols') a
deriving instance Functor (Symbols symbols)
class Contains x xs where
  inj :: x a -> Symbols xs a
  prj :: Symbols xs a -> Maybe (x a)
instance (Contains' x xs (IndexOf x xs)) => Contains x xs where
  inj = inj' (Index :: Index (IndexOf x xs))
  prj = prj' (Index :: Index (IndexOf x xs))
class Contains' x xs (n :: Nat) where
  inj' :: Index n -> x a -> Symbols xs a
  prj' :: Index n -> Symbols xs a -> Maybe (x a)
instance (Functor x,xs ~ (x' ': xs'),Contains' x xs' (IndexOf x xs')) => Contains' x xs (S n) where
  inj' _ = Table . inj' (Index :: Index (IndexOf x xs'))
  prj' _ (Table ss) = prj' (Index :: Index (IndexOf x xs')) ss
  prj' _ _ = Nothing
instance (Functor x,xs ~ (x ': xs')) => Contains' x xs Z where
  inj' _ = Symbol
  prj' _ (Symbol s) = Just s
  prj' _ _ = Nothing

class Pair (x :: * -> *) (y :: * -> *) | x -> y, y -> x where
  pair :: (a -> b -> r) -> x a -> y b -> r
instance Pair Identity Identity where
  pair f (Identity a) (Identity b) = f a b
instance Pair ((->) a) ((,) a) where
  pair p f g = uncurry (p . f) g
instance Pair ((,) a) ((->) a) where
  pair p (l,r) g = p r (g l)
instance ( Pair i symbol
         , Pair (Instructions is) (Symbols symbols)
         ) => Pair (Instructions (i ': is)) (Symbols (symbol ': symbols))
  where
    pair p (Instruction i is) (Symbol s) = pair p i s
    pair p (Instruction _ is) (Table ss) = pair p is ss
instance Pair (Instructions '[]) (Symbols '[]) where
  pair _ _ _ = error "Pairing empty lists; why would this get run?"

-- delta :: (Monad m, Functor x, Comonad w, Pair x y)
--       => CofreeT x w (m a) -> FreeT y m t -> m (CofreeT x w (m a), t)
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

type Admits x xs m = (MonadFree (Symbols xs) m,Contains' x xs (IndexOf x xs))

-- data X st k = X (st -> k) deriving Functor
-- x = liftF (inj (X id))
-- data X2 st k = X2 st k deriving Functor
-- x2 st = liftF (inj (X2 st ()))

-- data CoX st k = CoX st k deriving Functor
-- coX = CoX
-- data CoX2 st k = CoX2 (st -> k) deriving Functor
-- coX2 = CoX2 id

-- instance Pair (CoX st) (X st) where
--   pair p (CoX st k) (X stk) = pair p (st,k) stk

-- instance Pair (CoX2 st) (X2 st) where
--   pair p (CoX2 stk) (X2 st k) = pair p stk (st,k)

-- --computer :: (Monad m) => CofreeT (Instructions '[CoX Int,CoX2 Int]) Identity (m ())

-- computer i = coiterT instructions valueInContext
--   where
--     instructions = (coX' *:* none) *++* (coX2' *:* none)
--     coX' = coX i
--     coX2' = CoX2 . const
--     valueInContext = Identity (return ())

--

-- type XX2 st m a = Monad m => FreeT (Symbols '[X st,X2 st]) m a

-- test :: XX2 Int m Int
-- test = do
--   _ <- x :: XX2 Int m Int
--   x2 (2 :: Int)
--   x

-- main :: IO ()
-- main = do
--   let (comp,i::Int) = runIdentity $ delta (computer (1:: Int)) test
--   print i
--   return ()

-- --------------------------------------------------------------------------------
-- -- Testing

data State st k
  = Get (st -> k)
  | Put st k
  deriving Functor
data Store st k = Store (st -> (st,k))
  deriving Functor
instance Pair (Store st) (State st) where
  pair p (Store ststk) (Get stk) =
    let (st,k) = ststk st
    in p k (stk st)
  pair p (Store ststk) (Put st k) =
    let (st',k') = ststk st
    in p k' k

get = liftF (inj (Get id))
put st = liftF (inj (Put st ()))

store :: k -> Instructions '[Store st] k
store = single $ \wa -> Store $ \st' -> (st',wa)

state :: Monad m => CofreeT (Instructions '[Store st]) Identity (m ())
state = coiterT store (Identity (return ()))

type ComputerT instructions w m a = CofreeT (Instructions instructions) w (m a)
type Computer instructions m a = CofreeT (Instructions instructions) Identity (m a)
type Pure instructions a = CofreeT (Instructions instructions) Identity (Identity a)

type TapeT symbols m a = FreeT (Symbols symbols) m a
type Tape symbols a = TapeT symbols Identity a

test :: Tape '[State Int] Int
test = do
  put (3 :: Int)
  get

main = do
  let (comp,i :: Int) = runIdentity $ delta state test
  print i
  return ()

-- --------------------------------------------------------------------------------
-- -- Constraints and utilities for type-level lists


-- --------------------------------------------------------------------------------
-- -- Linear type-level union of functors



--------------------------------------------------------------------------------
-- Pattern synonyms for working with free monads

pattern If fb <- (runFree -> Free fb)
pattern Result x <- (runFree -> Pure x)

-- pattern Case x <- (If (prjPointer -> Just x))
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

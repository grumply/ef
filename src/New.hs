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
import           Data.Constraint
import           Data.Functor.Identity
import           Data.Proxy
import           Data.Type.Equality

data Hole = Hole

data Nat = Z | S Nat
data Index (n :: Nat) = Index

type family IndexOf (f :: k) (fs :: [k]) :: Nat where
  IndexOf f (f ': fs) = Z
  IndexOf f (any ': fs) = S (IndexOf f fs)

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True x = 'True
  Or x 'True = 'True
  Or x y = 'False

type family Is (x :: k) (y :: k) :: Bool where
  Is x x = 'True
  Is x y = 'False

type family Indexes (x :: k) (xs :: [k]) (n :: Nat) :: Bool where
  Indexes x (x ': ys) Z = 'True
  Indexes x (y ': ys) (S n) = Indexes x ys n

-- carriesElem :: Proxy x -> Proxy xs -> Proxy b -> (Elem x xs ~ 'True) :- (Elem x (b ': xs) ~ 'True)
-- carriesElem p1 p2 p3 = Sub Dict
-- carriesIndex :: Proxy x -> Proxy xs -> Proxy b -> (Indexes x xs n ~ 'True) :- (Indexes x (b ': xs) (S n) ~ 'True)
-- carriesIndex p1 p2 p3 = Sub Dict
-- carriesIndexElem :: Proxy x -> Proxy xs -> Proxy b
--                  ->    (Elem x xs        ~ 'True,Indexes x       xs     n  ~ 'True)
--                     :- (Elem x (b ': xs) ~ 'True,Indexes x (b ': xs) (S n) ~ 'True)
-- carriesIndexElem p1 p2 p3 = Sub Dict
{-
(\\) :: a => (b => r) -> (a :- b) -> r

Given that a :- b, derive something that needs a context b, using the context a
-}

data Instructions (is :: [* -> *]) a where
  Empty :: Instructions '[] a
  Instruction :: Functor f => f a -> Instructions is' a -> Instructions (f ': is') a
deriving instance Functor (Instructions is)

type family (:++:) (x :: [* -> *]) (y :: [* -> *]) :: [* -> *] where
  (:++:) a '[] = a
  (:++:) '[] b = b
  (:++:) (a ': as) b = a ': (as :++: b)
none :: a -> Instructions '[Identity] a
none a = Instruction (Identity a) Empty
single :: Functor h => (a -> h a) -> (a -> Instructions '[h] a)
single h = (\a -> Instruction (h a) Empty)
(*:*) :: Functor h => (a -> h a) -> (a -> Instructions t a) -> (a -> Instructions (h ': t) a)
(*:*) h g = (\a -> Instruction (h a) (g a))
class Append (t0 :: [* -> *]) (t1 :: [* -> *]) where
  (*++*) :: (ts ~ (t0 :++: t1)) => (a -> Instructions t0 a) -> (a -> Instructions t1 a) -> a -> Instructions ts a
instance Append '[] '[] where
  (*++*) _ _ = const Empty
instance Append '[] t' where
  (*++*) _ g = g
instance Append t' '[] where
  (*++*) f _ = f
instance (Append ts (t' ': ts')) => Append (t ': ts) (t' ': ts') where
  (*++*) f g a =
    case f a of
      Instruction (ta :: t a) (ts :: Instructions ts a) -> Instruction (ta :: t a) ((*++*) (const ts) g a)
class Admits (x :: * -> *) (xs :: [* -> *]) where
  pull :: Instructions xs a -> x a
  push :: x a -> Instructions xs a -> Instructions xs a
instance Admits' x xs (IndexOf x xs) => Admits x xs where
  pull = pull' (Index :: Index (IndexOf x xs))
  push = push' (Index :: Index (IndexOf x xs))
class Admits' (x :: * -> *) (xs :: [* -> *]) (n :: Nat) where
  pull' :: Index n -> Instructions xs a -> x a
  push' :: Index n -> x a -> Instructions xs a -> Instructions xs a
instance (xs ~ (x ': xs')) => Admits' x xs Z where
  pull' _ (Instruction xa _) = xa
  push' _ xa (Instruction _ xs) = Instruction xa xs
instance (xs ~ (x' ': xs'),Admits' x xs' (IndexOf x xs')) => Admits' x xs (S n) where
  pull' _ (Instruction _ xs') = pull' (Index :: Index (IndexOf x xs')) xs'
  push' _ xa (Instruction xb xs') = Instruction xb (push' (Index :: Index (IndexOf x xs')) xa xs')

class Setish (xs :: [* -> *])
instance Setish '[]
instance (NotIn x xs ~ 'True,Setish xs) => Setish (x ': xs)

type family NotIn (x :: * -> *) (xs :: [* -> *]) :: Bool where
  NotIn x '[] = 'True
  NotIn x (x ': xs') = 'False
  NotIn x (y ': xs') = NotIn x xs'

-- denies permits denying an instruction table a specific instruction
class Denies (x :: * -> *) (xs :: [* -> *])
instance Denies x '[]
instance (Denies x ys,Not x y ~ 'True) => Denies x (y ': ys)
type family Not (x :: k) (y :: k) :: Bool where
  Not x x = 'False
  Not x y = 'True

-- rebuild permits reordering of an instruction table
class Rebuild (xs :: [* -> *]) (ys :: [* -> *]) where
  rebuild :: Instructions xs a -> Instructions ys a
instance Rebuild xs '[] where
  rebuild _ = Empty
instance (Functor y,Admits' y xs (IndexOf y xs),Rebuild xs ys') => Rebuild xs (y ': ys') where
  rebuild is = Instruction (pull is) (rebuild is)

class Draw (xs :: [* -> *]) (ys :: [* -> *]) where
  draw :: Instructions ys a -> Instructions xs a
instance Draw '[] ys where
  draw _ = Empty
instance (Functor x,Admits x ys,Draw xs ys) => Draw (x ': xs) ys where
  draw ys = Instruction (pull ys) (draw ys)

view :: Instructions (x ': xs) a -> (x a,Instructions xs a)
view (Instruction xa xs) = (xa,xs)

class Merge (xs :: [* -> *]) (ys :: [* -> *]) where
  merge :: Instructions xs a -> Instructions ys a -> Instructions ys a
instance Merge '[] ys where
  merge _ ys = ys
instance (Admits' x ys (IndexOf x ys),Merge xs ys) => Merge (x ': xs) ys where
  merge xs ys =
    let (x,xs') = view xs
    in merge xs' (push x ys)

type family Without (x :: * -> *) (xs :: [* -> *]) :: [* -> *] where
  Without x '[] = '[]
  Without x (x ': xs) = xs
  Without x (x' ': xs) = x ': Without x xs
remove :: (Draw ys xs,ys ~ Without x xs) => Proxy (x :: * -> *) -> Instructions xs a -> Instructions ys a
remove _ = draw

type family Diff (xs :: [* -> *]) (ys :: [* -> *]) where
  Diff xs '[] = xs
  Diff xs (y ': ys') = Diff (Without y xs) ys'
diff :: (Draw ys xs,ys ~ Diff xs xs') => Proxy (xs' :: [* -> *]) -> Instructions xs a -> Instructions ys a
diff _ = draw

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

toComp   = coerce :: w (CofreeF f a (CofreeT f w a)) -> CofreeT f w a
fromComp = coerce :: CofreeT f w a -> w (CofreeF f a (CofreeT f w a))

type ComputerT instructions w m a = (Comonad w, Monad m) => CofreeT (Instructions instructions) w (m a)
type Computer  instructions   m a = ComputerT instructions Identity m        a
type Pure      instructions     a = Computer  instructions          Identity a

type TapeT symbols m a = Monad m => FreeT (Symbols symbols) m a
type Tape  symbols   a = TapeT symbols Identity a

type Instruction x xs = (Admits' x xs (IndexOf x xs))

{-# INLINE delta #-}
delta :: (Monad m, Functor x, Comonad w, Pair x y)
      => CofreeT x w (m a) -> FreeT y m t -> m (CofreeT x w (m a), t)
delta cof f = do
  a <- extract cof
  s <- runFreeT f
  case s of
    Free symbol ->
      pair delta (unwrap cof) symbol
    Pure result ->
      return
        (toComp $ fmap (bimap (const (return a)) id) $ fromComp cof,result)


--------------------------------------------------------------------------------
-- Testing

data State st k
  = Get (st -> k)
  | Put st k
  deriving Functor

data Store st k = Store
  { coGet :: (st,k)
  , coPut :: st -> k
  } deriving Functor

instance Pair (Store st) (State st) where
  pair p (Store stk _) (Get stk') = pair p stk stk'
  pair p (Store _ stk) (Put st k) = pair p stk (st,k)

get = liftF (inj (Get id))
put st = liftF (inj (Put st ()))


--------------------------------------------------------------------------------

insert :: (Functor f, Admits Identity is)
       => (a -> f a) -> Instructions is a -> Instructions (f ': is) a
insert f is = let i = runIdentity $ pull is in Instruction (f i) is

insertWith :: (Functor f,Admits Identity is,Draw xs is,Merge xs is)
           => (Instructions xs a -> a -> Instructions (f ': xs) a) -> Instructions is a -> Instructions (f ': is) a
insertWith iafa is =
  let xs = draw is
      a = runIdentity $ pull is
      is' = iafa xs a
      (f,xs') = view is'
  in Instruction f (merge xs' is)

return' :: a -> Instructions '[Identity] a
return' = none

bind' :: Admits Identity xs => Instructions xs a -> (a -> Instructions ys b) -> Instructions ys b
bind' a f = f (extract' a)

extract' :: Admits Identity xs => Instructions xs a -> a
extract' = runIdentity . pull

extend' :: (Instructions xs a -> b) -> Instructions xs a -> Instructions xs b
extend' f a = fmap (const (f a)) a

duplicate' :: Instructions xs a -> Instructions xs (Instructions xs a)
duplicate' a = fmap (const a) a

join' :: Admits Identity xs => Instructions xs (Instructions xs a) -> Instructions xs a
join' = fmap extract'

ap' :: Admits Identity xs => Instructions xs (a -> b) -> Instructions ys a -> Instructions ys b
ap' ab a = fmap (extract' ab) a

close :: ( ys ~ Without Identity xs, Draw ys xs, Denies Identity ys, Admits Identity xs
         ) => Instructions xs a -> Instructions ys a
close = remove (Proxy :: Proxy Identity)

{-# INLINE alterWith #-}
alterWith :: (Admits x xs, Admits x' xs) => Instructions xs a -> (x' a -> x a) -> Instructions xs a
alterWith is f = flip push is $ f $ pull is

{-# INLINE alter1With #-}
alter1With :: (Admits x xs) => Instructions xs a -> (x a -> x a) -> Instructions xs a
alter1With = alterWith

{-# INLINE adjust #-}
adjust :: (Comonad w, Admits' x1 xs (IndexOf x1 xs), Admits' x xs (IndexOf x xs))
       => CofreeT (Instructions xs) w a
       -> (x1 (CofreeT (Instructions xs) w a) -> x (CofreeT (Instructions xs) w a))
       -> CofreeT (Instructions xs) w a
adjust cf f =
  let adjust' (a :< fb) = a :< ((flip alterWith f) fb)
  in CofreeT $ extend (adjust' . extract) $ runCofreeT cf

{-# INLINE adjust1 #-}
adjust1 :: (Comonad w, Admits' x xs (IndexOf x xs))
       => CofreeT (Instructions xs) w a
       -> (x (CofreeT (Instructions xs) w a) -> x (CofreeT (Instructions xs) w a))
       -> CofreeT (Instructions xs) w a
adjust1 cf f =
  let adjust' (a :< fb) = a :< ((flip alter1With f) fb)
  in CofreeT $ extend (adjust' . extract) $ runCofreeT cf

buildWith :: (Comonad w,Monad m)
          => (m a -> Instructions f (m a))
          -> (w (m a) -> CofreeT (Instructions f) w (m a))
buildWith f = coiterT (\wma -> fmap (const wma) $ f (extract wma))

modifyWith :: (Comonad w,Monad m)
      => (m a -> m b)
      -> (CofreeT (Instructions f) w (m a) -> CofreeT (Instructions f) w (m b))
modifyWith f = CofreeT . f' . runCofreeT
  where
    f' = extend $ \cf ->
           let (ma :< as) = extract cf
           in (f ma) :< (fmap (modifyWith f) as)

something :: (Comonad w,Monad m)
  => (w (m a) -> Instructions g (w (m a)))
  -> (CofreeT (Instructions f) w (m a) -> CofreeT (Instructions g) w (m a))
something f = (\wcf ->
         let a :< as = extract wcf
         in coiterT f (extend (const a) wcf)
      ) . runCofreeT

-- the only way to implement this one is to rebuild the CofreeT; not sure if I
-- maintained invariants. The idea is to use the above bind' and return' to
-- build and modify a transformation of an instruction table and then lift
-- that transformation into a function over computers.
somethingElse :: (Comonad w,Monad m)
  => (Instructions f (m a) -> Instructions g (m a))
  -> (CofreeT (Instructions f) w (m a) -> CofreeT (Instructions g) w (m a))
somethingElse f = (\wcf ->
         let a :< as = extract wcf
         in coiterT (\wma -> fmap (\ma -> extend (const ma) wma)
                             $ f
                             $ fmap (headF . extract . runCofreeT)
                             $ as
                    ) (extend (const a) wcf)
      ) . runCofreeT

{-
Functionality desired:
Build, from an empty instruction table (containing only the Identity functor).
Modify an instruction table. Should the modification be:
  (Instructions f a -> Instructions g a)
  (Instructions f a -> Instructions g b)
  (CofreeT (Instructions f) w (m a) -> CofreeT (Instructions g) w (m b))
  (CofreeT (Instructions f) w (m a) -> CofreeT (Instructions g) w (m a))
 The first seems simplest.



-}


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

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

--------------------------------------------------------------------------------
-- Linear type-level union of functors

data Union (r :: [* -> *]) a where
  Exact :: (NotIn f r) => f a -> Union (f ': r) a
  Other :: (NotIn any r) => Union r a -> Union (any ': r) a

class PrjUnion' f r (n :: Nat) where
  prjUnion' :: Posn n -> Union r a -> Maybe (f a)

instance (r ~ (f ': r')) => PrjUnion' f r Z where
  prjUnion' _ (Exact fa) = Just fa

instance (r ~ (f' ': r'),PrjUnion' f r' (FindPosn f r')) => PrjUnion' f r (S n) where
  prjUnion' _ (Other loc) = prjUnion' (Posn :: Posn (FindPosn f r')) loc

class PrjUnion f r where
  prjUnion :: Union r a -> Maybe (f a)

instance (PrjUnion' f r (FindPosn f r)) => PrjUnion f r where
  prjUnion = prjUnion' (Posn :: Posn (FindPosn f r))

class InjUnion' f r (n :: Nat) where
  injUnion' :: Posn n -> f a -> Union r a

instance (NotIn f r',r ~ (f ': r')) => InjUnion' f r Z where
  injUnion' _ = Exact

instance (NotIn f r',NotIn f' r',r ~ (f' ': r'),InjUnion' f r' (FindPosn f r')) => InjUnion' f r (S n) where
  injUnion' _ = Other . injUnion' (Posn :: Posn (FindPosn f r'))

class InjUnion f r where
  injUnion :: f a -> Union r a

instance (InjUnion' f r (FindPosn f r)) => InjUnion f r where
  injUnion = injUnion' (Posn :: Posn (FindPosn f r))

data Table (r :: [* -> *]) a where
  Zero :: Table '[] a
  More :: (NotIn f r) => f a -> Table r a -> Table (f ': r) a

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

class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f g = uncurry (p . f) g

instance Pairing ((,) a) ((->) a) where
  pair p (l,r) g = p r (g l)

instance Pairing f (Union gs) where


instance (Pairing f g,) => Pairing (Table (f ': fs)) (Union gs) where
  pair p _ _ = _

-- delta :: ( Pairing fs gs
--          , Comonad w
--          ) => CofreeT (Table instruction) w (m a)
--            -> FreeT (Symbols)

-- class (Member' f r (FindPosn f r)) => Member f r where
--   inj :: f a -> Location r a
--   prj :: Location r a -> Maybe (f a)

-- instance (Member' f r (FindPosn f r)) => Member f r where
--   inj = inj' (Posn :: Posn (FindPosn f r))
--   prj = prj' (Posn :: Posn (FindPosn f r))

--------------------------------------------------------------------------------
--




-- --------------------------------------------------------------------------------
-- -- Base type synonyms

-- type Tape symbols actions result = FreeT (Union symbols) actions result

-- type Translation symbols actions result
--   = Tape symbols actions result -> actions (Tape symbols actions result)

-- type Computer instructions symbols context actions result start
--   = CofreeT (Queue actions context instructions start)
--             context (actions (Translation symbols actions result))


-- --------------------------------------------------------------------------------
-- -- The third implementation from Oleg's site....

-- -- The data constructors of Union are not exported

-- -- Essentially, the nested Either data type
-- -- t is can be a GADT and hence not necessarily a Functor
-- data Union (r :: [* -> * ]) v where
--   UNow  :: Functor t => t v -> Union (t ': r) v
--   UNext :: Union r v -> Union (any ': r) v
-- deriving instance Functor (Union r)

-- -- instance Functor (Union r) where
-- --   {-# INLINE fmap #-}
-- --   fmap f (UNow x)  = UNow (f x)
-- --   fmap f (UNext x) = UNext (fmap f x)

-- data P (n::Nat) = P

-- -- injecting/projecting at a specified position P n
-- class Member' t r (n :: Nat) where
--   inj' :: P n -> t v -> Union r v
--   prj' :: P n -> Union r v -> Maybe (t v)

-- instance (Functor t,r ~ (t ': r')) => Member' t r Z where
--   inj' _ = UNow
--   prj' _ (UNow x) = Just x
--   prj' _ _        = Nothing

-- instance (Functor t',r ~ (t' ': r'), Member' t r' n) => Member' t r (S n) where
--   inj' _ = UNext . inj' (P::P n)
--   prj' _ (UNow _)  = Nothing
--   prj' _ (UNext x) = prj' (P::P n) x

-- class (Member' t r (FindPosn t r)) => Member t r where
--   inj :: t v -> Union r v
--   prj :: Union r v -> Maybe (t v)

-- instance (Member' t r (FindPosn t r)) => Member t r where
--   inj = inj' (P::P (FindPosn t r))
--   prj = prj' (P::P (FindPosn t r))

-- {-# INLINE decomp #-}
-- decomp :: Union (t ': r) v -> Either (Union r v) (t v)
-- decomp (UNow x)  = Right x
-- decomp (UNext v) = Left v

-- weaken :: Union r w -> Union (any ': r) w
-- weaken = UNext

-- data Nat = Z | S Nat

-- -- Find an index of an element in a `list'
-- -- The element must exist
-- -- This closed type family disambiguates otherwise overlapping
-- -- instances
-- type family FindPosn (t :: * -> *) r :: Nat where
--   FindPosn t (t ': r)  = Z
--   FindPosn t (any ': r)  = S (FindPosn t r)

-- data Crumbs = Here | L Crumbs | R Crumbs

-- data Res = Found Crumbs | NotFound | Ambiguous


-- type family EQU (a :: k) (b :: k) :: Bool where
--   EQU a a = True
--   EQU a b = False

-- -- This class is used for emulating monad transformers
-- class Member t r => MemberU2 (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
-- instance (MemberU' (EQU t1 t2) tag t1 (t2 ': r)) => MemberU2 tag t1 (t2 ': r)

-- class Member t r =>
--       MemberU' (f::Bool) (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
-- instance Functor (tag e) => MemberU' True tag (tag e) (tag e ': r)
-- instance (Member t (t' ': r), MemberU2 tag t r) =>
--            MemberU' False tag t (t' ': r)

-- data Maybe1 (c :: * -> *) where
--   Nothing1 :: Maybe1 c
--   Just1    :: c a -> Maybe1 c           -- existential


-- -- Non-empty tree. Deconstruction operations make it more and more
-- -- left-leaning

-- data Queue m w g a b where
--   Leaf :: Functor g => (w (m a) -> g (w (m b))) -> Queue m w g a b
--   Node :: Functor g => Queue m w g a x -> Queue m w g x b -> Queue m w g a b
-- deriving instance (Functor actions,Functor context) => Functor (Queue actions context instructions result)

-- {-# INLINE tsingleton #-}
-- tsingleton :: Functor g => (w (m a) -> g (w (m b))) -> Queue m w g a b
-- tsingleton r = Leaf r

-- {-# INLINE (|>) #-}
-- (|>) :: Functor g => Queue m w g a x -> (w (m x) -> g (w (m b))) -> Queue m w g a b
-- t |> r = Node t (Leaf r)

-- {-# INLINE (><) #-}
-- (><) :: Functor g => Queue m w g a x -> Queue m w g x b -> Queue m w g a b
-- t1 >< t2 = Node t1 t2

-- -- Left-edge deconstruction
-- data ViewL m w g a b where
--   TOne  :: (w (m a) -> g (w (m b))) -> ViewL m w g a b
--   (:|)  :: (w (m a) -> g (w (m x))) -> Queue m w g x b -> ViewL m w g a b

-- tviewl :: Queue m w g a b -> ViewL m w g a b
-- tviewl (Leaf r) = TOne r
-- tviewl (Node t1 t2) = go t1 t2
--  where
--    go :: Queue m w g a x -> Queue m w g x b -> ViewL m w g a b
--    go (Leaf r) tr = r :| tr
--    go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)

-- --------------------------------------------------------------------------------
-- -- Pairing

-- class Pairing f g where
--   pair :: (a -> b -> r) -> f a -> g b -> r

-- instance Pairing Identity Identity where
--   pair f (Identity a) (Identity b) = f a b

-- instance Pairing ((->) a) ((,) a) where
--   pair p f g = uncurry (p . f) g

-- instance Pairing ((,) a) ((->) a) where
--   pair p (l,r) g = p r (g l)

-- instance (Pairing' f gs (FindPosn g gs),Pairing f g) => Pairing f (Union gs) where
--   pair p l r = pair' (P :: P (FindPosn g gs)) p l r

-- class Pairing' (f :: * -> *) (gs :: [* -> *]) (n :: Nat) where
--   pair' :: P n -> (a -> b -> r) -> f a -> Union gs b -> r

-- instance (Pairing' f gs' (FindPosn g gs'),gs ~ (x ': gs')) => Pairing' f gs (S n) where
--   pair' _ abr f (UNext x) = pair' (P :: P (FindPosn g gs')) abr f x

-- instance (Pairing f g,gs ~ (g ': gs'),FindPosn g gs ~ Z) => Pairing' f gs Z where
--   pair' _ abr f (UNow x) = pair abr f x

-- --------------------------------------------------------------------------------
-- -- Pattern synonyms for working with free monads

-- pattern If fb <- (runFree -> Free fb)
-- pattern Result x <- (runFree -> Pure x)

-- pattern Case x <- (If (prj -> Just x))
-- pattern Done <- (Result _)

-- --------------------------------------------------------------------------------
-- -- Fixpoint for free

-- -- This method may be built with a partially saturated iterT from free
-- class (Functor g,Monad m) => Fixable g m where
--   fixable :: FreeT g m a -> m a

-- instance (MonadFix m,Fixable f m) => MonadFix (FreeT f m) where
--   mfix = Trans.lift . mfix . (fixable .)

-- ---------------------------------------------------------------------------------
-- -- Lift instances for FreeT/FreeF/Identity

-- instance (Lift (f b),Lift a) => Lift (FreeF f a b) where
--   lift (Pure x) = [| Pure x |]
--   lift (Free fb) = [| Free fb |]

-- instance (Lift (m (FreeF f a (FreeT f m a)))) => Lift (FreeT f m a) where
--   lift (FreeT f) = [| FreeT f |]

-- instance Lift a => Lift (Identity a) where
--   lift (Identity a) = [| Identity a |]

-- --------------------------------------------------------------------------------
-- -- Overridden show methods for FreeT, etc...

-- showFT :: (Show (f a),Show a,Show (f (FreeT f Identity a))) => FreeT f Identity a -> String
-- showFT f = show $ runIdentity $ runFreeT f

-- showF :: (Show (f b),Show a) => FreeF f a b -> String
-- showF (Free fb) = show fb
-- showF (Pure a) = show a

-- showF' :: (Show (f (Free.Free f a)),Show a) => Free.Free f a -> String
-- showF' (Free.Free fb) = show fb
-- showF' (Free.Pure a) = show a

-- --------------------------------------------------------------------------------
-- -- Evaluation methods


-- delta :: ( Pairing (Tree instructions) (Union symbols)
--          , Functor instructions
--          , Comonad context
--          , Monad actions
--          ) => Computer instructions symbols context actions result start
--            -> Tape symbols actions result
--            -> actions (Computer instructions symbols context actions result start,result)
-- delta comp tape = do

--   translate <- extract comp

--   current <- runFreeT (joinFree (translate tape))

--   case current of

--     Free symbol ->

--       -- pair with delta after removing the head translation value in context.
--       pair delta (unwrap comp) symbol

--     Pure result ->

--       return -- remove the effects from the translation value in context
--         (toComp $ fmap (bimap (const (return translate)) id) $ fromComp comp,result)

-- fromComp = coerce :: CofreeT f w (m a) -> w (CofreeF f (m a) (CofreeT f w (m a)))
-- toComp = coerce :: w (CofreeF f (m a) (CofreeT f w (m a))) -> CofreeT f w (m a)

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

-- --------------------------------------------------------------------------------
-- -- Checked exceptions

-- -- | Checked exceptions
-- class Throws e where
--   throwChecked :: MC.MonadThrow m => e -> m a

-- -- | Wrap an action that may throw a checked exception
-- --
-- -- This is used internally in 'rethrowUnchecked' to avoid impredicative
-- -- instantiation of the type of 'unsafeCoerce'.
-- newtype Wrap e m a = Wrap (Throws e => m a)

-- -- | Rethrow checked exceptions as unchecked (regular) exceptions
-- rethrowUnchecked :: forall e a m. MC.MonadThrow m
--                                => (Throws e    => m a)
--                                -> (Exception e => m a)
-- rethrowUnchecked act = aux act MC.throwM
--   where
--     aux :: (Throws e => m a) -> ((e -> m a) -> m a)
--     aux = UNSAFE.unsafeCoerce . Wrap

-- -- | Catch a checked exception
-- --
-- -- This is the only way to discharge a 'Throws' type class constraint.
-- catchChecked :: (MC.MonadThrow m,MC.MonadCatch m,Exception e) => (Throws e => m a) -> (e -> m a) -> m a
-- catchChecked = MC.catch . rethrowUnchecked

-- -- | 'catchChecked' with the arguments reversed
-- handleChecked :: (MC.MonadCatch m,Exception e) => (e -> m a) -> (Throws e => m a) -> m a
-- handleChecked act handler = catchChecked handler act

-- -- | Throw an unchecked exception
-- --
-- -- This is just an alias for 'throw', but makes it evident that this is a very
-- -- intentional use of an unchecked exception.
-- throwUnchecked :: (MC.MonadThrow m,Exception e) => e -> m a
-- throwUnchecked = MC.throwM

-- -- | Rethrow IO exceptions as checked exceptions
-- checkIO :: (MonadIO m,MC.MonadCatch m,Throws IOException) => IO a -> m a
-- checkIO = MC.handle (\(ex :: IOException) -> throwChecked ex) . liftIO

-- --------------------------------------------------------------------------------
-- -- Testing

-- data Get st k = Get (st -> k)
--   deriving Functor
-- data CoGet st k = CoGet st k
--   deriving Functor
-- instance Pairing (CoGet st) (Get st) where
--   pair p (CoGet st k) (Get stk) = pair p (st,k) stk
-- get :: (MonadFree (Union r) m, Member (Get a) r)
--     => m a
-- get = liftF (inj (Get id))

-- data Put st k = Put st k -- k ~ (() -> k)
--   deriving Functor
-- data CoPut st k = CoPut (st -> k)
--   deriving Functor
-- instance Pairing (CoPut st) (Put st) where
--   pair p (CoPut stk) (Put st k) = pair p stk (st,k)
-- put :: (MonadFree (Union r) m, Member (Put st) r)
--     => st -> m ()
-- put x = liftF (inj (Put x ()))

-- type State st = Union '[Get st,Put st]

-- coGet :: st -> k -> CoGet st k
-- coGet st wa = CoGet st wa

-- coPut :: k -> CoPut st k
-- coPut wa = CoPut (const wa)

-- newtype TestState = TestState { runTestState :: Int }
-- increment :: TestState -> TestState
-- increment = TestState . succ . runTestState

-- test :: (MonadFree (Union r) m, Member (Put TestState) r, Member (Get TestState) r)
--      => m TestState
-- test = do
--   st <- get
--   put (increment st)
--   get

-- build = coiterT undefined undefined

-- test' :: (MonadFree (Union r) m,Member (Put TestState) r) => m ()
-- test' = put (TestState 1)

-- x :: Pairing (Queue actions context instructions start) (Union symbols)
--   => (Computer instructions symbols context actions () start,())
-- x = delta build test

-- main = do
--   --let (comp,()) = runIdentity $ delta build test'
--   return ()

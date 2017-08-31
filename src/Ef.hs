{-# LANGUAGE
        MultiParamTypeClasses
      , GADTs
      , DataKinds
      , RankNTypes
      , TypeFamilies
      , TypeOperators
      , FlexibleContexts
      , FlexibleInstances
      , ScopedTypeVariables
      , UndecidableInstances
      , ConstraintKinds
      , PolyKinds
      , FunctionalDependencies
      , NoMonoLocalBinds
      , InstanceSigs
      , BangPatterns
      , ViewPatterns
      , PatternSynonyms
      , DeriveFunctor
      , DeriveDataTypeable
      , StandaloneDeriving
  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Ef (module Ef, module Export) where

import Control.Applicative as Export
import Control.Monad as Export
import Control.Monad.Codensity as Export
import Control.Monad.Fix as Export
import Control.Monad.Free as Export hiding (unfold,cutoff)
import Control.Monad.IO.Class as Export
import Control.Monad.Trans.Class as Export
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Monad.Morph as Export
import Control.Monad.Base as Export
import Control.Monad.Trans.Resource
import Control.Monad.Catch
import Control.Comonad
import Control.Comonad.Cofree

import Data.Data
import Data.Functor.Compose
import Data.Functor.Sum
import Data.IORef
import Data.Proxy as Export
import Ef.Type.Bool as Export
import Ef.Type.Nat as Export
import Ef.Type.List as Export
import Ef.Type.Set as Export

import GHC.Exts

-- Features:
--
-- Effect monad paired with persistent interpreters as functional, immutable objects.
--
-- Functional and immutable.
--
-- Splits invocation from implementation in a manner similar to Java interfaces.
--
-- Pairs invocation and implementation uniquely to help preserve type inference.
--
-- Permits overriding of trait implementations during construction.
--
-- Subtyping and Subclassing and single/multiple inheritance via nesting of paired implementation/invocation contexts.
--
-- Permits subtyping of sets of messages.
--
-- Permits subclassing of sets of traits.
--
-- Permits functional extension/overriding of individual message invocations.
--
-- Invocation implemented as a free monad transformer with lift implemented individually.

data Modules (ts :: [* -> *]) (x :: *) where
  Empty :: Modules '[] x
  Mod :: t x -> Modules ts x -> Modules (t ': ts) x

data Messages ms a where
  Other :: Messages ms' a -> Messages (m ': ms') a
  Msg :: m a -> Messages (m ': ms') a

newtype Object ts c = Object { deconstruct :: Modules ts (Action ts c) }

data Narrative (f :: * -> *) c a
  = Do (f (Narrative f c a))
  | Lift (c (Narrative f c a))
  | Return a

type Ef (ms :: [* -> *]) (c :: * -> *) = Narrative (Messages ms) c

{-# INLINE viewMsg #-}
viewMsg :: (Can' ms m (Offset ms m)) => Ef ms c a -> Maybe (m (Ef ms c a))
viewMsg (Do m) = prj m
viewMsg _ = Nothing

pattern Module :: Has' ts t (Offset ts t) => t (Action ts c) -> Object ts c -> Object ts c
pattern Module x o <- (\o -> let x = pull (deconstruct o) in (x,o) -> (x,o)) where
  Module x o = Object $ push x $ deconstruct o

pattern Send :: Can' ms m (Offset ms m) => m (Ef ms c a) -> Ef ms c a
pattern Send x <- (viewMsg -> Just x) where
  Send x = Do (inj x)

{-# INLINE send #-}
send :: Functor f => f a -> Narrative f c a
send f = buildn $ \r _ d -> d (fmap r f)

{-# INLINE yields #-}
yields :: Functor f => f r -> Narrative f c r
yields fr = buildn $ \r l d -> d (fmap r fr)

{-# INLINE sends #-}
sends :: (ms <: '[f]) => f r -> Ef ms c r
sends = yields . inj

{-# INLINE super #-}
super :: Functor c => c a -> Narrative f c a
super s = buildn (\r l _ -> l (fmap r s))

deriving instance (Show a, Show (c (Narrative f c a)), Show (f (Narrative f c a))) => Show (Narrative f c a)
deriving instance (Eq a, Eq (c (Narrative f c a)), Eq (f (Narrative f c a))) => Eq (Narrative f c a)
deriving instance (Typeable f, Typeable c, Data a, Data (c (Narrative f c a)), Data (f (Narrative f c a))) => Data (Narrative f c a)

instance (MonadIO c, Functor f) => MonadIO (Narrative f c) where
  {-# INLINE liftIO #-}
  liftIO ioa = buildn $ \r l _ -> l (fmap r (liftIO ioa))

instance MonadTrans (Narrative f) where
  {-# INLINE lift #-}
  lift ca = buildn $ \r l _ -> l (fmap r ca)

instance Functor (Modules '[]) where
  {-# INLINE fmap #-}
  fmap _ _ = Empty

instance (Functor t, Functor (Modules ts)) => Functor (Modules (t ': ts)) where
  {-# INLINE fmap #-}
  fmap f (Mod t ts) = Mod (fmap f t) (fmap f ts)

instance Functor (Messages '[])

instance (Functor (Messages ms), Functor m) => Functor (Messages (m ': ms)) where
  {-# INLINE fmap #-}
  fmap = _fmapMsg

{-# INLINE [1] _fmapMsg #-}
_fmapMsg :: forall a b m ms. (Functor m, Functor (Messages ms)) => (a -> b) -> Messages (m ': ms) a -> Messages (m ': ms) b
_fmapMsg f = go
  where
    go :: Messages (m ': ms) a -> Messages (m ': ms) b
    go (Other ms) = Other (fmap f ms)
    go (Msg m) = Msg (fmap f m)

instance (Functor f, Functor c) => MonadFree f (Narrative f c) where
  {-# INLINE wrap #-}
  wrap = Do

instance (Functor f, Functor c) => Functor (Narrative f c) where
  {-# INLINE fmap #-}
  fmap = fmapn

instance (Functor f, Functor c) => Applicative (Narrative f c) where
  {-# INLINE pure #-}
  pure a = Return a
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance (Functor f, Functor c) => Monad (Narrative f c) where
  {-# INLINE return #-}
  return a = Return a
  {-# INLINE (>>=) #-}
  (>>=) = _bind
  {-# INLINE (>>) #-}
  (>>) ma mb = _bind ma (const mb)

instance (Applicative f, Monad c) => MonadPlus (Narrative f c) where
  {-# INLINE mzero #-}
  mzero = empty
  {-# INLINE mplus #-}
  mplus = (<|>)

instance (Applicative f, Monad c) => Alternative (Narrative f c) where
  {-# INLINE empty #-}
  empty = never
  {-# INLINE (<|>) #-}
  (<|>) =  zipsWith (liftA2 (,))

instance (Monad c, Monoid r, Functor f) => Monoid (Narrative f c r) where
  {-# INLINE mempty #-}
  mempty = return mempty
  {-# INLINE mappend #-}
  mappend a b = a >>= \w -> fmap (mappend w) b

instance Functor f => MFunctor (Narrative f) where
  {-# INLINE hoist #-}
  hoist = _hoist

instance Functor f => MMonad (Narrative f) where
  {-# INLINE embed #-}
  embed = _embed

instance (MonadBase b c, Functor f) => MonadBase b (Narrative f c) where
  {-# INLINE liftBase #-}
  liftBase b = Lift (fmap Return (liftBase b))

instance (MonadThrow c, Functor f) => MonadThrow (Narrative f c) where
  {-# INLINE throwM #-}
  throwM = lift . throwM

instance (MonadCatch c, Functor f) => MonadCatch (Narrative f c) where
  {-# INLINE catch #-}
  catch = _catch

instance (MonadResource c, Functor f) => MonadResource (Narrative f c) where
  {-# INLINE liftResourceT #-}
  liftResourceT = lift . liftResourceT

instance (MonadWriter w c, Functor f) => MonadWriter w (Narrative f c) where
  {-# INLINE writer #-}
  writer = lift . writer
  {-# INLINE tell #-}
  tell = lift . tell
  {-# INLINE listen #-}
  listen n = buildn go
    where
      go r l d =
        foldn
          (\a w -> r (a,w))
          (\c w -> l (fmap ($ w) c))
          (\f w -> d (fmap ($ w) f))
          n
          mempty
  {-# INLINE pass #-}
  pass n = buildn go
    where
      go r l d =
        foldn
          (\(a,f) w -> l $ pass $ return (r a,\_ -> f w))
          (\c w -> l (fmap ($ w) c))
          (\f w -> d (fmap ($ w) f))
          n
          mempty

instance (MonadReader r c, Functor f) => MonadReader r (Narrative f c) where
  {-# INLINE ask #-}
  ask = lift ask
  {-# INLINE local #-}
  local f n = buildn go
    where
      go r l d = foldn r (l . local f) d n
  {-# INLINE reader #-}
  reader = lift . reader

instance (MonadState s c, Functor f) => MonadState s (Narrative f c) where
  {-# INLINE get #-}
  get = lift get
  {-# INLINE put #-}
  put = lift . put
  {-# INLINE state #-}
  state = lift . state

instance (MonadError e c, Functor f) => MonadError e (Narrative f c) where
  {-# INLINE throwError #-}
  throwError = lift . throwError
  {-# INLINE catchError #-}
  catchError = _catchError

data Restore m = Unmasked | Masked (forall x . m x -> m x)

{-# INLINE liftMask #-}
liftMask
    :: forall f c x.
       (Functor f, MonadIO c, MonadCatch c)
    => (forall s . ((forall x . c x -> c x) -> c s) -> c s)
    -> ((forall x . Narrative f c x -> Narrative f c x) -> Narrative f c x)
    -> Narrative f c x
liftMask maskVariant k = do
    ioref <- liftIO $ newIORef Unmasked

    let -- mask adjacent actions in base monad
        loop :: Narrative f c r -> Narrative f c r
        loop (Do m) = Do (fmap loop m)
        loop (Lift m) = Lift $ maskVariant $ \unmaskVariant -> do
            -- stash base's unmask and merge action
            liftIO $ writeIORef ioref $ Masked unmaskVariant
            m >>= chunk >>= return . loop
        loop (Return r)         = Return r

        -- unmask adjacent actions in base monad
        unmask :: forall q. Narrative f c q -> Narrative f c q
        unmask (Do m) = Do (fmap unmask m)
        unmask (Lift m)            = Lift $ do
            -- retrieve base's unmask and apply to merged action
            Masked unmaskVariant <- liftIO $ readIORef ioref
            unmaskVariant (m >>= chunk >>= return . unmask)
        unmask (Return q)         = Return q

        -- merge adjacent actions in base monad
        chunk :: forall s. Narrative f c s -> c (Narrative f c s)
        chunk (Lift m) = m >>= chunk
        chunk s     = return s

    loop $ k unmask

instance (MonadMask c, MonadIO c, Functor f) => MonadMask (Narrative f c) where
    mask                = liftMask mask
    uninterruptibleMask = liftMask uninterruptibleMask

{-# INLINE _hoist #-}
_hoist :: (Functor f, Functor c') => (c' (Narrative f c a) -> c (Narrative f c a)) -> Narrative f c' a -> Narrative f c a
_hoist f n = foldn Return (Lift . f) Do n

{-# INLINE _embed #-}
_embed :: (Monad c, Functor f) => (t (Narrative f t a) -> Narrative f c (Narrative f t a)) -> Narrative f t a -> Narrative f c a
_embed f = go
  where
    go (Return r) = Return r
    go (Lift c) = f c >>= go
    go (Do m) = Do (fmap go m)

{-# INLINE _catch #-}
_catch :: (Exception e, Functor f, MonadCatch c) => Narrative f c a -> (e -> Narrative f c a) -> Narrative f c a
_catch n f = foldn Return (\c -> Lift (catch c (pure . f))) Do n

{-# INLINE _catchError #-}
_catchError :: (Functor f, MonadError e c) => Narrative f c a -> (e -> Narrative f c a) -> Narrative f c a
_catchError n f = foldn Return (\c -> Lift (catchError c (pure . f))) Do n

class Delta f g | f -> g where
  delta :: (a -> b -> r) -> f a -> g b -> r

type (<=>) ts ms = Delta (Modules ts) (Messages ms)

instance Delta ((->) a) ((,) a) where
  {-# INLINE delta #-}
  delta u f (l,r) = u (f l) r

instance Delta ((,) a) ((->) a) where
  {-# INLINE delta #-}
  delta u (l,r) g = u r (g l)

instance Delta (Modules '[]) (Messages '[]) where
  {-# INLINE delta #-}
  delta u _ _ = u undefined undefined

instance (t `Delta` m, Modules ts `Delta` Messages ms) => Delta (Modules (t ': ts)) (Messages (m ': ms)) where
  {-# INLINE delta #-}
  delta u (Mod t _) (Msg m) = delta u t m
  delta u (Mod _ ts) (Other ms) = delta u ts ms

type Mod t ts c = t (Action ts c)

type Action ts c = Object ts c -> c (Object ts c)

class Has (ts :: [* -> *]) (t :: * -> *) where
  push :: t a -> Modules ts a -> Modules ts a
  pull :: Modules ts a -> t a

instance (i ~ Offset ts t, Has' ts t i) => Has ts t where
  {-# INLINE push #-}
  push = let i = Index :: Index i in push' i
  {-# INLINE pull #-}
  pull = let i = Index :: Index i in pull' i

class Has' (ts :: [* -> *]) (t :: * -> *) (n :: Nat) where
  push' :: Index n -> t a -> Modules ts a -> Modules ts a
  pull' :: Index n -> Modules ts a -> t a

instance ts ~ (t ': xs) => Has' ts t 'Z where
  {-# INLINE push' #-}
  push' _ t (Mod _ ts) = Mod t ts
  {-# INLINE pull' #-}
  pull' _   (Mod t _) = t

instance (i ~ Offset ts t, Has' ts t i) => Has' (t' ': ts) t ('S n) where
  {-# INLINE push' #-}
  push' _ t (Mod t' ts) = let i = Index :: Index i in Mod t' (push' i t ts)
  {-# INLINE pull' #-}
  pull' _   (Mod _ ts)  = let i = Index :: Index i in pull' i ts

-- Subclassing; think BIG <. little
type family (<.) (ts :: [* -> *]) (ts' :: [* -> *]) :: Constraint where
  (<.) ts' '[] = ()
  (<.) ts' (t ': ts) = (Has' ts' t (Offset ts' t), ts' <. ts)

-- Superclassing; think little .> BIG
type ts .> ts' = ts' <. ts

class Can ms m where
  inj :: m a -> Messages ms a
  prj :: Messages ms a -> Maybe (m a)

instance (i ~ Offset ms m, Can' ms m i) => Can ms m where
  {-# INLINE inj #-}
  inj = let i = Index :: Index i in inj' i
  {-# INLINE prj #-}
  prj = let i = Index :: Index i in prj' i

class Can' ms m (n :: Nat) where
  inj' :: Index n -> m a -> Messages ms a
  prj' :: Index n -> Messages ms a -> Maybe (m a)

instance (i ~ Offset ms' m, Can' ms' m i) => Can' (m' ': ms') m ('S n) where
  {-# INLINE inj' #-}
  inj' _            = let i = Index :: Index i in Other . inj' i
  {-# INLINE prj' #-}
  prj' _ (Other ms) = let i = Index :: Index i in prj' i ms
  prj' _ _          = Nothing

instance (ms ~ (m ': ms')) => Can' ms m 'Z where
  {-# INLINE inj' #-}
  inj' _                   = Msg
  {-# INLINE prj' #-}
  prj' _ (Msg message) = Just message
  prj' _ (Other _)     = Nothing

-- Subtyping; think BIG <: little
type family (<:) ms ms' where
  ms <: '[] = (Functor (Messages ms), Typeable ms)
  ms <: (m ': ms') = (Can' ms m (Offset ms m), ms <: ms')

-- Supertyping; think little :> BIG
type ms :> ms' = ms' <: ms

infixr 6 *:*
{-# INLINE (*:*) #-}
(*:*) :: t a -> Modules ts a -> Modules (t ': ts) a
(*:*) = Mod

class Append ts ts' ts'' where
  (*++*) :: (Appended ts ts' ~ ts'') => Modules ts a -> Modules ts' a -> Modules ts'' a

instance (Appended ts ts' ~ ts'', Append ts ts' ts'') => Append (t ': ts) ts' (t ': ts'') where
  {-# INLINE (*++*) #-}
  (*++*) (Mod m ms) ys = m *:* (ms *++* ys)

instance Append '[] ts ts where
  {-# INLINE (*++*) #-}
  (*++*) Empty ys = ys

instance Append ts '[] ts where
  {-# INLINE (*++*) #-}
  (*++*) xs Empty = xs

{-# INLINE unit #-}
unit :: Proxy ()
unit = Proxy

{-# INLINE transform #-}
transform :: Monad c => (r -> a) -> (f (Narrative f c r) -> Narrative f' c a) -> Narrative f c r -> Narrative f' c a
transform f t = go
  where
    go (Do m) = t m
    go (Lift sup) = Lift (fmap go sup)
    go (Return r) = Return (f r)

{-# INLINE runWith #-}
runWith :: forall ts ms c a. ((Modules ts) `Delta` (Messages ms), Functor (Messages ms), Monad c) => Object ts c -> Ef ms c a -> c (Object ts c,a)
runWith o c = foldn runReturn runLift runDo c o
  where
    runReturn a o' = return (o',a)

    runLift c o = c >>= \f -> f o

    runDo :: Messages ms (Object ts c -> c (Object ts c,a)) -> Object ts c -> c (Object ts c,a)
    runDo ms o' =
      let ~(f,cont) = delta (,) (deconstruct o' :: Modules ts (Action ts c)) ms
      in f o' >>= cont

infixr 5 !
{-# INLINE (!) #-}
(!) :: ((Modules ts) `Delta` (Messages ms), Functor (Messages ms), Monad c) => Object ts c -> Ef ms c a -> c (Object ts c,a)
(!) = runWith

{-# INLINE [0] foldn #-}
foldn :: (Functor c, Functor f)
      => (a -> b)
      -> (c b -> b)
      -> (f b -> b)
      -> Narrative f c a
      -> b
foldn r l d = go
  where
    go (Return a) = r a
    go (Lift c) = l (fmap go c)
    go (Do m) = d (fmap go m)

{-# INLINE [1] buildn #-}
buildn :: (forall b. (a -> b) -> (c b -> b) -> (f b -> b) -> b) -> Narrative f c a
buildn f = f Return Lift Do

{-# INLINE run #-}
run :: (Functor f, Monad m) => (f (m a) -> m a) -> Narrative f m a -> m a
run = foldn return join

{-# INLINE thread #-}
thread :: (Functor f, Monad m) => (f (r -> m (r,a)) -> r -> m (r, a)) -> Narrative f m a -> r -> m (r,a)
thread = foldn (\a r -> return (r,a)) (\cf a -> cf >>= ($ a))

-- non-fusing
{-# INLINE [0] foldn' #-}
foldn' :: (Functor c, Functor f)
      => (a -> Narrative f c a)
      -> (c (Narrative f c a) -> Narrative f c a)
      -> (f (Narrative f c a) -> Narrative f c a)
      -> Narrative f c a
      -> Narrative f c a
foldn' r l d = go
  where
    go (Return a) = r a
    go (Lift c) = l (fmap go c)
    go (Do m) = d (fmap go m)

-- non-fusing
{-# INLINE [1] buildn' #-}
buildn' :: forall f c a. ((a -> Narrative f c a) -> (c (Narrative f c a) -> Narrative f c a) -> (f (Narrative f c a) -> Narrative f c a) -> Narrative f c a) -> Narrative f c a
buildn' f = f Return Lift Do

{-# INLINE [1] fmapn #-}
fmapn :: (Functor f, Functor c) => (a -> b) -> Narrative f c a -> Narrative f c b
fmapn f (Return r) = Return (f r)
fmapn f (Lift l) = Lift (fmap (fmapn f) l)
fmapn f (Do d) = Do (fmap (fmapn f) d)

{-# INLINE [0] fmapnR #-}
fmapnR :: (a -> r) -> (x -> a) -> x -> r
fmapnR r f = \c -> r (f c)

{-# INLINE [0] fmapnFB #-}
fmapnFB :: (a -> r) -> a -> r
fmapnFB l = \a -> l a

{-# INLINE [1] augmentn #-}
augmentn :: forall f c a. (forall b. (a -> b) -> (c b -> b) -> (f b -> b) -> b -> b) -> Narrative f c a -> Narrative f c a
augmentn f n = f Return Lift Do n

{-# INLINE [2] _bind #-}
_bind :: (Functor f, Functor c) => Narrative f c a -> (a -> Narrative f c b) -> Narrative f c b
_bind m k = foldn k Lift Do m

{-# INLINE [2] _fmap #-}
_fmap :: (Functor f, Functor c) => (a -> b) -> Narrative f c a -> Narrative f c b
_fmap f n = foldn (Return . f) Lift Do n

{-# RULES
"foldn/buildn" forall r l d (g :: forall b. (a -> b) -> (c b -> b) -> (f b -> b) -> b).
               foldn r l d (buildn g) = g r l d

-- buildn' is too specific for this rule to fire
"foldn'/buildn'" forall r l d g. foldn' r l d (buildn' g) = g r l d

-- buildn' is too specific
"foldn/buildn'" forall r l d (g :: forall b. (a -> b) -> (c b -> b) -> (f b -> b) -> b).
                foldn r l d (buildn' g) = g r l d

"foldn/augmentn" forall r l d (g :: forall b. (a -> b) -> (c b -> b) -> (f b -> b) -> b -> b) n.
                 foldn r l d (augmentn g n) = g r l d (foldn r l d n)

"fmapn" [~1] forall f n. fmapn f n = buildn (\r l d -> foldn (fmapnR r f) (fmapnFB l) (fmapnFB d) n)
"fmapnRFB" [1] forall f. foldn (fmapnR Return f) (fmapnFB Lift) (fmapnFB Do) = fmapn f
"fmapnR" forall r f g. fmapnR (fmapnR r f) g = fmapnR r (f . g)

"foldn (Return r)" forall r l d a. foldn r l d (Return a) = r a
"foldn (Lift c)"   forall r l d c. foldn r l d (Lift c)   = l (fmap (foldn r l d) c)
"foldn (Do m)"     forall r l d m. foldn r l d (Do m)     = d (fmap (foldn r l d) m)

"foldn' (Return r)" forall r l d a. foldn' r l d (Return a) = r a
"foldn' (Lift c)"   forall r l d c. foldn' r l d (Lift c)   = l (fmap (foldn' r l d) c)
"foldn' (Do m)"     forall r l d m. foldn' r l d (Do m)     = d (fmap (foldn' r l d) m)

"_bind (Return r)" [~2] forall r f. _bind (Return r) f = f r
"_bind (Lift c)"   [~2] forall c f. _bind (Lift c) f   = Lift (fmap (\a -> _bind a f) c)
"_bind (Do m)"     [~2] forall m f. _bind (Do m) f     = Do (fmap (\a -> _bind a f) m)

"_fmap (Return r)" [~2] forall r f. _fmap f (Return r) = Return (f r)
"_fmap (Lift c)"   [~2] forall c f. _fmap f (Lift c)   = Lift (fmap (_fmap f) c)
"_fmap (Do m)"     [~2] forall m f. _fmap f (Do m)     = Do (fmap (_fmap f) m)
  #-}

{- for reference
_unfold mk n = Lift $ mk n >>= return . either Return (Do . fmap go)
swapObj :: (Monad c, Delta (Modules ts) (Messages ms)) => Object ts c -> Cofree c (Object ts c, Ef ms c a) -> Cofree c (Object ts c, Ef ms c a)
swapObj obj = path obj . snd . extract

swapEf :: (Monad c, Delta (Modules ts) (Messages ms)) => Ef ms c a -> Cofree c (Object ts c, Ef ms c a) -> Cofree c (Object ts c, Ef ms c a)
swapEf code = flip path code . fst . extract

branch :: (Monad c, Delta (Modules ts) (Messages ms)) => Cofree c (Object ts c, Ef ms c a) -> Cofree c (Cofree c (Object ts c, Ef ms c a))
branch = duplicate

reseedObj :: (Monad c, Delta (Modules ts) (Messages ms)) => Object ts c -> Cofree c (Cofree c (Object ts c, Ef ms c a)) -> Cofree c (Cofree c (Object ts c, Ef ms c a))
reseedObj obj = fmap (swapObj obj)

reseedEf :: (Monad c, Delta (Modules ts) (Messages ms)) => Ef ms c a -> Cofree c (Cofree c (Object ts c, Ef ms c a)) -> Cofree c (Cofree c (Object ts c, Ef ms c a))
reseedEf code = fmap (swapEf code)
-}

----------------------------------------
-- Utilities

{-# INLINE reduce #-}
reduce :: (Functor f, Functor c) => Narrative f c a -> (f b -> b) -> (c b -> b) -> (a -> b) -> b
reduce n d l r = foldn r l d n

{-# INLINE observe #-}
observe :: (Functor f, Monad c) => Narrative f c r -> Narrative f c r
observe n = Lift $ foldn (return . Return) join (return . Do . fmap Lift) n

{-# INLINE never #-}
never :: (Applicative f, Applicative c) => Narrative f c r
never = loop
  where
    loop = Lift $ pure $ Do $ pure loop

{-# INLINE uncons #-}
uncons :: (Functor f, Monad c) => Narrative f c r -> c (Either r (f (Narrative f c r)))
uncons = go
  where
    go (Return r) = return (Left r)
    go (Lift m) = m >>= go
    go (Do fs) = return (Right fs)

zipsWith :: (Monad c, Functor f, Functor g, Functor h)
         => (forall x y . f x -> g y -> h (x,y))
         -> Narrative f c a -> Narrative g c a -> Narrative h c a
zipsWith = _zipsWith

{-# INLINE _zipsWith #-}
_zipsWith :: (Monad c, Functor f, Functor l, Functor r)
          => (l (Narrative l c a) -> r (Narrative r c a) -> f (Narrative l c a, Narrative r c a))
          -> Narrative l c a
          -> Narrative r c a
          -> Narrative f c a
_zipsWith f x y = Lift $ liftA2 (_zipsWithInternal f) (uncons x) (uncons y)

{-# INLINE _zipsWithInternal #-}
_zipsWithInternal :: (Monad c, Functor f, Functor l, Functor r)
                  => (l (Narrative l c a) -> r (Narrative r c a) -> f (Narrative l c a, Narrative r c a))
                  -> Either a (l (Narrative l c a))
                  -> Either a (r (Narrative r c a))
                  -> Narrative f c a
_zipsWithInternal f = go
  where
    go (Left x) _ = Return x
    go _ (Left y) = Return y
    go (Right x) (Right y) = Do (fmap (uncurry (_zipsWith f)) (f x y))

{-# INLINE zips #-}
zips :: (Monad c, Functor f, Functor g) => Narrative f c a -> Narrative g c a -> Narrative (Compose f g) c a
zips = zipsWith (\f g -> Compose (fmap (\x -> fmap (\y -> (x,y)) g) f))

{-# INLINE unzips #-}
unzips :: (Functor f, Functor g, Monad c) => Narrative (Compose f g) c r -> Narrative f (Narrative g c) r
unzips n = foldn Return (Lift . lift) (\(Compose fgn) -> Do (fmap (Lift . Do . fmap Return) fgn)) n

{-# INLINE interleaves #-}
interleaves :: (Monad c, Applicative f) => Narrative f c a -> Narrative f c a -> Narrative f c a
interleaves = zipsWith (liftA2 (,))

{-# INLINE decompose #-}
decompose :: (Functor f, Monad c) => Narrative (Compose c f) c r -> Narrative f c r
decompose n = buildn go
  where
    go r l d = foldn r l (\(Compose c) -> l $ c >>= return . d) n

{-# INLINE implode #-}
implode :: (Monad c) => Narrative c c r -> c r
implode = foldn return join join

{-# INLINE brackets #-}
brackets :: (Functor f, MonadResource c) => IO a -> (a -> IO ()) -> (a -> Narrative f c b) -> Narrative f c b
brackets alloc free inside = do
  (key, seed) <- lift (allocate alloc free)
  buildn $ \r l d -> foldn (\a -> l (release key >> return (r a))) l d (inside seed)

{-# INLINE cutoff #-}
cutoff :: (Functor f, Monad c) => Int -> Narrative f c r -> Narrative f c (Maybe r)
cutoff = go
  where
    go 0 _ = return Nothing
    go i n = do
        e <- lift $ uncons n
        case e of
          Left r -> return (Just r)
          Right n' -> Do $ fmap (go (i-1)) n'

{-# INLINE unfold #-}
unfold :: (Functor f, Monad c) => (s -> c (Either r (f s))) -> s -> Narrative f c r
unfold mk = go
  where
    go n = Lift $ mk n >>= return . either Return (Do . fmap go)

{-# INLINE iterTM #-}
iterTM :: (Functor f, Monad c, MonadTrans t, Monad (t c)) => (f (t c a) -> t c a) -> Narrative f c a -> t c a
iterTM f n = reduce n f (join . lift) return

{-# INLINE iterT #-}
iterT :: (Functor f, Monad c) => (f (c a) -> c a) -> Narrative f c a -> c a
iterT f n = reduce n f join return

{-# INLINE distribute #-}
distribute :: (Functor f, Monad c, Functor (t c), MonadTrans t, MFunctor t, Monad (t (Narrative f c)))
           => Narrative f (t c) r -> t (Narrative f c) r
distribute n = foldn (lift . return) (join . hoist lift) (join . lift . wrap . fmap return) n

{-# INLINE separate #-}
separate :: (Monad c, Functor f, Functor g) => Narrative (Sum f g) c r -> Narrative f (Narrative g c) r
separate n = buildn go
  where
    go r l d = foldn r (l . lift) (\x -> case x of InL fn -> d fn; InR gn -> l (yields gn)) n

{-# INLINE unseparate #-}
unseparate :: (Monad c, Functor f, Functor g) => Narrative f (Narrative g c) r -> Narrative (Sum f g) c r
unseparate n = buildn' go
  where
    go r _ d = foldn r (join . maps InR) (d . InL) n

-- Walk a narrative n functor steps and then repackage the rest of the narrative as a return value.
-- If a return constructor is seen, it is repackaged as a return value. Context steps are not counted.
{-# INLINE splitsAt #-}
splitsAt :: (Functor f, Monad c) => Int -> Narrative f c r -> Narrative f c (Narrative f c r)
splitsAt = go
  where
    go i n | i <= 0 = return n
    go i n =
      case n of
        Return _ -> Return n
        Lift c -> Lift (fmap (go i) c)
        Do m -> Do (fmap (go (i - 1)) m)

-- drops functor layers where the functor is comonadic (to guarantee extractability);
-- keeps effect layers. Note that Ef is non-droppable.
{-# INLINE drops #-}
drops :: (Comonad f, Monad c) => Int -> Narrative f c r -> Narrative f c r
drops = go
  where
    go i n | i <= 0 = n
    go i n =
      case n of
        Return a -> Return a
        Lift c -> Lift (fmap (go i) c)
        Do m -> go (i - 1) (extract m)

-- takes n functor layers; keeps effect layers.
-- Equivalent to `void . splitsAt n`
{-# INLINE takes #-}
takes :: (Functor f, Monad c) => Int -> Narrative f c r -> Narrative f c ()
takes = go
  where
    go i _ | i <= 0 = return ()
    go i n =
      case n of
        Return _ -> Return ()
        Lift c -> Lift (fmap (go i) c)
        Do m -> Do (fmap (go (i - 1)) m)

-- map a value-preserving transformation over functor layers in a Narrative.
{-# INLINE maps #-}
maps :: (Functor f, Functor g, Monad c) => (forall x. f x -> g x) -> Narrative f c r -> Narrative g c r
maps f n = buildn go
  where
    go r l d = foldn r l (d . f) n

-- map a value-preserving contextualized transformation over functor layers in a Narrative.
{-# INLINE mapsM #-}
mapsM :: (Functor f, Functor g, Monad c) => (forall x. f x -> c (g x)) -> Narrative f c r -> Narrative g c r
mapsM f n = buildn go
  where
    go r l d = foldn r l (l . fmap d . f) n

-- Convert functor layers to context layers; map a value-preserving contextualized
-- transformation converting from the base Narrative functor to the Narrative's context
-- and then implode the result to be rid of the Narrative.
{-# INLINE mapsM_ #-}
mapsM_ :: (Functor f, Monad c) => (forall x. f x -> c x) -> Narrative f c r -> c r
mapsM_ f = implode . maps f

{-# INLINE intersperses #-}
intersperses :: (Monad c, Monad (t c), MonadTrans t) => t c x -> Narrative (t c) c r -> Narrative (t c) c r
intersperses sep n = buildn $ \r l d -> foldn r l (\m -> d (m >>= \a -> sep >> return a)) n

-- Embed monadic transformer layers between functor layers where the functor layer is the monadic
-- transformer and collapse the resulting Narrative into the functor/transformer. Equivalent to
-- `concats . intersperses x` when the type specializes to that expected by concats.
{-# INLINE intercalates #-}
intercalates :: (Monad c, Monad (t c), MonadTrans t) => t c x -> Narrative (t c) c r -> t c r
intercalates sep = go0
  where
    go0 (Return a) = return a
    go0 (Lift c) = lift c >>= go0
    go0 (Do m) = m >>= go1

    go1 = foldn return (join . lift) (\m -> join (sep >> m))

-- Collapse functor layers into the embedding context when parent contexts match.
{-# INLINE concats #-}
concats :: (Functor f, Monad c) => Narrative (Narrative f c) c r -> Narrative f c r
concats n = buildn' go
  where
    go r l _ = foldn r l join n

-- Split functor layers based on a maximum size; produces a Narrative where the functor layers
-- are of the same type as the original Narrative. `concats . chunksOf n` == `id`
{-# INLINE chunksOf #-}
chunksOf :: (Functor f, Monad c) => Int -> Narrative f c r -> Narrative (Narrative f c) c r
chunksOf i n = buildn go
  where
    go r l d = go' n
      where
        go' (Return a) = r a
        go' (Lift c) = l (fmap go' c)
        go' (Do fs) = d (Do (fmap (fmap go' . splitsAt (i - 1)) fs))

-- repeat a functor layer ad infinitum
{-# INLINE repeats #-}
repeats :: (Functor f, Monad c) => f () -> Narrative f c r
repeats f = buildn go
  where
    go _ l d = go'
      where
        go' = l (return (d (fmap (\_ -> go') f)))

-- repeat a contextualized layer whose result is a functor layer
{-# INLINE repeatsM #-}
repeatsM :: (Functor f, Monad c) => c (f ()) -> Narrative f c r
repeatsM cf = buildn go
  where
    go _ l d = go'
      where
        go' = l $ cf >>= return . d . fmap (\_ -> go')

-- repeat a functor layer n times
{-# INLINE replicates #-}
replicates :: (Functor f, Monad c) => Int -> f () -> Narrative f c ()
replicates n f = fmap (const ()) (splitsAt n (repeats f))

-- repeat a contextualized layer whose result is a functor layer n times
{-# INLINE replicatesM #-}
replicatesM :: (Functor f, Monad c) => Int -> c (f ()) -> Narrative f c ()
replicatesM n f = fmap (const ()) (splitsAt n (repeatsM f))

-- repeat a Narrative ad infinitum; equivalent to `forever`
{-# INLINE cycles #-}
cycles :: (Monad m, Functor f) => Narrative f m r -> Narrative f m s
cycles str = loop where loop = str >> loop

{-# INLINE runs #-}
-- groups in streaming
runs :: forall f g c r.
        (Functor f, Functor g, Monad c)
     => Narrative (Sum f g) c r
     -> Narrative (Sum (Narrative f c) (Narrative g c)) c r
runs n = buildn' start
  where
    {-# INLINE start #-}
    start r l d = foldn r l (d . msg) n
      where
        msg (InL fn) = InL $ goL fn
        msg (InR gn) = InR $ goR gn

        goL n = buildn' $ \r' l' d' -> d' $ fmap
                   (foldn (r' . r) l' (\x ->
                      case x of
                        InL fn -> join fn
                        InR gn -> r' (d (fmap (d . InL) (InR gn)))
                    )) n

        goR n = buildn' $ \r' l' d' -> d' $ fmap
                  (foldn (r' . r) l' (\x ->
                      case x of
                        InL fn -> r' (d (fmap (d . InR) (InL fn)))
                        InR gn -> join gn
                  )) n


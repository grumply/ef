{-# LANGUAGE RankNTypes #-}
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
import Control.Monad.Trans.Control
import Control.Comonad
import Control.Comonad.Cofree
import Control.Concurrent

import Data.Coerce
import Data.Data
import Data.Functor.Compose
import Data.Functor.Sum
import Data.IORef
import Data.Proxy as Export
import Data.Typeable
import Ef.Type.Bool as Export
import Ef.Type.Nat as Export
import Ef.Type.List as Export
import Ef.Type.Set as Export

import GHC.Exts

import Unsafe.Coerce

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
  Mod :: !(t x) -> !(Modules ts x) -> Modules (t ': ts) x

data Messages ms a where
  Other :: Messages ms' a -> Messages (m ': ms') a
  Msg :: !(m a) -> Messages (m ': ms') a

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

pattern Module x o <- (\o -> let x = pull (deconstruct o) in (x,o) -> (x,o)) where
  Module x o = Object $ push x $ deconstruct o

pattern Send x <- (viewMsg -> Just x) where
  Send x = buildn $ \r l d -> d (inj x)

{-# INLINE yields #-}
yields :: (Functor f) => f r -> Narrative f c r
yields fr = buildn $ \r l d -> d (fmap r fr)

{-# INLINE sends #-}
sends :: (ms <: '[f]) => f r -> Ef ms c r
sends = yields . inj

{-# INLINE super #-}
super :: forall f c r. (Monad c) => c (Narrative f c r) -> Narrative f c r
super ca = buildn (\r l d -> l ca)

deriving instance (Show a, Show (c (Narrative f c a)), Show (f (Narrative f c a))) => Show (Narrative f c a)
deriving instance (Eq a, Eq (c (Narrative f c a)), Eq (f (Narrative f c a))) => Eq (Narrative f c a)
deriving instance (Typeable f, Typeable c, Data a, Data (c (Narrative f c a)), Data (f (Narrative f c a))) => Data (Narrative f c a)

instance (MonadIO c, Functor f) => MonadIO (Narrative f c) where
  {-# INLINE liftIO #-}
  liftIO ioa = buildn $ \r l _ -> l (fmap r (liftIO ioa))

instance MonadTrans (Narrative f) where
  {-# INLINE lift #-}
  lift ca = buildn $ \r l d -> l (fmap r ca)

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

{-# INLINE _fmapMsg #-}
_fmapMsg :: forall a b m ms. (Functor m, Functor (Messages ms)) => (a -> b) -> Messages (m ': ms) a -> Messages (m ': ms) b
_fmapMsg f = go
  where
    go :: Messages (m ': ms) a -> Messages (m ': ms) b
    go (Other ms) = Other (fmap f ms)
    go (Msg m) = Msg (fmap f m)

{-# RULES
  "fmap f (Other ms)" forall f ms. _fmapMsg f (Other ms) = Other (fmap f ms);
  "fmap f (Msg m)" forall f m. _fmapMsg f (Msg m) = Msg (fmap f m);
  #-}

instance (Monad c, Functor f) => MonadFree f (Narrative f c) where
  {-# INLINE wrap #-}
  wrap a = buildn $ \_ _ d -> d a

instance (Functor f, Monad c) => Functor (Narrative f c) where
  {-# INLINE fmap #-}
  fmap = _fmap

instance (Functor f, Monad c) => Applicative (Narrative f c) where
  {-# INLINE pure #-}
  pure a = return a
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance (Functor f, Monad c) => Monad (Narrative f c) where
  {-# INLINE return #-}
  return a = buildn $ \r l d -> r a

  {-# INLINE (>>=) #-}
  (>>=) = _bind

  {-# INLINE (>>) #-}
  (>>) ma mb = _bind ma (const mb)

instance (Monad c, Applicative f) => MonadPlus (Narrative f c) where
  {-# INLINE mzero #-}
  mzero = empty

  {-# INLINE mplus #-}
  mplus = (<|>)

instance (Monad c, Applicative f) => Alternative (Narrative f c) where
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
  throwM = lift . throwM
  {-# INLINE throwM #-}

instance (MonadCatch c, Functor f) => MonadCatch (Narrative f c) where
  catch = _catch
  {-# INLINE catch #-}

instance (MonadResource c, Functor f) => MonadResource (Narrative f c) where
  liftResourceT = lift . liftResourceT
  {-# INLINE liftResourceT #-}

instance (MonadReader r c, Functor f) => MonadReader r (Narrative f c) where
  ask = lift ask
  {-# INLINE ask #-}
  local f = hoist (local f)
  {-# INLINE local #-}

instance (MonadState s c, Functor f) => MonadState s (Narrative f c) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}
  state = lift . state
  {-# INLINE state #-}

instance (MonadError e c, Functor f) => MonadError e (Narrative f c) where
  throwError = lift . throwError
  {-# INLINE throwError #-}
  catchError = _catchError
  {-# INLINE catchError #-}

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
  -- where
  --   go (Return r) = Return r
  --   go (Lift c) = Lift $ catchError (fmap go c) (pure . f)
  --   go (Do f) = Do (fmap go f)

class Delta f g | f -> g where
  delta :: (a -> b -> r) -> f a -> g b -> r

type (<=>) ts ms = Delta (Modules ts) (Messages ms)

instance Delta ((->) a) ((,) a) where
  delta u f (l,r) = u (f l) r
  {-# INLINE delta #-}

instance Delta ((,) a) ((->) a) where
  delta u (l,r) g = u r (g l)
  {-# INLINE delta #-}

instance Delta (Modules '[]) (Messages '[]) where
  delta u _ _ = u undefined undefined
  {-# INLINE delta #-}

instance (t `Delta` m, Modules ts `Delta` Messages ms) => Delta (Modules (t ': ts)) (Messages (m ': ms)) where
  delta u (Mod t _) (Msg m) = delta u t m
  delta u (Mod _ ts) (Other ms) = delta u ts ms
  {-# INLINE delta #-}

-- Old runWith; does not use foldn.
-- {-# INLINE runWith #-}
-- runWith :: forall ts ms c a. ((Modules ts) `Delta` (Messages ms), Monad c) => Object ts c -> Ef ms c a -> c (Object ts c,a)
-- runWith object = go
--   where
--     go (Return a) = return (object,a)
--     go (Lift c) = c >>= go
--     go (Do m) = do
--       let (method,cont) = delta (,) (deconstruct object) m
--       object' <- method object
--       runWith object' cont

infixr 5 !
(!) :: ((Modules ts) `Delta` (Messages ms), Functor (Messages ms), Monad c) => Object ts c -> Ef ms c a -> c (Object ts c,a)
(!) = runWith

type Mod t ts c = t (Action ts c)

type Action ts c = Object ts c -> c (Object ts c)

class Has (ts :: [* -> *]) (t :: * -> *) where
  push :: t a -> Modules ts a -> Modules ts a
  pull :: Modules ts a -> t a

instance (i ~ Offset ts t, Has' ts t i) => Has ts t where
  push = let i = Index :: Index i in push' i
  {-# INLINE push #-}
  pull = let i = Index :: Index i in pull' i
  {-# INLINE pull #-}

class Has' (ts :: [* -> *]) (t :: * -> *) (n :: Nat) where
  push' :: Index n -> t a -> Modules ts a -> Modules ts a
  pull' :: Index n -> Modules ts a -> t a

{-# RULES
  "pull' i . push' i t" forall i t ts. pull' i (push' i t ts) = t
  #-}

instance ts ~ (t ': xs) => Has' ts t 'Z where
  push' _ t (Mod _ ts) = Mod t ts
  {-# INLINE push' #-}
  pull' _   (Mod t _) = t
  {-# INLINE pull' #-}

instance (i ~ Offset ts t, Has' ts t i) => Has' (t' ': ts) t ('S n) where
  push' _ t (Mod t' ts) = let i = Index :: Index i in Mod t' (push' i t ts)
  {-# INLINE push' #-}
  pull' _   (Mod _ ts)  = let i = Index :: Index i in pull' i ts
  {-# INLINE pull' #-}

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
  inj = let i = Index :: Index i in inj' i
  {-# INLINE inj #-}
  prj = let i = Index :: Index i in prj' i
  {-# INLINE prj #-}

class Can' ms m (n :: Nat) where
  inj' :: Index n -> m a -> Messages ms a
  prj' :: Index n -> Messages ms a -> Maybe (m a)

{-# RULES
  "inj i/prj i" forall i ma. prj' i (inj' i ma) = Just ma
  #-}

instance (i ~ Offset ms' m, Can' ms' m i) => Can' (m' ': ms') m ('S n) where
  inj' _            = let i = Index :: Index i in Other . inj' i
  {-# INLINE inj' #-}
  prj' _ (Other ms) = let i = Index :: Index i in prj' i ms
  prj' _ _          = Nothing
  {-# INLINE prj' #-}

instance (ms ~ (m ': ms')) => Can' ms m 'Z where
  inj' _                   = Msg
  {-# INLINE inj' #-}
  prj' _ (Msg message) = Just message
  prj' _ (Other _)     = Nothing
  {-# INLINE prj' #-}

-- Subtyping; think BIG <: little
type family (<:) ms ms' where
  ms <: '[] = (Functor (Messages ms), Typeable ms)
  ms <: (m ': ms') = (Can' ms m (Offset ms m), ms <: ms')

-- Supertyping; think little :> BIG
type ms :> ms' = ms' <: ms

infixr 6 *:*
(*:*) :: t a -> Modules ts a -> Modules (t ': ts) a
(*:*) = Mod
{-# INLINE (*:*) #-}

class Append ts ts' ts'' where
  (*++*) :: (Appended ts ts' ~ ts'') => Modules ts a -> Modules ts' a -> Modules ts'' a

instance (Appended ts ts' ~ ts'', Append ts ts' ts'') => Append (t ': ts) ts' (t ': ts'') where
  (*++*) (Mod m ms) ys = m *:* (ms *++* ys)
  {-# INLINE (*++*) #-}

instance Append '[] ts ts where
  (*++*) Empty ys = ys
  {-# INLINE (*++*) #-}

instance Append ts '[] ts where
  (*++*) xs Empty = xs
  {-# INLINE (*++*) #-}

{-# INLINE unit #-}
unit :: Proxy ()
unit = Proxy

data Interpreter ts ms c a = Interpreter
  { interpret :: (forall x. Ef ms c x -> c (Object ts c,x)) -> c (Object ts c,a) }

instance (Monad c, Functor (Messages ms)) => Functor (Interpreter ts ms c) where
  fmap f (Interpreter k) = Interpreter $ \k' -> fmap (fmap f) (k k')

instance (Monad c, Functor (Messages ms), Delta (Modules ts) (Messages ms)) => Applicative (Interpreter ts ms c) where
  pure = pure
  (<*>) = ap

instance (Monad c, Functor (Messages ms), Delta (Modules ts) (Messages ms)) => Monad (Interpreter ts ms c) where
  return a = Interpreter $ \k -> k (return a)
  (Interpreter k) >>= f = Interpreter $ \d -> k d >>= \(o,a) -> interpret (f a) (runWith o)

instance (Monad c, Functor (Messages ms), Delta (Modules ts) (Messages ms), MonadPlus c) => MonadPlus (Interpreter ts ms c) where
  mzero = Interpreter $ \k -> k (Lift mzero)
  mplus (Interpreter p0) (Interpreter p1) = Interpreter $ \k -> (p0 k) `mplus` (p1 k)

instance (Monad c, Functor (Messages ms), Delta (Modules ts) (Messages ms), MonadPlus c) => Alternative (Interpreter ts ms c) where
  empty = mzero
  (<|>) = mplus

instance (Functor (Messages ms), Delta (Modules ts) (Messages ms), MonadFix c) => MonadFix (Interpreter ts ms c) where
  mfix f = Interpreter $ \d -> mfix (\((_,x)) -> interpret (f x) d)

interp :: Ef ms c a -> Interpreter ts ms c a
interp n = Interpreter $ \k -> k n

infixr 5 !#
(!#) :: (MonadFix c, Functor (Messages ms), Delta (Modules ts) (Messages ms)) => Object ts c -> Interpreter ts ms c a -> c (Object ts c,a)
(!#) o i = interpret i (runWith o)

type Path ts ms c a = Cofree c (Object ts c,Ef ms c a)

lay :: (Monad c, Delta (Modules ts) (Messages ms)) => Object ts c -> Ef ms c a -> Path ts ms c a
lay obj nar = coiter (uncurry lay) (obj,nar)
  where
  lay o = go
    where
      go (Do m) =
        let (method,b) = delta (,) (deconstruct o) m
        in method o >>= \o' -> return (o',b)
      go (Lift m) = m >>= \c -> return (o,c)
      go (Return r) = return (o,Return r)

walk :: (Monad c, Delta (Modules ts) (Messages ms)) => Path ts ms c a -> c (Object ts c,a)
walk = step
  where
    step machine = do
      next <- unwrap machine
      let (obj,c) = extract next
      case c of
        (Return r) -> return (obj,r)
        _          -> step next

{-# INLINE runs #-}
-- groups in streaming
runs :: forall f g c r.
        (Functor f, Functor g, Monad c)
     => Narrative (Sum f g) c r
     -> Narrative (Sum (Narrative f c) (Narrative g c)) c r
runs n = buildn start
  where
    {-# INLINE start #-}
    start r l d = foldn r l (d . msg) n
      where
        {-# INLINE msg #-}
        msg (InL fn) = InL $ goL fn
        msg (InR gn) = InR $ goR gn

        {-# INLINE goL #-}
        goL n = buildn $ \r' l' d' -> d' $ fmap
                   (foldn (r' . r) l' (\x ->
                      case x of
                        InL fn -> join fn
                        InR gn -> r' (d (fmap (d . InL) (InR gn)))
                    )) n

        {-# INLINE goR #-}
        goR n = buildn $ \r' l' d' -> d' $ fmap
                  (foldn (r' . r) l' (\x ->
                      case x of
                        InL fn -> r' (d (fmap (d . InR) (InL fn)))
                        InR gn -> join gn
                  )) n

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
    runReturn a o = return (o,a)

    runLift c o = c >>= \f -> f o

    runDo :: forall ms ts c a. ((Modules ts) `Delta` (Messages ms), Functor (Messages ms), Monad c)
          => Messages ms (Object ts c -> c (Object ts c,a)) -> Object ts c -> c (Object ts c,a)
    runDo ms o =
      let ~(f,cont) = delta (,) (deconstruct o :: Modules ts (Action ts c)) ms
      in f o >>= \o' -> cont o'

{-# INLINE _fmap #-}
_fmap :: (Functor f, Monad c) => (a -> b) -> Narrative f c a -> Narrative f c b
_fmap f n = foldn (Return . f) Lift Do n

{-# INLINE _bind #-}
_bind :: (Functor f,Monad c) => Narrative f c a -> (a -> Narrative f c b) -> Narrative f c b
_bind ma amb = foldn amb Lift Do ma

{-# INLINE [0] foldn #-}
foldn :: (Functor f, Functor c) => (r -> b) -> (c b -> b) -> (f b -> b) -> Narrative f c r -> b
foldn r l d = go
  where
    go (Return a) = r a
    go (Lift c) = l (fmap go c)
    go (Do m) = d (fmap go m)

{-# INLINE [1] buildn #-}
buildn :: forall f c a. ((a -> Narrative f c a) -> (c (Narrative f c a) -> Narrative f c a) -> (f (Narrative f c a) -> Narrative f c a) -> Narrative f c a) -> Narrative f c a
buildn f = f Return Lift Do

{-# RULES
"foldn/buildn" forall r l d g. foldn r l d (buildn g) = g r l d

"foldn (Return r)" forall r l d a. foldn r l d (Return a) = r a
"foldn (Lift c)"   forall r l d c. foldn r l d (Lift c) = l (fmap (foldn r l d) c)
"foldn (Do m)"     forall r l d m. foldn r l d (Do m) = d (fmap (foldn r l d) m)

"_bind (Return r)" forall r f. _bind (Return r) f = f r
"_bind (Lift c)"   forall c f. _bind (Lift c) f   = Lift (fmap (\a -> _bind a f) c)
"_bind (Do m)"     forall m f. _bind (Do m) f     = Do (fmap (\a -> _bind a f) m)

"_fmap (Return r)" forall r f. _fmap f (Return r) = Return (f r)
"_fmap (Lift c)"   forall c f. _fmap f (Lift c)   = Lift (fmap (_fmap f) c)
"_fmap (Do m)"     forall m f. _fmap f (Do m)     = Do (fmap (_fmap f) m)

"_hoist (Return r)" forall f r. _hoist f (Return r) = Return r
"_hoist (Lift c)"   forall f c. _hoist f (Lift c)   = Lift (f (fmap (_hoist f) c))
"_hoist (Do m)"     forall f m. _hoist f (Do m)     = Do (fmap (_hoist f) m)

"_embed (Return r)" forall f r. _embed f (Return r) = Return r
"_embed (Lift c)"   forall f c. _embed f (Lift c) = _bind (f c) (_embed f)
"_embed (Do m)"     forall f m. _embed f (Do m) = Do (fmap (_embed f) m)

"_catch (Return r)" forall f r. _catch (Return r) f = Return r
"_catch (Lift c)"   forall f c. _catch (Lift c) f = Lift (catch (c >>= \a -> return (_catch a f)) (\a -> return (f a)))
"_catch (Do m)"     forall f m. _catch (Do m) f = Do (fmap (flip _catch f) m)

"_catchError (Return r)" forall f r. _catchError (Return r) f = Return r
"_catchError (Lift c)"   forall f (c :: forall g m a. (Functor g, MonadError e m) => Narrative g m a).
                                      _catchError (Lift c) f   = Lift (_catchError (fmap (flip _catchError f) c) (\a -> pure (f a)))
"_catchError (Do m)"     forall f m. _catchError (Do m) f     = Do (fmap (flip _catchError f) m)

"transform f (Do m k)"   forall f g m. transform f g (Do m)     = g m;
"transform f (Lift c)"   forall f g c. transform f g (Lift c)   = Lift (fmap (transform f g) c);
"transform f (Return r)" forall f g r. transform f g (Return r) = Return (f r);
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

data Child
data Sibling

data Fundament rel p ts k where
  Fundament :: Object ts c -> k -> (Object ts c -> k) -> Fundament rel p ts k

  GetFundament :: (Object ts c -> k) -> Fundament rel p ts k
  SetFundament :: Object ts c -> k -> Fundament rel p ts k

instance Functor (Fundament rel p ts) where
  fmap f (Fundament o k ok) = Fundament o (f k) (fmap f ok)
  fmap f (GetFundament ok) = GetFundament (fmap f ok)
  fmap f (SetFundament o k) = SetFundament o (f k)

child :: forall p ms ms' ts ts' c.
         ( Monad c
         , Delta (Modules ts') (Messages ms')
         , Delta (Modules ts) (Messages ms)
         , ts' <. '[Fundament Child p ts]
         )
      => Proxy p
      -> Object ts (Narrative (Messages ms') c)
      -> (Proxy '(p,Child,ts),Mod (Fundament Child p ts) ts' c)
child _ o = (Proxy,(fix $ \mk current -> Fundament current pure $ \new -> pure . Module (mk new)) o)

sibling :: forall p ms ms' ts ts' c.
           ( Monad c
           , Delta (Modules ts') (Messages ms')
           , Delta (Modules ts) (Messages ms)
           , ts' <. '[Fundament Sibling p ts]
           )
        => Proxy p
        -> Object ts c
        -> (Proxy '(p,Sibling,ts),Mod (Fundament Sibling p ts) ts' c)
sibling _ o = (Proxy,(fix $ \mk current -> Fundament current pure $ \new -> pure . Module (mk new)) o)

instance Delta (Fundament rel p ts) (Fundament rel p ts) where
  delta u (Fundament o t _) (GetFundament or) = u t (or (unsafeCoerce o))
  delta u (Fundament _ _ ot) (SetFundament o r) = u (ot (unsafeCoerce o)) r

tellChild :: forall p ms ms' ts c a.
             ( Functor (Messages ms')
             , Delta (Modules ts) (Messages ms)
             , Monad c
             , ms' <: '[Fundament Child p ts]
             , ms <: '[]
             )
          => Proxy '(p,Child,ts)
          -> Ef ms (Narrative (Messages ms') c) a
          -> Ef ms' c (Object ts (Narrative (Messages ms') c),Object ts (Narrative (Messages ms') c),a)
tellChild _ ma = do
  o <- Send (GetFundament Return :: Fundament Child p ts (Ef ms' c (Object ts (Narrative (Messages ms') c))))
  (o',a) <- o ! ma
  Send (SetFundament o' (Return ()) :: Fundament Child p ts (Ef ms' c ()))
  return (o,o',a)

tellSib :: forall p ms ms' ts c a.
            ( Functor (Messages ms')
            , Delta (Modules ts) (Messages ms)
            , Monad c
            , ms' <: '[Fundament Sibling p ts]
            , ms <: '[]
            )
        => Proxy '(p,Sibling,ts)
        -> Ef ms c a
        -> Ef ms' c (Object ts c,Object ts c,a)
tellSib _ ma = do
  o <- Send (GetFundament Return :: Fundament Sibling p ts (Ef ms' c (Object ts c)))
  (o',a) <- Lift (runWith o ma >>= return . Return)
  Send (SetFundament o' (Return ()) :: Fundament Sibling p ts (Ef ms' c ()))
  return (o,o',a)

getChild :: forall p ms ts c. (Monad c, ms <: '[Fundament Child p ts]) => Proxy '(p,Sibling,ts) -> Ef ms c (Object ts (Narrative (Messages ms) c))
getChild _ = Send (GetFundament Return :: Fundament Child p ts (Ef ms c (Object ts (Narrative (Messages ms) c))))

getSib :: forall p ms ts c. (Monad c, ms <: '[Fundament Sibling p ts]) => Proxy '(p,Sibling,ts) -> Ef ms c (Object ts c)
getSib _ = Send (GetFundament Return :: Fundament Sibling p ts (Ef ms c (Object ts c)))

----------------------------------------
-- Utilities


reduce :: (Monad c, Functor f) => Narrative f c a -> (f b -> b) -> (c b -> b) -> (a -> b) -> b
reduce n d l r = foldn r l d n

{-# INLINE observe #-}
observe :: (Functor f, Monad c) => Narrative f c r -> Narrative f c r
observe n = Lift $ foldn (return . Return) join (return . Do . fmap Lift) n

{-# INLINE never #-}
never :: (Monad c, Applicative f) => Narrative f c r
never = loop
  where
    loop = Lift $ return $ Do $ pure loop

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
          Right n -> Do $ fmap (go (i-1)) n

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
unseparate n = buildn go
  where
    go r l d = foldn r (join . maps InR) (d . InL) n

-- Walk a narrative n functor steps and then repackage the rest of the narrative as a return value.
-- If a return constructor is seen, it is repackaged as a return value. Context steps are not counted.
{-# INLINE splitsAt #-}
splitsAt :: (Functor f, Monad c) => Int -> Narrative f c r -> Narrative f c (Narrative f c r)
splitsAt = go
  where
    go i n | i <= 0 = return n
    go i n =
      case n of
        Return _ -> return n
        Lift c -> super (fmap (go i) c)
        Do m -> wrap (fmap (go (i - 1)) m)

-- drops functor layers where the functor is comonadic (to guarantee extractability);
-- keeps effect layers. Note that Ef is non-droppable.
{-# INLINE drops #-}
drops :: (Comonad f, Monad c) => Int -> Narrative f c r -> Narrative f c r
drops = go
  where
    go i n | i <= 0 = n
    go i n =
      case n of
        Return a -> return a
        Lift c -> super (fmap (go i) c)
        Do m -> go (i - 1) (extract m)

-- takes n functor layers; keeps effect layers.
-- Equivalent to `void . splitsAt n`
{-# INLINE takes #-}
takes :: (Functor f, Monad c) => Int -> Narrative f c r -> Narrative f c ()
takes = go
  where
    go i n | i <= 0 = return ()
    go i n =
      case n of
        Return a -> return ()
        Lift c -> super (fmap (go i) c)
        Do m -> wrap (fmap (go (i - 1)) m)

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
concats n = buildn go
  where
    go r l d = foldn r l join n

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
    go r l d = go'
      where
        go' = l (return (d (fmap (\_ -> go') f)))

-- repeat a contextualized layer whose result is a functor layer
{-# INLINE repeatsM #-}
repeatsM :: (Functor f, Monad c) => c (f ()) -> Narrative f c r
repeatsM cf = buildn go
  where
    go r l d = go'
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

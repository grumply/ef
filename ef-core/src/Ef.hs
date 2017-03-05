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

import Data.Data
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Proxy as Export
import Data.Typeable
import Ef.Type.Bool as Export
import Ef.Type.Nat as Export
import Ef.Type.List as Export
import Ef.Type.Set as Export

import Unsafe.Coerce

data Modules (ts :: [* -> *]) (x :: *) where
  Empty :: Modules '[] x
  Mod :: t x -> Modules ts x -> Modules (t ': ts) x

data Messages ms a where
  Other :: Messages ms' a -> Messages (m ': ms') a
  Msg :: m a -> Messages (m ': ms') a

newtype Object ts c = Object { deconstruct :: Modules ts (Action ts c) }

data Narrative (f :: * -> *) c a where
  Return ::                  a  -> Narrative f c a
  Lift   :: c (Narrative f c a) -> Narrative f c a
  Do     :: f (Narrative f c a) -> Narrative f c a

type Code (ms :: [* -> *]) (c :: * -> *) = Narrative (Messages ms) c

viewMsg :: (Can' ms m (Offset ms m)) => Code ms c a -> Maybe (m (Code ms c a))
viewMsg (Do m) = prj m
viewMsg _ = Nothing

pattern Module x o <- (\o -> let x = pull (deconstruct o) in (x,o) -> (x,o)) where
  Module x o = Object $ push x $ deconstruct o

pattern Send x <- (viewMsg -> Just x) where
  Send x = Do (inj x)

{-# NOINLINE [1] send #-}
send :: (Functor f, Monad c) => f r -> Narrative f c r
send fr = Do (fmap Return fr)

{-# NOINLINE [1] super #-}
super :: Monad c => c (Narrative f c a) -> Narrative f c a
super = Lift

deriving instance (Show a, Show (c (Narrative f c a)), Show (f (Narrative f c a))) => Show (Narrative f c a)
deriving instance (Eq a, Eq (c (Narrative f c a)), Eq (f (Narrative f c a))) => Eq (Narrative f c a)
deriving instance (Typeable f, Typeable c, Data a, Data (c (Narrative f c a)), Data (f (Narrative f c a))) => Data (Narrative f c a)

instance (MonadIO c, Functor f) => MonadIO (Narrative f c) where
  liftIO = _liftIO

{-# NOINLINE [1] _liftIO #-}
_liftIO :: MonadIO c => IO a -> Narrative f c a
_liftIO ioa = Lift (fmap Return (liftIO ioa))

instance MonadTrans (Narrative f) where
  lift ca = Lift (fmap Return ca)

instance Functor (Modules '[]) where
  fmap _ _ = Empty

instance (Functor t, Functor (Modules ts)) => Functor (Modules (t ': ts)) where
  fmap f (Mod t ts) = Mod (fmap f t) (fmap f ts)

instance Functor (Messages '[])

instance (Functor (Messages ms), Functor m) => Functor (Messages (m ': ms)) where
  fmap = _fmapMsg

{-# NOINLINE [1] _fmapMsg #-}
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
  wrap = Do

instance (Functor f, Functor c) => Functor (Narrative f c) where
  fmap = _fmap

instance (Functor f, Monad c) => Applicative (Narrative f c) where
  pure a = buildn $ \r _ _ -> r a
  (<*>) = ap

instance (Functor f, Monad c) => Monad (Narrative f c) where
  return a = buildn $ \r _ _ -> r a
  {-# INLINE return #-}
  (>>=) = _bind
  {-# INLINE (>>=) #-}
  (>>) a b = _bind a (\_ -> b)
  {-# INLINE (>>) #-}

instance (MonadPlus c, Applicative f) => MonadPlus (Narrative f c) where
  mzero = lift mzero
  mplus = _mplus

instance (Monad c, Applicative f) => Alternative (Narrative f c) where
  empty = never
  (<|>) =  zipsWith (liftA2 (,))

instance (Monad c, Functor f, Monoid r) => Monoid (Narrative f c r) where
  mempty = pure mempty
  mappend = _mappend

instance Functor f => MFunctor (Narrative f) where
  hoist = _hoist

instance Functor f => MMonad (Narrative f) where
  embed = _embed

instance (MonadBase b c, Functor f) => MonadBase b (Narrative f c) where
  liftBase = Lift . fmap Return . liftBase

instance (MonadThrow c, Functor f) => MonadThrow (Narrative f c) where
  throwM = lift . throwM

instance (MonadCatch c, Functor f) => MonadCatch (Narrative f c) where
  catch = _catch

instance (MonadResource c, Functor f) => MonadResource (Narrative f c) where
  liftResourceT = lift . liftResourceT

instance (MonadReader r c, Functor f) => MonadReader r (Narrative f c) where
  ask = lift ask
  local f = hoist (local f)

instance (MonadState s c, Functor f) => MonadState s (Narrative f c) where
  get = lift get
  put = lift . put
  state = lift . state

instance (MonadError e c, Functor f) => MonadError e (Narrative f c) where
  throwError = lift . throwError
  catchError = _catchError

{-# NOINLINE [1] _mappend #-}
_mappend :: (Monad c, Functor f, Monoid r) => Narrative f c r -> Narrative f c r -> Narrative f c r
_mappend p0 p1 = go p0
  where
    go (Return r) = fmap (mappend r) p1
    go (Lift c) = Lift (fmap go c)
    go (Do m) = Do (fmap go m)

{-# NOINLINE [1] _fmap #-}
_fmap :: (Functor f, Functor c) => (a -> b) -> Narrative f c a -> Narrative f c b
_fmap f = go
  where
    go (Return a) = Return (f a)
    go (Lift c) = Lift (fmap (fmap f) c)
    go (Do m) = Do (fmap (fmap f) m)

{-# NOINLINE [1] _bind #-}
_bind :: (Monad c, Functor f) => Narrative f c a -> (a -> Narrative f c b) -> Narrative f c b
_bind ma amb = go ma
  where
    go (Return a) = amb a
    go (Lift cma) = Lift (fmap (>>= amb) cma)
    go (Do mu) = Do (fmap (>>= amb) mu)

{-# NOINLINE [1] _mplus #-}
_mplus :: (MonadPlus c, Applicative f) => Narrative f c r -> Narrative f c r -> Narrative f c r
_mplus p0 p1 = go p0
  where
    go (Return r) = Return r
    go (Lift m) = Lift (fmap (\x -> mplus x p1) m)
    go (Do m) = Do (fmap (\x -> mplus x p1) m)

{-# NOINLINE [1] _hoist #-}
_hoist :: (Functor f, Functor c') => (c' (Narrative f c a) -> c (Narrative f c a)) -> Narrative f c' a -> Narrative f c a
_hoist f = go
  where
    go (Return r) = Return r
    go (Lift c) = Lift (f (fmap go c))
    go (Do m) = Do (fmap go m)

{-# NOINLINE [1] _embed #-}
_embed :: (Monad c, Functor f) => (t (Narrative f t a) -> Narrative f c (Narrative f t a)) -> Narrative f t a -> Narrative f c a
_embed f = go
  where
    go (Return r) = Return r
    go (Lift c) = f c >>= go
    go (Do m) = Do (fmap go m)

{-# NOINLINE [1] _catch #-}
_catch :: (Exception e, Functor f, MonadCatch c) => Narrative f c a -> (e -> Narrative f c a) -> Narrative f c a
_catch n f = go n
  where
    go (Return r) = Return r
    go (Lift c) = Lift (catch (c >>= return . go) (pure . f))
    go (Do m) = Do (fmap go m)

{-# NOINLINE [1] _catchError #-}
_catchError :: (Functor f, MonadError e c) => Narrative f c a -> (e -> Narrative f c a) -> Narrative f c a
_catchError n f = go n
  where
    go (Return r) = Return r
    go (Lift c) = Lift $ catchError (fmap go c) (pure . f)
    go (Do f) = Do (fmap go f)

class Delta f g | f -> g, g -> f where
  delta :: (a -> b -> r) -> f a -> g b -> r

instance Delta ((->) a) ((,) a) where
  delta u f (l,r) = u (f l) r

instance Delta ((,) a) ((->) a) where
  delta u (l,r) g = u r (g l)

instance Delta (Modules '[]) (Messages '[]) where
  delta u _ _ = u undefined undefined

instance (t `Delta` m, Modules ts `Delta` Messages ms) => Delta (Modules (t ': ts)) (Messages (m ': ms)) where
  delta u (Mod t _) (Msg m) = delta u t m
  delta u (Mod _ ts) (Other ms) = delta u ts ms

runWith :: forall ts ms c a. ((Modules ts) `Delta` (Messages ms), Monad c) => Object ts c -> Code ms c a -> c (Object ts c,a)
runWith object = go
  where
    go (Return a) = return (object,a)
    go (Lift c) = c >>= go
    go (Do m) = do
      let (method,cont) = delta (,) (deconstruct object) m
      object' <- method object
      runWith object' cont

infixr 5 !
(!) :: ((Modules ts) `Delta` (Messages ms), Monad c) => Object ts c -> Code ms c a -> c (Object ts c,a)
(!) = runWith

type Mod t ts c = t (Action ts c)

type Action ts c = Object ts c -> c (Object ts c)

class Has (ts :: [* -> *]) (t :: * -> *) where
  push :: t a -> Modules ts a -> Modules ts a
  pull :: Modules ts a -> t a

instance (i ~ Offset ts t, Has' ts t i) => Has ts t where
  push = let i = Index :: Index i in push' i
  pull = let i = Index :: Index i in pull' i

class Has' (ts :: [* -> *]) (t :: * -> *) (n :: Nat) where
  push' :: Index n -> t a -> Modules ts a -> Modules ts a
  pull' :: Index n -> Modules ts a -> t a

instance ts ~ (t ': xs) => Has' ts t 'Z where
  push' _ t (Mod _ ts) = Mod t ts
  pull' _   (Mod t _) = t

instance (i ~ Offset ts t, Has' ts t i) => Has' (t' ': ts) t ('S n) where
  push' _ t (Mod t' ts) = let i = Index :: Index i in Mod t' (push' i t ts)
  pull' _   (Mod _ ts)  = let i = Index :: Index i in pull' i ts

type ts .> ts' = ts' <. ts
type family (<.) (ts :: [* -> *]) (ts' :: [* -> *]) where
  (<.) (t ': '[]) ts' = (Has' ts' t (Offset ts' t))
  (<.) (t ': ts) ts' = (Has' ts' t (Offset ts' t), ts <. ts')

class Can ms m where
  inj :: m a -> Messages ms a
  prj :: Messages ms a -> Maybe (m a)

instance (i ~ Offset ms m, Can' ms m i) => Can ms m where
  inj = let i = Index :: Index i in inj' i
  prj = let i = Index :: Index i in prj' i

class Can' ms m (n :: Nat) where
  inj' :: Index n -> m a -> Messages ms a
  prj' :: Index n -> Messages ms a -> Maybe (m a)

instance (i ~ Offset ms' m, Can' ms' m i) => Can' (m' ': ms') m ('S n) where
  inj' _            = let i = Index :: Index i in Other . inj' i
  prj' _ (Other ms) = let i = Index :: Index i in prj' i ms
  prj' _ _          = Nothing

instance (ms ~ (m ': ms')) => Can' ms m 'Z where
  inj' _                   = Msg
  prj' _ (Msg message) = Just message
  prj' _ (Other _)     = Nothing

type ms :> ms' = ms' <: ms
type family (<:) ms ms' where
  '[] <: ms = (Functor (Messages ms))
  (m ': ms') <: ms = (Can' ms m (Offset ms m), ms' <: ms)

infixr 6 *:*
(*:*) :: t a -> Modules ts a -> Modules (t ': ts) a
(*:*) = Mod

class Append ts ts' ts'' where
  (*++*) :: (Appended ts ts' ~ ts'') => Modules ts a -> Modules ts' a -> Modules ts'' a

instance (Appended ts ts' ~ ts'', Append ts ts' ts'') => Append (t ': ts) ts' (t ': ts'') where
  (*++*) (Mod m ms) ys = m *:* (ms *++* ys)

instance Append '[] ts ts where
  (*++*) Empty ys = ys

instance Append ts '[] ts where
  (*++*) xs Empty = xs

unit :: Proxy ()
unit = Proxy



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
         , '[Fundament Child p ts] <. ts'
         )
      => Proxy p
      -> Object ts (Narrative (Messages ms') c)
      -> (Proxy '(p,Child,ts),Mod (Fundament Child p ts) ts' c)
child _ o = (Proxy,(fix $ \mk current -> Fundament current pure $ \new -> pure . Module (mk new)) o)

sibling :: forall p ms ms' ts ts' c.
           ( Monad c
           , Delta (Modules ts') (Messages ms')
           , Delta (Modules ts) (Messages ms)
           , '[Fundament Sibling p ts] <. ts'
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
             , '[Fundament Child p ts] <: ms'
             )
          => Proxy '(p,Child,ts)
          -> Code ms (Narrative (Messages ms') c) a
          -> Code ms' c (Object ts (Narrative (Messages ms') c),Object ts (Narrative (Messages ms') c),a)
tellChild _ ma = do
  o <- Send (GetFundament Return :: Fundament Child p ts (Code ms' c (Object ts (Narrative (Messages ms') c))))
  (o',a) <- o ! ma
  Send (SetFundament o' (Return ()) :: Fundament Child p ts (Code ms' c ()))
  return (o,o',a)

tellSib :: forall p ms ms' ts c a.
            ( Functor (Messages ms')
            , Delta (Modules ts) (Messages ms)
            , Monad c
            , '[Fundament Sibling p ts] <: ms'
            )
        => Proxy '(p,Sibling,ts)
        -> Code ms c a
        -> Code ms' c (Object ts c,Object ts c,a)
tellSib _ ma = do
  o <- Send (GetFundament Return :: Fundament Sibling p ts (Code ms' c (Object ts c)))
  (o',a) <- Lift (runWith o ma >>= return . Return)
  Send (SetFundament o' (Return ()) :: Fundament Sibling p ts (Code ms' c ()))
  return (o,o',a)

getChild :: forall p ms ts c. (Monad c, '[Fundament Child p ts] <: ms) => Proxy '(p,Sibling,ts) -> Code ms c (Object ts (Narrative (Messages ms) c))
getChild _ = Send (GetFundament Return :: Fundament Child p ts (Code ms c (Object ts (Narrative (Messages ms) c))))

getSib :: forall p ms ts c. (Monad c, '[Fundament Sibling p ts] <: ms) => Proxy '(p,Sibling,ts) -> Code ms c (Object ts c)
getSib _ = Send (GetFundament Return :: Fundament Sibling p ts (Code ms c (Object ts c)))

data Interpreter ts ms c a = Interpreter
  { interpret :: (forall x. Code ms c x -> c (Object ts c,x)) -> c (Object ts c,a) }

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

interp :: Code ms c a -> Interpreter ts ms c a
interp n = Interpreter $ \k -> k n

infixr 5 !#
(!#) :: (MonadFix c, Delta (Modules ts) (Messages ms)) => Object ts c -> Interpreter ts ms c a -> c (Object ts c,a)
(!#) o i = interpret i (runWith o)

type Path ts ms c a = Cofree c (Object ts c,Code ms c a)

lay :: (Monad c, Delta (Modules ts) (Messages ms)) => Object ts c -> Code ms c a -> Path ts ms c a
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


reduce :: (Monad c, Functor f) => Narrative f c a -> (f b -> b) -> (c b -> b) -> (a -> b) -> b
reduce n d l r = foldn r l d n

{-# INLINE [0] foldn #-}
foldn :: (Functor f, Functor c) => (r -> b) -> (c b -> b) -> (f b -> b) -> Narrative f c r -> b
foldn r l d = go
  where
    go (Return a) = r a
    go (Lift c) = l (fmap go c)
    go (Do m) = d (fmap go m)

{-# INLINE [1] buildn #-}
buildn :: (forall b. (r -> b) -> (c b -> b) -> (f b -> b) -> b) -> Narrative f c r
buildn f = f Return Lift Do

observe :: (Functor f, Monad c) => Narrative f c r -> Narrative f c r
observe = Lift . foldn (return . Return) join (return . Do . fmap Lift)

never :: (Monad c, Applicative f) => Narrative f c r
never =  let loop = Lift $ return $ Do $ pure loop in loop

uncons :: (Functor f, Monad c) => Narrative f c r -> c (Either r (f (Narrative f c r)))
uncons = _uncons

{-# NOINLINE [1] _uncons #-}
_uncons :: (Functor f, Monad c) => Narrative f c r -> c (Either r (f (Narrative f c r)))
_uncons (Return r) = return (Left r)
_uncons (Lift m) = m >>= _uncons
_uncons (Do fs) = return (Right fs)

zipsWith :: (Monad c, Functor f, Functor g, Functor h)
         => (forall x y . f x -> g y -> h (x,y))
         -> Narrative f c a -> Narrative g c a -> Narrative h c a
zipsWith = _zipsWith

{-# NOINLINE [1] _zipsWith #-}
_zipsWith :: (Monad c, Functor f, Functor l, Functor r)
          => (l (Narrative l c a) -> r (Narrative r c a) -> f (Narrative l c a, Narrative r c a))
          -> Narrative l c a
          -> Narrative r c a
          -> Narrative f c a
_zipsWith f l r = Lift $ liftA2 (_zipsWithInternal f) (uncons l) (uncons r)

{-# NOINLINE [1] _zipsWithInternal #-}
_zipsWithInternal :: (Monad c, Functor f, Functor l, Functor r)
                  => (l (Narrative l c a)
                  -> r (Narrative r c a) -> f (Narrative l c a, Narrative r c a))
                  -> Either a (l (Narrative l c a))
                  -> Either a (r (Narrative r c a))
                  -> Narrative f c a
_zipsWithInternal f = go
  where
    go (Left l) _ = Return l
    go _ (Left r) = Return r
    go (Right l) (Right r) = Do (fmap (uncurry (_zipsWith f)) (f l r))

zips :: (Monad c, Functor f, Functor g) => Narrative f c a -> Narrative g c a -> Narrative (Compose f g) c a
zips = zipsWith (\f g -> Compose (fmap (\x -> fmap (\y -> (x,y)) g) f))

unzips :: (Functor f, Functor g, Monad c) => Narrative (Compose f g) c r -> Narrative f (Narrative g c) r
unzips n = reduce n (\(Compose fgn) -> Do (fmap (Lift . send) fgn)) (Lift . lift) return

interleaves :: (Monad c, Applicative f) => Narrative f c a -> Narrative f c a -> Narrative f c a
interleaves = zipsWith (liftA2 (,))

cutoff :: (Functor f, Monad c) => Int -> Narrative f c r -> Narrative f c (Maybe r)
cutoff = go
  where
    go 0 _ = return Nothing
    go i n = do
        e <- lift $ uncons n
        case e of
          Left r -> return (Just r)
          Right n -> Do $ fmap (go (i-1)) n

unfold :: (Functor f, Monad c) => (s -> c (Either r (f s))) -> s -> Narrative f c r
unfold = _unfold

{-# NOINLINE [1] _unfold #-}
_unfold :: (Functor f, Monad c) => (s -> c (Either r (f s))) -> s -> Narrative f c r
_unfold mk n = Lift $ mk n >>= return . either Return (Do . fmap (_unfold mk))

iterTM :: (Functor f, Monad c, MonadTrans t, Monad (t c)) => (f (t c a) -> t c a) -> Narrative f c a -> t c a
iterTM f n = reduce n f (join . lift) return

iterT :: (Functor f, Monad c) => (f (c a) -> c a) -> Narrative f c a -> c a
iterT f n = reduce n f join return

distribute :: (Functor f, Monad c, MonadTrans t, MFunctor t, Monad (t (Narrative f c)))
           => Narrative f (t c) r -> t (Narrative f c) r
distribute = _distribute

{-# NOINLINE [1] _distribute #-}
_distribute :: (Functor f, Monad c, MonadTrans t, MFunctor t, Monad (t (Narrative f c)))
            => Narrative f (t c) r -> t (Narrative f c) r
_distribute = go
  where
    go (Return r) = lift (Return r)
    go (Lift c) = hoist lift c >>= go
    go (Do m) = join (lift (Do (fmap (Return . go) m)))

{-# RULES
"foldn (Return r)" forall r l d a. foldn r l d (Return a) = r a
"foldn (Lift c)"   forall r l d c. foldn r l d (Lift c)   = l (fmap (foldn r l d) c)
"foldn (Do m)"     forall r l d m. foldn r l d (Do m)     = d (fmap (foldn r l d) m)

"_bind (Return r)" forall r f. _bind (Return r) f = f r
"_bind (Lift c)"   forall c f. _bind (Lift c) f   = Lift (fmap (\a -> _bind a f) c)
"_bind (Do m)"     forall m f. _bind (Do m) f     = Do (fmap (\a -> _bind a f) m)

-- "foldn/_bind"      forall r l d (g::forall b. (r -> b) -> (c b -> b) -> (f b -> b) -> b) amb.
--                     foldn r l d (_bind (buildn g) amb) = foldn (\a -> foldn (unsafeCoerce r) l d (amb a)) l d (g r l d)

"foldn/buildn"    forall r l d (g::forall b. (r -> b) -> (c b -> b) -> (f b -> b) -> b) .
                  foldn r l d (buildn g) = g r l d

"_fmap (Return r)" forall r f. _fmap f (Return r) = Return (f r)
"_fmap (Lift c)"   forall c f. _fmap f (Lift c)   = Lift (fmap (_fmap f) c)
"_fmap (Do m)"     forall m f. _fmap f (Do m)     = Do (fmap (_fmap f) m)

"send"   forall f. send f = buildn $ \r _ d -> d (fmap r f)

"_liftIO" forall c. _liftIO c = Lift (liftIO (fmap Return c))

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

"_mplus (Return res)"   forall r res. _mplus (Return res) r = Return res
"_mplus (Lift sup)"     forall c r.   _mplus (Lift c) r     = Lift (fmap (\x -> _mplus x r) c)
"_mplus (Do message k)" forall r m.   _mplus (Do m) r       = Do (fmap (\x -> _mplus x r) m)

"_mappend (Return res)" forall r res. _mappend (Return res) r = fmap (mappend res) r
"_mappend (Lift c)"     forall c r.   _mappend (Lift c) r     = Lift (fmap (\x -> _mappend x r) c)
"_mappend (Do m k)"     forall r m.   _mappend (Do m) r       = Do (fmap (\x -> _mappend x r) m)

-- "_observe (Return r)" forall r. _observe (Return r) = return (Return r)
-- "_observe (Lift c)"   forall c. _observe (Lift c)   = c >>= _observe
-- "_observe (Do m)"     forall m. _observe (Do m)     = return (Do (fmap (Lift . _observe) m))

"_distribute (Return r)" forall r. _distribute (Return r) = lift (Return r)
"_distribute (Lift c)"   forall c. _distribute (Lift c)   = hoist lift c >>= _distribute
"_distribute (Do m)"     forall m. _distribute (Do m)     = join (lift (Do (fmap (Return . _distribute) m)))

"_zipsWith f l r" forall f l r. _zipsWith f l r = Lift $ liftA2 (_zipsWithInternal f) (uncons l) (uncons r)

"_zipsWithInternal f (Left l) r"         forall f l r. _zipsWithInternal f (Left l) r = Return l
"_zipsWithInternal f l (Left r)"         forall f l r. _zipsWithInternal f l (Left r) = Return r
"_zipsWithInernal f (Right l) (Right r)" forall f l r. _zipsWithInternal f (Right l) (Right r) = Do (fmap (uncurry (_zipsWith f)) (f l r))

"_unfold" forall mk n. _unfold mk n = Lift (mk n >>= \a -> return (either Return (\x -> Do (fmap (_unfold mk) x)) a))

-- -- Watch these.
"_uncons (Return r)" forall r. _uncons (Return r) = return (Left r)
"_uncons (Lift c)"   forall c. _uncons (Lift c)   = c >>= _uncons
"_uncons (Do m)"     forall m. _uncons (Do m)     = return (Right m)
 #-}

{- for reference

_unfold mk n = Lift $ mk n >>= return . either Return (Do . fmap go)
swapObj :: (Monad c, Delta (Modules ts) (Messages ms)) => Object ts c -> Cofree c (Object ts c, Code ms c a) -> Cofree c (Object ts c, Code ms c a)
swapObj obj = path obj . snd . extract

swapCode :: (Monad c, Delta (Modules ts) (Messages ms)) => Code ms c a -> Cofree c (Object ts c, Code ms c a) -> Cofree c (Object ts c, Code ms c a)
swapCode code = flip path code . fst . extract

branch :: (Monad c, Delta (Modules ts) (Messages ms)) => Cofree c (Object ts c, Code ms c a) -> Cofree c (Cofree c (Object ts c, Code ms c a))
branch = duplicate

reseedObj :: (Monad c, Delta (Modules ts) (Messages ms)) => Object ts c -> Cofree c (Cofree c (Object ts c, Code ms c a)) -> Cofree c (Cofree c (Object ts c, Code ms c a))
reseedObj obj = fmap (swapObj obj)

reseedCode :: (Monad c, Delta (Modules ts) (Messages ms)) => Code ms c a -> Cofree c (Cofree c (Object ts c, Code ms c a)) -> Cofree c (Cofree c (Object ts c, Code ms c a))
reseedCode code = fmap (swapCode code)
-}

module Ef (module Ef, module Export) where

import Control.Applicative as Export
import Control.Monad as Export
import Control.Monad.Codensity as Export
import Control.Monad.Fix as Export
import Control.Monad.Free as Export
import Control.Monad.IO.Class as Export
import Control.Monad.Trans.Class as Export
import Control.Comonad
import Control.Comonad.Cofree

import Data.Proxy as Export
import Ef.Type.Bool as Export
import Ef.Type.Nat as Export
import Ef.Type.List as Export
import Ef.Type.Set as Export

import Unsafe.Coerce

-- To fully understand the language, read the first 200 lines.

data Modules (ts :: [* -> *]) (x :: *) where
  Empty :: Modules '[] x
  Mod :: t x -> Modules ts x -> Modules (t ': ts) x

data Messages ms a where
  Other :: Messages ms' a -> Messages (m ': ms') a
  Msg :: m a -> Messages (m ': ms') a

newtype Object ts c = Object { deconstruct :: Modules ts (Action ts c) }

data Narrative (f :: * -> *) c a where
  Do     :: f (Narrative f c a) -> Narrative f c a
  Lift   :: c (Narrative f c a) -> Narrative f c a
  Return ::                  a  -> Narrative f c a

type Code (ms :: [* -> *]) (c :: * -> *) = Narrative (Messages ms) c

viewMsg :: (Can' ms m (Offset ms m)) => Code ms c a -> Maybe (m (Code ms c a))
viewMsg (Do m) = prj m
viewMsg _ = Nothing

pattern Module x o <- (\o -> let x = pull (deconstruct o) in (x,o) -> (x,o)) where
  Module x o = Object (push x (deconstruct o))

pattern Send x <- (viewMsg -> Just x) where
  Send x = Do (inj x)

{-# INLINABLE super #-}
super :: Monad c => c (Narrative f c a) -> Narrative f c a
super = Lift

instance (MonadIO c, Functor f) => MonadIO (Narrative f c) where
  liftIO m = Lift (liftIO (fmap Return m))

instance MonadTrans (Narrative f) where
  lift m = super (fmap Return m)

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

instance (Functor f, Monad c) => Functor (Narrative f c) where
  fmap = _fmap

instance (Functor f, Monad c) => Applicative (Narrative f c) where
  pure = Return
  (<*>) = ap

instance (Functor f, Monad c) => Monad (Narrative f c) where
  return = Return
  (>>=) = _bind

{-# NOINLINE [1] _fmap #-}
_fmap :: (Monad c, Functor f) => (a -> b) -> Narrative f c a -> Narrative f c b
_fmap f = go where
  go (Return a) = Return (f a)
  go (Do m) = Do (fmap (fmap f) m)
  go (Lift c) = Lift (fmap (fmap f) c)

{-# NOINLINE [1] _bind #-}
_bind :: (Monad c, Functor f) => Narrative f c a -> (a -> Narrative f c b) -> Narrative f c b
_bind ma amb = go ma where
  go (Do mu) = Do (fmap (>>= amb) mu)
  go (Lift cma) = Lift (fmap (>>= amb) cma)
  go (Return a) = amb a

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

{-# INLINE runWith #-}
runWith :: forall ts ms c a. ((Modules ts) `Delta` (Messages ms), Monad c) => Object ts c -> Code ms c a -> c (Object ts c,a)
runWith object = go where
  go (Do m) = do
    let (method,cont) = delta (,) (deconstruct object) m
    object' <- method object
    runWith object' cont
  go (Lift c) = c >>= go
  go (Return a) = return (object,a)

infixr 5 ! 
{-# INLINABLE (!) #-}
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
{-# INLINABLE (*:*) #-}
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

--------------------------------------------------------------------------------

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

{-# INLINE interp #-}
interp :: Code ms c a -> Interpreter ts ms c a
interp n = Interpreter $ \k -> k n

infixr 5 !#
{-# INLINE (!#) #-}
(!#) :: (MonadFix c, Delta (Modules ts) (Messages ms)) => Object ts c -> Interpreter ts ms c a -> c (Object ts c,a)
(!#) o i = interpret i (runWith o)

instance (MonadPlus c, Functor f) => MonadPlus (Narrative f c) where
  mzero = super mzero
  mplus = _mplus

instance (MonadPlus c, Functor f) => Alternative (Narrative f c) where
  empty = mzero
  (<|>) = _mplus

{-# NOINLINE [1] _mplus #-}
_mplus :: (MonadPlus c, Functor f) => Narrative f c r -> Narrative f c r -> Narrative f c r
_mplus p0 p1 = go p0
  where
    go (Lift m) = Lift (fmap (\x -> mplus x p1) m)
    go (Do m) = Do (fmap (\x -> mplus x p1) m)
    go result = result

instance (Monad c, Functor f, Monoid r) => Monoid (Narrative f c r) where
  mempty = pure mempty
  mappend = _mappend

{-# NOINLINE [1] _mappend #-}
_mappend :: (Monad c, Functor f, Monoid r) => Narrative f c r -> Narrative f c r -> Narrative f c r
_mappend p0 p1 = go p0
  where
    go (Return r) = fmap (mappend r) p1
    go (Lift c) = Lift (fmap go c)
    go (Do m) = Do (fmap go m)

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
      go done = return (o,done)

walk :: (Monad c, Delta (Modules ts) (Messages ms)) => Path ts ms c a -> c (Object ts c,a)
walk = step
  where
    step machine = do
      next <- unwrap machine
      let (obj,c) = extract next
      case c of
        (Return r) -> return (obj,r)
        _          -> step next

type family Reinjectable m ms ms' where
  Reinjectable m ms '[] = (Functor (Messages ms))
  Reinjectable m ms (m ': ms') = Reinjectable m ms ms'
  Reinjectable m ms (m' ': ms') = (Can' ms m' (Offset ms m'), Reinjectable m ms ms')

type Subtract m ms ms' = ('[m] <: ms, Removed ms m ~ ms', Reinjectable m ms ms')

transform :: Monad c => (r -> a) -> (Messages ms (Code ms c r) -> Code ms' c a) -> Code ms c r -> Code ms' c a
transform = _transform

{-# NOINLINE [1] _transform #-}
_transform :: Monad c => (r -> a) -> (Messages ms (Code ms c r) -> Code ms' c a) -> Code ms c r -> Code ms' c a
_transform f t = go
  where
    go (Do m) = t m
    go (Lift sup) = Lift (fmap go sup)
    go (Return r) = Return (f r)

{-# RULES
    "(Do m) >>= f"     forall m f. _bind (Do m) f     = Do (fmap (\a -> _bind a f) m);
    "(Lift c) >>= f"   forall c f. _bind (Lift c) f   = Lift (c >>= \a -> return (_bind a f));
    "(Return r) >>= f" forall r f. _bind (Return r) f = f r;

    "fmap f (Do m k)"   forall m f. _fmap f (Do m)     = Do (fmap (_fmap f) m);
    "fmap f (Lift c)"   forall c f. _fmap f (Lift c)   = Lift (fmap (_fmap f) c);
    "fmap f (Return r)" forall r f. _fmap f (Return r) = Return (f r);

    "_mplus (Lift sup) r"     forall c r.   _mplus (Lift c) r     = Lift (fmap (\x -> _mplus x r) c);
    "_mplus (Return res) r"   forall r res. _mplus (Return res) r = Return res;
    "_mplus (Do message k) f" forall r m.   _mplus (Do m) r       = Do (fmap (\x -> _mplus x r) m);

    "_mappend (Lift c) r"     forall c r.   _mappend (Lift c) r     = Lift (fmap (\x -> _mappend x r) c);
    "_mappend (Return res) r" forall r res. _mappend (Return res) r = fmap (mappend res) r;
    "_mappend (Do m k) f"     forall r m.   _mappend (Do m) r       = Do (fmap (\x -> _mappend x r) m);

    "_transform f (Do m k)"   forall f g m. _transform f g (Do m)     = g m;
    "_transform f (Lift c)"   forall f g c. _transform f g (Lift c)   = Lift (fmap (_transform f g) c);
    "_transform f (Return r)" forall f g r. _transform f g (Return r) = Return (f r);
  #-}

{- for reference
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

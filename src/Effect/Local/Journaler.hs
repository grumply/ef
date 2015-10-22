module Effect.Local.Journaler
    ( Tracer, tracer
    , Journaler, journaler
    , Journal, log, eavesdrop, reconfigure, intercept
    ) where

import Mop

import Data.Monoid

import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

import Prelude hiding (log)

data Tracer k
    = FreshScope (Integer -> k)
    | forall a. Log Integer a
    | forall fs m a w. Eavesdrop Integer w (Plan fs m a)
    | forall fs m w. Reconfigure Integer (w -> w -> w)

data Journal fs m w = Journal
    { log :: w -> Plan fs m ()
    , eavesdrop :: forall a. w -> Plan fs m a -> Plan fs m (w,a)
    , reconfigure :: (w -> w -> w) -> Plan fs m ()
    }

intercept :: Monad m => Journal fs m w -> (w -> b) -> w -> Plan fs m a -> Plan fs m (b,a)
intercept Journal{..} f w m = do
    ~(w, a) <- eavesdrop w m
    return (f w,a)

tracer :: forall fs m w r. (Has Tracer fs m,Monoid w)
       => (w -> w -> w) -> w -> (Journal fs m w -> Plan fs m r) -> Plan fs m (w,r)
tracer c w f = do
    scope <- symbol (FreshScope id)
    transform scope c w $ f Journal
        { log = \w -> symbol (Log scope w)
        , eavesdrop = \w' p -> symbol (Eavesdrop scope w' p)
        , reconfigure = \c' -> symbol (Reconfigure scope c')
        }
  where
    transform scope = go where
        go c = go' where
            go' w = go'' where
                go'' p = case p of
                    Step sym bp -> case prj sym of
                        Just x  -> case x of
                            Log i w' ->
                                if i == scope
                                then go' (c w (unsafeCoerce w'))
                                         (bp (unsafeCoerce ()))
                                else Step sym (\b -> go'' (bp b))
                            Eavesdrop i w' p ->
                                if i == scope
                                then do
                                  ~(w'',a) <- go' (unsafeCoerce w') (unsafeCoerce p)
                                  go' (c w w'') (bp (unsafeCoerce (w'',a)))
                                else Step sym (\b -> go'' (bp b))
                            Reconfigure i c' ->
                                if i == scope
                                then go (unsafeCoerce c') (unsafeCoerce w) (bp (unsafeCoerce ()))
                                else Step sym (\b -> go'' (bp b))
                            _ -> Step sym (\b -> go'' (bp b))
                        _ -> Step sym (\b -> go'' (bp b))
                    M m -> M (fmap go'' m)
                    Pure r -> Pure (w,r)

data Journaler k = Journaler (IORef Integer) k

journaler :: Uses Journaler fs m => Attribute Journaler fs m
journaler = Journaler (unsafePerformIO $ newIORef 0) $ \fs ->
    let Journaler i k = (fs&)
        x = unsafePerformIO (modifyIORef i succ)
    in x `seq` return fs

journalMisuse :: String -> a
journalMisuse method = error $
  "Journal misuse: " ++ method ++ " used outside of its 'writer' block. \
  \Do not return a Log or its internal fields from its instantiation block."

instance Pair Journaler Tracer where
    pair p (Journaler i k) (FreshScope ik) =
        let n = unsafePerformIO (readIORef i)
        in n `seq` p k (ik n)
    pair p _ (Log _ _) = journalMisuse "Effect.Local.Tracer.Lazy.Log"
    pair p _ (Eavesdrop _ _ _) = journalMisuse "Effect.Local.Tracer.Lazy.Eavesdrop"
    pair p _ (Reconfigure _ _) = journalMisuse "Effect.Local.Tracer.Lazy.Reconfigure"

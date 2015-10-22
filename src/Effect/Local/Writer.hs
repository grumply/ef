{- | This module implements a simple scoped Writer interface in the API style of
     mtl's Control.Monad.Writer.
-}
module Effect.Local.Writer
   ( Writer, writer
   , Logger, logger
   , Log, tell, listen, listens
   ) where

import Mop

import Data.Monoid

import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

data Writer k
    = FreshScope (Integer -> k)
    | forall a. Tell Integer a
    | forall fs m a w. Listen Integer (Plan fs m a)

data Log fs m w = Log
    { tell :: w -> Plan fs m ()
    , listen :: forall a. Plan fs m a -> Plan fs m (w,a)
    }

{-# INLINE listens #-}
listens :: Monad m => Log fs m w -> (w -> b) -> Plan fs m a -> Plan fs m (b,a)
listens Log{..} f m = do
    ~(w, a) <- listen m
    return (f w,a)

{-# INLINE writer #-}
writer :: forall fs m w r. (Has Writer fs m,Monoid w)
       => (Log fs m w -> Plan fs m r) -> Plan fs m (w,r)
writer f = do
    scope <- symbol (FreshScope id)
    transform scope mempty $ f Log
        { tell = \w -> symbol (Tell scope w)
        , listen = \p -> symbol (Listen scope p)
        }
  where
    transform scope = go where
        go w = go' where
            go' p = case p of
                Step sym bp -> case prj sym of
                    Just x  -> case x of
                        Tell i w' ->
                            if i == scope
                            then go (w <> (unsafeCoerce w')) (bp (unsafeCoerce ()))
                            else Step sym (\b -> go' (bp b))
                        Listen i p ->
                            if i == scope
                            then do
                              ~(w',a) <- go mempty (unsafeCoerce p)
                              go (w <> w') (bp (unsafeCoerce (w',a)))
                            else Step sym (\b -> go' (bp b))
                        _ -> Step sym (\b -> go' (bp b))
                    _ -> Step sym (\b -> go' (bp b))
                M m -> M (fmap go' m)
                Pure r -> Pure (w,r)

data Logger k = Logger (IORef Integer) k

{-# INLINE logger #-}
logger :: Uses Logger fs m => Attribute Logger fs m
logger = Logger (unsafePerformIO $ newIORef 0) $ \fs ->
    let Logger i k = (fs&)
        x = unsafePerformIO (modifyIORef i succ)
    in x `seq` return fs

logMisuse :: String -> a
logMisuse method = error $
  "Log misuse: " ++ method ++ " used outside of its 'writer' block. \
  \Do not return a Log or its internal fields from its instantiation block."

instance Pair Logger Writer where
    pair p (Logger i k) (FreshScope ik) =
        let n = unsafePerformIO (readIORef i)
        in n `seq` p k (ik n)
    pair p _ (Tell _ _) = logMisuse "Effect.Local.Writer.Strict.Tell"
    pair p _ (Listen _ _) = logMisuse "Effect.Local.Writer.Strict.Listen"

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Effect.Exception where

import Mop

data Throw e k = Throw e
data Handle e k = Handle (e -> k)

data Catch e k' k = Catch (e -> k') k
data HandleInjector e k' k = HandleInjector ((e -> k') -> k)

-- throw :: Has (Throw e) syms m => e -> Plan syms m a
throw e = symbol (Throw e)

-- catch :: Has (Catch e k) syms m => (e -> k) -> Plan syms m ()
catch ek = symbol (Catch ek ())

-- handle :: Show e => Instruction (Handle e) instrs syms m a
handle = Handle (error . ("Uncaught exception: " ++) . show)

-- messy type
handler f = HandleInjector (\ep -> modI (push (Handle (\e -> modP ((f $ ep e) >>)))))

instance Pair (Handle e) (Throw e) where
  pair p (Handle ekk) (Throw e) = p (ekk e) undefined

instance Pair (HandleInjector e k) (Catch e k) where
  pair p (HandleInjector hkk) (Catch h k) = p (hkk h) k

--------------------------------------------------------------------------------

type App e m a = '[Throw e,Catch e (Excepted e m a)]
type Ctx e m a = '[Handle e,HandleInjector e (Excepted e m a)]
newtype Excepted e m a = Excepted { getExcepted :: Plan (App e m a) m a }

data Bad = Bad deriving Show

-- plan :: String -> Plan (App Bad IO ()) IO ()
plan nm0 = do
  catch (\Bad -> Excepted $ return () :: Excepted Bad IO ())
  nm <- lift (putStr "Name: " *> getLine)
  if nm /= nm0
  then lift (putStrLn "Bad!") >> throw Bad
  else return ()
  lift (print "Test")

comp :: Instructions (Ctx Bad IO ()) (App Bad IO ()) IO ()
comp =
  let hndl = handle
      hndlr = handler getExcepted
  in Instructions $ hndl *:* hndlr *:* Empty

main = do
  delta comp (plan "Sean")

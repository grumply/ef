{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Effect.Exception where

import Mop

data Throw e k = Throw e
data Handle e k = Handle (e -> k)

data Catch e k' k = Catch (e -> k') k
data HandleInjector e k' k = HandleInjector ((e -> k') -> k)

throw :: Has (Throw e) syms m
      => e -> Plan syms m a
throw e = symbol (Throw e)

catch :: Has (Catch e (Excepted e m a)) (App e m a) m
      => (e -> Excepted e m a) -> Plan (App e m a) m ()
catch ek = symbol (Catch ek ())

handle :: (Show e,Applicative m) => Instruction (Handle e) instrs syms m a
handle = Handle (error . ("Uncaught exception: " ++) . show)

handler :: (Uses (Handle e) (Ctx e m a) (App e m a) m,Applicative m)
        => Instruction (HandleInjector e (Excepted e m a)) (Ctx e m a) (App e m a) m a
handler = HandleInjector (\ep -> modI (push (Handle (\e -> modP ((getExcepted $ ep e) >>)))))

instance Pair (Handle e) (Throw e) where
  pair p (Handle ekk) (Throw e) = p (ekk e) undefined

instance Pair (HandleInjector e (Excepted e m a)) (Catch e (Excepted e m a)) where
  pair p (HandleInjector hkk) (Catch h k) = p (hkk h) k

type App e m a = '[Throw e,Catch e (Excepted e m a)]
type Ctx e m a = '[Handle e,HandleInjector e (Excepted e m a)]
newtype Excepted e m a = Excepted { getExcepted :: Plan (App e m a) m a }

data Bad = Bad deriving Show

-- plan :: String -> Plan (App Bad IO ()) IO ()
plan nm0 = do
  catch (\Bad -> Excepted $ return ())
  nm <- lift (putStr "Name: " *> getLine)
  if nm /= nm0
  then lift (putStrLn "Bad!") >> throw Bad
  else return ()
  lift (print "Test")

comp :: Instructions (Ctx Bad IO ()) (App Bad IO ()) IO ()
comp =
  let hndl = handle   :: Instruction (Handle Bad) (Ctx Bad IO ()) (App Bad IO ()) IO ()
      hndlr = handler :: Instruction (HandleInjector Bad (Excepted Bad IO ()))
                                     (Ctx Bad IO ())
                                     (App Bad IO ())
                                     IO
                                     ()
  in Instructions $ push hndl $ push hndlr $ unsafeBuild'

main = do
  delta comp (plan "Sean")

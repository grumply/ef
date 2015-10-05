module Effect.Exception where

import Mop

data Throw e k' k = Throw e
data Handle e k' k = Handle (e -> k') k -- rather than simply (e -> k) ?

data Catch e k' k = Catch (e -> k') k
data HandleInjector e k' k = HandleInjector ((e -> k') -> k)

throw :: forall e fs m a . Has (Throw e (Plan fs m a)) fs m => e -> Plan fs m a
throw e = symbol (Throw e :: Throw e (Plan fs m a) a)

catch :: Has (Catch e (Plan fs m a)) fs m => (e -> Plan fs m a) -> Plan fs m a -> Plan fs m a
catch ek k = do
  symbol (Catch ek ())
  k

handle :: (Show e,Applicative m) => Handle e (Plan fs m a) (Transformation fs gs m a)
handle = Handle (error . ("Uncaught exception: " ++) . show) pure

handler :: Uses (Handle e (Plan fs m a)) gs fs m
        => Instruction (HandleInjector e (Plan fs m a)) gs fs m a
handler = HandleInjector (\ep -> modI (push (Handle (\e -> modP (const (ep e))))))

instance Pair (Handle e k) (Throw e k) where
  pair p (Handle ek k) (Throw e) = p (ek e) _

instance Pair (HandleInjector e k) (Catch e k) where
  pair p (HandleInjector hkk) (Catch h k) = p (hkk h) k

-- It seems like every time I want to implement control flow,
-- it requires fully encapsulating the program type.
-- Why is this fundamental?


{-
Because, with stacks of monadic effects, the control flow affects everything
below the control flow effect layer. Similarly, since every effect is on the
same level in this approach, we must fully encapsulate the entire program type....
-}

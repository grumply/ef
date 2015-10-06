module Effect.Exception
  (throw,catch,throwHandler,Throw(..),ThrowHandler(..),Exception(..),SomeException(..))
  where

import Mop
import Control.Exception hiding (throw,catch)

data Throw k = Throw SomeException
data ThrowHandler k = ThrowHandler k

-- abstract over this pattern-matching pattern, it seems common and useful.
scrub :: (Has Throw syms m,Exception e) => (e -> Plan syms m a) -> Plan syms m a -> Plan syms m a
scrub sub p0 = go p0
  where
    go p0 =
      case p0 of
        Step syms bp ->
          case prj syms of
            Just (Throw se) ->
              case fromException se of
                Just x -> sub x
                Nothing -> Step syms (\b -> go (bp b))
                -- ^ Look closely here.
            Nothing -> Step syms (\b -> go (bp b))
        M mp -> M (fmap go mp)
        Pure r -> Pure r

catch :: (Has Throw syms m,Exception e) => (e -> Plan syms m a) -> Plan syms m a -> Plan syms m a
catch = scrub

throw e = symbol (Throw e)

throwHandler :: Applicative m => Instruction ThrowHandler instrs syms m a
throwHandler = ThrowHandler pure

instance Pair ThrowHandler Throw where
  pair p (ThrowHandler k) (Throw e) = p k (error $ "Uncaught exception: " ++ show e)

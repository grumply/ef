module Effect.Exception
  (throw,catch,throwHandler,Throw(..),ThrowHandler(..),Exception(..),SomeException(..))
  where

import Mop
import Control.Exception hiding (throw,catch)

data Throw k = Throw SomeException
data ThrowHandler k = ThrowHandler k

scrub :: (Has Throw syms m,Exception e) => (e -> Plan syms m a) -> Plan syms m a -> Plan syms m a
scrub sub = go
  where
    go p =
      case p of
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
catch = scrub'

throw e = symbol (Throw $ toException e)

throwHandler :: Monad m => Instruction ThrowHandler instrs syms m a
throwHandler = ThrowHandler return

instance Pair ThrowHandler Throw where
  pair p (ThrowHandler k) (Throw e) = p k (error $ "Uncaught exception: " ++ show e)

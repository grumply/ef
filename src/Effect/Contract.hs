{-# LANGUAGE CPP #-}
module Effect.Contract
   ( contract
   , analyze
   ) where
-- Note: does not export Contract; thus only catchable by catching SomeException

import Mop.Core
import Effect.Exception
import Effect.Divergence

import Control.Exception (Exception(..))
import Control.Monad (unless)
import Data.Typeable

data Contract = Contract String deriving Show
instance Exception Contract

-- | instrument a contract that guarantees pre- and post- conditions
--   of a given method. Failure of a condition causes an asynchronous
--   exception to be thrown.
-- @
--   contract (preFailInfo,precondition)
--            (postFailInfo,postcondition) $ \vars -> do
--       someMethod/Messages
--     where
--       precondition vars = _
--       postcondition a = _
-- @
contract :: (Has Throw fs m,Has Diverge fs m,Pair (Attrs gs) (Symbol fs),Typeable gs,Typeable m)
         => (    String
            ,    vars
              -> Plan fs m Bool
            )           -- ^ precondition
         -> (    String
            ,    a
              -> Plan fs m Bool
            )           -- ^ postcondition
         -> (    vars
              -> Plan fs m a
            )           -- ^ method
         -> vars
         -> Plan fs m a
contract (pre,precondition) (post,postcondition) method vs =
#ifndef NO_CONTRACTS
  flip catch c $ do
    preconditionResult <- precondition vs
    unless preconditionResult $ do
      ty <- typeOfSelf
      throw (Contract ("contract: pre-condition failed: " ++ pre ++ "\n\tin: " ++ show ty))
#else
  do
#endif
    a <- method vs
#ifndef NO_CONTRACTS
    postconditionResult <- postcondition a
    unless postconditionResult $ do
      ty <- typeOfSelf
      throw (Contract ("contract: post-condition failed: " ++ post ++ "\n\tin: " ++ show ty))
#endif
    return a
#ifndef NO_CONTRACTS
  where
    c (Contract str) = error str
#endif


-- | analyze permits slightly stronger analyses than 'contract' by including
--   object introspection. Failure of a condition causes an asynchronous
--   exception to be thrown.
-- @
--   analyze (preFailInfo,precondition)
--           (postFailInfo,postcondition) $ \vars -> do
--       someMethod/Messages
--     where
--       precondition vars obj = _
--       postcondition a obj = _
-- @
analyze :: (Has Throw fs m,Has Diverge fs m,Pair (Attrs gs) (Symbol fs),Typeable gs,Typeable m)
        => (    String
           ,    vars
             -> Object gs m
             -> Plan fs m Bool
           )
        -> (    String
           ,    a
             -> Object gs m
             -> Plan fs m Bool
           )
        -> (    vars
             -> Plan fs m a
           )
        -> vars
        -> Plan fs m a
analyze (pre,precondition) (post,postcondition) method vs =
#ifndef NO_CONTRACTS
  flip catch c $ do
    preconditionResult <- modself $ \slf -> do
      pr <- precondition vs slf
      return (slf,pr)
    unless preconditionResult $ do
      ty <- typeOfSelf
      throw (Contract ("analyze: pre-condition failed: " ++ pre ++ "\n\tin: " ++ show ty))
#else
  do
#endif
    a <- method vs
#ifndef NO_CONTRACTS
    postconditionResult <- modself $ \slf -> do
      pr <- postcondition a slf
      return (slf,pr)
    unless postconditionResult $ do
      ty <- typeOfSelf
      throw (Contract ("contract: post-condition failed: " ++ post ++ "\n\tin: " ++ show ty))
#endif
    return a
#ifndef NO_CONTRACTS
  where
    c (Contract str) = error str
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.Lang.Contract
   ( contract
   , analyze
   ) where
-- Note: does not export Contract; thus only catchable by catching SomeException

import Ef.Core
import Ef.Lang.Except
import Ef.Lang.Scoped.Diverge

import Control.Monad (unless)
import Data.Typeable

data Contract = Contract String deriving Show
instance Exception Contract

{-# INLINE contract #-}
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
contract :: (Is Excepting fs m,Is Diverging fs m,Symmetry (Attrs gs) (Symbol fs),Typeable gs,Typeable m)
         => (    String
            ,    vars
              -> Pattern fs m Bool
            )           -- ^ precondition
         -> (    String
            ,    a
              -> Pattern fs m Bool
            )           -- ^ postcondition
         -> (    vars
              -> Pattern fs m a
            )           -- ^ method
         -> vars
         -> Pattern fs m a
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


{-# INLINE analyze #-}
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
analyze :: (Is Excepting fs m,Is Diverging fs m,Symmetry (Attrs gs) (Symbol fs),Typeable gs,Typeable m)
        => (    String
           ,    vars
             -> Object gs m
             -> Pattern fs m Bool
           )
        -> (    String
           ,    a
             -> Object gs m
             -> Pattern fs m Bool
           )
        -> (    vars
             -> Pattern fs m a
           )
        -> vars
        -> Pattern fs m a
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

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-inline-rule-shadowing  #-}
module Ef
    ( module Core
    , delta
    , ($.)
    , module Control.Monad.Catch
    , module Control.Monad.IO.Class
    , module Control.Monad.Trans.Class
    , module Control.Monad.Fix
    , Interpreter(..)
    , interp
    , runInterpreter
    ) where

import Ef.Type.Set as Core (type (/==),Union)

import Ef.Type.Nat as Core (Offset)

import Ef.Object as Core
import Ef.Traits as Core

import Ef.Narrative as Core
import Ef.Messages as Core

import Ef.Machine as Core

import Ef.Codensity as Core

import Ef.Ma as Core

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Coerce

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.State (MonadState(..))
import Control.Monad.Reader (MonadReader(..))

-- | Send a narrative to an object for invocation; returns a new, modified object
-- and a result.
--
-- >    (resultObj,result) <- delta obj narrative
delta
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super, MonadThrow super
       )
    => Object traits super
    -> Narrative messages super result
    -> super (Object traits super,result)
delta = _delta
{-# INLINE [2] delta #-}


infixr 5 $.
-- | Synonym for 'delta'. Send a narrative to an object.
--
-- >    (resultObj,result) <- obj $. narrative
($.)
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super, MonadThrow super
       )
    => Object traits super
    -> Narrative messages super result
    -> super (Object traits super,result)
($.) = _delta
{-# INLINE [2] ($.) #-}



_delta
    :: ( (Traits traits) `Ma` (Messages messages)
       , Monad super, MonadThrow super
       )
    => Object traits super
    -> Narrative messages super result
    -> super (Object traits super,result)
_delta object = go
  where
    go (Say symbol k) =
        let ~(method,b) = ma (,) (coerce object) symbol
        in do object' <- method object
              _delta object' (k b)

    go (Fail e) =
        throwM e

    go (Super m) = m >>= go

    go (Return result) = pure (object,result)
{-# NOINLINE [2] _delta #-}


{-# RULES

    "_delta obj (Fail e)"
        forall obj e.
            _delta obj (Fail e) =
                throwM e

    ;

    "_delta obj (Super m)"
        forall obj m.
            _delta obj (Super m) =
                m >>= _delta obj

    ;

    "_delta obj (Return result)"
        forall obj result.
            _delta obj (Return result) =
                return (obj,result)

    ;

    "_delta obj (Say symbol k)"
        forall obj symbol k.
            _delta obj (Say symbol k) =
                let
                    ~(method,b) =
                        ma (,) (coerce obj) symbol

                in
                    do
                        object' <- method obj
                        _delta object' (k b)

  #-}

-- Partial saturation of delta can be used to create a threaded interpreter for
-- use with MonadFix. 'runTest' works as expected, returning 'b'!
--
-- > test :: Interpreter '[State Char] IO Char
-- > test = do
-- >   rec b <- return (succ a)
-- >       a <- interp get
-- >   return b
-- >
-- > runTest = runInterpreter (Object (state 'a' *:* Empty)) test
--
-- What's lost in the lifting to Interpreter is the ability to transform
-- a computation as it is running as well as many of the embedding classes
-- like MonadReader and MonadCatch. And it still lacks the ability to run
-- value-recursion inside another Narrative, i.e. super ~ Narrative ...
--
-- As a further example, note that this slight modification /does not work/
-- because the value of b can only be witnessed outside of the value recursion
-- block, just like in IO.
--
-- > test :: Interpreter '[State Char] IO Char
-- > test = do
-- >  rec b <- return $ succ a
-- >      a <- interp get
-- >      liftIO $ print a
-- >      liftIO $ print b -- indentation change here
-- > return b
-- >
-- > runTest = runInterpreter (Object (state 'a' *:* Empty)) test
--
-- So, ultimately, and as with IO, value recursion with Ef is most useful for
-- tying knots, i.e. recursive references.

foldInterpreter :: (MonadFix super, Ma (Traits traits) (Messages self))
       => (forall x. Narrative self super x -> super (Object traits super,x))
       -> Interpreter self super a
       -> super (Object traits super,a)
foldInterpreter k e = interpret e k

data Interpreter self super a = Interpreter
  { interpret :: forall traits.
                  (MonadFix super, Ma (Traits traits) (Messages self))
              => (forall x. Narrative self super x -> super (Object traits super,x))
              -> super (Object traits super,a)
  }

instance Functor (Interpreter self super) where
  fmap f (Interpreter k) = Interpreter $ \k' -> fmap (fmap f) (k k')

instance (MonadThrow super) => Applicative (Interpreter self super) where
  pure = return
  (<*>) = ap

instance (MonadThrow super) => Monad (Interpreter self super) where
  return a = Interpreter $ \k -> k (return a)
  (Interpreter k) >>= f = Interpreter $ \d -> do
    (o,a) <- k d
    let (Interpreter k') = f a
    interpret (f a) (delta o)

instance (MonadThrow super, MonadPlus super)
  => MonadPlus (Interpreter self super)
  where
    mzero = Interpreter $ \k -> k (super mzero)
    mplus (Interpreter p0) (Interpreter p1) = Interpreter $ \k ->
      (p0 k) `mplus` (p1 k)

instance (MonadThrow super, MonadPlus super)
  => Alternative (Interpreter self super)
  where
    empty = mzero
    (<|>) = mplus

instance (MonadThrow super)
  => MonadFix (Interpreter self super)
  where
    mfix f = Interpreter $ \d -> mfix (foldInterpreter d . f . snd)

instance (MonadThrow super, MonadIO super)
  => MonadIO (Interpreter self super)
  where
    liftIO ioa = Interpreter $ \k -> k (liftIO ioa)

instance MonadTrans (Interpreter self) where
  lift m = Interpreter $ \k -> k (lift m)

interp :: Narrative self super a -> Interpreter self super a
interp n = Interpreter $ \k -> k n

runInterpreter :: ( MonadFix super
                  , MonadThrow super
                  , Ma (Traits traits) (Messages self)
                  )
                => Object traits super
                -> Interpreter self super a
                -> super (Object traits super,a)
runInterpreter o (Interpreter k) = k (delta o)

-- Interestingly, it seems impossible to implement MFunctor since the object
-- being held inside a call to `delta` in the interpreter maintains a reference
-- to super that cannot be transformed since access to the structure of the object
-- is implicitly limited by the structure and interaction of the methods it
-- composes. If we were to remove the access to super from objects, we would lose
-- the ability to modularize effects at runtime, i.e. you couldn't swap out one
-- method that writes to disk with another that serializes over the wire; they
-- would be forced to be implemented separately and concretely if encapsulation
-- were to be maintained when using objects in a more OOP-style. More seriously,
-- some of the benefits of nested simulation would be lost.

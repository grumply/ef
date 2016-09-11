module Ef.Pipes.Group
  ( Groups, groups
  , takes
  , concats
  , stdinLn, stdoutLn
  ) where

import Ef
import Ef.Pipes

import Unsafe.Coerce

import Control.Monad
import System.IO

import Foreign.C.Error (Errno(Errno),ePIPE)
import GHC.IO.Exception as G

import Debug.Trace

stdoutLn = consumer go
  where
    go await = go'
      where
        go' = do
          str <- await
          x   <- try $ liftIO $ putStrLn str
          case x of
            Left (G.IOError { G.ioe_type  = G.ResourceVanished
                           , G.ioe_errno = Just ioe })
                | Errno ioe == ePIPE
                    -> return ()
            Left e  -> throwM e
            Right () -> go'

data Delimited k
  = Delimiter

newtype Groups a super x =
  Groups
    { getGroups :: Narrative '[Pipes,Delimited] super x
    }

groups :: forall a m r.
          (Monad m, Eq a)
       => Producer a m r -> Groups a m r
groups = Groups . start . runProxy
  where
    start :: Narrative '[Pipes] m r -> Narrative '[Pipes,Delimited] m r
    start p =
      case p of
        Return r -> Return r
        Fail e -> Fail e
        Super sup -> Super (fmap start sup)
        Say msg k ->
          case prj msg of
            Just (Respond b f) ->
              Say (inj (Respond b f)) (go (unsafeCoerce b) . k)

    go :: a -> Narrative '[Pipes] m r -> Narrative '[Pipes,Delimited] m r
    go b = go'
      where
        go' p =
          case p of
            Return r -> Return r
            Fail e -> Fail e
            Super sup -> Super (fmap (go b) sup)
            Say msg k ->
              case prj msg of
                Just (Respond b' f) ->
                  if b == (unsafeCoerce b' :: a)
                  then Say (inj (Respond b' f)) (go' . k)
                  else do
                    self Delimiter
                    Say (inj (Respond b' f)) (go (unsafeCoerce b') . k)

takes :: forall a m r.
         (Monad m)
      => Int -> Groups a m () -> Groups a m ()
takes i = Groups . go i . getGroups
  where
    go 0 _ = Return ()
    go n (Return r) = Return r
    go n (Fail e) = Fail e
    go n (Super sup) = Super (fmap (go n) sup)
    go n (Say msg k) =
      case prj msg of
        Just Delimiter -> Say msg (go (n - 1) . k)
        _ -> Say msg (go n . k)

concats :: forall a m r.
           (Monad m)
        => Groups a m r -> Producer a m r
concats = Proxy . go . getGroups
  where
    go :: Narrative '[Pipes,Delimited] m r
       -> Narrative '[Pipes] m r
    go p =
      case p of
        Return r -> Return r
        Fail e -> Fail e
        Super sup -> Super (fmap go sup)
        Say msg k ->
          case prj msg of
            Just Delimiter ->
              go $ k (unsafeCoerce ())
            _ ->
              case prj msg of
                Just (Respond b' f) ->
                  Say (inj (Respond b' f)) (go . k)

stdinLn = producer go
  where
    go yield = go'
      where
        go' = do
          eof <- liftIO isEOF
          unless eof $ do
            str <- liftIO getLine
            yield str
            go'

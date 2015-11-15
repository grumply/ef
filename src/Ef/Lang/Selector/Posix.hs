{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Lang.Selector.Posix where

import Ef.Core
import Ef.Lang.Except.Checked

import Data.Functor.Identity

{-
NOTES:

This is an attempt at an encoding of POSIX fully portable filenames.
For Linux files, use Ef.Lang.Selector.Unix. This is likely NOT what
you want to use on Linux, as this module enforces a file name limit
of 14, for example, to comply with the POSIX standard.

-}

data Rooting = Absolute | Relative
data Visibility = Visible | Hidden

newtype PosixPath rt = PosixPath (Rooting,Visibility,Pattern '[Selecting] Identity ())

isAbs :: PosixPath rt -> Bool
isAbs (PosixPath (Absolute,_,_)) = True
isAbs _ = False

isRel :: PosixPath rt -> Bool
isRel (PosixPath (Relative,_,_)) = True
isRel _ = False

isHidden :: PosixPath rt -> Bool
isHidden (PosixPath (_,Hidden,_)) = True
isHidden _ = False

isVisible :: PosixPath rt -> Bool
isVisible (PosixPath (_,Visible,_)) = True
isVisible _ = False

leastVisible :: Visibility -> Visibility -> Visibility
leastVisible Hidden _ = Hidden
leastVisible _ Hidden = Hidden
leastVisible _ _ = Visible

(</>) :: PosixPath rt -> PosixPath Relative -> PosixPath rt
(</>) (PosixPath (rtng,visl,pthl)) (PosixPath (_,visr,pthr)) = PosixPath (rtng,leastVisible visl visr,pthl >> pthr)

toString :: PosixPath rt -> String
toString (PosixPath (rtng,vis,pth)) = undefined

type AbsolutePosixPath = PosixPath Absolute
type RelativePosixPath = PosixPath Relative

type DirName = String
type FileName = String
type Extension = String

data Selecting k
  = Up k
  | Current k
  | Dir Visibility DirName k
  | File Visibility FileName Extension k
  | View

data Selectable k = Selectable k

data RelativePosixSelector fs m = RelativePosixSelector
  { up         :: Pattern fs m ()
  , current    :: Pattern fs m ()
  , dir        :: DirName -> Pattern fs m ()
  , hiddenDir  :: DirName -> Pattern fs m ()
  , file       :: FileName -> Extension -> Pattern fs m ()
  , hiddenFile :: FileName -> Extension -> Pattern fs m ()
  , ext        :: Extension -> Pattern fs m ()
  , viewPosixPath   :: Pattern fs m (PosixPath 'Relative)
  }

data InvalidPosixPath
  = InvalidPosixExt
  | InvalidPosixFile
  | InvalidPosixDir
  | InvalidFileInFile
  | InvalidPosixPathLength
  deriving Show
instance Exception InvalidPosixPath

relative
  :: forall fs m a.
     (Monad m,Is Selecting fs m,Is Excepting fs m)
  => (RelativePosixSelector fs m -> Pattern fs m a)
  -> (Throws InvalidPosixPath => Pattern fs m (a,RelativePosixPath))
relative p0 = start (return ()) $ p0 RelativePosixSelector
    { up = self (Up ())
    , current = self (Current ())
    , dir = \d -> self (Dir Visible d ())
    , hiddenDir = \d -> self (Dir Hidden d ())
    , file = \f e -> self (File Visible f e ())
    , hiddenFile = \f e -> self (File Hidden f e ())
    , ext = \e -> self (File Visible "" e ())
    , viewPosixPath = self View
    }
  where
    start :: Pattern '[Selecting] Identity ()
          -> Pattern fs m a
          -> (Throws InvalidPosixPath => Pattern fs m (a,RelativePosixPath))
    start acc = go
      where
        go :: Pattern fs m a
           -> (Throws InvalidPosixPath => Pattern fs m (a,RelativePosixPath))
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Up k -> start (acc >> self (Up ())) (bp k)
                    Current k -> start (acc >> self (Current ())) (bp k)
                    File v f e k  ->
                      if validPosixFile f
                      then if validPosixExt e
                           then infile (acc >> self (File v f e ())) (bp k)
                           else throwChecked InvalidPosixExt
                      else throwChecked InvalidPosixFile
                    Dir v d k ->
                      if validPosixDir d
                      then indir (acc >> self (Dir v d ())) (bp k)
                      else throwChecked InvalidPosixDir
                    View -> go (bp (accToPosixPath acc))
                Nothing -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure (r,accToPosixPath acc)
    indir :: Pattern '[Selecting] Identity ()
          -> Pattern fs m a
          -> (Throws InvalidPosixPath => Pattern fs m (a,RelativePosixPath))
    indir = go
      where
        go acc p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Up k -> go (acc >> self (Up ())) (bp k)
                    Current k -> go (acc >> self (Current ())) (bp k)
                    File v f e k ->
                      if validPosixFile f
                      then if validPosixExt e
                           then infile (acc >> self (File v f e ())) (bp k)
                           else throwChecked InvalidPosixExt
                      else throwChecked InvalidPosixFile
                    Dir v d k ->
                      if validPosixDir d
                      then go (acc >> self (Dir v d ())) (bp k)
                      else throwChecked InvalidPosixDir
                    View -> go acc (bp (accToPosixPath acc))
                Nothing -> Step sym (\b -> go acc (bp b))
            M m -> M (fmap (go acc) m)
            Pure r -> Pure (r,accToPosixPath acc)
    infile :: Pattern '[Selecting] Identity ()
           -> Pattern fs m a
           -> (Throws InvalidPosixPath => Pattern fs m (a,RelativePosixPath))
    infile = go
      where
        go acc p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Up k -> go (acc >> self (Up ())) (bp k)
                    Current k -> go (acc >> self (Current ())) (bp k)
                    File _ "" e k -> go (acc >> self (File Visible "" e ())) (bp k)
                    File _ _ _ _ -> throwChecked InvalidFileInFile
                    Dir v d k ->
                      if validPosixDir d
                      then go (acc >> self (Dir v d ())) (bp k)
                      else throwChecked InvalidPosixDir
                    View -> go acc (bp (accToPosixPath acc))
                Nothing -> Step sym (\b -> go acc (bp b))
            M m -> M (fmap (go acc) m)
            Pure r -> Pure (r,accToPosixPath acc)
    validPosixExt e = True
    accToPosixPath acc = undefined
    validPosixDir d = undefined
    validPosixFile f = undefined

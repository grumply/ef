{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ef.Lang.Path.POSIX.Portable where

import Ef.Core
import Ef.Lang.IO
import Ef.Lang.Except
import Ef.Lang.Except.Checked

import Data.Functor.Identity
import Data.String
import System.Environment

import Data.Coerce

{-
NOTES:

This is an attempt at an encoding of POSIX fully portable filenames.
For Linux files, use Ef.Lang.Selector.Unix. This is likely NOT what
you want to use on Linux, as this module enforces a file name limit
of 14 and a limited file name character set, for example, to comply
with the POSIX standard.

-}


data InvalidPOSIXPath
  = InvalidPOSIXExt
  | InvalidPOSIXFile
  | InvalidPOSIXDir
  | InvalidPOSIXFileInFile
  | InvalidPOSIXDirInFile
  | InvalidPOSIXPathLength Int String
  | InvalidPOSIXPathCharacter Char String
  | InvalidPOSIXPortableFileNameCharacters String
  | InvalidPOSIXPathRooting Rooting
  | InvalidPOSIXPathVisibility Visibility
  deriving Show
instance Exception InvalidPOSIXPath

newtype POSIXPortableFileName = PPFN String
instance IsString POSIXPortableFileName where
  fromString = ppfn_fromString

ppfn_fromString :: String -> POSIXPortableFileName
ppfn_fromString str = snd $ runIdentity $ delta (Object $ excepter *:* Empty) $ do
  let xs = filter (not . (`elem` portablePOSIXFileNameCharset)) str
  case xs of
    [] -> return (PPFN str)
    xs -> throw (InvalidPOSIXPortableFileNameCharacters xs)

unsafe_ppfn_fromString :: String -> POSIXPortableFileName
unsafe_ppfn_fromString = PPFN

portablePOSIXFileNameCharset = lower ++ upper ++ nums ++ misc
  where
    lower = ['a'..'z']
    upper = ['A'..'Z']
    nums = ['0'..'9']
    misc = "._-"

data Rooting = Absolute | Relative deriving Show
data Visibility = Visible | Hidden deriving Show

newtype POSIXPath rt = POSIXPath (Rooting,Visibility,Pattern '[PortablePOSIXFileSelector] Identity ())

instance IsString (POSIXPath rt) where
  fromString = pp_fromString

pp_fromString :: String -> POSIXPath rt
pp_fromString str = undefined

isAbs :: POSIXPath rt -> Bool
isAbs (POSIXPath (Absolute,_,_)) = True
isAbs _ = False

isRel :: POSIXPath rt -> Bool
isRel (POSIXPath (Relative,_,_)) = True
isRel _ = False

isHidden :: POSIXPath Absolute -> Bool
isHidden (POSIXPath (_,Hidden,_)) = True
isHidden _ = False

isVisible :: POSIXPath Absolute -> Bool
isVisible (POSIXPath (_,Visible,_)) = True
isVisible _ = False

isVisiblePathComponent :: POSIXPath rt -> Bool
isVisiblePathComponent (POSIXPath (_,Visible,_)) = True
isVisiblePathComponent _ = False

isHiddenPathComponent :: POSIXPath rt -> Bool
isHiddenPathComponent (POSIXPath (_,Hidden,_)) = True
isHiddenPathComponent _ = False

leastVisible :: Visibility -> Visibility -> Visibility
leastVisible Hidden _ = Hidden
leastVisible _ Hidden = Hidden
leastVisible _ _ = Visible

(</>) :: POSIXPath rt -> POSIXPath Relative -> POSIXPath rt
(</>) (POSIXPath (rtng,visl,pthl)) (POSIXPath (_,visr,pthr)) = POSIXPath (rtng,leastVisible visl visr,pthl >> pthr)

makeAbsolute :: POSIXPath Absolute -> POSIXPath Relative -> POSIXPath Absolute
makeAbsolute = (</>)

guaranteeAbsolute :: Is Excepting fs m
                  => POSIXPath rt
                  -> (Throws InvalidPOSIXPath => Pattern fs m (POSIXPath Absolute))
guaranteeAbsolute pp@(POSIXPath (Absolute,_,_)) = return $ coerce pp
guaranteeAbsolute _ = throwChecked (InvalidPOSIXPathRooting Relative)

guaranteeRelative :: Is Excepting fs m
                 => POSIXPath rt
                 -> (Throws InvalidPOSIXPath => Pattern fs m (POSIXPath Relative))
guaranteeRelative pp@(POSIXPath (Relative,_,_)) = return $ coerce pp
guaranteeRelative _ = throwChecked (InvalidPOSIXPathRooting Absolute)

reify :: Lift IO m => POSIXPath Relative -> Pattern fs m (POSIXPath Absolute)
reify pp = do
  cd <- io (getEnv "PWD")
  let d = pp_fromString cd
  return (makeAbsolute d pp)

toString :: POSIXPath rt -> String
toString (POSIXPath (rtng,vis,pth)) = undefined

type AbsolutePOSIXPath = POSIXPath Absolute
type RelativePOSIXPath = POSIXPath Relative

type DirName = String
type FileName = String
type Extension = String

data PortablePOSIXFileSelector k
  = Up k
  | Current k
  | Dir Visibility DirName k
  | File Visibility FileName Extension k
  | View

data POSIXSelectable k = POSIXSelectable k

data RelativePOSIXSelector fs m = RelativePOSIXSelector
  { up         :: Pattern fs m ()
  , current    :: Pattern fs m ()
  , dir        :: DirName -> Pattern fs m ()
  , hiddenDir  :: DirName -> Pattern fs m ()
  , file       :: FileName -> Extension -> Pattern fs m ()
  , hiddenFile :: FileName -> Extension -> Pattern fs m ()
  , ext        :: Extension -> Pattern fs m ()
  , viewPath   :: Pattern fs m (POSIXPath 'Relative)
  }


data Validation = Ok | Err InvalidPOSIXPath

relative
  :: forall fs m a.
     (Monad m,Is PortablePOSIXFileSelector fs m,Is Excepting fs m)
  => (RelativePOSIXSelector fs m -> Pattern fs m a)
  -> (Throws InvalidPOSIXPath => Pattern fs m (a,RelativePOSIXPath))
relative p0 = start (return ()) $ p0 RelativePOSIXSelector
    { up = self (Up ())
    , current = self (Current ())
    , dir = \d -> self (Dir Visible d ())
    , hiddenDir = \d -> self (Dir Hidden d ())
    , file = \f e -> self (File Visible f e ())
    , hiddenFile = \f e -> self (File Hidden f e ())
    , ext = \e -> self (File Visible "" e ())
    , viewPath = self View
    }
  where
    start :: Pattern '[PortablePOSIXFileSelector] Identity ()
          -> Pattern fs m a
          -> (Throws InvalidPOSIXPath => Pattern fs m (a,RelativePOSIXPath))
    start acc = go
      where
        go :: Pattern fs m a
           -> (Throws InvalidPOSIXPath => Pattern fs m (a,RelativePOSIXPath))
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Up k -> start (acc >> self (Up ())) (bp k)
                    Current k -> start (acc >> self (Current ())) (bp k)
                    File v f e k  ->
                      case validPOSIXFile f e of
                        Ok -> infile (acc >> self (File v f e ())) (bp k)
                        Err e -> throwChecked e
                    Dir v d k ->
                      case validPOSIXDir d of
                        Ok -> indir (acc >> self (Dir v d ())) (bp k)
                        Err e -> throwChecked e
                    View -> go (bp (accToPOSIXPath acc))
                Nothing -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure (r,accToPOSIXPath acc)
    indir :: Pattern '[PortablePOSIXFileSelector] Identity ()
          -> Pattern fs m a
          -> (Throws InvalidPOSIXPath => Pattern fs m (a,RelativePOSIXPath))
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
                      case validPOSIXFile f e of
                        Ok -> infile (acc >> self (File v f e ())) (bp k)
                        Err e -> throwChecked e
                    Dir v d k ->
                      case validPOSIXDir d of
                        Ok -> go (acc >> self (Dir v d ())) (bp k)
                        Err e -> throwChecked e
                    View -> go acc (bp (accToPOSIXPath acc))
                Nothing -> Step sym (\b -> go acc (bp b))
            M m -> M (fmap (go acc) m)
            Pure r -> Pure (r,accToPOSIXPath acc)
    infile :: Pattern '[PortablePOSIXFileSelector] Identity ()
           -> Pattern fs m a
           -> (Throws InvalidPOSIXPath => Pattern fs m (a,RelativePOSIXPath))
    infile = go
      where
        go acc p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    File _ "" e k -> go (acc >> self (File Visible "" e ())) (bp k)
                    File _ _ _ _ -> throwChecked InvalidPOSIXFileInFile
                    View -> go acc (bp (accToPOSIXPath acc))
                    _ -> throw InvalidPOSIXDirInFile
                Nothing -> Step sym (\b -> go acc (bp b))
            M m -> M (fmap (go acc) m)
            Pure r -> Pure (r,accToPOSIXPath acc)
    accToPOSIXPath acc = undefined
    validPOSIXDir d = undefined
    validPOSIXFile acc f e = let l = length f + length e in
      if l > 14
      then Err (InvalidPOSIXPathLength l
                  (toString accToPOSIXPath ++ "/" ++ f ++ "." ++ e)
               )
      else if not (all (`elem` portablePOSIXFileNameCharset) (f ++ e))
           then Err (InvalidPOSIXPathCharacter
                       (head (filter (not . (`elem` portablePOSIXFileNameCharset)) (f ++ e))
                       (toString (accToPOSIXPath acc) ++ "/" + f ++ "." ++ e))
                    )
           else case f of
                  '-':_ -> Err (InvalidPOSIXPathCharacter
                                  '-' (toString (accToPOSIXPath acc))
                               )
                  _ -> Ok

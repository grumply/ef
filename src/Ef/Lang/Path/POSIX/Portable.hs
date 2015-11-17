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

Names got a little long, so I shortened PortablePOSIXPath to PPP

This is an attempt at an encoding of POSIX fully portable filenames. For Linux
files, use Ef.Lang.Selector.Unix. This is likely NOT what you want to use on
Linux, as this module enforces a file name limit of 14 and a limited file name
character set, for example, to comply with the POSIX standard.

Invariants supported:

Maximum segment length: 14
Character set limitation: [a-zA-Z0-9.-_]
File and Directories do not start with: '-'
</> takes Rel or Abs on the left and only Rel on the right

-}

--------------------------------------------------------------------------------
-- PPP Errors

-- | PPPErr represents any invalid construct in this module. A previous approach
-- that split errors based on error type was scrapped because, while it was nice
-- to have a range of exceptions, they were not recoverable in practice.
data PPPErr
  = PPPErr PPP String
  deriving Show
instance Exception PPPErr

data Validation = Ok | Err PPPErr

charsetPPPDir :: String
charsetPPPDir = lower ++ upper ++ nums ++ misc
  where
    lower = ['a'..'z']
    upper = ['A'..'Z']
    nums  = ['0'..'9']
    misc  = "._-"

validatePPPDir :: PPP -> String -> Validation
validatePPPDir ppp dir =
  if length dir > 14
  then Err (PPPErr ppp ("Directory name too long (max length 14): \n\tdir: " ++ dir))
  else
    case filter (not . (`elem` charsetPPPDir)) dir of
      [] -> Ok
      xs -> Err (PPPErr ppp ("Bad characters in directory name: \n\
                             \\tfile: " ++ dir ++
                             "\n\tbad chars: " ++ xs
                            )
                )

charsetPPPFile :: String
charsetPPPFile = lower ++ upper ++ nums ++ misc
  where
    lower = ['a'..'z']
    upper = ['A'..'Z']
    nums = ['0'..'9']
    misc = "._-"

charsetPPPExt :: String
charsetPPPExt = lower ++ upper ++ nums ++ misc
  where
    lower = ['a'..'z']
    upper = ['A'..'Z']
    nums = ['0'..'9']
    misc = "_-"

validatePPPFile :: PPP -> String -> String -> Validation
validatePPPFile ppp file ext =
  let l = length file + length ext
  in if l > 13
     then Err (PPPErr ppp ("Bad file.extension; too long. \
                           \(max length, with '.', 14): \n\
                           \\tfile: " ++ file ++ "." ++ ext
                          )
              )
     else
       case filter (not . (`elem` charsetPPPFile)) (file ++ ext) of
         [] -> Ok
         xs -> (PPPErr ppp ("Bad characters in file.extension: \n\
                            \\tfile: " ++ file ++ "." ++ ext ++
                            "\n\tbad chars: " ++ xs
                           )
               )

--------------------------------------------------------------------------------
-- PPP Implementation including DSL and higher-level functional API

data Rooting = Abs | Rel deriving Show
data Visibility = Vis | Hid deriving Show

newtype PPP rt = PPP (Rooting,Visibility,Pattern '[PPPSelector] Identity ())

instance IsString (PPP rt) where
  fromString = ppp_fromString

ppp_fromString :: String -> PPP rt
ppp_fromString str = undefined

isAbs :: PPP rt -> Bool
isAbs (PPP (Abs,_,_)) = True
isAbs _ = False

isRel :: PPP rt -> Bool
isRel (PPP (Rel,_,_)) = True
isRel _ = False

isHid :: PPP Abs -> Bool
isHid (PPP (_,Hid,_)) = True
isHid _ = False

isVis :: PPP Abs -> Bool
isVis (PPP (_,Vis,_)) = True
isVis _ = False

isVisPPPComponent :: PPP rt -> Bool
isVisPPPComponent (PPP (_,Vis,_)) = True
isVisPPPComponent _ = False

isHidPPPComponent :: PPP rt -> Bool
isHidPPPComponent (PPP (_,Hid,_)) = True
isHidPPPComponent _ = False

leastVis :: Visibility -> Visibility -> Visibility
leastVis Hid _ = Hid
leastVis _ Hid = Hid
leastVis _ _ = Vis

(</>) :: PPP rt -> PPP Rel -> PPP rt
(</>) (PPP (rtng,visl,pthl)) (PPP (_,visr,pthr)) = PPP (rtng,leastVis visl visr,pthl >> pthr)

makeAbs :: PPP Abs -> PPP Rel -> PPP Abs
makeAbs = (</>)

guaranteeAbs :: Is Excepting fs m
                  => PPP rt
                  -> (Throws PPPErr => Pattern fs m (PPP Abs))
guaranteeAbs ppp@(PPP (Abs,_,_)) = return $ coerce ppp
guaranteeAbs ppp = throwChecked
  (PPPErr ppp "guranteeAbs: expected Absolute PPP, but got Relative PPP.")

guaranteeRel :: Is Excepting fs m
                 => PPP rt
                 -> (Throws PPPErr => Pattern fs m (PPP Rel))
guaranteeRel ppp@(PPP (Rel,_,_)) = return $ coerce ppp
guaranteeRel ppp = throwChecked
  (PPPErr ppp "guaranteeRel: expected Relative PPP, but got Absolute PPP.")

reify :: (Lift IO m,Is Excepting fs m) => PPP Rel -> Pattern fs m (PPP Abs)
reify pp = do
  cd <- io (getEnv "PWD")
  let d = ppp_fromString cd
  return (makeAbs d pp)

toString :: PPP rt -> String
toString (PPP (rtng,vis,pth)) = undefined


data PortablePOSIXFileSelector k
  = Up k
  | Current k
  | Dir Visibility DirName k
  | File Visibility FileName Extension k
  | View

data POSIXSelectable k = POSIXSelectable k

data RelPPPSelector fs m = RelPPPSelector
  { up         :: Pattern fs m ()
  , current    :: Pattern fs m ()
  , dir        :: DirName -> Pattern fs m ()
  , hiddenDir  :: DirName -> Pattern fs m ()
  , file       :: FileName -> Extension -> Pattern fs m ()
  , hiddenFile :: FileName -> Extension -> Pattern fs m ()
  , ext        :: Extension -> Pattern fs m ()
  , viewPath   :: Pattern fs m (PPP Rel)
  }


relative
  :: forall fs m a.
     (Monad m,Is PortablePOSIXFileSelector fs m,Is Excepting fs m)
  => (RelPOSIXSelector fs m -> Pattern fs m a)
  -> (Throws PPPErr => Pattern fs m (a,PPP Rel))
relative p0 = start (return ()) $ p0 RelPOSIXSelector
    { up = self (Up ())
    , current = self (Current ())
    , dir = \d -> self (Dir Vis d ())
    , hiddenDir = \d -> self (Dir Hid d ())
    , file = \f e -> self (File Vis f e ())
    , hiddenFile = \f e -> self (File Hid f e ())
    , ext = \e -> self (File Vis "" e ())
    , viewPath = self View
    }
  where
    start :: Pattern '[PortablePOSIXFileSelector] Identity ()
          -> Pattern fs m a
          -> (Throws PPPErr => Pattern fs m (a,PPP Rel))
    start acc = go
      where
        go :: Pattern fs m a
           -> (Throws PPPErr => Pattern fs m (a,PPP Rel))
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Up k -> start (acc >> self (Up ())) (bp k)
                    Current k -> start (acc >> self (Current ())) (bp k)
                    File v f e k  ->
                      case validPOSIXFile acc f e of
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
          -> (Throws PPPErr => Pattern fs m (a,PPP Rel))
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
                      case validPOSIXFile acc f e of
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
           -> (Throws PPPErr => Pattern fs m (a,PPP Rel))
    infile = go
      where
        go acc p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    File _ "" e k -> go (acc >> self (File Vis "" e ())) (bp k)
                    File _ _ _ _ -> throwChecked (PPPErr (PPP ()))
                    View -> go acc (bp (accToPOSIXPath acc))
                    _ -> throw (PPPErr )
                Nothing -> Step sym (\b -> go acc (bp b))
            M m -> M (fmap (go acc) m)
            Pure r -> Pure (r,accToPOSIXPath acc)
    accToPOSIXPath acc = undefined
    validPOSIXDir d = undefined
    validPOSIXFile acc f e = let l = length f + length e in
      if l > 14
      then Err (InvalidPOSIXPathLength l
                  (toString (accToPOSIXPath acc) ++ "/" ++ f ++ "." ++ e)
               )
      else if not (all (`elem` portablePOSIXFileNameCharset) (f ++ e))
           then Err (PPPErr (acc >> self (File ))"")
           else case f of
                  '-':_ -> Err (InvalidPOSIXPortableFileNameStart
                                  "-" (toString (accToPOSIXPath acc))
                               )
                  _ -> Ok

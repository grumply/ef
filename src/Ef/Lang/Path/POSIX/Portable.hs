{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Ef.Lang.Path.POSIX.Portable where

import Ef.Core
import Ef.Lang.IO
import Ef.Lang.Except
import Ef.Lang.Except.Checked

import Data.Functor.Identity
import Data.String
import System.Environment

import Data.List
import Data.Coerce
import Unsafe.Coerce

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

Approach:

x Embeddable so path generation can be interspersed with other DSL constructs
Overloaded so paths can be created with a standard string implementation
Quotable so paths can be created at compilation and statically guaranteed correct

-}

data PPPErr
  = forall rt. PPPErr (Pattern '[PPPSelect] Identity ()) String
  | PPPFromStringErr String
instance Show PPPErr where
  show (PPPErr p str) =
    "PPPErr: " ++ str ++ "\n\tExceptional PPP:" ++ ppp_show p
  show (PPPFromStringErr str) = "PPPErr in fromString: " ++ str
instance Exception PPPErr

ppp_show :: Pattern '[PPPSelect] Identity () -> String
ppp_show = concat . intersperse "/" . reverse . start
  where
    start p =
      case p of
        Step sym bp ->
          case prj sym of
            ~(Just x) ->
              case x of
                Root k -> continue [""] (bp k)
                Up k -> continue [".."] (bp k)
                Current k -> start (bp k)
                Dir v d k -> if v == Vis
                             then continue [d] (bp k)
                             else continue ['.':d] (bp k)
                File v f "" k -> if v == Vis
                                 then continue [f] (bp k)
                                 else continue ['.':f] (bp k)
                ~(File v f e k) -> if v == Vis
                                   then continue [f ++ '.':e] (bp k)
                                   else continue ['.':f ++ '.':e] (bp k)
        M (Identity m) -> start m
        Pure () -> [""]
    continue acc p =
      case p of
        Step sym bp ->
          case prj sym of
            ~(Just x) ->
              case x of
                Up k -> continue ("..":acc) (bp k)
                Current k -> continue (".":acc) (bp k)
                Dir v d k -> if v == Vis
                             then continue (d:acc) (bp k)
                             else continue (('.':d):acc) (bp k)
                File v f "" k -> if v == Vis
                                 then continue (f:acc) (bp k)
                                 else continue (('.':f):acc) (bp k)
                ~(File v f e k) -> if v == Vis
                                   then continue ((f ++ '.':e):acc) (bp k)
                                   else continue (('.':f ++ '.':e):acc) (bp k)
        M (Identity m) -> continue acc m
        Pure _ -> acc

data Validation = Ok | Err PPPErr

validatePPPDir :: Pattern '[PPPSelect] Identity () -> String -> Validation
validatePPPDir ppp dir
  | null dir        = Err (PPPErr ppp "Empty directory name.")
  | length dir > 14 = Err (PPPErr ppp ("Directory" `tooLong` dir))
  | otherwise       = let xs = filter (not . (`elem` charsetPPPDir)) dir
                      in if null xs
                         then Ok
                         else Err (PPPErr ppp ("directory" `badChars` xs))
  where
    charsetPPPDir = lower ++ upper ++ nums ++ misc
      where
        lower = ['a'..'z']
        upper = ['A'..'Z']
        nums  = ['0'..'9']
        misc  = "._-"

tooLong i fd = i ++ " name too long (max length 14; got length: " ++ show (length fd) ++ "): " ++ fd

badChars i xs = "Bad characters in " ++ i ++ ": " ++ xs

validatePPPFile :: Pattern '[PPPSelect] Identity () -> String -> String -> Validation
validatePPPFile ppp file ext
  | null file = Err (PPPErr ppp "Empty file names not supported.")
  | length (file ++ '.':ext) > 14 = Err (PPPErr ppp ("File and Extension" `tooLong` (file ++ '.':ext)))
  | otherwise = let f_bad = filter (not . (`elem` charsetPPPFile)) file
                    e_bad = filter (not . (`elem` charsetPPPExt)) ext
                in if null f_bad
                   then if null e_bad
                        then Ok
                        else Err (PPPErr ppp ("extension" `badChars` e_bad))
                   else Err (PPPErr ppp ("file" `badChars` f_bad))
  where
    charsetPPPFile = lower ++ upper ++ nums ++ misc_file
    charsetPPPExt = lower ++ upper ++ nums ++ misc_ext
    lower = ['a'..'z']
    upper = ['A'..'Z']
    nums = ['0'..'9']
    misc_file = "._-"
    misc_ext = "_-"

data Rooting = Abs | Rel deriving (Show,Eq)
data Visibility = Vis | Hid deriving (Show,Eq)

newtype PPP rt = PPP (Rooting,Visibility,Pattern '[PPPSelect] Identity ())

instance Show (PPP rt) where
  show (PPP (_,_,ppp)) = ppp_show ppp

instance IsString (PPP rt) where
  fromString = ppp_fromString

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
guaranteeAbs (PPP (_,_,p)) = throwChecked
  (PPPErr p "guranteeAbs: expected Absolute PPP, but got Relative PPP.")

guaranteeRel :: Is Excepting fs m
                 => PPP rt
                 -> (Throws PPPErr => Pattern fs m (PPP Rel))
guaranteeRel ppp@(PPP (Rel,_,_)) = return $ coerce ppp
guaranteeRel (PPP (_,_,p)) = throwChecked
  (PPPErr p "guaranteeRel: expected Relative PPP, but got Absolute PPP.")

reifyRel :: (Lift IO m,Is Excepting fs m) => PPP Rel -> Pattern fs m (PPP Abs)
reifyRel pp = do
  cd <- io (getEnv "PWD")
  let d = ppp_fromString cd
  return (makeAbs d pp)

type FileName = String
type DirName = String
type Extension = String

data PPPSelect k
  = Root k
  | Up k
  | Current k
  | Dir Visibility DirName k
  | File Visibility FileName Extension k
  | View

data PPPSelectable k = PPPSelectable k
ppps :: Uses PPPSelectable fs m => Attribute PPPSelectable fs m
ppps = PPPSelectable return

instance Symmetry PPPSelectable PPPSelect

data POSIXSelectable k = POSIXSelectable k

data PPPSelector fs m = PPPSelector
  { up         :: Pattern fs m ()
  , current    :: Pattern fs m ()
  , dir        :: DirName -> Pattern fs m ()
  , hiddenDir  :: DirName -> Pattern fs m ()
  , file       :: FileName -> Extension -> Pattern fs m ()
  , hiddenFile :: FileName -> Extension -> Pattern fs m ()
  , ext        :: Extension -> Pattern fs m ()
  , viewPath   :: forall rt. Pattern fs m (PPP rt)
  }

relative :: forall fs m a.
            (Monad m,Is PPPSelect fs m,Is Excepting fs m)
         => (PPPSelector fs m -> Pattern fs m a)
         -> (Throws PPPErr => Pattern fs m (a,PPP Rel))
relative = pppSelector False

rooted :: forall fs m a.
          (Monad m,Is PPPSelect fs m,Is Excepting fs m)
       => (PPPSelector fs m -> Pattern fs m a)
       -> (Throws PPPErr => Pattern fs m (a,PPP Abs))
rooted = pppSelector True

pppSelector
  :: forall fs m a rt.
     (Monad m,Is PPPSelect fs m,Is Excepting fs m)
  => Bool
  -> (PPPSelector fs m -> Pattern fs m a)
  -> (Throws PPPErr => Pattern fs m (a,PPP rt))
pppSelector b p0 = start True (if b then self (Root ()) else (return ())) $ p0 PPPSelector
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
    start abs acc = go
      where
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Up k -> start False (acc >> self (Up ())) (bp k)
                    Current k -> start False (acc >> self (Current ())) (bp k)
                    File v f e k  ->
                      case validatePPPFile acc f e of
                        Ok -> infile False (v == Vis) (acc >> self (File v f e ())) (bp k)
                        Err e -> throwChecked e
                    Dir v d k ->
                      case validatePPPDir acc d of
                        Ok -> indir False (v == Vis) (acc >> self (Dir v d ())) (bp k)
                        Err e -> throwChecked e
                    View -> go (bp (unsafeCoerce (PPP (if abs then Abs else Rel,Vis,acc))))
                Nothing -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure (r,PPP (if abs then Abs else Rel,Vis,acc))

    indir abs vis = go
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
                      case validatePPPFile acc f e of
                        Ok -> infile abs (v == Vis && vis) (acc >> self (File v f e ())) (bp k)
                        Err e -> throwChecked e
                    Dir v d k ->
                      case validatePPPDir acc d of
                        Ok -> indir abs (v == Vis && vis) (acc >> self (Dir v d ())) (bp k)
                        Err e -> throwChecked e
                    View -> go acc (bp (unsafeCoerce (PPP (if abs then Abs else Rel,if vis then Vis else Hid,acc))))
                Nothing -> Step sym (\b -> go acc (bp b))
            M m -> M (fmap (go acc) m)
            Pure r -> Pure (r,PPP (if abs then Abs else Rel,if vis then Vis else Hid,acc))

    infile abs vis = go
      where
        go acc p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    File _ "" e k -> go (acc >> self (File Vis "" e ())) (bp k)
                    File _ _ _ _ -> throwChecked (PPPErr acc "Tried to nest a file inside a file.")
                    View -> go acc (bp (unsafeCoerce (PPP (if abs then Abs else Rel,if vis then Vis else Hid,acc))))
                    _ -> throw (PPPErr acc "Tried to nest directory segment inside file.")
                Nothing -> Step sym (\b -> go acc (bp b))
            M m -> M (fmap (go acc) m)
            Pure r -> Pure (r,PPP (if abs then Abs else Rel,if vis then Vis else Hid,acc))

ppp_fromString :: String -> PPP rt
ppp_fromString ('/':str) = snd $ runIdentity (ppp_fromString_ True str)
ppp_fromString str = snd $ runIdentity (ppp_fromString_ False str)

ppp_fromString_ b str = delta (Object $ ppps *:* excepter *:* Empty) $ do
  mp <- tryChecked $ pppSelector b $ flip fromString_ str
  case mp of
    Left e@(PPPErr _ _) -> error (show e)
    Right (_,p) -> return p
  where
    fromString_ PPPSelector{..} = go
      where
        go [] = return ()
        go ('/':_) = dir "" -- results in a throwChecked
        go ('.':'/':xs) = current >> go xs
        go ('.':'.':'/':xs) = up >> go xs
        go xs = do
          let (dof,rest) = span (/= '/') xs
          case rest of
            ('/':xs) -> do
              analyzeDir dof
              go xs
            _ -> analyzeFile dof
          where
            analyzeDir ('.':xs) = hiddenDir xs
            analyzeDir xs = dir xs
            analyzeFile ('.':xs) = do
              let (fn,rest) = span (/= '.') xs
              undefined

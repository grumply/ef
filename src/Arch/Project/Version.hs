{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Arch.Project.Version
    ( VersionLexicon
    , VersionAttribute
    , version
    , incMilestone
    , incMajor
    , incMinor
    , incPatch
    , alias
    , checkForAlias
    )
    where

-- | Versions are up to 4 natural number components known
-- as Milestone, Major, Minor, and Patch, followed,
-- optionally, by a dash '-' and a colon-delimited list of
-- version aliases.
-- Examples:
--   3
--   0.3.4.0
--   0.2-internalVersion-1.4:phase3 2

import Ef.Core
import Ef.Lang.Contract

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import Data.Binary
import qualified Data.Binary as B
import Data.List
import Data.Maybe
import Data.String
import GHC.Generics



data VersionLexicon k
    where

        ViewMilestone
            :: (Int -> k)
            -> VersionLexicon k

        IncrementMilestone
            :: k
            -> VersionLexicon k

        ViewMajor
            :: (Int -> k)
            -> VersionLexicon k

        IncrementMajor
            :: k
            -> VersionLexicon k

        ViewMinor
            :: (Int -> k)
            -> VersionLexicon k

        IncrementMinor
            :: k
            -> VersionLexicon k

        ViewPatch
            :: (Int -> k)
            -> VersionLexicon k

        IncrementPatch
            :: k
            -> VersionLexicon k

        ViewAliases
            :: ([String] -> k)
            -> VersionLexicon k

        Alias
            :: String
            -> k
            -> VersionLexicon k

        HasAlias
            :: String
            -> (Bool -> k)
            -> VersionLexicon k


viewMilestone
    :: Method VersionLexicon scope parent Int

viewMilestone =
    self (ViewMilestone id)



incMilestone
    :: Method VersionLexicon scope parent ()

incMilestone =
    self (IncrementMilestone ())



viewMajor
    :: Method VersionLexicon scope parent Int

viewMajor =
    self (ViewMajor id)



incMajor
    :: Method VersionLexicon scope parent ()

incMajor =
    self (IncrementMajor ())


viewMinor
    :: Method VersionLexicon scope parent Int

viewMinor =
    self (ViewMinor id)



incMinor
    :: Method VersionLexicon scope parent ()

incMinor =
    self (IncrementMinor ())



viewPatch
    :: Method VersionLexicon scope parent Int

viewPatch =
    self (ViewPatch id)



incPatch
    :: Method VersionLexicon scope parent ()

incPatch =
    self (IncrementPatch ())



viewVersion
    :: Method VersionLexicon scope parent (Int,Int,Int,Int)

viewVersion =
    (,,,) <$> viewMilestone <*> viewMajor <*> viewMinor <*> viewPatch



viewAliases
    :: Method VersionLexicon scope parent [String]

viewAliases =
    self (ViewAliases id)



alias
    :: String
    -> Method VersionLexicon scope parent Bool

alias newAlias =
    let
        noColons =
            Precondition $
                let
                    hasColon =
                        ':' `elem` newAlias

                in
                   when hasColon $ throw (AliasContainsColon newAlias)

        considerations =
            [ noColons ]

        method =
            self (Alias newAlias True)

        aliasContract =
            Contract considerations method

    in
        contract "alias" aliasContract

data AliasContainsColon =
    AliasContainsColon String
    deriving Show
instance Exception AliasContainsColon



checkForAlias
    :: String
    -> Method VersionLexicon scope parent Bool

checkForAlias alias_ =
    self (HasAlias alias_ id)


type VersionObj parent =
    Attribute VersionAttribute '[VersionAttribute] parent


versionObj
    :: Monad parent
    => [String]
    -> Int
    -> Int
    -> Int
    -> Int
    -> Ef.Core.Object '[VersionAttribute] parent

versionObj aliases milestone major minor patch =
    let
        attr =
            version aliases milestone major minor patch

    in
        Ef.Core.Object (attr *:* Empty)

simpleVersionObj
    :: Monad parent
    => Ef.Core.Object '[VersionAttribute] parent

simpleVersionObj =
    versionObj [] 0 0 0 0

type Version attrs parent =
    Uses VersionAttribute attrs parent
    => Attribute VersionAttribute attrs parent

data VersionAttribute k =
    Version
        {
        -- Associated data + views
          milestone
              :: (Int,k)

        , major
              :: (Int,k)

        , minor
              :: (Int,k)

        , patch
              :: (Int,k)

        , aliases
              :: ([String],k)

        -- methods
        , incrementMilestone
              :: k

        , incrementMajor
              :: k

        , incrementMinor
              :: k

        , incrementPatch
              :: k

        , addAlias
              :: String
              -> k

        , hasAlias
              :: String
              -> (Bool,k)

        }

instance Witnessing VersionAttribute VersionLexicon
    where
        witness use Version {..} (ViewMilestone k) =
            use (snd milestone) (k $ fst milestone)

        witness use Version {..} (IncrementMilestone k) =
            use incrementMilestone k

        witness use Version {..} (ViewMajor k) =
            use (snd major) (k $ fst major)

        witness use Version {..} (IncrementMajor k) =
            use incrementMajor k

        witness use Version {..} (ViewMinor k) =
            use (snd minor) (k $ fst minor)

        witness use Version {..} (IncrementMinor k) =
            use incrementMinor k

        witness use Version {..} (ViewPatch k) =
            use (snd patch) (k $ fst patch)

        witness use Version {..} (IncrementPatch k) =
            use incrementPatch k

        witness use Version {..} (ViewAliases k) =
            use (snd aliases) (k $ fst aliases)

        witness use Version {..} (Alias newAlias k) =
            use (addAlias newAlias) k

        witness use Version {..} (HasAlias alias resultk) =
            let
                (result,k) =
                    hasAlias alias

                k' =
                    resultk result

            in
                use k k'



instance Show (VersionAttribute k)
    where

        show Version {..} =
            let
                aliasesString =
                    intercalate ":" $ fst aliases

                milestoneString =
                    show $ fst milestone

                majorString =
                    show $ fst major

                minorString =
                    show $ fst minor

                patchString =
                    show $ fst patch

                versionString =
                    let
                        shortenedReverse =
                            dropWhile (== "0") [patchString,minorString,majorString,milestoneString]

                        versionStrings =
                            reverse shortenedReverse

                        tags =
                            if length aliasesString == 0 then
                                ""
                            else
                                '-':aliasesString

                    in
                        if length versionStrings == 0 then
                            '0':tags
                        else
                            let
                                untaggedVersionString =
                                    intercalate "." versionStrings

                            in
                                untaggedVersionString ++ tags

            in
                versionString



instance ToJSON (VersionAttribute k)
    where

        toJSON Version {..} =
            object
                [
                  "aliases" A..=
                      (toJSON $ fst aliases)

                , "milestone" A..=
                      (toJSON $ fst milestone)

                , "major" A..=
                      (toJSON $ fst major)

                , "minor" A..=
                      (toJSON $ fst minor)

                , "patch" A..=
                      (toJSON $ fst patch)

                ]

instance Uses VersionAttribute attrs parent
    => FromJSON (Attribute VersionAttribute attrs parent)
    where

        parseJSON (A.Object v) =
            do
                mayAliases   <- v .:? "aliases"
                mayMilestone <- v .:? "milestone"
                mayMajor     <- v .:? "major"
                mayMinor     <- v .:? "minor"
                mayPatch     <- v .:? "patch"
                constructVersion mayAliases mayMilestone mayMajor mayMinor mayPatch
            where

                constructVersion mayAliases mayMilestone mayMajor mayMinor mayPatch =
                    let
                        aliases =
                            fromMaybe [] mayAliases

                        milestone =
                            fromMaybe 0 mayMilestone

                        major =
                            fromMaybe 0 mayMajor

                        minor =
                            fromMaybe 0 mayMinor

                        patch =
                            fromMaybe 0 mayPatch

                    in
                        pure $ version aliases milestone major minor patch

        parseJSON _ =
            empty



instance Eq (VersionAttribute k)
    where

        v0 == v1 =
            let
                sameMilestone =
                    fst (milestone v0) == fst (milestone v1)

                sameMajor =
                    fst (major v0) == fst (major v1)

                sameMinor =
                    fst (minor v0) == fst (minor v1)

                samePatch =
                    fst (patch v0) == fst (patch v1)

            in
                sameMilestone &&
                sameMajor &&
                sameMinor &&
                samePatch



instance Ord (VersionAttribute k)
    where

        v0 <= v1 =
            let
                smallerOrSameMilestone =
                    fst (milestone v0) <= fst (milestone v1)

                smallerOrSameMajor =
                    fst (major v0) <= fst (major v1)

                smallerOrSameMinor =
                    fst (minor v0) <= fst (minor v1)

                smallerOrSamePatch =
                    fst (patch v0) <= fst (patch v1)

            in
                smallerOrSameMilestone &&
                smallerOrSameMajor &&
                smallerOrSameMinor &&
                smallerOrSamePatch



instance Uses VersionAttribute attrs parent
    => Binary (Attribute VersionAttribute attrs parent)
    where

        get =
            version <$> B.get <*> B.get <*> B.get <*> B.get <*> B.get

        put Version {..} =
            do
                B.put (fst aliases)
                B.put (fst milestone)
                B.put (fst major)
                B.put (fst minor)
                B.put (fst patch)



instance Uses VersionAttribute attrs parent
    => IsString (Attribute VersionAttribute attrs parent)
    where

        fromString versionString =
            let
                (versionComponent,aliasComponent) =
                    span (/= '-') versionString

                versionStringComponents =
                    elemSlice '.' versionComponent

                versionIntComponents =
                    map read versionStringComponents

                tags =
                    if length aliasComponent > 0 then
                        elemSlice ':' (tail aliasComponent)
                    else
                        []

                [milestone,major,minor,patch] =
                    sequence [(!!0),(!!1),(!!2),(!!3)] versionIntComponents

            in
                case length versionStringComponents of

                    0 ->
                        version tags 0 0 0 0

                    1 ->
                        version tags milestone 0 0 0

                    2 ->
                        version tags milestone major 0 0

                    3 ->
                        version tags milestone major minor 0

                    4 ->
                        version tags milestone major minor patch

                    _ ->
                        error "Correct Version string format is one of (without braces; omitted tail versions = 0):\
                              \\n\t\"{milestone}.{major}.{minor}.{patch}\"\
                              \\n\t\"{milestone}.{major}.{minor}\"\
                              \\n\t\"{milestone}.{major}\"\
                              \\n\t\"{milestone}\"\
                              \\n\t\"\""
            where

                elemSlice
                    :: Eq a
                    => a
                    -> [a]
                    -> [[a]]

                elemSlice elem list =
                    let
                        indices =
                            elemIndices elem list

                    in
                        slice 0 list indices
                    where

                        slice !count list [] =
                            [list]

                        slice count list (next:rest) =
                            let
                                size =
                                    next - count

                                !sizeWithElem =
                                    size + 1

                                !newCount =
                                    count + sizeWithElem

                                newList =
                                    drop sizeWithElem list

                            in
                                take size list : slice newCount newList rest




version
    :: Uses VersionAttribute attrs parent
    => [String]
    -> Int
    -> Int
    -> Int
    -> Int
    -> Attribute VersionAttribute attrs parent

version alss mlst mjr mnr ptch =
    let
        aliases =
            (alss,pure)

        milestone =
            (mlst,pure)

        major =
            (mjr,pure)

        minor =
            (mnr,pure)

        patch =
            (ptch,pure)

        incrementMilestone obj =
            let
                Version {..} = view obj

            in
                pure $ obj .=
                    Version
                        {
                          milestone =
                              first succ milestone

                        , ..

                        }

        incrementMajor obj =
            let
                Version {..} = view obj

            in
                pure $ obj .=
                    Version
                        {
                          major =
                              first succ major

                        , ..

                        }

        incrementMinor obj =
            let
                Version {..} = view obj

            in
                pure $ obj .=
                    Version
                        {
                          minor =
                              first succ minor

                        , ..

                        }

        incrementPatch obj =
            let
                Version {..} = view obj

            in
                pure $ obj .=
                    Version
                        {
                          patch =
                              first succ patch

                        , ..

                        }

        addAlias newAlias obj =
            let
                Version {..} = view obj

                newAliases =
                    first (newAlias :) aliases

            in
                pure $ obj .=
                    Version
                        {
                          aliases =
                              newAliases

                        , hasAlias =
                              \needle ->
                                  (needle `elem` (fst newAliases),pure)

                        , ..

                        }

        hasAlias needle =
                (needle `elem` alss, pure)

    in
        Version {..}

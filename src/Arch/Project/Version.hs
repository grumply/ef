{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module Arch.Project.Version where

-- | Versions are up to 3 natural number components, followed, 
-- optionally by a dash '-' and a colon-delimited list of 
-- version aliases. 
-- Examples: 
--   3
--   0.3.4
--   0.2-internalVersion0:milestone-2.0

import Control.Applicative
import Data.Aeson
import Data.Binary
import Data.List
import Data.Maybe
import Data.String
import GHC.Generics



newtype VersionAlias =

    VersionAlias
        { versionAlias
              :: String
        }

    deriving (Eq,Generic,ToJSON,FromJSON)

instance Show VersionAlias
    where

        show =
            versionAlias

instance Binary VersionAlias

instance IsString VersionAlias
    where

        fromString =
            VersionAlias



newtype MajorVersion =

    MajorVersion
        { majorVersion
              :: Int
        }

    deriving (Enum,Eq,Ord,Show,Generic,ToJSON,FromJSON)

instance Binary MajorVersion

instance IsString MajorVersion
    where

        fromString =
            MajorVersion . read



newtype MinorVersion =

    MinorVersion
        { minorVersion
              :: Int
        }

    deriving (Enum,Eq,Ord,Show,Generic,ToJSON,FromJSON)

instance Binary MinorVersion

instance IsString MinorVersion
    where

        fromString =
            MinorVersion . read



newtype PatchVersion =

    PatchVersion
        { patchVersion
              :: Int
        }

    deriving (Enum,Eq,Ord,Show,Generic,ToJSON,FromJSON)

instance Binary PatchVersion

instance IsString PatchVersion
    where

        fromString =
            PatchVersion . read



data ProjectVersion =

    ProjectVersion
        {
          aliases
              :: [VersionAlias]

        , major
              :: MajorVersion

        , minor
              :: MinorVersion

        , patch
              :: PatchVersion

        }

    deriving (Generic)

instance Show ProjectVersion
    where

        show ProjectVersion {..} =
            let
                aliasesString =
                    intercalate ":" (map show aliases)

                majorString =
                    show (majorVersion major)

                minorString =
                    show (minorVersion minor)

                patchString =
                    show (patchVersion patch)

                versionString =
                    let
                        shortenedReverse =
                            dropWhile (== "0") [patchString,minorString,majorString]

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

instance ToJSON ProjectVersion
    where

        toJSON ProjectVersion {..} =
            object
                [
                  "aliases" .=
                      toJSON aliases

                , "major" .=
                      toJSON major

                , "minor" .=
                      toJSON minor

                , "patch" .=
                      toJSON patch

                ]

instance FromJSON ProjectVersion
    where

        parseJSON (Object v) =
            do
                mayAliases <- v .:? "aliases"
                mayMajor   <- v .:? "major"
                mayMinor   <- v .:? "minor"
                mayPatch   <- v .:? "patch"
                constructProjectVersion mayAliases mayMajor mayMinor mayPatch
            where

                constructProjectVersion mayAliases mayMajor mayMinor mayPatch =
                    let
                        aliases =
                            fromMaybe [] mayAliases

                        major =
                            fromMaybe "0" mayMajor

                        minor =
                            fromMaybe "0" mayMinor

                        patch =
                            fromMaybe "0" mayPatch

                    in
                        pure ProjectVersion {..}

        parseJSON _ =
            empty



instance Eq ProjectVersion
    where

        v0 == v1 =
            let
                sameMajorVersion =
                    major v0 == major v1

                sameMinorVersion =
                    minor v0 == minor v1

                samePatchVersion =
                    patch v0 == patch v1

            in
                sameMajorVersion &&
                sameMinorVersion &&
                samePatchVersion



instance Ord ProjectVersion
    where

        v0 <= v1 =
            let
                smallerOrSameMajorVersion =
                    major v0 <= major v1

                smallerOrSameMinorVersion =
                    minor v0 <= minor v1

                smallerOrSamePatchVersion =
                    patch v0 <= patch v1

            in
                smallerOrSameMajorVersion &&
                smallerOrSameMinorVersion &&
                smallerOrSamePatchVersion



instance Binary ProjectVersion

instance IsString ProjectVersion
    where

        fromString versionString =
            let
                (versionComponent,aliasComponent) =
                    span (/= '-') versionString

                versionStringComponents =
                    elemSlice '.' versionComponent

                versionIntComponents =
                    map read versionStringComponents

                aliasStrings =
                    if length aliasComponent > 0 then
                        elemSlice ':' (tail aliasComponent)
                    else
                        []

                tags =
                    map VersionAlias aliasStrings

                [major,minor,patch] =
                    sequence [(!!0),(!!1),(!!2)] versionIntComponents

            in
                case length versionStringComponents of

                    0 ->
                        version tags 0 0 0

                    1 ->
                        version tags major 0 0

                    2 ->
                        version tags major minor 0

                    3 ->
                        version tags major minor patch

                    _ ->
                        error "Correct ProjectVersion string format is one of (without braces):\
                                  \\n\t\"{major}.{minor}.{patch}\"\
                                  \\n\t\"{major}.{minor}\"\
                                  \\n\t\"{major}\"\
                                  \\n\t\"\" -> equivalent to version 0.0.0"



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
    :: [VersionAlias]
    -> Int
    -> Int
    -> Int
    -> ProjectVersion

version aliases mjr mnr ptch =
    let
        major =
            MajorVersion mjr

        minor =
            MinorVersion mnr

        patch =
            PatchVersion ptch

    in
        ProjectVersion {..}



incrementMajorVersion
    :: ProjectVersion
    -> ProjectVersion

incrementMajorVersion ProjectVersion {..} =
    ProjectVersion
        {
          major =
              succ major

        , ..

        }



incrementMinorVersion
    :: ProjectVersion
    -> ProjectVersion

incrementMinorVersion ProjectVersion {..} =
    ProjectVersion
        {
          minor =
              succ minor

        , ..

        }



incrementPatchVersion
    :: ProjectVersion
    -> ProjectVersion

incrementPatchVersion ProjectVersion {..} =
    ProjectVersion
        {
          patch =
              succ patch

        , ..

        }



tagVersion
    :: VersionAlias
    -> ProjectVersion
    -> ProjectVersion

tagVersion tag ProjectVersion {..} =
    ProjectVersion
        {
          aliases =
              tag:aliases

        , ..

        }

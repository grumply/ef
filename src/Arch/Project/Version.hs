{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module Arch.Project.Version where

-- | Versions are up to 4 natural number components known
-- as Milestone, Major, Minor, and Patch, followed,
-- optionally by a dash '-' and a colon-delimited list of
-- version aliases.
-- Examples:
--   3
--   0.3.4.0
--   0.2-internalVersion1:projectPhase3

import Control.Applicative
import Data.Aeson
import Data.Binary
import Data.List
import Data.Maybe
import Data.String
import GHC.Generics



newtype Alias =

    Alias
        { versionAlias
              :: String
        }

    deriving (Eq,Generic,ToJSON,FromJSON)

instance Show Alias
    where

        show =
            versionAlias

instance Binary Alias

instance IsString Alias
    where

        fromString =
            Alias



newtype Milestone =

    Milestone
        { milestoneVersion
              :: Int
        }

    deriving (Enum,Eq,Ord,Show,Generic,ToJSON,FromJSON)

instance Binary Milestone

instance IsString Milestone
    where

        fromString =
            Milestone . read



newtype Major =

    Major
        { majorVersion
              :: Int
        }

    deriving (Enum,Eq,Ord,Show,Generic,ToJSON,FromJSON)

instance Binary Major

instance IsString Major
    where

        fromString =
            Major . read



newtype Minor =

    Minor
        { minorVersion
              :: Int
        }

    deriving (Enum,Eq,Ord,Show,Generic,ToJSON,FromJSON)

instance Binary Minor

instance IsString Minor
    where

        fromString =
            Minor . read



newtype Patch =

    Patch
        { patchVersion
              :: Int
        }

    deriving (Enum,Eq,Ord,Show,Generic,ToJSON,FromJSON)

instance Binary Patch

instance IsString Patch
    where

        fromString =
            Patch . read



data Version =

    Version
        {
          aliases
              :: [Alias]

        , milestone
              :: Milestone

        , major
              :: Major

        , minor
              :: Minor

        , patch
              :: Patch

        }

    deriving (Generic)

instance Show Version
    where

        show Version {..} =
            let
                aliasesString =
                    intercalate ":" (map show aliases)

                milestoneString =
                    show (milestoneVersion milestone)

                majorString =
                    show (majorVersion major)

                minorString =
                    show (minorVersion minor)

                patchString =
                    show (patchVersion patch)

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

instance ToJSON Version
    where

        toJSON Version {..} =
            object
                [
                  "aliases" .=
                      toJSON aliases

                , "milestone" .=
                      toJSON milestone

                , "major" .=
                      toJSON major

                , "minor" .=
                      toJSON minor

                , "patch" .=
                      toJSON patch

                ]

instance FromJSON Version
    where

        parseJSON (Object v) =
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
                            fromMaybe "0" mayMilestone

                        major =
                            fromMaybe "0" mayMajor

                        minor =
                            fromMaybe "0" mayMinor

                        patch =
                            fromMaybe "0" mayPatch

                    in
                        pure Version {..}

        parseJSON _ =
            empty



instance Eq Version
    where

        v0 == v1 =
            let
                sameMilestone =
                    milestone v0 == milestone v1

                sameMajor =
                    major v0 == major v1

                sameMinor =
                    minor v0 == minor v1

                samePatch =
                    patch v0 == patch v1

            in
                sameMilestone &&
                sameMajor &&
                sameMinor &&
                samePatch



instance Ord Version
    where

        v0 <= v1 =
            let
                smallerOrSameMilestone =
                    milestone v0 <= milestone v1

                smallerOrSameMajor =
                    major v0 <= major v1

                smallerOrSameMinor =
                    minor v0 <= minor v1

                smallerOrSamePatch =
                    patch v0 <= patch v1

            in
                smallerOrSameMilestone &&
                smallerOrSameMajor &&
                smallerOrSameMinor &&
                smallerOrSamePatch



instance Binary Version

instance IsString Version
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
                    map Alias aliasStrings

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
                        error "Correct Version string format is one of (without braces):\
                                  \\n\t\"{milestone}.{major}.{minor}.{patch}\"\
                                  \\n\t\"{milestone}.{major}.{minor}\"\
                                  \\n\t\"{milestone}.{major}\"\
                                  \\n\t\"{milestone}\"\
                                  \\n\t\"\" -> equivalent to version 0.0.0.0"
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
    :: [Alias]
    -> Int
    -> Int
    -> Int
    -> Int
    -> Version

version aliases mlstn mjr mnr ptch =
    let
        milestone =
            Milestone mlstn

        major =
            Major mjr

        minor =
            Minor mnr

        patch =
            Patch ptch

    in
        Version {..}



isTagged
    :: Version
    -> Alias
    -> Bool

isTagged Version {..} alias =
    alias `elem` aliases



removeTag
    :: Version
    -> Alias
    -> Version

removeTag Version {..} alias =
    Version
        {
          aliases =
              filter (/= alias) aliases

        , ..

        }



incrementMilestone
    :: Version
    -> Version

incrementMilestone Version {..} =
    Version
        {
          milestone =
              succ milestone

        , ..

        }



incrementMajor
    :: Version
    -> Version

incrementMajor Version {..} =
    Version
        {
          major =
              succ major

        , ..

        }



incrementMinor
    :: Version
    -> Version

incrementMinor Version {..} =
    Version
        {
          minor =
              succ minor

        , ..

        }



incrementPatch
    :: Version
    -> Version

incrementPatch Version {..} =
    Version
        {
          patch =
              succ patch

        , ..

        }



tagVersion
    :: Alias
    -> Version
    -> Version

tagVersion tag Version {..} =
    Version
        {
          aliases =
              tag:aliases

        , ..

        }

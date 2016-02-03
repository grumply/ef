{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Runtime.Before.Directories where



import Ef.Core.Object
import Ef.Core.Narrative
import Ef.Core.Inflect
import Ef.Core.Narrative.Exception
import Ef.Lang.IO

import Data.Binary

import System.Environment
import System.FilePath



data DirectoriesConfiguration k =
    DirectoriesConfiguration
        {
          userDataDirectory
              :: (FilePath,k)

        , putUserDataDirectory
              :: FilePath
              -> k

        , userCacheDirectory
              :: (FilePath,k)

        , putUserCacheDirectory
              :: FilePath
              -> k

        , studioDirectory
              :: (FilePath,k)

        , putStudioDirectory
              :: FilePath
              -> k

        }

instance Eq (DirectoriesConfiguration k) where

    dirConf1 == dirConf2 =
        fst (userDataDirectory dirConf1) == fst (userDataDirectory dirConf2) &&
        fst (userCacheDirectory dirConf1) == fst (userCacheDirectory dirConf2) &&
        fst (studioDirectory dirConf1) == fst (studioDirectory dirConf2)


instance ( Lift IO environment
         , Has DirectoriesConfiguration contexts environment
         )
        => Binary (DirectoriesConfiguration (Morphism contexts environment))
    where

        get =
            createDefaultDirectoriesConfiguration <$> get <*> get <*> get

        put DirectoriesConfiguration{..} =
            do
                put (fst userDataDirectory)
                put (fst userCacheDirectory)
                put (fst studioDirectory)



data DirectoriesConfigurationLang k
    where

        GetUserDataDirectory
            :: (FilePath -> k)
            -> DirectoriesConfigurationLang k

        PutUserDataDirectory
            :: FilePath
            -> k
            -> DirectoriesConfigurationLang k

        GetUserCacheDirectory
            :: (FilePath -> k)
            -> DirectoriesConfigurationLang k

        PutUserCacheDirectory
            :: FilePath
            -> k
            -> DirectoriesConfigurationLang k

        GetStudioDirectory
            :: (FilePath -> k)
            -> DirectoriesConfigurationLang k

        PutStudioDirectory
            :: FilePath
            -> k
            -> DirectoriesConfigurationLang k



getUserDataDirectory =
    say (GetUserDataDirectory id)

setNewUserDataDirectory newUserDataDirectory =
    say (PutUserDataDirectory newUserDataDirectory ())

getUserCacheDirectory =
    say (GetUserCacheDirectory id)

setUserCacheDirectory newUserCacheDirectory =
    say (PutUserCacheDirectory newUserCacheDirectory ())

getStudioDirectory =
    say (GetStudioDirectory id)

setStudioDirectory newStudioDirectory =
    say (PutStudioDirectory newStudioDirectory ())



createDirectoriesConfiguration =
    do
        let
            defaultDataDir =
                "$HOME/.local/share"

            defaultCacheDir =
                "$HOME/.cache"

            tryEnv name def =
                do
                    envVar <- try $ io (getEnv name)
                    return $ either (const def :: SomeException -> FilePath) id envVar

        dataDir <- tryEnv "$XDG_DATA_HOME" defaultDataDir
        cacheDir <- tryEnv "$XDG_CACHE_HOME" defaultCacheDir
        let
            studioDir =
                dataDir </> "efreet/studio"

        return $ createDefaultDirectoriesConfiguration dataDir cacheDir studioDir



createDefaultDirectoriesConfiguration dataDir cacheDir studioDir =
    DirectoriesConfiguration
        { userDataDirectory = (dataDir,pure)
        , userCacheDirectory = (cacheDir,pure)
        , studioDirectory = (studioDir,pure)
        , ..
        }
        where

            putUserDataDirectory newUserDataDirectory fs =
                let
                    directories = view fs

                in
                    pure $ fs .=
                        directories { userDataDirectory = (newUserDataDirectory,pure) }

            putUserCacheDirectory newUserCacheDirectory fs =
                let
                    directories = view fs

                in
                    pure $ fs .=
                        directories { userCacheDirectory = (newUserCacheDirectory,pure) }

            putStudioDirectory newStudioDirectory fs =
                let
                    directories = view fs

                in
                    pure $ fs .=
                        directories { studioDirectory = (newStudioDirectory,pure) }



instance Inflection DirectoriesConfiguration DirectoriesConfigurationLang where

    inflect use DirectoriesConfiguration{..} (GetUserDataDirectory withDir) =
        inflect use userDataDirectory withDir

    inflect use DirectoriesConfiguration{..} (PutUserDataDirectory newUserDataDir k) =
        inflect use putUserDataDirectory (newUserDataDir,k)

    inflect use DirectoriesConfiguration{..} (GetUserCacheDirectory withDir) =
        inflect use userCacheDirectory withDir

    inflect use DirectoriesConfiguration{..} (PutUserCacheDirectory newUserCacheDir k) =
        inflect use putUserCacheDirectory (newUserCacheDir,k)

    inflect use DirectoriesConfiguration{..} (GetStudioDirectory withDir) =
        inflect use studioDirectory withDir

    inflect use DirectoriesConfiguration{..} (PutStudioDirectory newStudioDir k) =
        inflect use putStudioDirectory (newStudioDir,k)


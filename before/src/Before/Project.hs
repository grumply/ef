{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Runtime.Before.Project where



import Ef.Core.Object
import Ef.Core.Narrative
import Ef.Core.Inflect
import Ef.Lang.IO

import Data.Binary
import GHC.Generics
import GHC.Fingerprint
import System.FilePath



type ProjectName = String

type ProjectDirectory = FilePath

type Hash = Fingerprint

data Project k =
    Project
        {
          projectName 
              :: (ProjectName,k)

        , changeProjectName
              :: ProjectName
              -> k

        , projectAbsoluteRootDirectory
              :: (ProjectDirectory,k)

        , projectRelativeSourceDirectory
              :: (FilePath,k)

        , projectRelativeCompiledDirectory
              :: (FilePath,k)

        , projectSourceFiles
              :: ([(FilePath,Hash)],k)

        , addProjectSourceFile
              :: FilePath
              -> k

        }

-- If two projects have the same name, root directory, source directory, and compiled directory,
-- as well as the same files, including file hashes, they're equal.
instance Eq (Project k) where
    p1 == p2 =
        fst (projectName p1) == fst (projectName p2) &&
        fst (projectAbsoluteRootDirectory p1) == fst (projectAbsoluteRootDirectory p2) &&
        fst (projectRelativeSourceDirectory p1) == fst (projectRelativeSourceDirectory p2) &&
        fst (projectRelativeCompiledDirectory p1) == fst (projectRelativeCompiledDirectory p2) &&
        fst (projectSourceFiles p1) == fst (projectSourceFiles p2)



instance ( Has Project contexts environment
         , Lift IO environment
         )
        => Binary (Project (Morphism contexts environment))
    where

        get =
            createProject <$> get <*> get <*> get <*> get <*> get

        put Project{..} =
            do
                put (fst projectName)
                put (fst projectAbsoluteRootDirectory)
                put (fst projectRelativeSourceDirectory)
                put (fst projectRelativeCompiledDirectory)
                put (fst projectSourceFiles)



data ProjectLang k
    where

        GetProjectName
            :: (ProjectName -> k)
            -> ProjectLang k

        SetProjectName
            :: ProjectName
            -> k
            -> ProjectLang k

        GetProjectAbsoluteRootDirectory
            :: (ProjectDirectory -> k)
            -> ProjectLang k

        GetProjectRelativeSourceDirectory
            :: (FilePath -> k)
            -> ProjectLang k

        GetProjectRelativeCompiledDirectory
            :: (FilePath -> k)
            -> ProjectLang k

        GetProjectSourceFiles
            :: ([(FilePath,Hash)] -> k)
            -> ProjectLang k

        AddProjectSourceFile
            :: FilePath
            -> k
            -> ProjectLang k



getProjectName =
    say (GetProjectName id)

setProjectName newProjectName =
    say (SetProjectName newProjectName ())

getProjectDirectory =
    say (GetProjectAbsoluteRootDirectory id)

getSourceDirectory =
    say (GetProjectRelativeSourceDirectory id)

getCompiledDirectory =
    say (GetProjectRelativeCompiledDirectory id)

getSourceFiles =
    say (GetProjectSourceFiles id)

addSource newRelativeSourceFile =
    say (AddProjectSourceFile newRelativeSourceFile ())



instance Inflection Project ProjectLang where

    inflect use Project{..} (GetProjectName pnk) =
        inflect use projectName pnk

    inflect use Project{..} (SetProjectName newProjectName k) =
        inflect use changeProjectName (newProjectName,k)

    inflect use Project{..} (GetProjectAbsoluteRootDirectory dirk) =
        inflect use projectAbsoluteRootDirectory dirk

    inflect use Project{..} (GetProjectRelativeSourceDirectory dirk) =
        inflect use projectRelativeSourceDirectory dirk

    inflect use Project{..} (GetProjectSourceFiles srcsk) =
        inflect use projectSourceFiles srcsk

    inflect use Project{..} (AddProjectSourceFile newRelativeSourceFile k) =
        inflect use addProjectSourceFile (newRelativeSourceFile,k)



newDefaultProject name rootDirectory =
    createProject name rootDirectory "src" ".ef" []



createProject name rootDirectory sourceDirectory compiledDirectory sourceFiles =
    Project
        {
          projectName =
              projectName_

        , changeProjectName =
              changeProjectName_

        , projectAbsoluteRootDirectory =
              projectAbsoluteRootDirectory_

        , projectRelativeSourceDirectory =
              projectRelativeSourceDirectory_

        , projectRelativeCompiledDirectory =
              projectRelativeCompiledDirectory_

        , projectSourceFiles =
              projectSourceFiles_

        , addProjectSourceFile =
              addProjectSourceFile_
        }
    where

        projectName_ =
            (name,pure)

        changeProjectName_ newName fs =
            let
                project = view fs

            in
                pure $ fs .= project { projectName = (name,pure) }

        projectAbsoluteRootDirectory_ =
            (rootDirectory,pure)

        projectRelativeSourceDirectory_ =
            (sourceDirectory,pure)

        projectRelativeCompiledDirectory_ =
            (compiledDirectory,pure)

        projectSourceFiles_ =
            (sourceFiles,pure)

        addProjectSourceFile_ newRelativeFile fs =
            do
                let
                    Project{..} =
                        view fs

                    root =
                        fst projectAbsoluteRootDirectory

                    newAbsoluteFile =
                        root </> newRelativeFile

                newFileHash <- lift (getFileHash newAbsoluteFile)
                let
                    fileAndHash =
                        (newAbsoluteFile,newFileHash)

                    newProjectSourceFiles =
                        fileAndHash:fst projectSourceFiles

                    newProject =
                       Project { projectSourceFiles = (newProjectSourceFiles,pure), .. }

                return $
                    fs .= newProject

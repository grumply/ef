{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Runtime.Before.Studio
    ( createStudio
    , defaultStudio
    , withProjects
    ) where



import Ef.Core.Object
import Ef.Core.Narrative
import Ef.Core.Inflect
import Ef.Lang.IO

import Ef.Runtime.Before.Project

import Data.Binary
import Data.List
import Data.Typeable
import GHC.Generics
import System.FilePath



data Studio k =
    Studio
        {
          projects
              :: ([Object '[Project] IO],k)

        , setProjects
              :: [Object '[Project] IO]
              -> k

        }



instance ( Lift IO environment
         , Has Studio contexts environment
         )
        => Binary (Studio (Morphism contexts environment))
    where

        get =
            createStudio <$> get

        put Studio {..} =
            put (fst projects)



data StudioLang k
    where

        GetProjects
            :: ([Object '[Project] IO] -> k)
            -> StudioLang k

        PutProjects
            :: ([Object '[Project] IO],k)
            -> StudioLang k



getProjects
    :: ( Lift IO environment
       , Knows StudioLang lexicon environment
       )
    => Narrative lexicon environment [Object '[Project] IO]

getProjects =
    say (GetProjects id)



putProjects
    :: ( Lift IO environment
       , Knows StudioLang lexicon environment
       )
    => [Object '[Project] IO]
    -> Narrative lexicon environment ()

putProjects prjs =
    say (PutProjects (prjs,()))



withProjects
    :: ( Lift IO environment
       , Knows StudioLang lexicon environment
       )
    => ([Object '[Project] IO] -> Narrative lexicon environment ([Object '[Project] IO],result))
    -> Narrative lexicon environment result

withProjects useProjects =
    do
        prjs <- getProjects
        (newProjects,result) <- useProjects prjs
        putProjects newProjects
        return result


addProject
    :: ( Lift IO environment
       , Knows StudioLang lexicon environment
       )
    => Object '[Project] IO
    -> Narrative lexicon environment ()

addProject newProject =
    withProjects (\projects -> return $ (nub $ newProject:projects,()))



instance Inflection Studio StudioLang
    where

        inflect use Studio{..} (GetProjects psk) =
            inflect use projects psk

        inflect use Studio{..} (PutProjects psk) =
            inflect use setProjects psk



defaultStudio =
    createStudio []


createStudio
    :: Lift IO environment
    => [Object '[Project] IO]
    -> Use Studio contexts environment

createStudio initialProjects =
    Studio { projects = (initialProjects,pure), .. }
    where

        setProjects newProjects fs =
            let
                studio = view fs

            in
                pure $ fs .=
                    studio { projects = (newProjects,pure) }

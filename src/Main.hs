{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Ef hiding (Object(..),Attribute,Method)
import qualified Ef as Ef
import Ef.Core.Object hiding (Object(..),Attribute,Method)
import qualified Ef.Core.Object as Ef
import Ef.Core.Pattern

import Language.Haskell.TH hiding (reify)
import qualified Language.Haskell.TH as TH

{-

DSL-directed design of auto-gen library:

-- Notes:
--     'From' means brining data into program scope from an external source
--     'To' means to convert internal data from one format to another.

-}



q
    :: Lift Q parent
    => Q result
    -> Pattern scope parent result

q =
    lift



reify
    :: Lift Q parent
    => Name
    -> Pattern scope parent Info

reify nm =
    q (TH.reify nm)



data Openness
    where

        Open
            :: Openness

        Closed
            :: Openness



data Method =
    Method
        {
          methodParameters
              :: [Dec]

        , methodResults
              :: [Dec]

        , methodOpenness
              :: Openness

        }

data Attribute =
    Attribute
        {
          attributeParameters
              :: [Dec]

        , attributeResults
              :: [Dec]

        , attributeOpenness
              :: Openness

        }


data Object =
    Object
        {
          objectComponents
              :: [Attribute]

        , objectOpenness
              :: Openness

        }


data Language =
    Language
        {
          languageComponents
              :: [Method]

        , languageOpenness
              :: Openness

        }



data Reification k
    where

        AttributeFromType
            :: Openness
            -> Name
            -> Type
            -> (Attribute -> k)
            -> Reification k

        MethodFromType
            :: Openness
            -> Name
            -> Type
            -> (Method -> k)
            -> Reification k

        ObjectFromTypes
            :: Openness
            -> [(Name,Type)]
            -> (Object -> k)
            -> Reification k

        LanguageFromTypes
            :: Openness
            -> [(Name,Type)]
            -> (Language -> k)
            -> Reification k

data AttributeReificationFailure 
    where
    
        NameNotValidAttribute
            :: Loc
            -> Name
            -> Info
            -> AttributeReificationFailure
    
    deriving (Show)
instance Exception AttributeReificationFailure



attributeFromType
    :: ( Lift Q parent
       , Is Reification scope parent
       )
    => Openness
    -> Name
    -> Pattern scope parent Attribute

attributeFromType openness name =
    do
        info <- reify name
        case info of
            
            TyVarI name ty ->
                self (AttributeFromType openness name ty id)

            _ ->
                do
                    loc <- q location
                    throw (NameNotValidAttribute loc name info)



data MethodReificationFailure
    where
    
        NameNotValidMethod
            :: Loc
            -> Name
            -> Info
            -> MethodReificationFailure


    deriving (Show)
instance Exception MethodReificationFailure



methodFromType
    :: ( Lift Q parent
       , Is Reification scope parent
       )
    => Openness
    -> Name
    -> Pattern scope parent Method

methodFromType openness name =
    do
        info <- reify name
        case info of
            
            TyVarI name ty ->
                self (MethodFromType openness name ty id)

            _ ->
                do
                    loc <- q location
                    throw (NameNotValidMethod loc name info)



objectFromTypes
    :: ( Lift Q parent
       , Is Reification scope parent
       )
    => Openness
    -> [Name]
    -> Pattern scope parent Object

objectFromTypes openness names =
    do
        self (ObjectFromTypes openness undefined id)
        undefined



languageFromTypes
    :: ( Lift Q parent
       , Is Reification scope parent
       )
    => Openness
    -> [Name]
    -> Pattern scope parent Language

languageFromTypes openness names =
    do
        self (LanguageFromTypes openness undefined id)
        undefined



data Reifier k =
    Reifier
        {
          _attributeFromType
              :: Openness
              -> Name
              -> Type
              -> (Attribute,k)

        , _methodFromType
              :: Openness
              -> Name
              -> Type
              -> (Method,k)

        , _objectFromTypes
              :: Openness
              -> [(Name,Type)]
              -> (Object,k)

        , _languageFromTypes
              :: Openness
              -> [(Name,Type)]
              -> (Language,k)

        }



instance Witnessing Reifier Reification
    where

        witness use Reifier{..} (AttributeFromType openness name ty atk) =
            let
                (at,k) =
                    _attributeFromType openness name ty

                k' =
                    atk at

            in
                use k k'

        witness use Reifier{..} (MethodFromType openness name ty mtk) =
            let
                (mt,k) =
                    _methodFromType openness name ty

                k' =
                    mtk mt

            in
                use k k'

        witness use Reifier{..} (ObjectFromTypes openness nameTypes otk) =
            let
                (ot,k) =
                    _objectFromTypes openness nameTypes

                k' =
                    otk ot

            in
                use k k'


        witness use Reifier{..} (LanguageFromTypes openness nameTypes ltk) =
            let
                (lt,k) =
                    _languageFromTypes openness nameTypes

                k' =
                    ltk lt

            in
                use k k'




data Conversion k
    where

        MethodToAttribute
            :: ( Openness
               , Method
               )
            -> Openness
            -> (Attribute -> k)
            -> Conversion k

        AttributeToMethod
            :: ( Openness
               , Attribute
               )
            -> Openness
            -> (Method -> k)
            -> Conversion k

        ObjectToLanguage
            :: ( Openness
               , Object
               )
            -> Openness
            -> (Language -> k)
            -> Conversion k

        LanguageToObject
            :: ( Openness
               , Language
               )
            -> Openness
            -> (Object -> k)
            -> Conversion k


methodToAttribute
    :: ( Is Conversion scope parent
       , Lift Q parent
       )
    => ( Openness
       , Method
       )
    -> Openness
    -> Pattern scope parent Attribute

methodToAttribute omt o =
    self (MethodToAttribute omt o id)



attributeToMethod
    :: ( Is Conversion scope parent
       , Lift Q parent
       )
    => ( Openness
       , Attribute
       )
    -> Openness
    -> Pattern scope parent Method

attributeToMethod oat o =
    self (AttributeToMethod oat o id)



objectToLanguage
    :: ( Is Conversion scope parent
       , Lift Q parent
       )
    => ( Openness
       , Object
       )
    -> Openness
    -> Pattern scope parent Language

objectToLanguage oot o =
    self (ObjectToLanguage oot o id)



languageToObject
    :: ( Is Conversion scope parent
       , Lift Q parent
       )
    => ( Openness
       , Language
       )
    -> Openness
    -> Pattern scope parent Object

languageToObject olt o =
    self (LanguageToObject olt o id)



data Converter k =
    Converter
        {
          _methodToAttribute
              :: ( Openness
                 , Method
                 )
              -> Openness
              -> (Attribute,k)

        , _attributeToMethod
              :: ( Openness
                 , Attribute
                 )
              -> Openness
              -> (Method,k)

        , _objectToLanguage
              :: ( Openness
                 , Object
                 )
              -> Openness
              -> (Language,k)

        , _languageToObject
              :: ( Openness
                 , Language
                 )
              -> Openness
              -> (Object,k)
        }

instance Witnessing Converter Conversion
    where

        witness use Converter{..} (MethodToAttribute omt o atk) =
            let
                (at,k) =
                    _methodToAttribute omt o

                k' =
                    atk at

            in
                use k k'

        witness use Converter{..} (AttributeToMethod oat o mtk) =
            let
                (mt,k) =
                    _attributeToMethod oat o

                k' =
                    mtk mt

            in
                use k k'

        witness use Converter{..} (ObjectToLanguage oot o ltk) =
            let
                (lt,k) =
                    _objectToLanguage oot o

                k' =
                    ltk lt

            in
                use k k'

        witness use Converter{..} (LanguageToObject olt o otk) =
            let
                (ot,k) =
                    _languageToObject olt o

                k' =
                    otk ot

            in
                use k k'



reifier
    :: ( Uses Reifier attrs parent
       , Lift Q parent
       )
    => Ef.Attribute Reifier attrs parent

reifier =
    let
        _attributeFromType =
            \openness name ->
                undefined

        _methodFromType =
            \openness name ->
                undefined

        _objectFromTypes =
            \openness names ->
                undefined

        _languageFromTypes =
            \openness names ->
                undefined

    in
        Reifier{..}



converter
    :: ( Uses Converter attrs parent
       , Lift Q parent
       )
    => Ef.Attribute Converter attrs parent

converter =
    let
        _methodToAttribute =
            \opennessAndMethod desiredOpenness ->
                undefined

        _attributeToMethod =
            \opennessAndAttribute desiredOpenness ->
                undefined

        _objectToLanguage =
            \opennessAndObject desiredOpenness ->
                undefined

        _languageToObject =
            \opennessAndLanguage desiredOpenness ->
                undefined

    in
        Converter{..}

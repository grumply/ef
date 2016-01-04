{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Ef hiding (Object(..),Attribute,Method)
import qualified Ef as Ef
import Ef.Core.Object hiding (Object(..),Attribute,Method)
import qualified Ef.Core.Object as Ef
import Ef.Core.Pattern

import Language.Haskell.TH

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
            -> (Attribute -> k)
            -> Reification k

        MethodFromType
            :: Openness
            -> Name
            -> (Method -> k)
            -> Reification k

        ObjectFromTypes
            :: Openness
            -> [Name]
            -> (Object -> k)
            -> Reification k

        LanguageFromTypes
            :: Openness
            -> [Name]
            -> (Language -> k)
            -> Reification k



attributeFromType
    :: ( Lift Q parent
       , Is Reification scope parent
       )
    => Openness
    -> Name
    -> Pattern scope parent Attribute

attributeFromType openness name =
    self (AttributeFromType openness name id)



methodFromType
    :: ( Lift Q parent
       , Is Reification scope parent
       )
    => Openness
    -> Name
    -> Pattern scope parent Method

methodFromType openness name =
    self (MethodFromType openness name id)



objectFromTypes
    :: ( Lift Q parent
       , Is Reification scope parent
       )
    => Openness
    -> [Name]
    -> Pattern scope parent Object

objectFromTypes openness names =
    self (ObjectFromTypes openness names id)



languageFromTypes
    :: ( Lift Q parent
       , Is Reification scope parent
       )
    => Openness
    -> [Name]
    -> Pattern scope parent Language

languageFromTypes openness names =
    self (LanguageFromTypes openness names id)



data Reifier k =
    Reifier
        {
          _attributeFromType
              :: Openness
              -> Name
              -> (Attribute,k)

        , _methodFromType
              :: Openness
              -> Name
              -> (Method,k)

        , _objectFromTypes
              :: Openness
              -> [Name]
              -> (Object,k)

        , _languageFromTypes
              :: Openness
              -> [Name]
              -> (Language,k)

        }



instance Witnessing Reifier Reification
    where

        witness use Reifier{..} (AttributeFromType openness name atk) =
            let
                (at,k) =
                    _attributeFromType openness name

                k' =
                    atk at

            in
                use k k'

        witness use Reifier{..} (MethodFromType openness name mtk) =
            let
                (mt,k) =
                    _methodFromType openness name

                k' =
                    mtk mt

            in
                use k k'

        witness use Reifier{..} (ObjectFromTypes openness names otk) =
            let
                (ot,k) =
                    _objectFromTypes openness names

                k' =
                    otk ot

            in
                use k k'


        witness use Reifier{..} (LanguageFromTypes openness names ltk) =
            let
                (lt,k) =
                    _languageFromTypes openness names

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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Core.Narrative.Lexeme where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat



data Lexeme lexicon a
  where

    Lexeme
        :: Denies lexeme rest
        => lexeme a
        -> Lexeme (lexeme ': rest) a

    Further
        :: Denies lexeme rest
        => Lexeme rest a
        -> Lexeme (lexeme ': rest) a



class Grow small large
  where

    grow
        :: Lexeme small a
        -> Lexeme large a



instance Grow '[] '[]



instance ( Subset small large
         , small ~ (lexeme ': lexemes)
         , Allows lexeme large
         , Grow lexemes large
         )
    => Grow small large
  where

    grow (Further more) =
        grow more

    grow (Lexeme sa) =
        inj sa



instance Functor (Lexeme '[])



instance ( Functor lexeme
         , Functor (Lexeme lexemes')
         , lexemes ~ (lexeme ': lexemes')
         )
    => Functor (Lexeme lexemes)
  where

    fmap f (Lexeme lexeme) =
        Lexeme (fmap f lexeme)

    fmap f (Further lexemes') =
        Further (fmap f lexemes')



class Allows lexeme lexemes
  where

    inj
        :: lexeme a
        -> Lexeme lexemes a



    prj
        :: Lexeme lexemes a
        -> Maybe (lexeme a)



instance ( index ~ IndexOf lexeme lexemes
         , Allows' lexeme lexemes index
         )
    => Allows lexeme lexemes
  where

    inj lexeme =
        let
          index =
              Index :: Index (IndexOf lexeme lexemes)

        in
          inj' index lexeme



    prj lexemes =
        let
          index =
              Index :: Index (IndexOf lexeme lexemes)

        in
          prj' index lexemes



class Allows' lexeme lexemes (n :: Nat)
  where

    inj'
        :: Index n
        -> lexeme a
        -> Lexeme lexemes a

    prj'
        :: Index n
        -> Lexeme lexemes a
        -> Maybe (lexeme a)



instance ( Denies lexeme' lexemes'
         , lexemes ~ (lexeme' ': lexemes')
         , index ~ IndexOf lexeme lexemes'
         , Allows' lexeme lexemes' index
         )
    => Allows' lexeme lexemes ('S n)
  where

    inj' _ lexeme =
        let
          index =
              Index :: Index (IndexOf lexeme lexemes')

        in
          Further (inj' index lexeme)



    prj' _ (Further lexemes) =
        let
          index =
              Index :: Index (IndexOf lexeme lexemes')

        in
          prj' index lexemes

    prj' _ _ =
        Nothing



instance ( Denies lexeme lexemes'
         , lexemes ~ (lexeme ': lexemes')
         )
    => Allows' lexeme lexemes 'Z
  where

    inj' _ =
        Lexeme



    prj' _ (Lexeme lexeme) =
        Just lexeme

    prj' _ (Further _) =
        Nothing

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
module Ef.Core.Narrative.Lexicon where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat



data Lexicon lexicon a
  where

    Lexeme
        :: Denies lexeme rest
        => lexeme a
        -> Lexicon (lexeme ': rest) a

    Further
        :: Denies lexeme rest
        => Lexicon rest a
        -> Lexicon (lexeme ': rest) a



class Grow small large
  where

    grow
        :: small a
        -> large a



instance Grow (Lexicon '[]) (Lexicon '[])



instance ( Subset small large
         , small ~ (lexeme ': lexemes)
         , Can lexeme large
         , Grow (Lexicon lexemes) (Lexicon large)
         )
    => Grow (Lexicon small) (Lexicon large)
  where

    grow (Further more) =
        grow more

    grow (Lexeme sa) =
        inj sa



instance Functor (Lexicon '[])



instance ( Functor lexeme
         , Functor (Lexicon lexemes')
         , lexemes ~ (lexeme ': lexemes')
         )
    => Functor (Lexicon lexemes)
  where

    fmap f (Lexeme lexeme) =
        Lexeme (fmap f lexeme)

    fmap f (Further lexemes') =
        Further (fmap f lexemes')



class Can lexeme lexemes
  where

    inj
        :: lexeme a
        -> Lexicon lexemes a



    prj
        :: Lexicon lexemes a
        -> Maybe (lexeme a)



instance ( index ~ IndexOf lexeme lexemes
         , Can' lexeme lexemes index
         )
    => Can lexeme lexemes
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



class Can' lexeme lexemes (n :: Nat)
  where

    inj'
        :: Index n
        -> lexeme a
        -> Lexicon lexemes a

    prj'
        :: Index n
        -> Lexicon lexemes a
        -> Maybe (lexeme a)



instance ( Denies lexeme' lexemes'
         , lexemes ~ (lexeme' ': lexemes')
         , index ~ IndexOf lexeme lexemes'
         , Can' lexeme lexemes' index
         )
    => Can' lexeme lexemes ('S n)
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
    => Can' lexeme lexemes 'Z
  where

    inj' _ =
        Lexeme



    prj' _ (Lexeme lexeme) =
        Just lexeme

    prj' _ (Further _) =
        Nothing

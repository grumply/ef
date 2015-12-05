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
module Ef.Core.Pattern.Symbols where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat



data Symbol symbols a
  where

    Symbol
        :: Denies symbol ss
        => symbol a
        -> Symbol (symbol ': ss) a

    Further
        :: Denies symbol ss
        => Symbol ss a
        -> Symbol (symbol ': ss) a



class Cast small large
  where

    cast
        :: Symbol small a
        -> Symbol large a



instance Cast '[] '[]



instance ( Subset small large
         , small ~ (symbol ': symbols)
         , Allows symbol large
         , Cast symbols large
         )
    => Cast small large
  where

    cast (Further more) =
        cast more

    cast (Symbol sa) =
        inj sa



instance Functor (Symbol '[])



instance ( Functor symbol
         , Functor (Symbol symbols)
         , syms ~ (symbol ': symbols)
         )
    => Functor (Symbol syms)
  where

    fmap f (Symbol symbol) =
        Symbol (fmap f symbol)

    fmap f (Further symbols) =
        Further (fmap f symbols)



class Allows symbol symbols
  where

    inj
        :: symbol a
        -> Symbol symbols a



    prj
        :: Symbol symbols a
        -> Maybe (symbol a)



instance ( index ~ IndexOf symbol symbols
         , Allows' symbol symbols index
         )
    => Allows symbol symbols
  where

    inj symbol =
        let
          index =
              Index :: Index (IndexOf symbol symbols)

        in
          inj' index symbol



    prj symbols =
        let
          index =
              Index :: Index (IndexOf symbol symbols)

        in
          prj' index symbols



class Allows' symbol symbols (n :: Nat)
  where

    inj'
        :: Index n
        -> symbol a
        -> Symbol symbols a

    prj'
        :: Index n
        -> Symbol symbols a
        -> Maybe (symbol a)



instance ( Denies symbol' symbols'
         , symbols ~ (symbol' ': symbols')
         , index ~ IndexOf symbol symbols'
         , Allows' symbol symbols' index
         )
    => Allows' symbol symbols ('S n)
  where

    inj' _ symbol =
        let
          index =
              Index :: Index (IndexOf symbol symbols')

        in
          Further (inj' index symbol)



    prj' _ (Further symbols) =
        let
          index =
              Index :: Index (IndexOf symbol symbols')

        in
          prj' index symbols

    prj' _ _ =
        Nothing



instance ( Denies symbol symbols'
         , symbols ~ (symbol ': symbols')
         )
    => Allows' symbol symbols 'Z
  where

    inj' _ =
        Symbol



    prj' _ (Symbol symbol) =
        Just symbol

    prj' _ (Further _) =
        Nothing



type Is symbol symbols parent =
    ( Monad parent
    , Allows' symbol symbols (IndexOf symbol symbols)
    )



type (symbol :< symbols) parent =
    Is symbol symbols parent



type Invokes symbols' symbols parent =
    ( Monad parent
    , AllowsSubset symbols' symbols
    )



class AllowsSubset (symbols' :: [* -> *]) (symbols :: [* -> *])



instance AllowsSubset '[] symbols



instance ( index ~ IndexOf symbol symbols
         , Allows' symbol symbols index
         , AllowsSubset symbols' symbols
         )
  => AllowsSubset (symbol ': symbols') symbols

{-# LANGUAGE TypeFamilies #-}
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
{-# LANGUAGE Safe #-}
module Ef.Messages where



import Ef.Set
import Ef.Nat


data Messages messages a
  where

    Message
        :: Denies message rest
        => message a
        -> Messages (message ': rest) a

    Further
        :: Denies message rest
        => Messages rest a
        -> Messages (message ': rest) a



class Upcast small large
  where

    upcast
        :: small a
        -> large a



instance Upcast (Messages '[]) (Messages '[])



instance ( Subset small large
         , small ~ (message ': messages)
         , Can message large
         , Upcast (Messages messages) (Messages large)
         )
    => Upcast (Messages small) (Messages large)
  where

    upcast (Further more) =
        upcast more

    upcast (Message sa) =
        inj sa



instance Functor (Messages '[])



instance ( Functor message
         , Functor (Messages messages')
         , messages ~ (message ': messages')
         )
    => Functor (Messages messages)
  where

    fmap f (Message message) =
        Message (fmap f message)

    fmap f (Further messages') =
        Further (fmap f messages')



type family Subset (messages :: [* -> *]) messages' where

    Subset (message ': '[]) messages' =
        (Can message messages')

    Subset (message ': messages) messages' =
        ( Can message messages'
        , Subset messages messages'
        )



class Can message messages
  where

    inj
        :: message a
        -> Messages messages a



    prj
        :: Messages messages a
        -> Maybe (message a)



instance ( index ~ Offset message messages
         , Can' message messages index
         )
    => Can message messages
  where

    inj message =
        let
          index =
              Index :: Index (Offset message messages)

        in
          inj' index message



    prj messages =
        let
          index =
              Index :: Index (Offset message messages)

        in
          prj' index messages



class Can' message messages (n :: Nat)
  where

    inj'
        :: Index n
        -> message a
        -> Messages messages a

    prj'
        :: Index n
        -> Messages messages a
        -> Maybe (message a)



instance ( Denies message' messages'
         , messages ~ (message' ': messages')
         , index ~ Offset message messages'
         , Can' message messages' index
         )
    => Can' message messages ('S n)
  where

    inj' _ message =
        let
          index =
              Index :: Index (Offset message messages')

        in
          Further (inj' index message)



    prj' _ (Further messages) =
        let
          index =
              Index :: Index (Offset message messages')

        in
          prj' index messages

    prj' _ _ =
        Nothing



instance ( Denies message messages'
         , messages ~ (message ': messages')
         )
    => Can' message messages 'Z
  where

    inj' _ =
        Message



    prj' _ (Message message) =
        Just message

    prj' _ (Further _) =
        Nothing

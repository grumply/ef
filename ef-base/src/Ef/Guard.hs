{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Guard
    ( Guarding
    , guards
    , Guardable
    , guarder
    , Guard(..)
    ) where



import Ef

import Data.Foldable
import Unsafe.Coerce


data Guarding k
    = FreshSelf (Int -> k)
    | forall a. Choose Int [a] (a -> k)
    | Cut Int


data Guard self super = Guard
    { choose :: forall f a. Foldable f => f a -> Narrative self super a
    , cut :: forall b. Narrative self super b
    }


data Guardable k = Guardable Int k


guarder =
    Guardable 0 $ \fs ->
        let Guardable i k = view fs
        in return $ fs .= Guardable (succ i) k


instance Ma Guardable Guarding where
    ma use (Guardable i k) (FreshSelf ik) = use k (ik i)


guards :: Knows Guarding self super
       => (Guard self super -> Narrative self super result)
       -> Narrative self super (Maybe result)
guards l = do
    scope <- self (FreshSelf id)
    transform (rewrite scope) $ Just <$> l Guard
        { choose = \foldable -> let list = toList foldable
                                in self (Choose scope list id)
        , cut = self (Cut scope)
        }


rewrite :: Knows Guarding self super
        => Int
        -> Messages self x
        -> (x -> Narrative self super (Maybe result))
        -> Narrative self super (Maybe result)

rewrite scope message k =
    let ignore = Say message (transform (rewrite scope) . k)
        check i scoped = if i == scope then scoped else ignore
    in case prj message of
          Just x ->
              case x of
                  Choose i as _ ->
                      check i $
                          choosing scope as (unsafeCoerce k) (return Nothing)
                  _ -> ignore
          Nothing -> Say message (transform (rewrite scope) . k)


choosing
    :: Knows Guarding self super
    => Int
    -> [a]
    -> (a -> Narrative self super r)
    -> Narrative self super r
    -> Narrative self super r
choosing _ [] _ alt =
    alt

choosing self (a:as) bp alt =
    transform (nestedChoosing self as alt bp) (bp a)



nestedChoosing
    :: Knows Guarding self super
    => Int
    -> [a]
    -> Narrative self super result
    -> (a -> Narrative self super result)
    -> Messages self x
    -> (x -> Narrative self super result)
    -> Narrative self super result

nestedChoosing scope choices alt superContinue message childContinue =
    let
      ignore =
          Say message (transform (nestedChoosing scope choices alt superContinue) . childContinue)

      check i scoped = if i == scope then scoped else ignore

    in case prj message of

           Just x ->
               case x of

                   Choose i nestedChoices _ ->
                       check i $
                           choosing scope (unsafeCoerce nestedChoices) (unsafeCoerce childContinue)
                               (choosing scope choices superContinue alt)

                   Cut i -> check i $ choosing scope choices superContinue alt

                   _ -> ignore
           Nothing -> ignore


{-# INLINE guarder #-}
{-# INLINE guards #-}

module Ef.Guard
    ( Guard
    , guard
    , Guards(..)
    , guards
    ) where

import Ef

import Data.Foldable
import Unsafe.Coerce

data Guard k
    = Guard Int k
    | FreshSelf (Int -> k)
    | forall a. Choose Int [a] (a -> k)
    | Cut Int

instance Ma Guard Guard where
    ma use (Guard i k) (FreshSelf ik) = use k (ik i)

data Guards self super = Guards
    { choose :: forall f a. Foldable f => f a -> Narrative self super a
    , cut :: forall b. Narrative self super b
    }

guard :: (Monad super, '[Guard] .> traits)
      => Trait Guard traits super
guard =
    Guard 0 $ \fs ->
        let Guard i k = view fs
        in return $ fs .= Guard (succ i) k
{-# INLINE guard #-}

guards :: (Monad super, '[Guard] :> self)
       => (Guards self super -> Narrative self super result)
       -> Narrative self super (Maybe result)
guards l = do
    scope <- self (FreshSelf id)
    transform (rewrite scope) $ Just <$> l Guards
        { choose = \foldable -> let list = toList foldable
                                in self (Choose scope list id)
        , cut = self (Cut scope)
        }
{-# INLINE guards #-}

rewrite :: (Monad super, '[Guard] :> self)
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
    :: (Monad super, '[Guard] :> self)
    => Int
    -> [a]
    -> (a -> Narrative self super r)
    -> Narrative self super r
    -> Narrative self super r
choosing _ [] _ alt = alt
choosing scope (a:as) bp alt =
    transform (nestedChoosing scope as alt bp) (bp a)

nestedChoosing
    :: (Monad super, '[Guard] :> self)
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

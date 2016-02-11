{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
module Ef.Codensity where



import Ef.Narrative


import Control.Applicative
import Control.Monad


newtype Codensity messages super result =

    Codensity
        { runCodensity
              :: forall b.
                 (result -> Narrative messages super b)
              -> Narrative messages super b
        }



instance Functor (Codensity messages super)
  where

    -- fmap a function, f, over a Codensity computation by unwrapping the
    -- computation and applying it to a continuation composed with f and
    -- rewrapping.
    fmap f (Codensity computation) =
        let
          compose continue =
              computation (continue . f)

        in
          Codensity compose



instance Applicative (Codensity messages super)
  where

    -- lift a value, x, into a Codensity computation by applying a
    -- continuation to the value and wrapping in Codensity.
    pure x =
        let
          computation continue =
              continue x
        
        in
          Codensity computation



    (<*>) =
        ap



instance Monad (Codensity messages super)
  where

    return =
        pure


    {-# INLINE (>>=) #-}
    (>>=)
        :: forall firstResult secondResult.
           Codensity messages super firstResult
        -> (    firstResult
             -> Codensity messages super secondResult
           )
        -> Codensity messages super secondResult

    one >>= twoFrom =
        Codensity composed
      where

        -- compose two steps of a Codensity computation
        composed
            :: forall thirdResult.
               (    secondResult
                 -> Narrative messages super thirdResult
               )
            -> Narrative messages super thirdResult

        composed threeFrom =
            runCodensity one (two threeFrom)
          where

            -- build the second step of a computation with:
            --     1. the first result
            --     2. a continuation of the second result
            two
                :: (    secondResult
                     -> Narrative messages super thirdResult
                   )
                -> firstResult
                -> Narrative messages super thirdResult

            two threeFrom firstResult =
                runCodensity (twoFrom firstResult) threeFrom



instance ( Alternative super
         , MonadPlus super
         )
    => Alternative (Codensity messages super)
  where

    empty =
        Codensity (const empty)



    Codensity m <|> Codensity n =
        Codensity $ \k ->
            m k <|> n k



instance MonadPlus super
    => MonadPlus (Codensity messages super)
  where

    mzero =
        Codensity (const mzero)



    Codensity m `mplus` Codensity n =
        Codensity $ \k ->
            m k `mplus` n k


{-# INLINE toCodensity #-}
toCodensity
    :: Monad super
    => Narrative messages super result
    -> Codensity messages super result

toCodensity f =
    Codensity (f >>=)


{-# INLINE fromCodensity #-}
fromCodensity
    :: Monad super
    => Codensity messages super result
    -> Narrative messages super result

fromCodensity a =
    runCodensity a return

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
module Ef.Codensity where



import Ef.Narrative


import Control.Applicative
import Control.Monad



--------------------------------------------------------------------------------
-- | Codensity improves asymptotics of repeated left-associated binds by
--   conversion to right-associated binds.
--
-- This example demonstrates a performance improvement over the standard
-- replicateM.
--
-- >>> import Data.Time
-- >>> import System.Timeout
-- >>> :{
--   let
--     time f =
--         do
--           s <- getCurrentTime
--           r <- f
--           e <- r `seq` getCurrentTime
--           return (diffUTCTime e s,r)
--
--     replicateP n =
--         fromCodensity . Control.Monad.replicateM n . toCodensity
--
--   in
--     do
--       let
--         unit =
--             return ()
--
--         test f =
--             run (f 10 unit)
--
--       mResult <- time (run replicateM)
--       pResult <- time (run replicateP)
--       let
--         (conventionalTime,mRes) =
--             mResult
--
--         (codensityTime,pRes) =
--             pResult
--
--       return (conventionalTime - codensityTime > 0)
--
-- :}
--True

-- | Codensity hides intermediate results by way of composition. That is,
-- (>>=), for Codesnity, ties the result of a computation into a continuation
-- of that result to create a wrapped sequence of computational steps.
-- The wrapping enables the compiler to see that the intermediate results
-- cannot be inspected - modulo 'unsafeCoerce'. Some (all?) Free monad
-- encodings are amenable to a Codensity representation and Narrative is a
-- Free monad representation.
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



toCodensity
    :: Monad super
    => Narrative messages super result
    -> Codensity messages super result

toCodensity f =
    Codensity (f >>=)



fromCodensity
    :: Monad super
    => Codensity messages super result
    -> Narrative messages super result

fromCodensity a =
    runCodensity a return

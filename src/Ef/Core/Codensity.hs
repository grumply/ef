{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Core.Codensity where



import Ef.Core.Narrative


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
newtype Codensity lexicon environment result =

    Codensity
        { runCodensity
              :: forall b.
                 (result -> Narrative lexicon environment b)
              -> Narrative lexicon environment b
        }



instance Functor (Codensity lexicon environment)
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



instance Applicative (Codensity lexicon environment)
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



instance Monad (Codensity lexicon environment)
  where

    return =
        pure



    (>>=)
        :: forall firstResult secondResult.
           Codensity lexicon environment firstResult
        -> (    firstResult
             -> Codensity lexicon environment secondResult
           )
        -> Codensity lexicon environment secondResult

    one >>= twoFrom =
        Codensity composed
      where

        -- compose two steps of a Codensity computation
        composed
            :: forall thirdResult.
               (    secondResult
                 -> Narrative lexicon environment thirdResult
               )
            -> Narrative lexicon environment thirdResult

        composed threeFrom =
            runCodensity one (two threeFrom)
          where

            -- build the second step of a computation with:
            --     1. the first result
            --     2. a continuation of the second result
            two
                :: (    secondResult
                     -> Narrative lexicon environment thirdResult
                   )
                -> firstResult
                -> Narrative lexicon environment thirdResult

            two threeFrom firstResult =
                runCodensity (twoFrom firstResult) threeFrom



instance ( Alternative environment
         , MonadPlus environment
         )
    => Alternative (Codensity lexicon environment)
  where

    empty =
        Codensity (const empty)



    Codensity m <|> Codensity n =
        Codensity $ \k ->
            m k <|> n k



instance MonadPlus environment
    => MonadPlus (Codensity lexicon environment)
  where

    mzero =
        Codensity (const mzero)



    Codensity m `mplus` Codensity n =
        Codensity $ \k ->
            m k `mplus` n k



toCodensity
    :: Monad environment
    => Narrative lexicon environment result
    -> Codensity lexicon environment result

toCodensity f =
    Codensity (f >>=)



fromCodensity
    :: Monad environment
    => Codensity lexicon environment result
    -> Narrative lexicon environment result

fromCodensity a =
    runCodensity a return

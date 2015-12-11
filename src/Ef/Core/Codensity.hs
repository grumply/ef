{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Core.Codensity where



import Ef.Core.Pattern


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
-- encodings are amenable to a Codensity representation and Pattern is a
-- Free monad representation.
newtype Codensity scope parent result =

    Codensity
        { runCodensity
              :: forall b.
                 (result -> Pattern scope parent b)
              -> Pattern scope parent b
        }



instance Functor (Codensity scope parent)
  where

    fmap f (Codensity m) =
        Codensity $ m . (. f)



instance Applicative (Codensity scope parent)
  where

    pure x =
        Codensity ($ x)



    (<*>) =
        ap



instance Monad (Codensity scope parent)
  where

    return x =
        Codensity ($ x)



    (>>=)
        :: forall firstResult secondResult.
           Codensity scope parent firstResult
        -> (    firstResult
             -> Codensity scope parent secondResult
           )
        -> Codensity scope parent secondResult

    one >>= twoFrom =
        Codensity composed
      where

        -- compose two steps of a Codensity computation
        composed
            :: forall thirdResult.
               (    secondResult
                 -> Pattern scope parent thirdResult
               )
            -> Pattern scope parent thirdResult

        composed threeFrom =
            runCodensity one (two threeFrom)
          where

            -- build the second step of a computation with:
            --     1. the first result
            --     2. a continuation of the second result
            two
                :: (    secondResult
                     -> Pattern scope parent thirdResult
                   )
                -> firstResult
                -> Pattern scope parent thirdResult

            two threeFrom firstResult =
                runCodensity (twoFrom firstResult) threeFrom



instance ( Alternative parent
         , MonadPlus parent
         )
    => Alternative (Codensity scope parent)
  where

    empty =
        Codensity (const empty)



    Codensity m <|> Codensity n =
        Codensity $ \k ->
            m k <|> n k



instance MonadPlus parent
    => MonadPlus (Codensity scope parent)
  where

    mzero =
        Codensity (const mzero)



    Codensity m `mplus` Codensity n =
        Codensity $ \k ->
            m k `mplus` n k



toCodensity
    :: Monad parent
    => Pattern scope parent result
    -> Codensity scope parent result

toCodensity f =
    Codensity (f >>=)



fromCodensity
    :: Monad parent
    => Codensity scope parent result
    -> Pattern scope parent result

fromCodensity a =
    runCodensity a return

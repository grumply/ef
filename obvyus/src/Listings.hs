{-# language RecordWildCards #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
module Listings where

import Ef
import Ef.Event

import Lotus

import Listing

import Data.Array

import Unsafe.Coerce

data ListingsType = ProvocativeListings | InterestingListings

data Listings (pg :: x) k
    = Listings
          { listingsArray :: forall self super. (Array Int (Object '[Listing] (Narrative self super)),k)
          }
    | forall self super. ListingsArray (Array Int (Object '[Listing] (Narrative self super)) -> k)

listings :: Use (Listings pg) methods super
listings = Listings (unsafeCoerce $ array (1,20) [],return)

provocativeListingsArray :: forall self super.
                            (Monad super, '[Listings ProvocativeListings] <: self)
                         => Narrative self super (Array Int (Object '[Listing] (Narrative self super)))
provocativeListingsArray = self (ListingsArray id :: Listings ProvocativeListings (Array Int (Object '[Listing] (Narrative self super))))

interestingListingsArray :: forall self super.
                            (Monad super, '[Listings InterestingListings] <: self)
                         => Narrative self super (Array Int (Object '[Listing] (Narrative self super)))
interestingListingsArray = self (ListingsArray id :: Listings InterestingListings (Array Int (Object '[Listing] (Narrative self super))))

instance Ma (Listings pg) (Listings pg) where
    ma use Listings{..} (ListingsArray ak) = use (snd listingsArray) (ak $ fst listingsArray)

getProvocativeListing :: (Monad super, '[Listings ProvocativeListings] <: self)
                      => Int -> Narrative self super (Object '[Listing] (Narrative self super))
getProvocativeListing n = do
    pla <- provocativeListingsArray
    return $ pla ! n

getInterestingListing :: (Monad super, '[Listings InterestingListings] <: self)
                      => Int -> Narrative self super (Object '[Listing] (Narrative self super))
getInterestingListing n = do
    ila <- interestingListingsArray
    return $ ila ! n

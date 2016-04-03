{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language PolyKinds #-}
module App (App, CSS, Route, HTML, Component, app) where

import Ef

import Iron as Export

import Silicon
import Magnesium
import Carbon
import Hydrogen
import Carbon
import Oxygen

import Listings

app base = listings *:* listings *:* base

type App = '[
              Listings ProvocativeListings,
              Listings InterestingListings,
             -- Obvyus,
             -- Menu,
             Oxygen,
             Silicon,
             SingleKnot
            ]

type CSS = Narrative '[Carbon] (Narrative App IO)
type HTML = Narrative '[Hydrogen] (Narrative App IO)
type Route = Narrative '[Magnesium] (Narrative App IO)

type Component = Atom App IO

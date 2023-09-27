{-# LANGUAGE DataKinds        #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      : Data.EBird.Client
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Functions that support querying the official [eBird
-- API](https://documenter.getpostman.com/view/664302/S1ENwy59) as defined in
-- the [ebird-api](https://hackage.haskell.org/package/ebird-api) library.

module Data.EBird.Client (
    -- * Execute client functions
    askEBird

    -- * eBird API client functions
    -- ** Observations queries
  , recentObservations
  , recentNotableObservations
  , recentNearbyObservations
  , recentNearbySpeciesObservations
  , recentNearestSpeciesObservations
  , recentNearbyNotableObservations
  , historicalObservations

    -- ** Product queries
  , recentChecklists
  , top100
  , checklistFeed
  , regionalStatistics
  , speciesList
  , viewChecklist

    -- ** Hotspot queries
  , regionHotspots
  , nearbyHotspots
  , hotspotInfo

    -- ** Taxonomy queries
  , taxonomy
  , taxonomicForms
  , taxaLocaleCodes
  , taxonomyVersions
  , taxonomicGroups

    -- ** Region queries
  , regionInfo
  , subRegionList
  , adjacentRegions

    -- * Less convenient, generated queries
  , module Data.EBird.Client.Generated

    -- * Convenient re-exports
  , module Data.EBird.API
  , ClientError
  ) where

import Network.HTTP.Client.TLS
import Servant.Client

import Data.EBird.API
import Data.EBird.Client.Generated
import Data.EBird.Client.Hotspots
import Data.EBird.Client.Observations
import Data.EBird.Client.Product
import Data.EBird.Client.Regions
import Data.EBird.Client.Taxonomy

-- | Send a request to the official eBird API.
askEBird :: ClientM a -> IO (Either ClientError a)
askEBird question = do
    manager' <- newTlsManager
    runClientM question (mkClientEnv manager' eBirdHQ)
  where
    -- Home of the official eBird API
    eBirdHQ :: BaseUrl
    eBirdHQ = BaseUrl Https "api.ebird.org" 443 ""

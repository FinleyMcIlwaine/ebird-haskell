{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      : Data.EBird.Client.Generated
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Client functions generated using
-- [servant-client](https://hackage.haskell.org/package/servant-client). The
-- queries here match exactly the schemas defined in
-- [ebird-api](https://hackage.haskell.org/package/ebird-api), and are therefore
-- potentially a bit more clunky to use. See the wrappers in 'Data.EBird.Client' for
-- more convenient options.

module Data.EBird.Client.Generated
  ( -- * Generated eBird API client functions
    --
    -- | Generated directly from the definition of the API in
    -- [ebird-api](https://hackage.haskell.org/package/ebird-api).

    -- ** Observations queries
    recentObservations_
  , recentNotableObservations_
  , recentSpeciesObservations_
  , recentNearbyObservations_
  , recentNearbySpeciesObservations_
  , recentNearestSpeciesObservations_
  , recentNearbyNotableObservations_
  , historicalObservations_

    -- ** Product queries
  , recentChecklists_
  , top100_
  , checklistFeed_
  , regionalStatistics_
  , speciesList_
  , viewChecklist_

    -- ** Hotspot queries
  , regionHotspots_
  , nearbyHotspots_
  , hotspotInfo_

    -- ** Taxonomy queries
  , taxonomy_
  , taxonomicForms_
  , taxaLocaleCodes_
  , taxonomyVersions_
  , taxonomicGroups_

    -- ** Region queries
  , regionInfo_
  , subRegionList_
  , adjacentRegions_
  ) where


import Data.Text (Text)
import Data.Proxy
import Servant.API.Alternative
import Servant.Client

import Data.EBird.API

-------------------------------------------------------------------------------
-- Observation APIs
-------------------------------------------------------------------------------

-- | Get a list of recent observations within a region. Results only include the
-- most recent observation for each species in the region.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#3d2a17c1-2129-475c-b4c8-7d362d6000cd).
recentObservations_
  :: Text
  -- ^ eBird API key
  -> RegionCode
  -- ^ Region(s) to get observations from
  -> Maybe Integer
  -- ^ How many days back to look for observations
  --
  -- /1 - 30, default: 14/
  -> Maybe TaxonomyCategories
  -- ^ Only include observations in these taxonomy categories
  --
  -- /default: all categories/
  -> Maybe Bool
  -- ^ Only get observations from hotspots
  --
  -- /default: 'False'/
  -> Maybe Bool
  -- ^ Include observations which have not been reviewed
  --
  -- /default: 'False'/
  -> Maybe Integer
  -- ^ Maximum number of observations to get
  --
  -- /1 - 10000, default: all/
  -> Maybe RegionCode
  -- ^ Up to 10 extra regions to get observations from
  --
  -- /default: none/
  -> Maybe SPPLocale
  -- ^ Return observations with common names in this locale
  --
  -- /default: 'En'/
  -> ClientM [Observation 'Simple]

-- | Get a list of recent notable observations within a region. Results only
-- include the most recent observation for each species in the region.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#397b9b8c-4ab9-4136-baae-3ffa4e5b26e4).
recentNotableObservations_
  :: Text
  -- ^ eBird API key
  -> RegionCode
  -- ^ Region(s) to get observations from
  -> Maybe Integer
  -- ^ How many days back to look for observations
  --
  -- /1 - 30, default: 14/
  -> Maybe DetailLevel
  -- ^ Detail level for the resulting observations
  --
  -- /default: 'Simple'/
  -> Maybe Bool
  -- ^ Only get observations from hotspots
  --
  -- /default: 'False'/
  -> Maybe Integer
  -- ^ Maximum number of observations to get
  --
  -- /1 - 10000, default: all/
  -> Maybe RegionCode
  -- ^ Up to 10 extra regions to get observations from
  --
  -- /default: none/
  -> Maybe SPPLocale
  -- ^ Return observations with common names in this locale
  --
  -- /default: 'En'/
  -> ClientM [SomeObservation]

-- | Get a list of recent observations of a specific species within a region.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#755ce9ab-dc27-4cfc-953f-c69fb0f282d9).
recentSpeciesObservations_
  :: Text
  -- ^ eBird API key
  -> RegionCode
  -- ^ Region(s) to get observations from
  -> SpeciesCode
  -- ^ Species to get observations of (e.g. "barswa" for Barn Swallow)
  -> Maybe Integer
  -- ^ How many days back to look for observations
  --
  -- /1 - 30, default: 14/
  -> Maybe Bool
  -- ^ Only get observations from hotspots
  --
  -- /default: 'False'/
  -> Maybe Bool
  -- ^ Include observations which have not been reviewed
  --
  -- /default: 'False'/
  -> Maybe Integer
  -- ^ Maximum number of observations to get
  --
  -- /1 - 10000, default: all/
  -> Maybe RegionCode
  -- ^ Up to 10 extra regions to get observations from
  --
  -- /default: none/
  -> Maybe SPPLocale
  -- ^ Return observations with common names in this locale
  --
  -- /default: 'En'/
  -> ClientM [Observation 'Simple]

-- | Get a list of recent observations within some radius of some
-- latitude/longitude.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#62b5ffb3-006e-4e8a-8e50-21d90d036edc).
recentNearbyObservations_
  :: Text
  -- ^ eBird API key
  -> Double
  -- ^ Latitude of the location to get observations near
  -> Double
  -- ^ Longitude of the location to get observations near
  -> Maybe Integer
  -- ^ Search radius from the given latitude/longitude in kilometers
  --
  -- /0 - 50, default: 25/
  -> Maybe Integer
  -- ^ How many days back to look for observations
  --
  -- /1 - 30, default: 14/
  -> Maybe TaxonomyCategories
  -- ^ Only include observations in these taxonomy categories
  --
  -- /default: all/
  -> Maybe Bool
  -- ^ Only get observations from hotspots
  --
  -- /default: 'False'/
  -> Maybe Bool
  -- ^ Include observations which have not been reviewed
  --
  -- /default: 'False'/
  -> Maybe Integer
  -- ^ Maximum number of observations to get
  --
  -- /1 - 10000, default: all/
  -> Maybe SortObservationsBy
  -- ^ Sort observations by taxonomy ('SortObservationsBySpecies') or by date
  -- ('SortObservationsByDate', most recent first)
  --
  -- /default: 'SortObservationsByDate'/
  -> Maybe SPPLocale
  -- ^ Return observations with common names in this locale
  --
  -- /default: 'En'/
  -> ClientM [Observation 'Simple]

-- | Get a list of recent observations of a species within some radius of some
-- latitude/longitude.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#20fb2c3b-ee7f-49ae-a912-9c3f16a40397).
recentNearbySpeciesObservations_
  :: Text
  -- ^ eBird API key
  -> SpeciesCode
  -- ^ Species to get observations of (e.g. "bohwax" for Bohemian Waxwing)
  -> Double
  -- ^ Latitude of the location to get observations near
  -> Double
  -- ^ Longitude of the location to get observations near
  -> Maybe Integer
  -- ^ Search radius from the given latitude/longitude in kilometers
  --
  -- /0 - 50, default: 25/
  -> Maybe Integer
  -- ^ How many days back to look for observations
  --
  -- /1 - 30, default: 14/
  -> Maybe TaxonomyCategories
  -- ^ Only include observations in these taxonomy categories
  --
  -- /default: all/
  -> Maybe Bool
  -- ^ Only get observations from hotspots
  --
  -- /default: 'False'/
  -> Maybe Bool
  -- ^ Include observations which have not been reviewed
  --
  -- /default: 'False'/
  -> Maybe Integer
  -- ^ Maximum number of observations to get
  --
  -- /1 - 10000, default: all/
  -> Maybe SortObservationsBy
  -- ^ Sort observations by taxonomy ('SortObservationsBySpecies') or by date
  -- ('SortObservationsByDate', most recent first)
  --
  -- /default: 'SortObservationsByDate'/
  -> Maybe SPPLocale
  -- ^ Return observations with common names in this locale
  --
  -- /default: 'En'/
  -> ClientM [Observation 'Simple]

-- | Get a list of recent observations of some species nearest to some
-- latitude/longitude.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#6bded97f-9997-477f-ab2f-94f254954ccb).
recentNearestSpeciesObservations_
  :: Text
  -- ^ eBird API key
  -> SpeciesCode
  -- ^ Species to get observations of
  -> Double
  -- ^ Latitude of the location to get observations near
  -> Double
  -- ^ Longitude of the location to get observations near
  -> Maybe Integer
  -- ^ Search radius from the given latitude/longitude in kilometers
  --
  -- /0 - 50, default: 25/
  -> Maybe Integer
  -- ^ How many days back to look for observations
  --
  -- /0 - 30, default: 14/
  -> Maybe Bool
  -- ^ Only get observations from hotspots
  --
  -- /default: 'False'/
  -> Maybe Bool
  -- ^ Include observations which have not been reviewed
  --
  -- /default: 'False'/
  -> Maybe Integer
  -- ^ Maximum number of observations to get
  --
  -- /1 - 10000, default: all/
  -> Maybe SPPLocale
  -- ^ Return observations with common names in this locale
  --
  -- /default: 'En'/
  -> ClientM [Observation 'Simple]

-- | Get a list of recent /notable/ observations of some near some
-- latitude/longitude.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#caa348bb-71f6-471c-b203-9e1643377cbc).
recentNearbyNotableObservations_
  :: Text
  -- ^ eBird API key
  -> Double
  -- ^ Latitude of the location to get observations near
  -> Double
  -- ^ Longitude of the location to get observations near
  -> Maybe Integer
  -- ^ Search radius from the given latitude/longitude in kilometers
  --
  -- /0 - 50, default: 25/
  -> Maybe DetailLevel
  -- ^ Detail level for the resulting observations
  --
  -- /default: 'Simple'/
  -> Maybe Integer
  -- ^ How many days back to look for observations
  --
  -- /0 - 30, default: 14/
  -> Maybe Bool
  -- ^ Only get observations from hotspots
  --
  -- /default: 'False'/
  -> Maybe Integer
  -- ^ Maximum number of observations to get
  --
  -- /1 - 10000, default: all/
  -> Maybe SPPLocale
  -- ^ Return observations with common names in this locale
  --
  -- /default: 'En'/
  -> ClientM [SomeObservation]

-- | Get a list of observations for each species seen on a specific date. The
-- specific observations returned are determined by the 'SelectObservation'
-- parameter - first observation of the species ('SelectFirstObservation') or
-- last observation ('SelectLastObservation', default).
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#2d8c6ee8-c435-4e91-9f66-6d3eeb09edd2).
historicalObservations_
  :: Text
  -- ^ eBird API key
  -> RegionCode
  -- ^ Region(s) to get observation from
  -> Integer
  -- ^ Year, from 1800 to present
  -> Integer
  -- ^ Month (1 - 12)
  -> Integer
  -- ^ Day in the month
  -> Maybe TaxonomyCategories
  -- ^ Only include observations in these taxonomy categories
  --
  -- /default: all/
  -> Maybe DetailLevel
  -- ^ Detail level for the resulting observations
  --
  -- /default: 'Simple'/
  -> Maybe Bool
  -- ^ Only get observations from hotspots
  --
  -- /default: 'False'/
  -> Maybe Bool
  -- ^ Include observations which have not been reviewed
  --
  -- /default: 'False'/
  -> Maybe Integer
  -- ^ Maximum number of observations to get
  --
  -- /1 - 10000, default: all/
  -> Maybe SelectObservation
  -- ^ Whether to display the first or last observation of a species on the
  -- date, in the case that there are multiple observations of the same species
  -- on the date
  --
  -- /default: 'SelectLastObservation'/
  -> Maybe RegionCode
  -- ^ Up to 50 extra regions to get observations from
  --
  -- /default: none/
  -> Maybe SPPLocale
  -- ^ Return observations with common names in this locale
  --
  -- /default: 'En'/
  -> ClientM [SomeObservation]

-------------------------------------------------------------------------------
-- Product APIs
-------------------------------------------------------------------------------

-- | Get a list recently submitted checklists within a region.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#95a206d1-a20d-44e0-8c27-acb09ccbea1a).
recentChecklists_
  :: Text
  -- ^ eBird API key
  -> RegionCode
  -- ^ Region(s) to get checklists from
  -> Maybe Integer
  -- ^ Maximum number of checklists to fetch
  --
  -- /1 - 200, default: 10/
  -> ClientM [ChecklistFeedEntry]

-- | Get a list of top contributors for a region on a specific date, ranked by
-- number of species observed or number of checklists submitted.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#2d8d3f94-c4b0-42bd-9c8e-71edfa6347ba).
top100_
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to fetch the ranking for
  -> Integer
  -- ^ Year, from 1800 to present
  -> Integer
  -- ^ Month (1 - 12)
  -> Integer
  -- ^ Day in the month
  -> Maybe RankTop100By
  -- ^ Rank the resulting list by number of species observed or by number of
  -- checklists completed
  --
  -- /default: 'RankTop100BySpecies'/
  -> Maybe Integer
  -- ^ Maximum number of entries to fetch
  --
  -- /1 - 100, default: 100/
  -> ClientM [Top100ListEntry]

-- | Get a list of checklists submitted within a region on a specific date.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#4416a7cc-623b-4340-ab01-80c599ede73e).
checklistFeed_
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to fetch the checklist feed for
  -> Integer
  -- ^ Year, from 1800 to present
  -> Integer
  -- ^ Month (1 - 12)
  -> Integer
  -- ^ Day in the month
  -> Maybe SortChecklistsBy
  -- ^ Sort the resulting list by date of checklist submission or date of
  -- checklist creation
  --
  -- /default: 'SortChecklistsByDateCreated'/
  -> Maybe Integer
  -- ^ Maximum number of checklists to fetch
  --
  -- /1 - 200, default: 10/
  -> ClientM [ChecklistFeedEntry]

-- | Get the 'RegionalStatistics' for a region on a specific date.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#506e63ab-abc0-4256-b74c-cd9e77968329).
regionalStatistics_
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to fetch the statistics for
  -> Integer
  -- ^ Year, from 1800 to present
  -> Integer
  -- ^ Month (1 - 12)
  -> Integer
  -- ^ Day in the month
  -> ClientM RegionalStatistics

-- | Get a list of all species ever seen in a region.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#55bd1b26-6951-4a88-943a-d3a8aa1157dd).
speciesList_
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to fetch the species list for
  -> ClientM [SpeciesCode]

-- | Get information about a checklist.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#2ee89672-4211-4fc1-8493-5df884fbb386).
viewChecklist_
  :: Text
  -- ^ eBird API key
  -> Text
  -- ^ Checklist submission ID, e.g. \"S144646447\"
  -> ClientM Checklist

-------------------------------------------------------------------------------
-- Hotspot APIs
-------------------------------------------------------------------------------

-- | Get all hotspots in a list of one or more regions ('RegionCode').
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#f4f59f90-854e-4ba6-8207-323a8cf0bfe0).
--
-- __NOTE:__ The eBird API is broken. Always hardcode the 'CSVOrJSONFormat'
-- argument to 'JSONFormat'.
regionHotspots_
  :: RegionCode
  -- ^ Region(s) to get hotspots in
  -> Maybe Integer
  -- ^ Only fetch hotspots that have been visited within this many days ago
  --
  -- /1 - 30, default: no limit/
  -> Maybe CSVOrJSONFormat
  -- ^ Format results in CSV or JSON format
  --
  -- __NOTE:__ This argument should /always/ be hardcoded to 'JSONFormat', even
  -- though the default is 'CSVFormat'. It is only here for to be consistent
  -- with the eBird API. Unfortunately, the endpoint for this query switches
  -- content types based on this query parameter instead of an \"Accept\" header.
  -- That means servant is unable to determine whether the result will be CSV or
  -- JSON encoded. For now, the workaround is to always hardcode this to
  -- 'JSONFormat'.
  --
  -- /default: 'CSVFormat'/
  -> ClientM [Hotspot]

-- | Get all hotspots within a radius of some latitude/longitude.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#674e81c1-6a0c-4836-8a7e-6ea1fe8e6677).
--
-- __NOTE:__ The eBird API is broken. Always hardcode the 'CSVOrJSONFormat'
-- argument to 'JSONFormat'.
nearbyHotspots_
  :: Double
  -- ^ Latitude of the location to get hotspots near
  -> Double
  -- ^ Longitude of the location to get hotspots near
  -> Maybe Integer
  -- ^ Only fetch hotspots that have been visited within this many days ago
  --
  -- /1 - 30, default: no limit/
  -> Maybe Integer
  -- ^ Search radius in kilometers
  --
  -- /0 - 50, default: 25/
  -> Maybe CSVOrJSONFormat
  -- ^ Format results in CSV or JSON format
  --
  -- __NOTE:__ This argument should /always/ be hardcoded to 'JSONFormat', even
  -- though the default is 'CSVFormat'. It is only here for to be consistent
  -- with the eBird API. Unfortunately, the endpoint for this query switches
  -- content types based on this query parameter instead of an \"Accept\" header.
  -- That means servant is unable to determine whether the result will be CSV or
  -- JSON encoded. For now, the workaround is to always hardcode this to
  -- 'JSONFormat'.
  --
  -- /default: 'CSVFormat'/
  -> ClientM [Hotspot]

-- | Get information about a hotspot.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#e25218db-566b-4d8b-81ca-e79a8f68c599).
hotspotInfo_
  :: Text
  -- ^ Location ID of the hotspot (e.g. \"L2373040\")
  -> ClientM LocationData

-------------------------------------------------------------------------------
-- Taxonomy APIs
-------------------------------------------------------------------------------

-- | Get any version of the eBird taxonomy, with optional filtering based on
-- taxonomy categories and species.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#952a4310-536d-4ad1-8f3e-77cfb624d1bc).
--
-- __NOTE:__ The eBird API is broken. Always hardcode the 'CSVOrJSONFormat'
-- argument to 'JSONFormat'.
taxonomy_
  :: Maybe TaxonomyCategories
  -- ^ Only include species of these 'TaxonomyCategory's in the taxonomy
  --
  -- /default: all categories/
  -> Maybe CSVOrJSONFormat
  -- ^ Format the taxonomy in CSV or JSON
  --
  -- __NOTE:__ This argument should /always/ be hardcoded to 'JSONFormat', even
  -- though the default is 'CSVFormat'. It is only here for to be consistent
  -- with the eBird API. Unfortunately, the endpoint for this query switches
  -- content types based on this query parameter instead of an \"Accept\" header.
  -- That means servant is unable to determine whether the result will be CSV or
  -- JSON encoded. For now, the workaround is to always hardcode this to
  -- 'JSONFormat'.
  --
  -- /default: 'CSVFormat'/
  -> Maybe SPPLocale
  -- ^ Use this locale for common names
  --
  -- /default: 'En'/
  -> Maybe SpeciesCodes
  -- ^ Only fetch records for these species
  --
  -- /default: all/
  -> Maybe Text
  -- ^ Fetch this version of the eBird taxonomy
  --
  -- /default: latest/
  -> ClientM [Taxon]

-- | Get the list of sub species of a given species recognized in the eBird
-- taxonomy.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#e338e5a6-919d-4603-a7db-6c690fa62371).
taxonomicForms_
  :: Text
  -- ^ eBird API key
  -> SpeciesCode
  -- ^ Species to get the sub species of
  -> ClientM SpeciesCodes

-- | Get the supported locale codes and names for species common names, with the
-- last time they were updated.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#3ea8ff71-c254-4811-9e80-b445a39302a6).
taxaLocaleCodes_
  :: Text
  -- ^ eBird API key
  -> Maybe SPPLocale
  -- ^ Value for the "Accept-Language" header, for translated language names,
  -- when available
  --
  -- /default: 'En'/
  -> ClientM [SPPLocaleListEntry]

-- | Get all versions of the taxonomy, with a flag indicating which is latest.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#9bba1ff5-6eb2-4f9a-91fd-e5ed34e51500).
taxonomyVersions_ :: ClientM [TaxonomyVersionListEntry]

-- | Get the list of species groups, in either Merlin or eBird grouping.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#aa9804aa-dbf9-4a53-bbf4-48e214e4677a).
taxonomicGroups_
  :: SPPGrouping
  -- ^ 'MerlinGrouping' groups like birds together, with falcons next to hawks,
  -- while 'EBirdGrouping' groups in taxonomy order
  -> Maybe SPPLocale
  -- ^ Locale to use for species group names. 'En' is used for any locale whose
  -- translations are unavailable at this endpoint
  --
  -- /default: 'En'/
  -> ClientM [TaxonomicGroupListEntry]

-------------------------------------------------------------------------------
-- Region APIs
-------------------------------------------------------------------------------

-- | Get a 'RegionInfo' for an eBird region.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#07c64240-6359-4688-9c4f-ff3d678a7248).
regionInfo_
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to get information for
  -> Maybe RegionNameFormat
  -- ^ How to format the region name in the response
  --
  -- /default: 'Full'/
  -> ClientM RegionInfo

-- | Get a list of sub-regions of a given region type within a given region.
-- Keep in mind that many combinations of sub region and parent region are
-- invalid, e.g. 'CountryType' regions within "US-WY".
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#382da1c8-8bff-4926-936a-a1f8b065e7d5).
subRegionList_
  :: Text
  -- ^ eBird API key
  -> RegionType
  -- ^ Type of subregions to fetch
  -> RegionCode
  -- ^ Parent 'RegionCode'
  -> ClientM [RegionListEntry]

-- | Get a list of regions adjacent to a given region. Only 'Subnational2'
-- region codes in the United States, New Zealand, or Mexico are currently
-- supported.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#3aca0519-3105-47fc-8611-a4dfd500a32f).
adjacentRegions_
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to fetch the adjacent regions of
  -> ClientM [RegionListEntry]

recentObservations_
  :<|> recentNotableObservations_
  :<|> recentSpeciesObservations_
  :<|> recentNearbyObservations_
  :<|> recentNearbySpeciesObservations_
  :<|> recentNearestSpeciesObservations_
  :<|> recentNearbyNotableObservations_
  :<|> historicalObservations_
  :<|> recentChecklists_
  :<|> top100_
  :<|> checklistFeed_
  :<|> regionalStatistics_
  :<|> speciesList_
  :<|> viewChecklist_
  :<|> regionHotspots_
  :<|> nearbyHotspots_
  :<|> hotspotInfo_
  :<|> taxonomy_
  :<|> taxonomicForms_
  :<|> taxaLocaleCodes_
  :<|> taxonomyVersions_
  :<|> taxonomicGroups_
  :<|> regionInfo_
  :<|> subRegionList_
  :<|> adjacentRegions_ = client (Proxy @EBirdAPI)

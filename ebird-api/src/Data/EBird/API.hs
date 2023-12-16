{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Data.EBird.API
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- A description of the
-- [eBird API](https://documenter.getpostman.com/view/664302/S1ENwy59)
-- as a [servant](https://hackage.haskell.org/package/servant) API type.
--
-- Intended for use by those who wish to write their own clients for the
-- [eBird API](https://documenter.getpostman.com/view/664302/S1ENwy59) using
-- [servant-client](https://hackage.haskell.org/package/servant-client).
--
-- If you are interested in querying the
-- [eBird API](https://documenter.getpostman.com/view/664302/S1ENwy59) using
-- an existing client, check out the
-- [ebird-client](https://hackage.haskell.org/package/ebird-client) library.

module Data.EBird.API
  ( -- * Servant API types
    --
    -- | Note: The individual endpoint types are only exported for those who
    -- wish to implement partial clients for the eBird API. Comprehensive client
    -- implementations should use the 'EBirdAPI' type.

    -- ** Top-level API
    EBirdAPI
  , WithAPIKey

    -- ** Observations APIs
    --
    -- | These endpoints can be found under the [data\/obs
    -- section](https://documenter.getpostman.com/view/664302/S1ENwy59#4e020bc2-fc67-4fb6-a926-570cedefcc34)
    -- of the eBird API documentation.
  , RecentObservationsAPI
  , RecentNotableObservationsAPI
  , RecentSpeciesObservationsAPI
  , RecentNearbyObservationsAPI
  , RecentNearbySpeciesObservationsAPI
  , RecentNearestSpeciesObservationsAPI
  , RecentNearbyNotableObservationsAPI
  , HistoricalObservationsAPI

    -- ** Product APIs
    --
    -- | These endpoints can be found under the [product
    -- section](https://documenter.getpostman.com/view/664302/S1ENwy59#af04604f-e406-4cea-991c-a9baef24cd78)
    -- of the eBird API documentation.
  , RecentChecklistsAPI
  , Top100API
  , ChecklistFeedAPI
  , RegionalStatisticsAPI
  , SpeciesListAPI
  , ViewChecklistAPI

    -- ** Hotspot APIs
    --
    -- | These endpoints can be found under the [ref\/hotspot
    -- section](https://documenter.getpostman.com/view/664302/S1ENwy59#5a1e27e9-128f-4ab5-80ad-88cd6de10026)
    -- of the eBird API documentation.
  , RegionHotspotsAPI
  , NearbyHotspotsAPI
  , HotspotInfoAPI

    -- ** Taxonomy APIs
    --
    -- | These endpoints can be found under the [ref\/taxonomy
    -- section](https://documenter.getpostman.com/view/664302/S1ENwy59#36c95b76-e18e-4788-9c9e-e539045f9166)
    -- of the eBird API documentation.
  , TaxonomyAPI
  , TaxonomicFormsAPI
  , TaxaLocaleCodesAPI
  , TaxonomyVersionsAPI
  , TaxonomicGroupsAPI

    -- ** Region APIs
    --
    -- | These endpoints can be found under the [ref\/region
    -- section](https://documenter.getpostman.com/view/664302/S1ENwy59#e18ea3b5-e80c-479f-87db-220ce8d9f3b6)
    -- of the eBird API documentation, except for the 'AdjacentRegionsAPI',
    -- which would be under the [ref\/geo
    -- section](https://documenter.getpostman.com/view/664302/S1ENwy59#c9947c5c-2dce-4c6d-9911-7d702235506c)
    -- of the eBird API documentation.
  , RegionInfoAPI
  , SubregionListAPI
  , AdjacentRegionsAPI

    -- * eBird checklists
  , module Data.EBird.API.Checklists

    -- * eBird observations
  , module Data.EBird.API.Observations

    -- * eBird products
  , module Data.EBird.API.Product

    -- * eBird hotspots
  , module Data.EBird.API.Hotspots

    -- * eBird regions
  , module Data.EBird.API.Regions

    -- * eBird taxonomy
  , module Data.EBird.API.Taxonomy

    -- * Date and time utilities
  , module Data.EBird.API.Util.Time

    -- * 'EBirdString' class
  , module Data.EBird.API.EBirdString
  ) where

import Data.Text (Text)
import Servant.API

import Data.EBird.API.Checklists
import Data.EBird.API.EBirdString
import Data.EBird.API.Observations
import Data.EBird.API.Product
import Data.EBird.API.Hotspots
import Data.EBird.API.Regions
import Data.EBird.API.Taxonomy
import Data.EBird.API.Util.Time

{-
Note [dependently typed APIs]

Some eBird API abservation endpoints (such as the "recent notable observations"
endpoint) allow the caller to specify a detail level ("simple" or "full") for
the observations contained in the response. "Full" detail observations contain
more fields than "simple" detail observations. This means that "simple"  and
"full" observations are most accurately represented as different types.
Furthermore, this means that the result type of these endpoints is determined by
the value of the "detail" query parameter. Since the type of the API depends on
the value of an input variable, we say that the API is dependently typed.

There are two ways we could deal with this. First, we could model this situation
at the value level. To do this, we create a tagged union of simple and full
detail observations and have all of the observation endpoints return this
combined type, regardless of the value of "detail" query parameter. The benefit
of this approach is the simplicity. It requires no fancy type-level voodoo to
model this or to use it. The drawback of this approach is the lack of type
safety. Clients of this API know whether "simple" or "full" detail observations
are being requested, yet they still must do a runtime check of the response to
ensure that the observations in the response are detailed as expected. This is
not an error condition that clients of the API should need to worry about.

Second, we could model this situation at the type level, much like is done in
the fantastic [Well-Typed article on dependently typed
servers](https://www.well-typed.com/blog/2015/12/dependently-typed-servers/).
The drawback of this approach is the fancy type-level programming overhead it
introduces for both this library and any consumers.

For now, we model this at the value level.
-}

-- | The [eBird API](https://documenter.getpostman.com/view/664302/S1ENwy59) as
-- a Haskell type.
type EBirdAPI =
    -- Observation APIs
         RecentObservationsAPI
    :<|> RecentNotableObservationsAPI
    :<|> RecentSpeciesObservationsAPI
    :<|> RecentNearbyObservationsAPI
    :<|> RecentNearbySpeciesObservationsAPI
    :<|> RecentNearestSpeciesObservationsAPI
    :<|> RecentNearbyNotableObservationsAPI
    :<|> HistoricalObservationsAPI

    -- Product APIs
    :<|> RecentChecklistsAPI
    :<|> Top100API
    :<|> ChecklistFeedAPI
    :<|> RegionalStatisticsAPI
    :<|> SpeciesListAPI
    :<|> ViewChecklistAPI

    -- Hotspot APIs
    :<|> RegionHotspotsAPI
    :<|> NearbyHotspotsAPI
    :<|> HotspotInfoAPI

    -- Taxonomy APIs
    :<|> TaxonomyAPI
    :<|> TaxonomicFormsAPI
    :<|> TaxaLocaleCodesAPI
    :<|> TaxonomyVersionsAPI
    :<|> TaxonomicGroupsAPI

    -- Region APIs
    :<|> RegionInfoAPI
    :<|> SubregionListAPI
    :<|> AdjacentRegionsAPI

-- | Convenient synonym for requiring an @x-ebirdapitoken@ on a route
type WithAPIKey = Header' '[Required] "x-ebirdapitoken" Text

-------------------------------------------------------------------------------
-- Observations APIs
-------------------------------------------------------------------------------

-- | Recent observations within a region. Note that this endpoint only ever
-- returns 'Simple' detail observations.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#3d2a17c1-2129-475c-b4c8-7d362d6000cd).
type RecentObservationsAPI =
       "v2" :> "data" :> "obs"
    :> WithAPIKey
    :> Capture "regionCode" RegionCode
    :> "recent"
    :> QueryParam "back" Integer
    :> QueryParam "cat" TaxonomyCategories
    :> QueryParam "hotspot" Bool
    :> QueryParam "includeProvisional" Bool
    :> QueryParam "maxResults" Integer
    :> QueryParam "r" RegionCode
    :> QueryParam "sppLocale" SPPLocale
    :> Get '[JSON] [Observation 'Simple]

-- | Recent /notable/ observations within a region. Since this endpoint can
-- return both 'Simple' and 'Full' detail observations, depending on the value
-- provided for the "detail" query parameter, we existentially quantify the
-- detail level of the resulting observations.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#397b9b8c-4ab9-4136-baae-3ffa4e5b26e4).
type RecentNotableObservationsAPI =
       "v2" :> "data" :> "obs"
    :> WithAPIKey
    :> Capture "regionCode" RegionCode
    :> "recent"
    :> "notable"
    :> QueryParam "back" Integer
    :> QueryParam "detail" DetailLevel
    :> QueryParam "hotspot" Bool
    :> QueryParam "maxResults" Integer
    :> QueryParam "r" RegionCode
    :> QueryParam "sppLocale" SPPLocale
    :> Get '[JSON] [SomeObservation]

-- | Recent observations of a species within a region. Note that this endpoint
-- only ever returns 'Simple' detail observations.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#755ce9ab-dc27-4cfc-953f-c69fb0f282d9).
type RecentSpeciesObservationsAPI =
    "v2" :> "data" :> "obs"
    :> WithAPIKey
    :> Capture "regionCode" RegionCode
    :> "recent"
    :> Capture "speciesCode" SpeciesCode
    :> QueryParam "back" Integer
    :> QueryParam "hotspot" Bool
    :> QueryParam "includeProvisional" Bool
    :> QueryParam "maxResults" Integer
    :> QueryParam "r" RegionCode
    :> QueryParam "sppLocale" SPPLocale
    :> Get '[JSON] [Observation 'Simple]

-- | Recent observations within some radius of some latitude/longitude.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#62b5ffb3-006e-4e8a-8e50-21d90d036edc).
type RecentNearbyObservationsAPI =
    "v2" :> "data" :> "obs" :> "geo" :> "recent"
    :> WithAPIKey
    :> QueryParam' '[Required] "lat" Double
    :> QueryParam' '[Required] "lng" Double
    :> QueryParam "dist" Integer
    :> QueryParam "back" Integer
    :> QueryParam "cat" TaxonomyCategories
    :> QueryParam "hotspot" Bool
    :> QueryParam "includeProvisional" Bool
    :> QueryParam "maxResults" Integer
    :> QueryParam "sort" SortObservationsBy
    :> QueryParam "sppLocale" SPPLocale
    :> Get '[JSON] [Observation 'Simple]

-- | Recent observations of a species within some radius of some
-- latitude/longitude.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#20fb2c3b-ee7f-49ae-a912-9c3f16a40397).
type RecentNearbySpeciesObservationsAPI =
    "v2" :> "data" :> "obs" :> "geo" :> "recent"
    :> WithAPIKey
    :> Capture "species" SpeciesCode
    :> QueryParam' '[Required] "lat" Double
    :> QueryParam' '[Required] "lng" Double
    :> QueryParam "dist" Integer
    :> QueryParam "back" Integer
    :> QueryParam "cat" TaxonomyCategories
    :> QueryParam "hotspot" Bool
    :> QueryParam "includeProvisional" Bool
    :> QueryParam "maxResults" Integer
    :> QueryParam "sort" SortObservationsBy
    :> QueryParam "sppLocale" SPPLocale
    :> Get '[JSON] [Observation 'Simple]

-- | Nearest recent observations including a species.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#6bded97f-9997-477f-ab2f-94f254954ccb).
type RecentNearestSpeciesObservationsAPI =
    "v2" :> "data" :> "nearest" :> "geo" :> "recent"
    :> WithAPIKey
    :> Capture "species" SpeciesCode
    :> QueryParam' '[Required] "lat" Double
    :> QueryParam' '[Required] "lng" Double
    :> QueryParam "dist" Integer
    :> QueryParam "back" Integer
    :> QueryParam "hotspot" Bool
    :> QueryParam "includeProvisional" Bool
    :> QueryParam "maxResults" Integer
    :> QueryParam "sppLocale" SPPLocale
    :> Get '[JSON] [Observation 'Simple]

-- | Recent /notable/ observations of a within some radius of some
-- latitude/longitude.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#caa348bb-71f6-471c-b203-9e1643377cbc).
type RecentNearbyNotableObservationsAPI =
    "v2" :> "data" :> "obs" :> "geo" :> "recent" :> "notable"
    :> WithAPIKey
    :> QueryParam' '[Required] "lat" Double
    :> QueryParam' '[Required] "lng" Double
    :> QueryParam "dist" Integer
    :> QueryParam "detail" DetailLevel
    :> QueryParam "back" Integer
    :> QueryParam "hotspot" Bool
    :> QueryParam "maxResults" Integer
    :> QueryParam "sppLocale" SPPLocale
    :> Get '[JSON] [SomeObservation]

-- | A list of all observations for each taxa seen in some 'RegionCode' on a
-- specific date. The specific observations returned are determined by the
-- @rank@ parameter - first observation of the species
-- ('SelectFirstObservation', default) or last observation
-- ('SelectLastObservation').
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#2d8c6ee8-c435-4e91-9f66-6d3eeb09edd2).
type HistoricalObservationsAPI =
    "v2" :> "data" :> "obs"
    :> WithAPIKey
    :> Capture "regionCode" RegionCode
    :> "historic"
    :> Capture "year" Integer
    :> Capture "month" Integer
    :> Capture "day" Integer
    :> QueryParam "cat" TaxonomyCategories
    :> QueryParam "detail" DetailLevel
    :> QueryParam "hotspot" Bool
    :> QueryParam "includeProvisional" Bool
    :> QueryParam "maxResults" Integer
    :> QueryParam "rank" SelectObservation
    :> QueryParam "r" RegionCode
    :> QueryParam "sppLocale" SPPLocale
    :> Get '[JSON] [SomeObservation]

-------------------------------------------------------------------------------
-- Product APIs
-------------------------------------------------------------------------------

-- | A list of recent checklists submitted in a region.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#95a206d1-a20d-44e0-8c27-acb09ccbea1a).
type RecentChecklistsAPI =
    "v2" :> "product" :> "lists"
    :> WithAPIKey
    :> Capture "regionCode" RegionCode
    :> QueryParam "maxResults" Integer
    :> Get '[JSON] [ChecklistFeedEntry]

-- | A list of the top 100 contributors on a given date, ranked by species count
-- or checklist count.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#2d8d3f94-c4b0-42bd-9c8e-71edfa6347ba).
type Top100API =
    "v2" :> "product" :> "top100"
    :> WithAPIKey
    :> Capture "regionCode" Region
    :> Capture "year" Integer
    :> Capture "month" Integer
    :> Capture "day" Integer
    :> QueryParam "rankedBy" RankTop100By
    :> QueryParam "maxResults" Integer
    :> Get '[JSON] [Top100ListEntry]

-- | A list of checklists submitted in a region on a date.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#4416a7cc-623b-4340-ab01-80c599ede73e).
type ChecklistFeedAPI =
    "v2" :> "product" :> "lists"
    :> WithAPIKey
    :> Capture "regionCode" Region
    :> Capture "year" Integer
    :> Capture "month" Integer
    :> Capture "day" Integer
    :> QueryParam "sortKey" SortChecklistsBy
    :> QueryParam "maxResults" Integer
    :> Get '[JSON] [ChecklistFeedEntry]

-- | A list of checklists submitted on a date.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#506e63ab-abc0-4256-b74c-cd9e77968329).
type RegionalStatisticsAPI =
    "v2" :> "product" :> "stats"
    :> WithAPIKey
    :> Capture "regionCode" Region
    :> Capture "year" Integer
    :> Capture "month" Integer
    :> Capture "day" Integer
    :> Get '[JSON] RegionalStatistics

-- | A list of all species ever seen in a region.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#55bd1b26-6951-4a88-943a-d3a8aa1157dd).
type SpeciesListAPI =
    "v2" :> "product" :> "spplist"
    :> WithAPIKey
    :> Capture "regionCode" Region
    :> Get '[JSON] [SpeciesCode]

-- | The details and observations for a checklist.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#2ee89672-4211-4fc1-8493-5df884fbb386).
type ViewChecklistAPI =
    "v2" :> "product" :> "checklist" :> "view"
    :> WithAPIKey
    :> Capture "subId" Text
    :> Get '[JSON] Checklist

-------------------------------------------------------------------------------
-- Hotspot APIs
-------------------------------------------------------------------------------

-- | The hotspots within a list of one or more regions.
--
-- NOTE: This endpoint switches the content type of the response based on a
-- query parameter, not an \"Accept\" header, and for some reason it chooses to
-- make the default content type CSV. Any client for this endpoint should
-- hardcode the "fmt" parameter to 'JSONFormat'.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#f4f59f90-854e-4ba6-8207-323a8cf0bfe0).
type RegionHotspotsAPI =
    "v2" :> "ref" :> "hotspot"
    :> Capture "regionCode" RegionCode
    :> QueryParam "back" Integer
    :> QueryParam "fmt" CSVOrJSONFormat
    :> Get '[JSON] [Hotspot]

-- | The hotspots within a radius of some latitude/longitude.
--
-- NOTE: This endpoint switches the content type of the response based on a
-- query parameter, not an \"Accept\" header, and for some reason it chooses to
-- make the default content type CSV. Any client for this endpoint should
-- hardcode the "fmt" parameter to 'JSONFormat'.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#674e81c1-6a0c-4836-8a7e-6ea1fe8e6677).
type NearbyHotspotsAPI =
    "v2" :> "ref" :> "hotspot" :> "geo"
    :> QueryParam' '[Required] "lat" Double
    :> QueryParam' '[Required] "lng" Double
    :> QueryParam "back" Integer
    :> QueryParam "dist" Integer
    :> QueryParam "fmt" CSVOrJSONFormat
    :> Get '[JSON] [Hotspot]

-- | Information about a hotspot.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#e25218db-566b-4d8b-81ca-e79a8f68c599).
type HotspotInfoAPI =
    "v2" :> "ref" :> "hotspot" :> "info"
    :> Capture "locId" Text
    :> Get '[JSON] LocationData

-------------------------------------------------------------------------------
-- Taxonomy API
-------------------------------------------------------------------------------

-- | The eBird taxonomy, in part or in full.
--
-- NOTE: This endpoint switches the content type of the response based on a
-- query parameter, not an \"Accept\" header, and for some reason it chooses to
-- make the default content type CSV. Any client for this endpoint should
-- hardcode the "fmt" parameter to 'JSONFormat'.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#952a4310-536d-4ad1-8f3e-77cfb624d1bc).
type TaxonomyAPI =
    "v2" :> "ref" :> "taxonomy" :> "ebird"
    :> QueryParam "cat" TaxonomyCategories
    :> QueryParam "fmt" CSVOrJSONFormat
    :> QueryParam "locale" SPPLocale
    :> QueryParam "species" SpeciesCodes
    :> QueryParam "version" Text
    :> Get '[JSON] [Taxon]

-- | The list of subspecies of a given species recognized in the taxonomy.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#e338e5a6-919d-4603-a7db-6c690fa62371).
type TaxonomicFormsAPI =
    "v2" :> "ref" :> "taxon" :> "forms"
    :> WithAPIKey
    :> Capture "speciesCode" SpeciesCode
    :> Get '[JSON] SpeciesCodes

-- | The supported locale codes and names for species common names.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#3ea8ff71-c254-4811-9e80-b445a39302a6).
type TaxaLocaleCodesAPI =
    "v2" :> "ref" :> "taxa-locales" :> "ebird"
    :> WithAPIKey
    :> Header "Accept-Language" SPPLocale
    :> Get '[JSON] [SPPLocaleListEntry]

-- | The complete list of taxonomy versions, with a flag indicating which is the
-- latest.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#9bba1ff5-6eb2-4f9a-91fd-e5ed34e51500).
type TaxonomyVersionsAPI =
    "v2" :> "ref" :> "taxonomy" :> "versions"
    :> Get '[JSON] [TaxonomyVersionListEntry]

-- | The list of species groups, e.g. terns, finches, etc.
--
-- See [the eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#aa9804aa-dbf9-4a53-bbf4-48e214e4677a).
type TaxonomicGroupsAPI =
    "v2" :> "ref" :> "sppgroup"
    :> Capture "speciesGrouping" SPPGrouping
    :> QueryParam "groupNameLocale" SPPLocale
    :> Get '[JSON] [TaxonomicGroupListEntry]

-------------------------------------------------------------------------------
-- Regions API
-------------------------------------------------------------------------------

-- | Get a 'RegionInfo' for a 'Region'.
--
-- See the [eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#07c64240-6359-4688-9c4f-ff3d678a7248).
type RegionInfoAPI =
    "v2" :> "ref" :> "region"
    :> "info"
    :> WithAPIKey
    :> Capture "regionCode" Region
    :> QueryParam "regionNameFormat" RegionNameFormat
    :> Get '[JSON] RegionInfo

-- | Get a list of subregions of a certain 'RegionType' within a 'RegionCode'.
--
-- See the [eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#382da1c8-8bff-4926-936a-a1f8b065e7d5).
type SubregionListAPI =
    "v2" :> "ref" :> "region"
    :> "list"
    :> WithAPIKey
    :> Capture "regionType" RegionType
    :> Capture "regionCode" RegionCode
    :> Get '[JSON] [RegionListEntry]


-- | Adjacent regions to a given region. Only 'Subnational2' region codes in the
-- United States, New Zealand, or Mexico are currently supported.
--
-- See the [eBird API documentation for this
-- route](https://documenter.getpostman.com/view/664302/S1ENwy59#3aca0519-3105-47fc-8611-a4dfd500a32f).
type AdjacentRegionsAPI =
    "v2" :> "ref" :> "adjacent"
    :> WithAPIKey
    :> Capture "regionCode" Region
    :> Get '[JSON] [RegionListEntry]

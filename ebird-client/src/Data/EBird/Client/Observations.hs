{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Data.EBird.Client.Observations
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Types and functions for observation-related eBird API queries.

module Data.EBird.Client.Observations where

import Data.Default
import Data.Text
import Optics.TH
import Servant.Client

import Data.EBird.API
import Data.EBird.Client.Generated

-------------------------------------------------------------------------------
-- * Recent observations
-------------------------------------------------------------------------------

-- | Get a list of recent observations within a region. Results only include the
-- most recent observation for each species in the region.
--
-- For example, get up to 10 recent observations from the last 5 days in Park
-- County, Wyoming (using @-XOverloadedLabels@ and @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   recentObservations key
--     "US-WY-029"
--     (def & #maxResults ?~ 10 & #back ?~ 5)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#3d2a17c1-2129-475c-b4c8-7d362d6000cd).
recentObservations
  :: Text
  -- ^ eBird API key
  -> RegionCode
  -- ^ Region(s) to get observations from
  -> RecentObservationsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultRecentObservationsParams'/
  -> ClientM [Observation 'Simple]
recentObservations k r RecentObservationsParams{..} =
    recentObservations_ k r
      _recentObservationsParamsBack
      _recentObservationsParamsCategories
      _recentObservationsParamsHotspot
      _recentObservationsParamsProvisional
      _recentObservationsParamsMaxResults
      _recentObservationsParamsExtraRegions
      _recentObservationsParamsLocale

-- | Optional parameters accepted by the 'RecentObservationsAPI'.
--
-- Note that 'defaultRecentObservationsParams' (or the 'Default' instance's
-- 'def' value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_recentObservationsParamsBack' field to 30:
--
-- > def & recentObservationsParamsBack ?~ 30
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #back ?~ 30
data RecentObservationsParams =
    RecentObservationsParams
      { -- | How many days back to look for observations
        --
        -- /1 - 30, default: 14/
        _recentObservationsParamsBack :: Maybe Integer

        -- | Only include observations in these taxonomy categories
        --
        -- /default: all categories/
      , _recentObservationsParamsCategories :: Maybe TaxonomyCategories

        -- | Only get observations from hotspots
        --
        -- /default: 'False'/
      , _recentObservationsParamsHotspot :: Maybe Bool

        -- | Include observations which have not been reviewed
        --
        -- /default: 'False'/
      , _recentObservationsParamsProvisional :: Maybe Bool

        -- | Maximum number of observations to get
        --
        -- /1 - 10000, default: all/
      , _recentObservationsParamsMaxResults :: Maybe Integer

        -- | Up to 10 extra regions to get observations from
        --
        -- /default: none/
      , _recentObservationsParamsExtraRegions :: Maybe RegionCode

        -- | Return observations with common names in this locale
        --
        -- /default: 'En'/
      , _recentObservationsParamsLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultRecentObservationsParams :: RecentObservationsParams
defaultRecentObservationsParams =
    RecentObservationsParams
      { _recentObservationsParamsBack = Nothing
      , _recentObservationsParamsCategories = Nothing
      , _recentObservationsParamsHotspot = Nothing
      , _recentObservationsParamsProvisional = Nothing
      , _recentObservationsParamsMaxResults = Nothing
      , _recentObservationsParamsExtraRegions = Nothing
      , _recentObservationsParamsLocale = Nothing
      }

instance Default RecentObservationsParams where
  def = defaultRecentObservationsParams

-- ** Optics for 'RecentObservationsParams'
makeLenses ''RecentObservationsParams
makeFieldLabels ''RecentObservationsParams

-------------------------------------------------------------------------------
-- * Recent notable observations
-------------------------------------------------------------------------------

-- | Get a list of recent notable observations within a region. Results only
-- include the most recent observation for each species in the region.
--
-- For example, get up to 10 recent notable observations from the last 30 days
-- in Park County, Wyoming (using @-XOverloadedLabels@ and
-- @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   recentNotableObservations key
--     "US-WY-029"
--     (def & #maxResults ?~ 10 & #back ?~ 30)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#397b9b8c-4ab9-4136-baae-3ffa4e5b26e4).
recentNotableObservations
  :: Text
  -- ^ eBird API key
  -> RegionCode
  -- ^ Region(s) to get observations from
  -> RecentNotableObservationsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultRecentNotableObservationsParams'/
  -> ClientM [SomeObservation]
recentNotableObservations k r RecentNotableObservationsParams{..} =
    recentNotableObservations_ k r
      _recentNotableObservationsParamsBack
      _recentNotableObservationsParamsDetail
      _recentNotableObservationsParamsHotspot
      _recentNotableObservationsParamsMaxResults
      _recentNotableObservationsParamsExtraRegions
      _recentNotableObservationsParamsLocale

-- | Optional parameters accepted by the 'RecentNotableObservationsAPI'.
--
-- Note that 'defaultRecentNotableObservationsParams' (or the 'Default'
-- instance's 'def' value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_recentNotableObservationsParamsBack' field to 30:
--
-- > def & recentNotableObservationsParamsBack ?~ 30
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #back ?~ 30
data RecentNotableObservationsParams =
    RecentNotableObservationsParams
      { -- | How many days back to look for observations
        --
        -- /1 - 30, default: 14/
        _recentNotableObservationsParamsBack :: Maybe Integer

        -- | Detail level for the resulting observations
        --
        -- /default: 'Simple'/
      , _recentNotableObservationsParamsDetail :: Maybe DetailLevel

        -- | Only get observations from hotspots
        --
        -- /default: 'False'/
      , _recentNotableObservationsParamsHotspot :: Maybe Bool

        -- | Maximum number of observations to get
        --
        -- /1 - 10000, default: all/
      , _recentNotableObservationsParamsMaxResults :: Maybe Integer

        -- | Up to 10 extra regions to get observations from
        --
        -- /default: none/
      , _recentNotableObservationsParamsExtraRegions :: Maybe RegionCode

        -- | Return observations with common names in this locale
        --
        -- /default: 'En'/
      , _recentNotableObservationsParamsLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultRecentNotableObservationsParams :: RecentNotableObservationsParams
defaultRecentNotableObservationsParams =
    RecentNotableObservationsParams
      { _recentNotableObservationsParamsBack = Nothing
      , _recentNotableObservationsParamsDetail = Nothing
      , _recentNotableObservationsParamsHotspot = Nothing
      , _recentNotableObservationsParamsMaxResults = Nothing
      , _recentNotableObservationsParamsExtraRegions = Nothing
      , _recentNotableObservationsParamsLocale = Nothing
      }

instance Default RecentNotableObservationsParams where
  def = defaultRecentNotableObservationsParams

-- ** Optics for 'RecentNotableObservationsParams'
makeLenses ''RecentNotableObservationsParams
makeFieldLabels ''RecentNotableObservationsParams

-------------------------------------------------------------------------------
-- * Recent species observations
-------------------------------------------------------------------------------

-- | Get a list of recent observations of a specific species within a region.
--
-- For example, get observations of Peregrine Falcons from the last 30 days in
-- Park County, Wyoming (using @-XOverloadedLabels@ and @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   recentSpeciesObservations key
--     "US-WY-029"
--     "perfal"
--     (def & #back ?~ 30)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#755ce9ab-dc27-4cfc-953f-c69fb0f282d9).
recentSpeciesObservations
  :: Text
  -- ^ eBird API key
  -> RegionCode
  -- ^ Region(s) to get observations from
  -> SpeciesCode
  -- ^ Species to get observations of (e.g. "barswa" for Barn Swallow)
  -> RecentSpeciesObservationsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultRecentSpeciesObservationsParams'/
  -> ClientM [Observation 'Simple]
recentSpeciesObservations k r sp RecentSpeciesObservationsParams{..} =
    recentSpeciesObservations_ k r sp
      _recentSpeciesObservationsParamsBack
      _recentSpeciesObservationsParamsHotspot
      _recentSpeciesObservationsParamsProvisional
      _recentSpeciesObservationsParamsMaxResults
      _recentSpeciesObservationsParamsExtraRegions
      _recentSpeciesObservationsParamsLocale

-- | Optional parameters accepted by the 'RecentSpeciesObservationsAPI'.
--
-- Note that 'defaultRecentSpeciesObservationsParams' (or the 'Default'
-- instance's 'def' value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_recentSpeciesObservationsParamsBack' field to 30:
--
-- > def & recentSpeciesObservationsParamsBack ?~ 30
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #back ?~ 30
data RecentSpeciesObservationsParams =
    RecentSpeciesObservationsParams
      { -- | How many days back to look for observations
        --
        -- /1 - 30, default: 14/
        _recentSpeciesObservationsParamsBack :: Maybe Integer

        -- | Only get observations from hotspots
        --
        -- /default: 'False'/
      , _recentSpeciesObservationsParamsHotspot :: Maybe Bool

        -- | Include observations which have not been reviewed
        --
        -- /default: 'False'/
      , _recentSpeciesObservationsParamsProvisional :: Maybe Bool

        -- | Maximum number of observations to get
        --
        -- /1 - 10000, default: all/
      , _recentSpeciesObservationsParamsMaxResults :: Maybe Integer

        -- | Up to 10 extra regions to get observations from
        --
        -- /default: none/
      , _recentSpeciesObservationsParamsExtraRegions :: Maybe RegionCode

        -- | Return observations with common names in this locale
        --
        -- /default: 'En'/
      , _recentSpeciesObservationsParamsLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultRecentSpeciesObservationsParams :: RecentSpeciesObservationsParams
defaultRecentSpeciesObservationsParams =
    RecentSpeciesObservationsParams
      { _recentSpeciesObservationsParamsBack = Nothing
      , _recentSpeciesObservationsParamsHotspot = Nothing
      , _recentSpeciesObservationsParamsProvisional = Nothing
      , _recentSpeciesObservationsParamsMaxResults = Nothing
      , _recentSpeciesObservationsParamsExtraRegions = Nothing
      , _recentSpeciesObservationsParamsLocale = Nothing
      }

instance Default RecentSpeciesObservationsParams where
  def = defaultRecentSpeciesObservationsParams

-- ** Optics for 'RecentSpeciesObservationsParams'
makeLenses ''RecentSpeciesObservationsParams
makeFieldLabels ''RecentSpeciesObservationsParams

-------------------------------------------------------------------------------
-- * Recent nearby observations
-------------------------------------------------------------------------------

-- | Get a list of recent observations within some radius of some
-- latitude/longitude.
--
-- For example, get up to 5 nearby observations within 10km of Cody, Wyoming
-- (using @-XOverloadedLabels@ and @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   recentNearbyObservations key
--     44.526340 (-109.056534)
--     (def & #maxResults ?~ 5 & #radius ?~ 10)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#397b9b8c-4ab9-4136-baae-3ffa4e5b26e4).
recentNearbyObservations
  :: Text
  -- ^ eBird API key
  -> Double
  -- ^ Latitude of the location to get observations near
  -> Double
  -- ^ Longitude of the location to get observations near
  -> RecentNearbyObservationsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultRecentNearbyObservationsParams'/
  -> ClientM [Observation 'Simple]
recentNearbyObservations k lat lng RecentNearbyObservationsParams{..} =
    recentNearbyObservations_ k lat lng
      _recentNearbyObservationsParamsRadius
      _recentNearbyObservationsParamsBack
      _recentNearbyObservationsParamsCategories
      _recentNearbyObservationsParamsHotspot
      _recentNearbyObservationsParamsProvisional
      _recentNearbyObservationsParamsMaxResults
      _recentNearbyObservationsParamsSortBy
      _recentNearbyObservationsParamsLocale

-- | Optional parameters accepted by the 'RecentNearbyObservationsAPI'.
--
-- Note that 'defaultRecentNearbyObservationsParams' (or the 'Default'
-- instance's 'def' value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_recentNearbyObservationsParamsRadius' field to 10km:
--
-- > def & recentNearbyObservationsParamsRadius ?~ 10
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #radius ?~ 10
data RecentNearbyObservationsParams =
    RecentNearbyObservationsParams
      { -- | Search radius from the given latitude/longitude in kilometers
        --
        -- /0 - 50, default: 25/
        _recentNearbyObservationsParamsRadius :: Maybe Integer

        -- | How many days back to look for observations
        --
        -- /1 - 30, default: 14/
      , _recentNearbyObservationsParamsBack :: Maybe Integer

        -- | Only include observations in these taxonomy categories
        --
        -- /default: all/
      , _recentNearbyObservationsParamsCategories :: Maybe TaxonomyCategories

        -- | Only get observations from hotspots
        --
        -- /default: 'False'/
      , _recentNearbyObservationsParamsHotspot :: Maybe Bool

        -- | Include observations which have not been reviewed
        --
        -- /default: 'False'/
      , _recentNearbyObservationsParamsProvisional :: Maybe Bool

        -- | Maximum number of observations to get
        --
        -- /1 - 10000, default: all/
      , _recentNearbyObservationsParamsMaxResults :: Maybe Integer

        -- | Sort observations by taxonomy ('SortObservationsBySpecies') or by
        -- date ('SortObservationsByDate', most recent first)
        --
        -- /default: 'SortObservationsByDate'/
      , _recentNearbyObservationsParamsSortBy :: Maybe SortObservationsBy

        -- | Return observations with common names in this locale
        --
        -- /default: 'En'/
      , _recentNearbyObservationsParamsLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultRecentNearbyObservationsParams :: RecentNearbyObservationsParams
defaultRecentNearbyObservationsParams =
    RecentNearbyObservationsParams
      { _recentNearbyObservationsParamsRadius = Nothing
      , _recentNearbyObservationsParamsBack = Nothing
      , _recentNearbyObservationsParamsCategories = Nothing
      , _recentNearbyObservationsParamsHotspot = Nothing
      , _recentNearbyObservationsParamsProvisional = Nothing
      , _recentNearbyObservationsParamsMaxResults = Nothing
      , _recentNearbyObservationsParamsSortBy = Nothing
      , _recentNearbyObservationsParamsLocale = Nothing
      }

instance Default RecentNearbyObservationsParams where
  def = defaultRecentNearbyObservationsParams

-- ** Optics for 'RecentNearbyObservationsParams'
makeLenses ''RecentNearbyObservationsParams
makeFieldLabels ''RecentNearbyObservationsParams

-------------------------------------------------------------------------------
-- * Recent nearby species observations
-------------------------------------------------------------------------------

-- | Get a list of recent observations of a species within some radius of some
-- latitude/longitude.
--
-- For example, get up to 5 nearby observations of Peregrine Falcons within 50km
-- of Cody, Wyoming (using @-XOverloadedLabels@ and @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   recentNearbySpeciesObservations key
--     "perfal"
--     44.526340 (-109.056534)
--     (def & #radius ?~ 50 & #maxResults ?~ 5)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#20fb2c3b-ee7f-49ae-a912-9c3f16a40397).
recentNearbySpeciesObservations
  :: Text
  -- ^ eBird API key
  -> SpeciesCode
  -- ^ Species to get observations of (e.g. "bohwax" for Bohemian Waxwing)
  -> Double
  -- ^ Latitude of the location to get observations near
  -> Double
  -- ^ Longitude of the location to get observations near
  -> RecentNearbySpeciesObservationsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultRecentNearbySpeciesObservationsParams'/
  -> ClientM [Observation 'Simple]
recentNearbySpeciesObservations k sp lat lng RecentNearbySpeciesObservationsParams{..} =
    recentNearbySpeciesObservations_ k sp lat lng
      _recentNearbySpeciesObservationsParamsRadius
      _recentNearbySpeciesObservationsParamsBack
      _recentNearbySpeciesObservationsParamsCategories
      _recentNearbySpeciesObservationsParamsHotspot
      _recentNearbySpeciesObservationsParamsProvisional
      _recentNearbySpeciesObservationsParamsMaxResults
      _recentNearbySpeciesObservationsParamsSortBy
      _recentNearbySpeciesObservationsParamsLocale

-- | Optional parameters accepted by the 'RecentNearbySpeciesObservationsAPI'.
--
-- Note that 'defaultRecentNearbySpeciesObservationsParams' (or the 'Default'
-- instance's 'def' value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_recentNearbySpeciesObservationsParamsRadius' field to 10km:
--
-- > def & recentNearbySpeciesObservationsParamsRadius ?~ 10
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #radius ?~ 10
data RecentNearbySpeciesObservationsParams =
    RecentNearbySpeciesObservationsParams
      { -- | Search radius from the given latitude/longitude in kilometers
        --
        -- /0 - 50, default: 25/
        _recentNearbySpeciesObservationsParamsRadius :: Maybe Integer

        -- | How many days back to look for observations
        --
        -- /1 - 30, default: 14/
      , _recentNearbySpeciesObservationsParamsBack :: Maybe Integer

        -- | Only include observations in these taxonomy categories
        --
        -- /default: all/
      , _recentNearbySpeciesObservationsParamsCategories :: Maybe TaxonomyCategories

        -- | Only get observations from hotspots
        --
        -- /default: 'False'/
      , _recentNearbySpeciesObservationsParamsHotspot :: Maybe Bool

        -- | Include observations which have not been reviewed
        --
        -- /default: 'False'/
      , _recentNearbySpeciesObservationsParamsProvisional :: Maybe Bool

        -- | Maximum number of observations to get
        --
        -- /1 - 10000, default: all/
      , _recentNearbySpeciesObservationsParamsMaxResults :: Maybe Integer

        -- | Sort observations by taxonomy ('SortObservationsBySpecies') or by
        -- date ('SortObservationsByDate', most recent first)
        --
        -- /default: 'SortObservationsByDate'/
      , _recentNearbySpeciesObservationsParamsSortBy :: Maybe SortObservationsBy

        -- | Return observations with common names in this locale
        --
        -- /default: 'En'/
      , _recentNearbySpeciesObservationsParamsLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultRecentNearbySpeciesObservationsParams :: RecentNearbySpeciesObservationsParams
defaultRecentNearbySpeciesObservationsParams =
    RecentNearbySpeciesObservationsParams
      { _recentNearbySpeciesObservationsParamsRadius = Nothing
      , _recentNearbySpeciesObservationsParamsBack = Nothing
      , _recentNearbySpeciesObservationsParamsCategories = Nothing
      , _recentNearbySpeciesObservationsParamsHotspot = Nothing
      , _recentNearbySpeciesObservationsParamsProvisional = Nothing
      , _recentNearbySpeciesObservationsParamsMaxResults = Nothing
      , _recentNearbySpeciesObservationsParamsSortBy = Nothing
      , _recentNearbySpeciesObservationsParamsLocale = Nothing
      }

instance Default RecentNearbySpeciesObservationsParams where
  def = defaultRecentNearbySpeciesObservationsParams

-- ** Optics for 'RecentNearbySpeciesObservationsParams'
makeLenses ''RecentNearbySpeciesObservationsParams
makeFieldLabels ''RecentNearbySpeciesObservationsParams

-------------------------------------------------------------------------------
-- * Recent nearest species observations
-------------------------------------------------------------------------------

-- | Get a list of recent observations of some species nearest to some
-- latitude/longitude.
--
-- For example, get the 5 nearest observations of Black-throated Gray Warblers
-- within 50km of Capitol Reef National Park (using @-XOverloadedLabels@ and
-- @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   recentNearestSpeciesObservations key
--     "btywar"
--     38.366970 (-111.261504)
--     (def & #radius ?~ 50 & #maxResults ?~ 5)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#6bded97f-9997-477f-ab2f-94f254954ccb).
recentNearestSpeciesObservations
  :: Text
  -- ^ eBird API key
  -> SpeciesCode
  -- ^ Species to get observations of (e.g. "bohwax" for Bohemian Waxwing)
  -> Double
  -- ^ Latitude of the location to get observations near
  -> Double
  -- ^ Longitude of the location to get observations near
  -> RecentNearestSpeciesObservationsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultRecentNearestSpeciesObservationsParams'/
  -> ClientM [Observation 'Simple]
recentNearestSpeciesObservations k sp lat lng RecentNearestSpeciesObservationsParams{..} =
    recentNearestSpeciesObservations_ k sp lat lng
      _recentNearestSpeciesObservationsParamsRadius
      _recentNearestSpeciesObservationsParamsBack
      _recentNearestSpeciesObservationsParamsHotspot
      _recentNearestSpeciesObservationsParamsProvisional
      _recentNearestSpeciesObservationsParamsMaxResults
      _recentNearestSpeciesObservationsParamsLocale

-- | Optional parameters accepted by the 'RecentNearestSpeciesObservationsAPI'.
--
-- Note that 'defaultRecentNearestSpeciesObservationsParams' (or the 'Default'
-- instance's 'def' value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_recentNearestSpeciesObservationsParamsRadius' field to 10km:
--
-- > def & recentNearestSpeciesObservationsParamsRadius ?~ 10
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #radius ?~ 10
data RecentNearestSpeciesObservationsParams =
    RecentNearestSpeciesObservationsParams
      { -- | Search radius from the given latitude/longitude in kilometers
        --
        -- /0 - 50, default: 25/
        _recentNearestSpeciesObservationsParamsRadius :: Maybe Integer

        -- | How many days back to look for observations
        --
        -- /1 - 30, default: 14/
      , _recentNearestSpeciesObservationsParamsBack :: Maybe Integer

        -- | Only get observations from hotspots
        --
        -- /default: 'False'/
      , _recentNearestSpeciesObservationsParamsHotspot :: Maybe Bool

        -- | Include observations which have not been reviewed
        --
        -- /default: 'False'/
      , _recentNearestSpeciesObservationsParamsProvisional :: Maybe Bool

        -- | Maximum number of observations to get
        --
        -- /1 - 10000, default: all/
      , _recentNearestSpeciesObservationsParamsMaxResults :: Maybe Integer

        -- | Return observations with common names in this locale
        --
        -- /default: 'En'/
      , _recentNearestSpeciesObservationsParamsLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultRecentNearestSpeciesObservationsParams :: RecentNearestSpeciesObservationsParams
defaultRecentNearestSpeciesObservationsParams =
    RecentNearestSpeciesObservationsParams
      { _recentNearestSpeciesObservationsParamsRadius = Nothing
      , _recentNearestSpeciesObservationsParamsBack = Nothing
      , _recentNearestSpeciesObservationsParamsHotspot = Nothing
      , _recentNearestSpeciesObservationsParamsProvisional = Nothing
      , _recentNearestSpeciesObservationsParamsMaxResults = Nothing
      , _recentNearestSpeciesObservationsParamsLocale = Nothing
      }

instance Default RecentNearestSpeciesObservationsParams where
  def = defaultRecentNearestSpeciesObservationsParams

-- ** Optics for 'RecentNearestSpeciesObservationsParams'
makeLenses ''RecentNearestSpeciesObservationsParams
makeFieldLabels ''RecentNearestSpeciesObservationsParams

-------------------------------------------------------------------------------
-- * Recent nearby notable observations
-------------------------------------------------------------------------------

-- | Get a list of recent /notable/ observations of some near some
-- latitude/longitude.
--
-- For example, get 5 notable observations within 25km of Capitol Reef National
-- Park (using @-XOverloadedLabels@ and @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   recentNearbyNotableObservations key
--     38.366970 (-111.261504)
--     (def & #radius ?~ 25 & #maxResults ?~ 5)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#caa348bb-71f6-471c-b203-9e1643377cbc).
recentNearbyNotableObservations
  :: Text
  -- ^ eBird API key
  -> Double
  -- ^ Latitude of the location to get observations near
  -> Double
  -- ^ Longitude of the location to get observations near
  -> RecentNearbyNotableObservationsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultRecentNearbyNotableObservationsParams'/
  -> ClientM [SomeObservation]
recentNearbyNotableObservations k lat lng RecentNearbyNotableObservationsParams{..} =
    recentNearbyNotableObservations_ k lat lng
      _recentNearbyNotableObservationsParamsRadius
      _recentNearbyNotableObservationsParamsDetail
      _recentNearbyNotableObservationsParamsBack
      _recentNearbyNotableObservationsParamsHotspot
      _recentNearbyNotableObservationsParamsMaxResults
      _recentNearbyNotableObservationsParamsLocale

-- | Optional parameters accepted by the 'RecentNearbyNotableObservationsAPI'.
--
-- Note that 'defaultRecentNearbyNotableObservationsParams' (or the 'Default'
-- instance's 'def' value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_recentNearbyNotableObservationsParamsRadius' field to 10km:
--
-- > def & recentNearbyNotableObservationsParamsRadius ?~ 10
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #radius ?~ 10
data RecentNearbyNotableObservationsParams =
    RecentNearbyNotableObservationsParams
      { -- | Search radius from the given latitude/longitude in kilometers
        --
        -- /0 - 50, default: 25/
        _recentNearbyNotableObservationsParamsRadius :: Maybe Integer

        -- | Detail level for the resulting observations
        --
        -- /default: 'Simple'/
      , _recentNearbyNotableObservationsParamsDetail :: Maybe DetailLevel

        -- | How many days back to look for observations
        --
        -- /1 - 30, default: 14/
      , _recentNearbyNotableObservationsParamsBack :: Maybe Integer

        -- | Only get observations from hotspots
        --
        -- /default: 'False'/
      , _recentNearbyNotableObservationsParamsHotspot :: Maybe Bool

        -- | Maximum number of observations to get
        --
        -- /1 - 10000, default: all/
      , _recentNearbyNotableObservationsParamsMaxResults :: Maybe Integer

        -- | Return observations with common names in this locale
        --
        -- /default: 'En'/
      , _recentNearbyNotableObservationsParamsLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultRecentNearbyNotableObservationsParams :: RecentNearbyNotableObservationsParams
defaultRecentNearbyNotableObservationsParams =
    RecentNearbyNotableObservationsParams
      { _recentNearbyNotableObservationsParamsRadius = Nothing
      , _recentNearbyNotableObservationsParamsDetail = Nothing
      , _recentNearbyNotableObservationsParamsBack = Nothing
      , _recentNearbyNotableObservationsParamsHotspot = Nothing
      , _recentNearbyNotableObservationsParamsMaxResults = Nothing
      , _recentNearbyNotableObservationsParamsLocale = Nothing
      }

instance Default RecentNearbyNotableObservationsParams where
  def = defaultRecentNearbyNotableObservationsParams

-- ** Optics for 'RecentNearbyNotableObservationsParams'
makeLenses ''RecentNearbyNotableObservationsParams
makeFieldLabels ''RecentNearbyNotableObservationsParams

-------------------------------------------------------------------------------
-- * Historical observations
-------------------------------------------------------------------------------

-- | Get a list of observations for each species seen on a specific date.
--
-- For example, get a list of 10 fully detailed observations for each species
-- seen on July 11th, 2023 in Park County, Wyoming (using @-XOverloadedLabels@
-- and @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   historicalObservations key
--     "US-WY-029"
--     "2023-07-11"
--     (def & #maxResults ?~ 10 & #detail ?~ Full)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#2d8c6ee8-c435-4e91-9f66-6d3eeb09edd2).
historicalObservations
  :: Text
  -- ^ eBird API key
  -> RegionCode
  -- ^ Region(s) to get observations from
  -> EBirdDate
  -- ^ Date to get observations on, from year 1800 to present
  -> HistoricalObservationsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultHistoricalObservationsParams'/
  -> ClientM [SomeObservation]
historicalObservations k r date HistoricalObservationsParams{..} =
    historicalObservations_ k r y m d
      _historicalObservationsParamsCategories
      _historicalObservationsParamsDetail
      _historicalObservationsParamsHotspot
      _historicalObservationsParamsProvisional
      _historicalObservationsParamsMaxResults
      _historicalObservationsParamsSelect
      _historicalObservationsParamsExtraRegions
      _historicalObservationsParamsLocale
  where
    (y,m,d) = eBirdDateToGregorian date

-- | Optional parameters accepted by the 'HistoricalObservationsAPI'.
--
-- Note that 'defaultHistoricalObservationsParams' (or the 'Default'
-- instance's 'def' value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_historicalObservationsParamsDetail' field to 'Full':
--
-- > def & historicalObservationsParamsDetail ?~ Full
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #detail ?~ Full
data HistoricalObservationsParams =
    HistoricalObservationsParams
      { -- | Only include observations in these taxonomy categories
        --
        -- /default: all/
        _historicalObservationsParamsCategories :: Maybe TaxonomyCategories

        -- | Detail level for the resulting observations
        --
        -- /default: 'Simple'/
      , _historicalObservationsParamsDetail :: Maybe DetailLevel

        -- | Only get observations from hotspots
        --
        -- /default: 'False'/
      , _historicalObservationsParamsHotspot :: Maybe Bool

        -- | Include observations which have not been reviewed
        --
        -- /default: 'False'/
      , _historicalObservationsParamsProvisional :: Maybe Bool

        -- | Maximum number of observations to get
        --
        -- /1 - 10000, default: all/
      , _historicalObservationsParamsMaxResults :: Maybe Integer

        -- | Whether to display the first or last observation of a species on
        -- the date, in the case that there are multiple observations of the
        -- same species on the date
        --
        -- /default: 'SelectLastObservation'/
      , _historicalObservationsParamsSelect :: Maybe SelectObservation

        -- | Up to 50 extra regions to get observations from
        --
        -- /default: none/
      , _historicalObservationsParamsExtraRegions :: Maybe RegionCode

        -- | Return observations with common names in this locale
        --
        -- /default: 'En'/
      , _historicalObservationsParamsLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultHistoricalObservationsParams :: HistoricalObservationsParams
defaultHistoricalObservationsParams =
    HistoricalObservationsParams
      { _historicalObservationsParamsCategories = Nothing
      , _historicalObservationsParamsDetail = Nothing
      , _historicalObservationsParamsHotspot = Nothing
      , _historicalObservationsParamsProvisional = Nothing
      , _historicalObservationsParamsMaxResults = Nothing
      , _historicalObservationsParamsSelect = Nothing
      , _historicalObservationsParamsExtraRegions = Nothing
      , _historicalObservationsParamsLocale = Nothing
      }

instance Default HistoricalObservationsParams where
  def = defaultHistoricalObservationsParams

-- ** Optics for 'HistoricalObservationsParams'
makeLenses ''HistoricalObservationsParams
makeFieldLabels ''HistoricalObservationsParams

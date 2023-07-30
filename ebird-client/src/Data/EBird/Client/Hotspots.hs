{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Data.EBird.Client.Hotspots
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Types and functions for hotspot-related eBird API queries.

module Data.EBird.Client.Hotspots where

import Data.Default
import Data.Text
import Optics.TH
import Servant.Client

import Data.EBird.API
import Data.EBird.Client.Generated

-------------------------------------------------------------------------------
-- * Region hotspots
-------------------------------------------------------------------------------

-- | Get all hotspots in a list of one or more regions ('RegionCode').
--
-- For example, get the hotspots in Albany County, Wyoming and Park County,
-- Wyoming that have been visited in the last 5 days (using @-XOverloadedLabels@
-- and @-XOverloadedStrings@):
--
-- @
-- askEBird $ regionHotspots "US-WY-001,US-WY-029" (def & #back ?~ 5)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#f4f59f90-854e-4ba6-8207-323a8cf0bfe0).
regionHotspots
  :: RegionCode
  -- ^ Region(s) to get hotspots in
  -> RegionHotspotsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultRegionHotspotsParams'/
  -> ClientM [Hotspot]
regionHotspots r RegionHotspotsParams{..} =
    regionHotspots_ r _regionHotspotsParamsBack
      -- Hard coded to JSONFormat because it makes no difference and CSVFormat
      -- does not work like it should. See the note on the generated function's
      -- parameter documentation.
      (Just JSONFormat)

-- | Optional parameters accepted by the 'RegionHotspotsAPI'.
--
-- Note that 'defaultRegionHotspotsParams' (or the 'Default' instance's 'def'
-- value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_regionHotspotsParamsBack' field to 10:
--
-- > def & regionHotspotsParamsBack ?~ 10
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #back ?~ 10
newtype RegionHotspotsParams =
    RegionHotspotsParams
      { -- | Only fetch hotspots that have been visited within this many days
        -- ago
        --
        -- /1 - 30, default: no limit/
        _regionHotspotsParamsBack :: Maybe Integer
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultRegionHotspotsParams :: RegionHotspotsParams
defaultRegionHotspotsParams =
    RegionHotspotsParams
      { _regionHotspotsParamsBack = Nothing
      }

instance Default RegionHotspotsParams where
  def = defaultRegionHotspotsParams

-- ** Optics for 'RegionHotspotsParams'
makeLenses ''RegionHotspotsParams
makeFieldLabels ''RegionHotspotsParams

-------------------------------------------------------------------------------
-- * Nearby hotspots
-------------------------------------------------------------------------------

-- | Get all hotspots within a radius of some latitude/longitude.
--
-- For example, get the hotspots within 30km of Cody, Wyoming that have been
-- visited in the last 5 days (using @-XOverloadedLabels@
-- and @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   nearbyHotspots
--     44.526340 (-109.056534)
--     (def & #radius ?~ 30 & #back ?~ 5)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#674e81c1-6a0c-4836-8a7e-6ea1fe8e6677).
nearbyHotspots
  :: Double
  -- ^ Latitude of the location to get hotspots near
  -> Double
  -- ^ Longitude of the location to get hotspots near
  -> NearbyHotspotsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultNearbyHotspotsParams'/
  -> ClientM [Hotspot]
nearbyHotspots lat lng NearbyHotspotsParams{..} =
    nearbyHotspots_ lat lng
      _nearbyHotspotsParamsBack
      _nearbyHotspotsParamsRadius
      -- Hard coded to JSONFormat because it makes no difference and CSVFormat
      -- does not work like it should. See the note on the generated function's
      -- parameter documentation.
      (Just JSONFormat)

-- | Optional parameters accepted by the 'NearbyHotspotsAPI'.
--
-- Note that 'defaultNearbyHotspotsParams' (or the 'Default' instance's 'def'
-- value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_nearbyHotspotsParamsBack' field to 10:
--
-- > def & nearbyHotspotsParamsBack ?~ 10
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #back ?~ 10
data NearbyHotspotsParams =
    NearbyHotspotsParams
      { -- | Only fetch hotspots that have been visited within this many days
        -- ago
        --
        -- /1 - 30, default: no limit/
        _nearbyHotspotsParamsBack :: Maybe Integer

        -- ^ Search radius in kilometers
        --
        -- /0 - 50, default: 25/
      , _nearbyHotspotsParamsRadius :: Maybe Integer
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultNearbyHotspotsParams :: NearbyHotspotsParams
defaultNearbyHotspotsParams =
    NearbyHotspotsParams
      { _nearbyHotspotsParamsBack = Nothing
      , _nearbyHotspotsParamsRadius = Nothing
      }

instance Default NearbyHotspotsParams where
  def = defaultNearbyHotspotsParams

-- ** Optics for 'NearbyHotspotsParams'
makeLenses ''NearbyHotspotsParams
makeFieldLabels ''NearbyHotspotsParams

-------------------------------------------------------------------------------
-- * Hotspot info
-------------------------------------------------------------------------------

-- | Get information about a hotspot.
--
-- For example, get information for a hotspot with location ID
-- \"L2373040\" (using @-XOverloadedStrings@):
--
-- @
-- askEBird $ hotspotInfo "L2373040"
-- @
--
-- Note that the endpoint for this query is simple enough that 'hotspotInfo'
-- is equivalent to the generated 'hotspotInfo_'.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#e25218db-566b-4d8b-81ca-e79a8f68c599).
hotspotInfo
  :: Text
  -- ^ Hotspot location ID, e.g. \"L2373040\"
  -> ClientM LocationData
hotspotInfo = hotspotInfo_

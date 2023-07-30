{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Data.EBird.Client.Regions
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Types and functions for region-related eBird API queries.

module Data.EBird.Client.Regions where

import Data.Default
import Data.Text
import Optics.TH
import Servant.Client

import Data.EBird.API
import Data.EBird.Client.Generated

-------------------------------------------------------------------------------
-- * Region info
-------------------------------------------------------------------------------

-- | Get a 'RegionInfo' for an eBird region.
--
-- For example, get information about the Park County, Wyoming region (using
-- @-XOverloadedLabels@ and @-XOverloadedStrings@):
--
-- @
-- askEBird $ regionInfo key "US-WY-029" def
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#07c64240-6359-4688-9c4f-ff3d678a7248).
regionInfo
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to get information for
  -> RegionInfoParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultRegionInfoParams'/
  -> ClientM RegionInfo
regionInfo k r RegionInfoParams{..} = regionInfo_ k r _regionInfoParamsFormat

-- | Optional parameters accepted by the 'RegionInfoAPI'.
--
-- Note that 'defaultRegionInfoParams' (or the 'Default' instance's 'def' value)
-- may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_regionInfoParamsFormat' field to 'NameOnlyNameFormat' (using
-- @-XOverloadedString@):
--
-- > def & regionInfoParamsFormat ?~ "nameonly"
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #format ?~ "nameonly"
newtype RegionInfoParams =
    RegionInfoParams
      { -- | How to format the region name in the response
        --
        -- /default: 'Full'/
        _regionInfoParamsFormat :: Maybe RegionNameFormat
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultRegionInfoParams :: RegionInfoParams
defaultRegionInfoParams =
    RegionInfoParams
      { _regionInfoParamsFormat = Nothing
      }

instance Default RegionInfoParams where
  def = defaultRegionInfoParams

-- ** Optics for 'RegionInfoParams'
makeLenses ''RegionInfoParams
makeFieldLabels ''RegionInfoParams

-------------------------------------------------------------------------------
-- * Sub region list
-------------------------------------------------------------------------------

-- | Get a list of sub-regions of a given region type within a given region.
-- Keep in mind that many combinations of sub-region and parent region are
-- invalid, e.g. 'CountryType' regions within \"US-WY\".
--
-- For example, get county sub regions of Wyoming (using @-XOverloadedStrings@):
--
-- @
-- askEBird $ subRegionList key Subnational2Type "US-WY"
-- @
--
-- Note that the endpoint for this query is simple enough that 'subRegionList'
-- is equivalent to the generated 'subRegionList_'.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#382da1c8-8bff-4926-936a-a1f8b065e7d5).
subRegionList
  :: Text
  -- ^ eBird API key
  -> RegionType
  -- ^ Type of subregions to fetch
  -> RegionCode
  -- ^ Parent 'RegionCode'
  -> ClientM [RegionListEntry]
subRegionList = subRegionList_

-------------------------------------------------------------------------------
-- * Adjacent regions
-------------------------------------------------------------------------------

-- | Get a list of regions adjacent to a given region. 'Subnational2' region
-- codes are only currently supported in the United States, New Zealand, or
-- Mexico.
--
-- For example, get regions adjacent to Wyoming (using @-XOverloadedStrings@):
--
-- @
-- askEBird $ adjacentRegions key "US-WY"
-- @
--
-- Note that the endpoint for this query is simple enough that 'adjacentRegions'
-- is equivalent to the generated 'adjacentRegions_'.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#3aca0519-3105-47fc-8611-a4dfd500a32f).
adjacentRegions
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to fetch the adjacent regions of
  -> ClientM [RegionListEntry]
adjacentRegions = adjacentRegions_

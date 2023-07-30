{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Data.EBird.Client.Product
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Types and functions for product-related eBird API queries.

module Data.EBird.Client.Product where

import Data.Default
import Data.Text
import Optics.TH
import Servant.Client

import Data.EBird.API
import Data.EBird.Client.Generated

-------------------------------------------------------------------------------
-- * Recent checklists
-------------------------------------------------------------------------------

-- | Get a list recently submitted checklists within a region.
--
-- For example, get up to 3 recent checklists submitted in Park County, Wyoming
-- (using @-XOverloadedLabels@ and @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   recentChecklists key
--     "US-WY-029"
--     (def & #maxResults ?~ 3)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#95a206d1-a20d-44e0-8c27-acb09ccbea1a).
recentChecklists
  :: Text
  -- ^ eBird API key
  -> RegionCode
  -- ^ Region(s) to get Checklists from
  -> RecentChecklistsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultRecentChecklistsParams'/
  -> ClientM [ChecklistFeedEntry]
recentChecklists k r RecentChecklistsParams{..} =
    recentChecklists_ k r _recentChecklistsParamsMaxResults

-- | Optional parameters accepted by the 'RecentChecklistsAPI'.
--
-- Note that 'defaultRecentChecklistsParams' (or the 'Default' instance's
-- 'def' value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_recentChecklistsParamsMaxResults' field to 3:
--
-- > def & recentChecklistsParamsMaxResults ?~ 3
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #maxResults ?~ 3
newtype RecentChecklistsParams =
    RecentChecklistsParams
      { -- | Maximum number of checklists to get
        --
        -- /1 - 200, default: 10/
        _recentChecklistsParamsMaxResults :: Maybe Integer
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultRecentChecklistsParams :: RecentChecklistsParams
defaultRecentChecklistsParams =
    RecentChecklistsParams
      { _recentChecklistsParamsMaxResults = Nothing
      }

instance Default RecentChecklistsParams where
  def = defaultRecentChecklistsParams

-- ** Optics for 'RecentChecklistsParams'
makeLenses ''RecentChecklistsParams
makeFieldLabels ''RecentChecklistsParams

-------------------------------------------------------------------------------
-- * Top 100
-------------------------------------------------------------------------------

-- | Get a list of top contributors for a region on a specific date, ranked by
-- number of species observed or number of checklists submitted.
--
-- For example, get the top 10 contributors by number of species observed on
-- July 11th, 2023 in Wyoming (using @-XOverloadedLabels@ and
-- @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   top100 key
--     "US-WY"
--     "2023-07-11"
--     (def & #maxResults ?~ 10)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#2d8d3f94-c4b0-42bd-9c8e-71edfa6347ba).
top100
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to fetch the ranking for
  --
  -- __Note:__ Only country, subnational1, or location regions are supported for
  -- this endpoint of the eBird API.
  -> EBirdDate
  -- ^ Date to get the top 100 on
  -> Top100Params
  -- ^ Optional parameters
  --
  -- /default: 'defaultTop100Params'/
  -> ClientM [Top100ListEntry]
top100 k r date Top100Params{..} =
    top100_ k r y m d _top100ParamsRankBy _top100ParamsMaxResults
  where
    (y,m,d) = eBirdDateToGregorian date

-- | Optional parameters accepted by the 'Top100API'.
--
-- Note that 'defaultTop100Params' (or the 'Default' instance's 'def' value) may
-- be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_top100ParamsMaxResults' field to 50:
--
-- > def & top100ParamsMaxResults ?~ 50
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #maxResults ?~ 50
data Top100Params =
    Top100Params
      { -- | Rank the resulting list by number of species observed or by number of
        -- checklists completed
        --
        -- /default: 'RankTop100BySpecies'/
        _top100ParamsRankBy :: Maybe RankTop100By

        -- | Maximum number of entries to fetch
        --
        -- /1 - 100, default: 100/
      , _top100ParamsMaxResults :: Maybe Integer
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultTop100Params :: Top100Params
defaultTop100Params =
    Top100Params
      { _top100ParamsRankBy = Nothing
      , _top100ParamsMaxResults = Nothing
      }

instance Default Top100Params where
  def = defaultTop100Params

-- ** Optics for 'Top100Params'
makeLenses ''Top100Params
makeFieldLabels ''Top100Params

-------------------------------------------------------------------------------
-- * Checklist feed
-------------------------------------------------------------------------------

-- | Get a list of checklists submitted within a region on a specific date.
--
-- For example, get a feed of 10 checklists submitted in Park County, Wyoming on
-- July 11th, 2023 (using @-XOverloadedLabels@ and @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   checklistFeed key
--     "US-WY-029"
--     "2023-07-11"
--     (def & #maxResults ?~ 10)
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#4416a7cc-623b-4340-ab01-80c599ede73e).
checklistFeed
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to fetch the checklist feed for
  -> EBirdDate
  -- ^ Date to get the checklist feed on
  -> ChecklistFeedParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultChecklistFeedParams'/
  -> ClientM [ChecklistFeedEntry]
checklistFeed k r date ChecklistFeedParams{..} =
    checklistFeed_ k r y m d
      _checklistFeedParamsSortBy
      _checklistFeedParamsMaxResults
  where
    (y,m,d) = eBirdDateToGregorian date

-- | Optional parameters accepted by the 'ChecklistFeedAPI'.
--
-- Note that 'defaultChecklistFeedParams' (or the 'Default' instance's 'def' value) may
-- be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_checklistFeedParamsMaxResults' field to 50:
--
-- > def & checklistFeedParamsMaxResults ?~ 50
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #maxResults ?~ 50
data ChecklistFeedParams =
    ChecklistFeedParams
      { -- | Sort the resulting list by date of checklist submission or date of
        -- checklist creation
        --
        -- /default: 'SortChecklistsByDateCreated'/
        _checklistFeedParamsSortBy :: Maybe SortChecklistsBy

        -- | Maximum number of checklists to get
        --
        -- /1 - 200, default: 10/
      , _checklistFeedParamsMaxResults :: Maybe Integer
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultChecklistFeedParams :: ChecklistFeedParams
defaultChecklistFeedParams =
    ChecklistFeedParams
      { _checklistFeedParamsSortBy = Nothing
      , _checklistFeedParamsMaxResults = Nothing
      }

instance Default ChecklistFeedParams where
  def = defaultChecklistFeedParams

-- ** Optics for 'ChecklistFeedParams'
makeLenses ''ChecklistFeedParams
makeFieldLabels ''ChecklistFeedParams

-------------------------------------------------------------------------------
-- * Regional statistics
-------------------------------------------------------------------------------

-- | Get the 'RegionalStatistics' for a region on a specific date.
--
-- For example, get the statistics for Wyoming on July 11th, 2023 (using
-- @-XOverloadedStrings@):
--
-- @
-- askEBird $
--   regionalStatistics key
--     "US-WY"
--     "2023-07-11"
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#506e63ab-abc0-4256-b74c-cd9e77968329).
regionalStatistics
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to fetch the statistics for
  -> EBirdDate
  -- ^ Date to get the statistics on
  -> ClientM RegionalStatistics
regionalStatistics k r date =
    regionalStatistics_ k r y m d
  where
    (y,m,d) = eBirdDateToGregorian date

-------------------------------------------------------------------------------
-- * Species list
-------------------------------------------------------------------------------

-- | Get a list of all species ever seen in a 'Region'.
--
-- For example, get all species ever seen in Park County, Wyoming (using
-- @-XOverloadedStrings@):
--
-- @
-- askEBird $ speciesList key "US-WY-029"
-- @
--
-- Note that the endpoint for this query is simple enough that 'speciesList' is
-- equivalent to the generated 'speciesList_'.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#55bd1b26-6951-4a88-943a-d3a8aa1157dd).
speciesList
  :: Text
  -- ^ eBird API key
  -> Region
  -- ^ Region to fetch the species list for
  -> ClientM [SpeciesCode]
speciesList = speciesList_

-------------------------------------------------------------------------------
-- * View checklist
-------------------------------------------------------------------------------

-- | Get information about a checklist.
--
-- For example, get information for a checklist with submission ID
-- \"S144646447\" (using @-XOverloadedStrings@):
--
-- @
-- askEBird $ viewChecklist key "S144646447"
-- @
--
-- Note that the endpoint for this query is simple enough that 'viewChecklist'
-- is equivalent to the generated 'viewChecklist_'.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#2ee89672-4211-4fc1-8493-5df884fbb386).
viewChecklist
  :: Text
  -- ^ eBird API key
  -> Text
  -- ^ Checklist submission ID, e.g. \"S144646447\"
  -> ClientM Checklist
viewChecklist = viewChecklist_

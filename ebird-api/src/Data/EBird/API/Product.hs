{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Data.EBird.API.Product
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Types related to eBird product API values.

module Data.EBird.API.Product where

import Control.Arrow
import Data.Aeson
import Data.Attoparsec.Text
import Data.Function
import Data.Functor
import Data.String
import Data.Text as Text
import Optics
import Servant.API (ToHttpApiData(..))

import Data.EBird.API.EBirdString

-------------------------------------------------------------------------------
-- * Top 100 contributors API types
-------------------------------------------------------------------------------

-- | Values held in the top 100 contributors list returned by the eBird API.
data Top100ListEntry =
    Top100ListEntry
      { -- | The profile handle of the user, whocse profile may be seen at
        -- ebird.org/profile/{handle} if they have a profile
        _top100ListEntryProfileHandle :: Maybe Text

        -- | The display name of the user (typically their full name)
      , _top100ListEntryUserDisplayName :: Text

        -- | The number of species the user observed on the date
      , _top100ListEntryNumSpecies :: Integer

        -- | The number of complete checklists the user contributed on the date
      , _top100ListEntryNumCompleteChecklists :: Integer

        -- | The ranking of the user
      , _top100ListEntryRowNum :: Integer

        -- | The user ID od the user
      , _top100ListEntryUserId :: Text
      }
  deriving (Show, Read, Eq)

-- ** Optics for the Top100ListEntry type

makeLenses ''Top100ListEntry
makeFieldLabels ''Top100ListEntry

-- | How to rank the list returned by the 'Data.EBird.API.Top100API'.
data RankTop100By
      -- | Rank the list by the number of species seen
    = RankTop100BySpecies

      -- | Rank the list by number of contributed checklists
    | RankTop100ByChecklists
  deriving (Show, Read, Eq)

-------------------------------------------------------------------------------
-- * Regional statistics API types
-------------------------------------------------------------------------------

-- | Values returned by the 'Data.EBird.API.RegionalStatisticsAPI'.
data RegionalStatistics =
    RegionalStatistics
      { -- | Number of checklists submitted in the region
        _regionalStatisticsNumChecklists :: Integer

        -- | Number of contributors who have submitted checklists in the region
      , _regionalStatisticsNumContributors :: Integer

        -- | Number of species included in checklists in the region
      , _regionalStatisticsNumSpecies :: Integer
      }
  deriving (Show, Read, Eq)

-- ** Optics for the RegionalStatistics type

makeLenses ''RegionalStatistics
makeFieldLabels ''RegionalStatistics

-------------------------------------------------------------------------------
-- Aeson instances
-------------------------------------------------------------------------------

-- | Explicit instance for compatibility with their field names
instance FromJSON Top100ListEntry where
  parseJSON = withObject "Top100ListEntry" $ \v ->
      Top100ListEntry
        <$> v .:? "profileHandle"
        <*> v .: "userDisplayName"
        <*> v .: "numSpecies"
        <*> v .: "numCompleteChecklists"
        <*> v .: "rowNum"
        <*> v .: "userId"

-- | Explicit instance for compatibility with their field names
instance ToJSON Top100ListEntry where
  toJSON Top100ListEntry{..} =
      object $
        [ "userDisplayname" .= _top100ListEntryUserDisplayName
        , "numSpecies" .= _top100ListEntryNumSpecies
        , "numCompleteChecklists" .= _top100ListEntryNumCompleteChecklists
        , "rowNum" .= _top100ListEntryRowNum
        , "userId" .= _top100ListEntryUserId
        ]
        -- Fields that may or may not be included, depending on the contributor
        -- data
        <> ["profileHandle" .= profileHandle
           | Just profileHandle <- [_top100ListEntryProfileHandle]
           ]

-- | Explicit instance for compatibility with their field names
instance FromJSON RegionalStatistics where
  parseJSON = withObject "RegionalStatistics" $ \v ->
      RegionalStatistics
        <$> v .: "numChecklists"
        <*> v .: "numContributors"
        <*> v .: "numSpecies"

-- | Explicit instance for compatibility with their field names
instance ToJSON RegionalStatistics where
  toJSON RegionalStatistics{..} =
      object
        [ "numChecklists" .= _regionalStatisticsNumChecklists
        , "numContributors" .= _regionalStatisticsNumContributors
        , "numSpecies" .= _regionalStatisticsNumSpecies
        ]

-------------------------------------------------------------------------------
-- 'EBirdString' instances
-------------------------------------------------------------------------------

-- | The eBird string for a 'RankTop100By' value is either "spp" or "cl".
instance EBirdString RankTop100By where
  toEBirdString =
      \case
        RankTop100BySpecies -> "spp"
        RankTop100ByChecklists -> "cl"

  fromEBirdString str =
      parseOnly parseRankTop100By str
    & left (("Failed to parse RankTop100By: " <>) . Text.pack)

-------------------------------------------------------------------------------
-- IsString instances
-------------------------------------------------------------------------------

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString RankTop100By where
  fromString = unsafeFromEBirdString . Text.pack

-------------------------------------------------------------------------------
-- * attoparsec parsers
-------------------------------------------------------------------------------

-- | Parse a 'RankTop100By' value
parseRankTop100By :: Parser RankTop100By
parseRankTop100By =
    choice
      [ "spp" $> RankTop100BySpecies
      , "cl" $> RankTop100ByChecklists
      ]
  where
    _casesCovered :: RankTop100By -> ()
    _casesCovered =
      \case
        RankTop100BySpecies -> ()
        RankTop100ByChecklists -> ()

-------------------------------------------------------------------------------
-- 'ToHttpApiData' instances
-------------------------------------------------------------------------------

instance ToHttpApiData RankTop100By where
  toUrlPiece = toEBirdString

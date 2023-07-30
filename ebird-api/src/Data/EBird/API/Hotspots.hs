{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Data.EBird.API.Hotspots
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Types and functions related to eBird hotspot API values.

module Data.EBird.API.Hotspots where

import Control.Arrow
import Data.Aeson
import Data.Attoparsec.Text
import Data.Function
import Data.Functor
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Optics
import Servant.API

import Data.EBird.API.EBirdString
import Data.EBird.API.Regions
import Data.EBird.API.Util.Time

-------------------------------------------------------------------------------
-- * Hotspot type
-------------------------------------------------------------------------------

-- | eBird hotspots, as returned by the 'Data.EBird.API.RegionHotspotsAPI'
data Hotspot =
    Hotspot
      { -- | Location ID of the hotspot
        _hotspotLocationId :: Text

        -- | Name of the hotspot
      , _hotspotLocationName :: Text

        -- | The country the hotspot is in
      , _hotspotCountryCode :: Region

        -- | The state the hotspot is in
      , _hotspotSubnational1Code :: Region

        -- | The county the hotspot is in
      , _hotspotSubnational2Code :: Region

        -- | The latitude of the hotspot
      , _hotspotLatitude :: Double

        -- | The longitude of the hotspot
      , _hotspotLongitude :: Double

        -- | The date and time of the latest observation at the hotspot. Could
        -- be 'Nothing' if the hotspot has never been birded
      , _hotspotLatestObsDateTime :: Maybe EBirdDateTime

        -- | The number of species ever seen at the hotspot. Could be 'Nothing'
        -- if the hotspot has never been birded
      , _hotspotNumSpeciesAllTime :: Maybe Integer
      }
  deriving (Show, Read, Eq)

-- ** Optics for the Hotspot type

makeLenses ''Hotspot
makeFieldLabels ''Hotspot

-------------------------------------------------------------------------------
-- * Auxiliary eBird hotspot-related API types
-------------------------------------------------------------------------------

-- | Used to specify what format hotspot values should be returned in from the
-- hotspots APIs.
data CSVOrJSONFormat = CSVFormat | JSONFormat
  deriving (Show, Read, Eq)

-------------------------------------------------------------------------------
-- aeson instances
-------------------------------------------------------------------------------

-- | Explicit instance for compatibility with their field names
instance FromJSON Hotspot where
  parseJSON = withObject "Hotspot" $ \v ->
      Hotspot
        <$> v .: "locId"
        <*> v .: "locName"
        <*> v .: "countryCode"
        <*> v .: "subnational1Code"
        <*> v .: "subnational2Code"
        <*> v .: "lat"
        <*> v .: "lng"
        <*> v .:? "latestObsDt"
        <*> v .:? "numSpeciesAllTime"

-- | Explicit instance for compatibility with their field names
instance ToJSON Hotspot where
  toJSON Hotspot{..} =
      object $
        [ "locId" .= _hotspotLocationId
        , "locName" .= _hotspotLocationName
        , "countryCode" .= _hotspotCountryCode
        , "subnational1Code" .= _hotspotSubnational1Code
        , "subnational2Code" .= _hotspotSubnational2Code
        , "lat" .= _hotspotLatitude
        , "lng" .= _hotspotLongitude
        ]
        -- Fields that may or may not be present depending on the data
        <> [ "latestObsDt" .= latestObsDt
           | Just latestObsDt <- [_hotspotLatestObsDateTime]
           ]
        <> [ "numSpeciesAllTime" .= numSpecies
           | Just numSpecies <- [_hotspotNumSpeciesAllTime]
           ]

-------------------------------------------------------------------------------
-- EBirdString instances
-------------------------------------------------------------------------------

-- | The eBird string of a 'CSVOrJSONFormat' value is either "csv" or "json".
instance EBirdString CSVOrJSONFormat where
  toEBirdString =
      \case
        CSVFormat -> "csv"
        JSONFormat -> "json"

  fromEBirdString str =
        parseOnly parseCSVOrJSONFormat str
      & left (("Failed to parse CSVOrJSONFormat: " <>) . Text.pack)

-------------------------------------------------------------------------------
-- IsString instances
-------------------------------------------------------------------------------

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString CSVOrJSONFormat where
  fromString = unsafeFromEBirdString . Text.pack

-------------------------------------------------------------------------------
-- * attoparsec parsers
-------------------------------------------------------------------------------

-- | Parse a list of eBird API taxononomy categories. To avoid the partial
-- behavior of converting a 'sepBy1' result into a 'Data.List.NonEmpty', we
-- manually parse the first category followed by an optional tail.
parseCSVOrJSONFormat :: Parser CSVOrJSONFormat
parseCSVOrJSONFormat =
    choice
      [ "csv" $> CSVFormat
      , "json" $> JSONFormat
      ]
  where
    _casesCovered :: CSVOrJSONFormat -> ()
    _casesCovered =
      \case
        CSVFormat -> ()
        JSONFormat -> ()

-------------------------------------------------------------------------------
-- 'ToHttpApiData' instances
-------------------------------------------------------------------------------

instance ToHttpApiData CSVOrJSONFormat where
  toUrlPiece = toEBirdString

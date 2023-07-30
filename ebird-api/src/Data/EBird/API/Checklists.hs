{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Data.EBird.API.Checklists
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Types related to eBird checklist API values.

module Data.EBird.API.Checklists where

import Control.Arrow
import Data.Aeson
import Data.Attoparsec.Text
import Data.Function
import Data.Functor
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Optics
import Servant.API (ToHttpApiData(..))

import Data.EBird.API.EBirdString
import Data.EBird.API.Regions
import Data.EBird.API.Taxonomy
import Data.EBird.API.Util.Time

-------------------------------------------------------------------------------
-- * Checklist types
-------------------------------------------------------------------------------

-- | Values returned by the 'Data.EBird.API.ViewChecklistAPI'
data Checklist =
    Checklist
      { -- | Project ID, e.g. \"EBIRD\"
        _checklistProjectId :: Text

        -- | Checklist submission ID, e.g. \"S144646447\"
      , _checklistSubId :: Text

        -- | Checklist protocol ID, e.g. \"P21\"
      , _checklistProtocolId :: Text

        -- | Checklist location ID
      , _checklistLocationId :: Text

        -- | Checklist group ID
      , _checklistGroupId :: Text

        -- | Checklist duration, only 'Just' for checklists of appropriate
        -- protocols (e.g. not incidentals)
      , _checklistDurationHours :: Maybe Double

        -- | Was every bird observed reported?
      , _checklistAllObsReported :: Bool

        -- | What date and time was the checklist created (i.e. submitted)?
      , _checklistCreationDateTime :: EBirdDateTime

        -- | What date and time what the checklist last edited?
      , _checklistLastEditedDateTime :: EBirdDateTime

        -- | What date and time what the checklist started?
      , _checklistObsDateTime :: EBirdDateTime

        -- | TODO: Not sure what this is for
      , _checklistObsTimeValid :: Bool

        -- | The ID of the checklist, e.g. \"CL24936\"
      , _checklistChecklistId :: Text

        -- | The number of observers on this checklist
      , _checklistNumObservers :: Integer

        -- | Distance travelled during this checklist in kilometers, only 'Just'
        -- for checklists of appropriate protocols (e.g. not incidentals)
      , _checklistEffortDistanceKm :: Maybe Double

        -- | The unit of distance used for the checklist submission (e.g. "mi"),
        -- only 'Just' for checklists of appropriate protocols (e.g. not
        -- incidentals)
      , _checklistEffortDistanceEnteredUnit :: Maybe Text

        -- | The subnational1 region (state) that the checklist was submitted in
      , _checklistSubnational1Code :: Region

        -- | Method of checklist submission
      , _checklistSubmissionMethodCode :: Text

        -- | Version of the method of checklist submission, e.g. "2.13.2_SDK33"
      , _checklistSubmissionMethodVersion :: Text

        -- | Display-ready version of the method of checklist submission, e.g.
        -- "2.13.2"
      , _checklistSubmissionMethodVersionDisp :: Text

        -- | Display name of the user that submitted the checklist
      , _checklistUserDisplayName :: Text

        -- | Number of species included in observations on this checklist
      , _checklistNumSpecies :: Integer

        -- | Submission auxiliary entry methods
        --
        -- TODO: Not sure what these are about
      , _checklistSubAux :: [SubAux]

        -- | Submission auxiliary entry methods that use aritificial
        -- intelligence
        --
        -- TODO: Not sure what these are about
      , _checklistSubAuxAI :: [SubAuxAI]

        -- | Observations included in the checklist
      , _checklistObs:: [ChecklistObservation]
      }
  deriving (Show, Read, Eq)

-- | Observation values included in checklists.
data ChecklistObservation =
    ChecklistObservation
      { -- | Species code of the species, e.g. "norfli"
        _checklistObservationSpeciesCode :: SpeciesCode

        -- | The date and time of the observation. It is not clear when this
        -- would not be equal to the 'checklistObsDateTime' field of the enclosing
        -- checklist.
      , _checklistObservationObsDateTime :: EBirdDateTime

        -- | ID of the observation
      , _checklistObservationObsId :: Text

        -- | A string representation of the quantity of the observation. If just
        -- the presence is noted, the string will be \"X\"
      , _checklistObservationHowManyStr :: Text
      }
  deriving (Show, Read, Eq)

-- | Values included in the 'checklistSubAux' field of 'Checklist's.
data SubAux =
    SubAux
      { -- | Submission ID
        _subAuxSubId :: Text

        -- | E.g. "nocturnal"
      , _subAuxFieldName :: Text

        -- | E.g. "ebird_nocturnal"
      , _subAuxEntryMethodCode :: Text

        -- | E.g. "0"
      , _subAuxAuxCode :: Text
      }
  deriving (Show, Read, Eq)

-- | Values included in the 'checklistSubAuxAI' field of 'Checklist's.
data SubAuxAI =
    SubAuxAI
      { -- | Submission ID
        _subAuxAISubId :: Text

        -- | E.g. "concurrent"
      , _subAuxAIMethod :: Text

        -- | E.g. "sound"
      , _subAuxAIType :: Text

        -- | E.g. "merlin"
      , _subAuxAISource :: Text

        -- | E.g. 0
      , _subAuxEventId :: Integer
      }
  deriving (Show, Read, Eq)

-- | eBird checklists. Note that we do not include some redundant fields of
-- checklist values returned by the API (e.g. @subID@, which is always the same
-- value as @subId@).
data ChecklistFeedEntry =
    ChecklistFeedEntry
      { -- | The location ID of the checklist
        _checklistFeedEntryLocationId :: Text

        -- | Checklist submission ID
      , _checklistFeedEntrySubId :: Text

        -- | The display name of the user that submitted this checklist
      , _checklistFeedEntryUserDisplayName :: Text

        -- | Number of species included on this checklist
      , _checklistFeedEntryNumSpecies :: Integer

        -- | Date that this checklist was started
      , _checklistFeedEntryDate :: EBirdDate

        -- | Time that this checklist was started
      , _checklistFeedEntryTime :: EBirdTime

        -- | Location data for the checklist
      , _checklistFeedEntryLocationData :: LocationData
      }
  deriving (Show, Read, Eq)


-- | eBird checklist or hotspot location data. Note that we do not include some
-- redundant fields of location data values returned by the API (e.g. @locName@,
-- which is always the same value as @name@).
data LocationData =
    LocationData
      { -- | Name of the location
        _locationDataName :: Text

        -- | Latitude of the location
      , _locationDataLatitude :: Double

        -- | Longitude of the location
      , _locationDataLongitude :: Double

        -- | Country code of the location
      , _locationDataCountryCode :: Region

        -- | Country name of the location
      , _locationDataCountryName :: Text

        -- | Subnational1 region that this location is in
      , _locationDataSubnational1Code :: Region

        -- | Name of the subnational1 region that this location is in
      , _locationDataSubnational1Name :: Text

        -- | Subnational2 region that this location is in
      , _locationDataSubnational2Code :: Region

        -- | Name of the subnational2 region that this location is in
      , _locationDataSubnational2Name :: Text

        -- | Is this location an eBird hotspot?
      , _locationDataIsHotspot:: Bool

        -- | A compound name for the location consisting of the location name,
        -- county name, state name, and country name.
      , _locationDataHeirarchicalName:: Text
      }
  deriving (Show, Read, Eq)

-------------------------------------------------------------------------------
-- * Auxiliary eBird checklist-related API types
-------------------------------------------------------------------------------

-- | How to rank the list returned by the 'Data.EBird.API.Top100API'.
data SortChecklistsBy
      -- | Sort checklists by the date of the observations they contain
    = SortChecklistsByDateCreated

      -- | Sort checklists by the date they were submitted
    | SortChecklistsByDateSubmitted
  deriving (Show, Read, Eq)

-------------------------------------------------------------------------------
-- * Optics for checklist types
-------------------------------------------------------------------------------

makeLenses ''Checklist
makeFieldLabels ''Checklist
makeLenses ''ChecklistObservation
makeFieldLabels ''ChecklistObservation
makeLenses ''SubAux
makeFieldLabels ''SubAux
makeLenses ''SubAuxAI
makeFieldLabels ''SubAuxAI
makeLenses ''ChecklistFeedEntry
makeFieldLabels ''ChecklistFeedEntry
makeLenses ''LocationData
makeFieldLabels ''LocationData

-------------------------------------------------------------------------------
-- aeson instances
-------------------------------------------------------------------------------

-- | Explicit instance for compatibility with their field names
instance FromJSON Checklist where
  parseJSON = withObject "Checklist" $ \v ->
      Checklist
        <$> v .: "projId"
        <*> v .: "subId"
        <*> v .: "protocolId"
        <*> v .: "locId"
        <*> v .: "groupId"
        <*> v .:? "durationHrs"
        <*> v .: "allObsReported"
        <*> v .: "creationDt"
        <*> v .: "lastEditedDt"
        <*> v .: "obsDt"
        <*> v .: "obsTimeValid"
        <*> v .: "checklistId"
        <*> v .: "numObservers"
        <*> v .:? "effortDistanceKm"
        <*> v .:? "effortDistanceEnteredUnit"
        <*> v .: "subnational1Code"
        <*> v .: "submissionMethodCode"
        <*> v .: "submissionMethodVersion"
        <*> v .: "submissionMethodVersionDisp"
        <*> v .: "userDisplayName"
        <*> v .: "numSpecies"
        <*> v .: "subAux"
        <*> v .: "subAuxAi"
        <*> v .: "obs"

-- | Explicit instance for compatibility with their field names
instance ToJSON Checklist where
  toJSON Checklist{..} =
      object $
        [ "projId" .= _checklistProjectId
        , "subId" .= _checklistSubId
        , "protocolId" .= _checklistProtocolId
        , "locId" .= _checklistLocationId
        , "groupId" .= _checklistGroupId
        , "allObsReported" .= _checklistAllObsReported
        , "creationDt" .= _checklistCreationDateTime
        , "lastEditedDt" .= _checklistLastEditedDateTime
        , "obsDt" .= _checklistObsDateTime
        , "obsTimeValid" .= _checklistObsTimeValid
        , "checklistId" .= _checklistChecklistId
        , "numObservers" .= _checklistNumObservers
        , "subnational1Code" .= _checklistSubnational1Code
        , "submissionMethodCode" .= _checklistSubmissionMethodCode
        , "submissionMethodVersion" .= _checklistSubmissionMethodVersion
        , "submissionMethodVersionDisp" .= _checklistSubmissionMethodVersionDisp
        , "userDisplayName" .= _checklistUserDisplayName
        , "numSpecies" .= _checklistNumSpecies
        , "subAux" .= _checklistSubAux
        , "subAuxAi" .= _checklistSubAuxAI
        , "obs" .= _checklistObs
        ]
        -- Fields that may or may not be included, depending on the observation
        -- data
        <> [ "durationHrs" .= duration
           | Just duration <- [_checklistDurationHours]
           ]
        <> [ "effortDistanceKm" .= distance
           | Just distance <- [_checklistEffortDistanceKm]
           ]
        <> [ "effortDistanceEnteredUnit" .= unit
           | Just unit <- [_checklistEffortDistanceEnteredUnit]
           ]

-- | Explicit instance for compatibility with their field names
instance FromJSON ChecklistObservation where
  parseJSON = withObject "ChecklistObservation" $ \v ->
      ChecklistObservation
        <$> v .: "speciesCode"
        <*> v .: "obsDt"
        <*> v .: "obsId"
        <*> v .: "howManyStr"

-- | Explicit instance for compatibility with their field names
instance ToJSON ChecklistObservation where
  toJSON ChecklistObservation{..} =
      object
        [ "speciesCode" .= _checklistObservationSpeciesCode
        , "obsDt" .= _checklistObservationObsDateTime
        , "obsId" .= _checklistObservationObsId
        , "howManyStr" .= _checklistObservationHowManyStr
        ]

-- | Explicit instance for compatibility with their field names
instance FromJSON SubAux where
  parseJSON = withObject "SubAux" $ \v ->
      SubAux
        <$> v .: "subId"
        <*> v .: "fieldName"
        <*> v .: "entryMethodCode"
        <*> v .: "auxCode"

-- | Explicit instance for compatibility with their field names
instance ToJSON SubAux where
  toJSON SubAux{..} =
      object
        [ "subId" .= _subAuxSubId
        , "fieldName" .= _subAuxFieldName
        , "entryMethodCode" .= _subAuxEntryMethodCode
        , "auxCode" .= _subAuxAuxCode
        ]

-- | Explicit instance for compatibility with their field names
instance FromJSON SubAuxAI where
  parseJSON = withObject "SubAuxAI" $ \v ->
      SubAuxAI
        <$> v .: "subId"
        <*> v .: "method"
        <*> v .: "aiType"
        <*> v .: "source"
        <*> v .: "eventId"

-- | Explicit instance for compatibility with their field names
instance ToJSON SubAuxAI where
  toJSON SubAuxAI{..} =
      object
        [ "subId" .= _subAuxAISubId
        , "method" .= _subAuxAIMethod
        , "aiType" .= _subAuxAIType
        , "eventId" .= _subAuxEventId
        ]

-- | Explicit instance for compatibility with their field names
instance FromJSON ChecklistFeedEntry where
  parseJSON = withObject "ChecklistFeedEntry" $ \v ->
      ChecklistFeedEntry
        <$> v .: "locId"
        <*> v .: "subId"
        <*> v .: "userDisplayName"
        <*> v .: "numSpecies"
        <*> v .: "obsDt"
        <*> v .: "obsTime"
        <*> v .: "loc"

-- | Explicit instance for compatibility with their field names
instance ToJSON ChecklistFeedEntry where
  toJSON ChecklistFeedEntry{..} =
      object
        [ "locId" .= _checklistFeedEntryLocationId
        , "subId" .= _checklistFeedEntrySubId
        , "userDisplayName" .= _checklistFeedEntryUserDisplayName
        , "numSpecies" .= _checklistFeedEntryNumSpecies
        , "obsDt" .= _checklistFeedEntryDate
        , "obsTime" .= _checklistFeedEntryTime
        , "loc" .= _checklistFeedEntryLocationData
        ]

-- | Explicit instance for compatibility with their field names
instance FromJSON LocationData where
  parseJSON = withObject "LocationData" $ \v ->
      LocationData
        <$> v .: "name"
        <*> v .: "latitude"
        <*> v .: "longitude"
        <*> v .: "countryCode"
        <*> v .: "countryName"
        <*> v .: "subnational1Code"
        <*> v .: "subnational1Name"
        <*> v .: "subnational2Code"
        <*> v .: "subnational2Name"
        <*> v .: "isHotspot"
        <*> v .: "hierarchicalName"

-- | Explicit instance for compatibility with their field names
instance ToJSON LocationData where
  toJSON LocationData{..} =
      object
        [ "name" .= _locationDataName
        , "latitude" .= _locationDataLatitude
        , "longitude" .= _locationDataLongitude
        , "countryCode" .= _locationDataCountryCode
        , "countryName" .= _locationDataCountryName
        , "subnational1Code" .= _locationDataSubnational1Code
        , "subnational1Name" .= _locationDataSubnational1Name
        , "subnational2Code" .= _locationDataSubnational2Code
        , "subnational2Name" .= _locationDataSubnational2Name
        , "isHotspot" .= _locationDataIsHotspot
        , "hierarchicalName" .= _locationDataHeirarchicalName
        ]

-------------------------------------------------------------------------------
-- EBirdString instances
-------------------------------------------------------------------------------

-- | The eBird string for a 'SortChecklistsBy' value is either "obs_dt" or
-- "creation_dt".
instance EBirdString SortChecklistsBy where
  toEBirdString =
      \case
        SortChecklistsByDateCreated -> "obs_dt"
        SortChecklistsByDateSubmitted -> "creation_dt"

  fromEBirdString str =
        parseOnly parseSortChecklistsBy str
      & left (("Failed to parse SortChecklistsBy: " <>) . Text.pack)

-------------------------------------------------------------------------------
-- IsString isntances
-------------------------------------------------------------------------------

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString SortChecklistsBy where
  fromString = unsafeFromEBirdString . Text.pack

-------------------------------------------------------------------------------
-- * attoparsec parsers
-------------------------------------------------------------------------------

-- | Parse a 'SortChecklistsBy' value
parseSortChecklistsBy :: Parser SortChecklistsBy
parseSortChecklistsBy =
    choice
      [ "obs_dt" $> SortChecklistsByDateCreated
      , "creation_dt" $> SortChecklistsByDateSubmitted
      ]
  where
    _casesCovered :: SortChecklistsBy -> ()
    _casesCovered =
      \case
        SortChecklistsByDateCreated -> ()
        SortChecklistsByDateSubmitted -> ()

-------------------------------------------------------------------------------
-- 'ToHttpApiData' instances
-------------------------------------------------------------------------------

instance ToHttpApiData SortChecklistsBy where
  toUrlPiece = toEBirdString

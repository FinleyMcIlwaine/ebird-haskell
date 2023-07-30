{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


-- |
-- Module      : Data.EBird.API.Observations
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Types and functions related to eBird observation API values.

module Data.EBird.API.Observations where

import Control.Arrow
import Data.Aeson
import Data.Aeson.KeyMap
import Data.Attoparsec.Text
import Data.Function
import Data.Functor
import Data.Maybe
import Data.String
import Data.Text as Text
import Optics
import Servant.API (ToHttpApiData(..))

import Data.EBird.API.EBirdString
import Data.EBird.API.Regions
import Data.EBird.API.Util.Time

-------------------------------------------------------------------------------
-- * Observation types
-------------------------------------------------------------------------------

-- | An observation of a species submitted to eBird within a checklist. The
-- 'DetailLevel' index indicates whether the observation data includes "full"
-- details.
data Observation (detail :: DetailLevel) =
    Observation
      { -- | Species code, e.g. "bohwax"
        _observationSpeciesCode :: Text

        -- | Common name, e.g. "Bohemian Waxwing"
      , _observationCommonName :: Text

        -- | Scientific name, e.g. "Bombycilla garrulus"
      , _observationScientificName :: Text

        -- | Location ID, e.g. \"L7884500\"
      , _observationLocationId :: Text

        -- | Location name, e.g. "Frog Pond"
      , _observationLocationName :: Text

        -- | Date and time of observation
      , _observationDateTime :: EBirdDateTime

        -- | How many were seen? Sometimes omitted.
      , _observationHowMany :: Maybe Integer

        -- | Observation latitude
      , _observationLatitude :: Double

        -- | Observation longitude
      , _observationLongitude :: Double

        -- | Is this observation valid?
      , _observationValid :: Bool

        -- | Has this observation been reviewed?
      , _observationReviewed :: Bool

        -- | Is the location of this observation private?
      , _observationLocationPrivate :: Bool

        -- | Submission ID
      , _observationSubId :: Text

      , _observationFullDetail :: ObservationDetails detail
      }

deriving instance Show (Observation 'Simple)
deriving instance Show (Observation 'Full)
deriving instance Eq (Observation 'Simple)
deriving instance Eq (Observation 'Full)

-- | Extra details that may be attached to an observation. At the moment, it
-- only seems possible to get 'Full' detailed observations from the notable
-- observation endpoints (e.g. 'Data.EBird.API.RecentNotableObservationsAPI').
data ObservationDetails (detail :: DetailLevel) where
    NoDetails :: ObservationDetails 'Simple
    FullDetails ::
      { -- | The subnational2 region that this observation took place in
        _observationDetailsSubnational2Code :: Region

        -- | The name of the subnational2 region that this observation took
        -- place in
      , _observationDetailsSubnational2Name :: Text

        -- | The subnational1 region that this observation took place in
      , _observationDetailsSubnational1Code :: Region

        -- | The name of the subnational1 region that this observation took
        -- place in
      , _observationDetailsSubnational1Name :: Text

        -- | The country region that this observation took place in
      , _observationDetailsCountryCode :: Region

        -- | The name of the country region that this observation took place in
      , _observationDetailsCountryName :: Text

        -- | The display name of the user that submitted this observation
      , _observationDetailsUserDisplayName :: Text

        -- | The unique ID of this observation
      , _observationDetailsObsId :: Text

        -- | The ID of the checklist that this observation was submitted with,
        -- e.g. \"CL24936\"
      , _observationDetailsChecklistId :: Text

        -- | Whether the count for the observation was provided as just \"X\"
      , _observationDetailsPresenceNoted :: Bool

        -- | Whether this observation was submitted with comments
      , _observationDetailsHasComments :: Bool

        -- | The last name of the user that submitted this observation
      , _observationDetailsLastName :: Text

        -- | The first name of the user that submitted this observation
      , _observationDetailsFirstName :: Text

        -- | Whether this observation has media such as photos, videos, or
        -- audio attached
      , _observationDetailsHasRichMedia :: Bool
      } -> ObservationDetails 'Full

deriving instance Show (ObservationDetails 'Simple)
deriving instance Show (ObservationDetails 'Full)
deriving instance Eq (ObservationDetails 'Simple)
deriving instance Eq (ObservationDetails 'Full)

-- | 'Observation' values of existentially quantified detail.
data SomeObservation where
    SomeObservation :: Observation detail -> SomeObservation

instance Show SomeObservation where
  show (SomeObservation o) =
      case _observationFullDetail o of
        NoDetails -> show o
        FullDetails{} -> show o

-------------------------------------------------------------------------------
-- * Auxiliary eBird observation API types
-------------------------------------------------------------------------------

-- | The promoted constructors of this type are used as type-level indices on
-- the 'Observation' type to determine whether an observation is 'Simple' detail
-- or 'Full' detail.
data DetailLevel = Simple | Full
  deriving (Show, Read, Eq)

-- | Values representing the ways that observations may be sorted in responses
-- from the API.
data SortObservationsBy
    = SortObservationsByDate
    | SortObservationsBySpecies
  deriving (Show, Read, Eq)

-- | Values representing how to pick which 'Observation's are returned from the
-- 'Data.EBird.API.HistoricalObservationsAPI' in the case that there are several
-- observations of the same species on the date.
data SelectObservation
    = SelectFirstObservation
    | SelectLastObservation
  deriving (Show, Read, Eq)

-------------------------------------------------------------------------------
-- * Optics for observation types
-------------------------------------------------------------------------------

makeLenses ''Observation
makeFieldLabels ''Observation

observationDetailsSubnational2Code :: Lens' (ObservationDetails 'Full) Region
observationDetailsSubnational2Code =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsSubnational2Code = c })
      <$> f _observationDetailsSubnational2Code

observationDetailsSubnational2Name :: Lens' (ObservationDetails 'Full) Text
observationDetailsSubnational2Name =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsSubnational2Name = c })
      <$> f _observationDetailsSubnational2Name

observationDetailsSubnational1Code :: Lens' (ObservationDetails 'Full) Region
observationDetailsSubnational1Code =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsSubnational1Code = c })
      <$> f _observationDetailsSubnational1Code

observationDetailsSubnational1Name :: Lens' (ObservationDetails 'Full) Text
observationDetailsSubnational1Name =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsSubnational1Name = c })
      <$> f _observationDetailsSubnational1Name

observationDetailsCountryCode :: Lens' (ObservationDetails 'Full) Region
observationDetailsCountryCode =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsCountryCode = c })
      <$> f _observationDetailsCountryCode

observationDetailsCountryName :: Lens' (ObservationDetails 'Full) Text
observationDetailsCountryName =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsCountryName = c })
      <$> f _observationDetailsCountryName

observationDetailsUserDisplayName :: Lens' (ObservationDetails 'Full) Text
observationDetailsUserDisplayName =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsUserDisplayName = c })
      <$> f _observationDetailsUserDisplayName

observationDetailsObsId :: Lens' (ObservationDetails 'Full) Text
observationDetailsObsId =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsObsId = c })
      <$> f _observationDetailsObsId

observationDetailsChecklistId :: Lens' (ObservationDetails 'Full) Text
observationDetailsChecklistId =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsChecklistId = c })
      <$> f _observationDetailsChecklistId

observationDetailsPresenceNoted :: Lens' (ObservationDetails 'Full) Bool
observationDetailsPresenceNoted =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsPresenceNoted = c })
      <$> f _observationDetailsPresenceNoted

observationDetailsHasComments :: Lens' (ObservationDetails 'Full) Bool
observationDetailsHasComments =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsHasComments = c })
      <$> f _observationDetailsHasComments

observationDetailsLastName :: Lens' (ObservationDetails 'Full) Text
observationDetailsLastName =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsLastName = c })
      <$> f _observationDetailsLastName

observationDetailsFirstName :: Lens' (ObservationDetails 'Full) Text
observationDetailsFirstName =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsFirstName = c })
      <$> f _observationDetailsFirstName

observationDetailsHasRichMedia :: Lens' (ObservationDetails 'Full) Bool
observationDetailsHasRichMedia =
    lensVL $ \f d@FullDetails{..} ->
          (\c -> d { _observationDetailsHasRichMedia = c })
      <$> f _observationDetailsHasRichMedia

instance
     k ~ A_Lens
  => LabelOptic
       "subnational2Code" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Region
       Region
  where
    labelOptic = observationDetailsSubnational2Code

instance
     k ~ A_Lens
  => LabelOptic
       "subnational2Name" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Text
       Text
  where
    labelOptic = observationDetailsSubnational2Name

instance
     k ~ A_Lens
  => LabelOptic
       "subnational1Code" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Region
       Region
  where
    labelOptic = observationDetailsSubnational1Code

instance
     k ~ A_Lens
  => LabelOptic
       "subnational1Name" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Text
       Text
  where
    labelOptic = observationDetailsSubnational1Name

instance
     k ~ A_Lens
  => LabelOptic
       "countryCode" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Region
       Region
  where
    labelOptic = observationDetailsCountryCode

instance
     k ~ A_Lens
  => LabelOptic
       "countryName" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Text
       Text
  where
    labelOptic = observationDetailsCountryName

instance
     k ~ A_Lens
  => LabelOptic
       "userDisplayName" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Text
       Text
  where
    labelOptic = observationDetailsUserDisplayName

instance
     k ~ A_Lens
  => LabelOptic
       "obsId" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Text
       Text
  where
    labelOptic = observationDetailsObsId

instance
     k ~ A_Lens
  => LabelOptic
       "checklistId" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Text
       Text
  where
    labelOptic = observationDetailsChecklistId

instance
     k ~ A_Lens
  => LabelOptic
       "presenceNoted" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Bool
       Bool
  where
    labelOptic = observationDetailsPresenceNoted

instance
     k ~ A_Lens
  => LabelOptic
       "hasComments" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Bool
       Bool
  where
    labelOptic = observationDetailsHasComments

instance
     k ~ A_Lens
  => LabelOptic
       "lastName" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Text
       Text
  where
    labelOptic = observationDetailsLastName

instance
     k ~ A_Lens
  => LabelOptic
       "firstName" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Text
       Text
  where
    labelOptic = observationDetailsFirstName

instance
     k ~ A_Lens
  => LabelOptic
       "hasRichMedia" k
       (ObservationDetails 'Full)
       (ObservationDetails 'Full)
       Bool
       Bool
  where
    labelOptic = observationDetailsHasRichMedia

-------------------------------------------------------------------------------
-- aeson instances
-------------------------------------------------------------------------------

-- | Explicit instance for compatibility with their field names
instance FromJSON (Observation 'Simple) where
  parseJSON = withObject "Observation 'Simple" $ \v ->
      Observation
        <$> v .: "speciesCode"
        <*> v .: "comName"
        <*> v .: "sciName"
        <*> v .: "locId"
        <*> v .: "locName"
        <*> v .: "obsDt"
        <*> v .:? "howMany"
        <*> v .: "lat"
        <*> v .: "lng"
        <*> v .: "obsValid"
        <*> v .: "obsReviewed"
        <*> v .: "locationPrivate"
        <*> v .: "subId"
        <*> pure NoDetails

-- | Explicit instance for compatibility with their field names
instance ToJSON (Observation 'Simple) where
  toJSON Observation{..} =
      object $
        [ "speciesCode" .= _observationSpeciesCode
        , "comName" .= _observationCommonName
        , "sciName" .= _observationScientificName
        , "locId" .= _observationLocationId
        , "locName" .= _observationLocationName
        , "obsDt" .= _observationDateTime
        , "lat" .= _observationLatitude
        , "lng" .= _observationLongitude
        , "obsValid" .= _observationValid
        , "obsReviewed" .= _observationReviewed
        , "locationPrivate" .= _observationLocationPrivate
        , "subId" .= _observationSubId
        ]
        -- Fields that may or may not be included, depending on the observation
        -- data
        <> ["howMany" .= howMany | Just howMany <- [_observationHowMany]]

-- | Explicit instance for compatibility with their field names
instance FromJSON (Observation 'Full) where
  parseJSON = withObject "Observation 'Full" $ \v ->
      Observation
        <$> v .: "speciesCode"
        <*> v .: "comName"
        <*> v .: "sciName"
        <*> v .: "locId"
        <*> v .: "locName"
        <*> v .: "obsDt"
        <*> v .:? "howMany"
        <*> v .: "lat"
        <*> v .: "lng"
        <*> v .: "obsValid"
        <*> v .: "obsReviewed"
        <*> v .: "locationPrivate"
        <*> v .: "subId"
        <*> ( FullDetails
                <$> v .: "subnational2Code"
                <*> v .: "subnational2Name"
                <*> v .: "subnational1Code"
                <*> v .: "subnational1Name"
                <*> v .: "countryCode"
                <*> v .: "countryName"
                <*> v .: "userDisplayName"
                <*> v .: "obsId"
                <*> v .: "checklistId"
                <*> v .: "presenceNoted"
                <*> v .: "hasComments"
                <*> v .: "lastName"
                <*> v .: "firstName"
                <*> v .: "hasRichMedia"
            )

-- | Explicit instance for compatibility with their field names
instance ToJSON (Observation 'Full) where
  toJSON Observation{..} =
      object
        [ "speciesCode" .= _observationSpeciesCode
        , "comName" .= _observationCommonName
        , "sciName" .= _observationScientificName
        , "locId" .= _observationLocationId
        , "locName" .= _observationLocationName
        , "obsDt" .= _observationDateTime
        , "howMany" .= _observationHowMany
        , "lat" .= _observationLatitude
        , "lng" .= _observationLongitude
        , "obsValid" .= _observationValid
        , "obsReviewed" .= _observationReviewed
        , "locationPrivate" .= _observationLocationPrivate
        , "subId" .= _observationSubId
        , "subnational2Code" .=
            _observationDetailsSubnational2Code _observationFullDetail
        , "subnational2Name" .=
            _observationDetailsSubnational2Name _observationFullDetail
        , "subnational1Code" .=
            _observationDetailsSubnational1Code _observationFullDetail
        , "subnational1Name" .=
            _observationDetailsSubnational1Name _observationFullDetail
        , "countryCode" .=
            _observationDetailsCountryCode _observationFullDetail
        , "countryName" .=
            _observationDetailsCountryName _observationFullDetail
        , "userDisplayName" .=
            _observationDetailsUserDisplayName _observationFullDetail
        , "obsId" .=
            _observationDetailsObsId _observationFullDetail
        , "checklistId" .=
            _observationDetailsChecklistId _observationFullDetail
        , "presenceNoted" .=
            _observationDetailsPresenceNoted _observationFullDetail
        , "hasComments" .=
            _observationDetailsHasComments _observationFullDetail
        , "lastName" .=
            _observationDetailsLastName _observationFullDetail
        , "firstName" .=
            _observationDetailsFirstName _observationFullDetail
        , "hasRichMedia" .=
            _observationDetailsHasRichMedia _observationFullDetail
        ]

-- | Switches between parsing a 'Simple' detail 'Observation' and a 'Full'
-- detail 'Observation' depending on whether the "firstName" key is present.
instance FromJSON SomeObservation where
  parseJSON obj = withObject "SomeObservation"
      ( \v ->
          if isJust (v !? "firstName") then
            SomeObservation <$> parseJSON @(Observation 'Full) obj
          else
            SomeObservation <$> parseJSON @(Observation 'Simple) obj
      ) obj

-- | Switches between encoding a 'Simple' 'Observation' and a 'Full'
-- 'Observation' depending on the evidence introduced by pattern-matching on the
-- 'observationFullDetail' field.
instance ToJSON SomeObservation where
  toJSON (SomeObservation obs) =
      case _observationFullDetail obs of
        NoDetails -> toJSON @(Observation 'Simple) obs
        FullDetails {} -> toJSON @(Observation 'Full) obs

-------------------------------------------------------------------------------
-- 'EBirdString' instances
-------------------------------------------------------------------------------

-- | The eBird string for a 'DetailLevel' value is simply the lowercase
-- constructor name.
instance EBirdString DetailLevel where
  toEBirdString =
      \case
        Simple -> "simple"
        Full -> "full"

  fromEBirdString str =
        parseOnly parseDetailLevel str
      & left (("Failed to parse DetailLevel: " <>) . Text.pack)

-- | The eBird string for a 'SortObservationsBy' value is either "date" or
-- "species".
instance EBirdString SortObservationsBy where
  toEBirdString =
      \case
        SortObservationsByDate -> "date"
        SortObservationsBySpecies -> "species"

  fromEBirdString str =
        parseOnly parseSortObservationsBy str
      & left (("Failed to parse SortObservationsBy: " <>) . Text.pack)

-- | The eBird string for a 'SelectObservation' value is either "create" or
-- "mrec".
instance EBirdString SelectObservation where
  toEBirdString =
      \case
        SelectFirstObservation -> "create"
        SelectLastObservation -> "mrec"

  fromEBirdString str =
        parseOnly parseSelectObservation str
      & left (("Failed to parse SelectObservation: " <>) . Text.pack)

-------------------------------------------------------------------------------
-- IsString instances
-------------------------------------------------------------------------------

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString DetailLevel where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString SortObservationsBy where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString SelectObservation where
  fromString = unsafeFromEBirdString . Text.pack

-------------------------------------------------------------------------------
-- * attoparsec parsers
-------------------------------------------------------------------------------

-- | Parse a list of eBird API taxononomy categories. To avoid the partial
-- behavior of converting a 'sepBy1' result into a 'Data.List.NonEmpty', we
-- manually parse the first category followed by an optional tail.
parseDetailLevel :: Parser DetailLevel
parseDetailLevel =
    choice
      [ "simple" $> Simple
      , "full" $> Full
      ]
  where
    _casesCovered :: DetailLevel -> ()
    _casesCovered =
      \case
        Simple -> ()
        Full -> ()

-- | Parse a 'SortObservationsBy' value
parseSortObservationsBy :: Parser SortObservationsBy
parseSortObservationsBy =
    choice
      [ "date" $> SortObservationsByDate
      , "species" $> SortObservationsBySpecies
      ]
  where
    _casesCovered :: SortObservationsBy -> ()
    _casesCovered =
      \case
        SortObservationsByDate -> ()
        SortObservationsBySpecies -> ()

-- | Parse a 'SelectObservation' value
parseSelectObservation :: Parser SelectObservation
parseSelectObservation =
    choice
      [ "first" $> SelectFirstObservation
      , "last" $> SelectLastObservation
      ]
  where
    _casesCovered :: SelectObservation -> ()
    _casesCovered =
      \case
        SelectFirstObservation -> ()
        SelectLastObservation -> ()

-------------------------------------------------------------------------------
-- 'ToHttpApiData' instances
-------------------------------------------------------------------------------

instance ToHttpApiData DetailLevel where
  toUrlPiece = toEBirdString

instance ToHttpApiData SortObservationsBy where
  toUrlPiece = toEBirdString

instance ToHttpApiData SelectObservation where
  toUrlPiece = toEBirdString

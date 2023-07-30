{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Data.EBird.API.Regions
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Types related to eBird region API values.

module Data.EBird.API.Regions where

import Control.Arrow
import Data.Aeson
import Data.Attoparsec.Text
import Data.Function
import Data.Functor
import Data.List.NonEmpty (NonEmpty(..))
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Optics
import Servant.API (ToHttpApiData(..))

import Data.EBird.API.EBirdString

-------------------------------------------------------------------------------
-- * Region-related API types
-------------------------------------------------------------------------------

-- | eBird divides the world into countries, subnational1 regions (states) or
-- subnational2 regions (counties). 'Location' regions are eBird-specific
-- location identifiers.
data Region =
      -- | Regions may be specified as location IDs, e.g. @L227544@
      Location Integer

      -- | The world is a region
    | World

      -- | At the top level, the world is divided into countries
    | Country Text

      -- | Subnational1 regions are states within countries
    | Subnational1
        Text -- ^ The country
        Text -- ^ The state

      -- | Subnational2 regions are counties within states
    | Subnational2
        Text -- ^ The country
        Text -- ^ The state
        Text -- ^ The county
  deriving (Show, Read, Eq)

-- | One constructor per eBird "region type" (countries, subnational1 (states),
-- or subnational2 (counties)).
data RegionType =
      CountryType
    | Subnational1Type
    | Subnational2Type
  deriving (Show, Read, Eq)

-- | A 'RegionCode' is a list of one or more 'Region's.
newtype RegionCode = RegionCode { regionCodeRegions :: NonEmpty Region }
  deriving (Show, Read, Eq)

-- | 'RegionNameFormat' values specify what format the API should return region
-- names in. See the constructor docs for examples.
data RegionNameFormat =
      -- | 'DetailedNameFormat' region name values are fully qualified with only
      -- the country abbreviated, e.g. "Madison County, New York, US"
      DetailedNameFormat

      -- | 'DetailedNoQualNameFormat' region name values are like
      -- 'DetailedNameFormat' but without the country qualifier and no "county"
      -- annotation, e.g. "Madison, New York"
    | DetailedNoQualNameFormat

      -- | 'FullNameFormat' region name values are fully qualified with no
      -- abbreviated country name and no "county" annotation, e.g. "Madison,
      -- New York, United States"
    | FullNameFormat

      -- | 'NameQualNameFormat' region name values are just the annotated name,
      -- e.g. "Madison County"
    | NameQualNameFormat

      -- | 'NameOnlyNameFormat' region name values are just the name, e.g.
      -- \"Madison\"
    | NameOnlyNameFormat

      -- | 'RevDetailedNameFormat' region name values are like
      -- 'DetailedNameFormat' but with reverse qualifiers, e.g. "US, New York,
      -- Madison County"
    | RevDetailedNameFormat
  deriving (Show, Read, Eq)

-- | 'RegionInfo' specifies the name of a region (in some 'RegionNameFormat')
-- and the bounds of that region as 'RegionBounds'.
data RegionInfo =
    RegionInfo
      { _regionInfoName :: Text
      , _regionInfoBounds :: Maybe RegionBounds
      }
  deriving (Show, Read, Eq)


-- | 'RegionBounds' specify the corners of a bounding box around a region.
data RegionBounds =
    RegionBounds
      { _regionBoundsMinX :: Double
      , _regionBoundsMaxX :: Double
      , _regionBoundsMinY :: Double
      , _regionBoundsMaxY :: Double
      }
  deriving (Show, Read, Eq)

-- | The data structure returned by the eBird 'Data.EBird.API.SubRegionListAPI' and
-- 'Data.EBird.API.AdjacentRegionsAPI'.
data RegionListEntry =
    RegionListEntry
      { _regionListEntryRegion :: Region
      , _regionListEntryName :: Text
      }
  deriving (Show, Read, Eq)

-- ** Optics for region-related API types

makeLenses ''RegionInfo
makeFieldLabels ''RegionInfo
makeLenses ''RegionBounds
makeFieldLabels ''RegionBounds
makeLenses ''RegionListEntry
makeFieldLabels ''RegionListEntry

-------------------------------------------------------------------------------
-- aeson instances
-------------------------------------------------------------------------------

instance FromJSON RegionCode where
  parseJSON = withText "RegionCode" $ \t ->
      case parseOnly parseRegionCode t of
        Left _ -> fail "failed to parse region code"
        Right c -> return c

instance ToJSON RegionCode where
  toJSON = String . toEBirdString

instance FromJSON Region where
  parseJSON = withText "RegionCode" $ \t ->
      case parseOnly parseRegion t of
        Left _ -> fail "failed to parse region"
        Right r -> return r

instance ToJSON Region where
  toJSON = String . toEBirdString

-- | Explicit instance for compatibility with their field names
instance FromJSON RegionInfo where
  parseJSON = withObject "RegionInfo" $ \v ->
      RegionInfo
        <$> v .: "result"
        <*> v .:? "bounds"

-- | Explicit instance for compatibility with their field names
instance ToJSON RegionInfo where
  toJSON RegionInfo{..} =
      object $
        [ "result" .= _regionInfoName
        ]
        <> ["bounds" .= bs | Just bs <- [_regionInfoBounds]]

-- | Explicit instance for compatibility with their field names
instance FromJSON RegionBounds where
  parseJSON = withObject "RegionBounds" $ \v ->
      RegionBounds
        <$> v .: "minX"
        <*> v .: "maxX"
        <*> v .: "minY"
        <*> v .: "maxY"

-- | Explicit instance for compatibility with their field names
instance ToJSON RegionBounds where
  toJSON RegionBounds{..} =
      object
        [ "minX" .= _regionBoundsMinX
        , "maxX" .= _regionBoundsMaxX
        , "minY" .= _regionBoundsMinY
        , "maxY" .= _regionBoundsMaxY
        ]

-- | Explicit instance for compatibility with their field names
instance FromJSON RegionListEntry where
  parseJSON = withObject "RegionListEntry" $ \v ->
      RegionListEntry
        <$> v .: "code"
        <*> v .: "name"

-- | Explicit instance for compatibility with their field names
instance ToJSON RegionListEntry where
  toJSON RegionListEntry{..} =
      object
        [ "code" .= _regionListEntryRegion
        , "name" .= _regionListEntryName
        ]

-------------------------------------------------------------------------------
-- 'EBirdString' instances
-------------------------------------------------------------------------------

-- | A 'Region' eBird string is either:
--
--    * \"L227544\" for location regions, where L227544 is the location ID.
--    * "world" for 'World' regions.
--    * The country identifier (e.g. \"US\" for the United States) for 'Country'
--      regions.
--    * The country identifier and the state identifier separated by a hyphen
--      for 'Subnational1' regions (e.g. "US-WY" for Wyoming in the United
--      States).
--    * The county identifier, the state identifier, and the country identifier
--      separated by hyphens for 'Subnational2' regions (e.g. US-WY-013)
instance EBirdString Region where
  toEBirdString =
      \case
        Location n -> "L" <> Text.pack (show n)
        World -> "world"
        Country cr -> cr
        Subnational1 cr st -> cr <> "-" <> st
        Subnational2 cr st cy -> cr <> "-" <> st <> "-" <> cy

  fromEBirdString str =
        parseOnly parseRegion str
      & left (("Failed to parse Region: " <>) . Text.pack)

-- | Results in
-- [eBird region type format](https://documenter.getpostman.com/view/664302/S1ENwy59#382da1c8-8bff-4926-936a-a1f8b065e7d5)
instance EBirdString RegionType where
  toEBirdString =
      \case
        CountryType -> "country"
        Subnational1Type -> "subnational1"
        Subnational2Type -> "subnational2"

  fromEBirdString str =
        parseOnly parseRegionType str
      & left (("Failed to parse RegionType: " <>) . Text.pack)

-- | A 'RegionCode' eBird string is a comma-separated list of regions.
instance EBirdString RegionCode where
  toEBirdString (RegionCode (r :| rs)) =
      Text.intercalate "," $ map toEBirdString (r : rs)

  fromEBirdString str =
        parseOnly parseRegionCode str
      & left (("Failed to parse RegionCode: " <>) . Text.pack)

-- | A 'RegionNameFormat' is shown as the constructor name without the
-- @NameFormat@ suffix, in all lower-case.
instance EBirdString RegionNameFormat where
  toEBirdString =
      \case
        DetailedNameFormat -> "detailed"
        DetailedNoQualNameFormat -> "detailednoqual"
        FullNameFormat -> "full"
        NameQualNameFormat -> "namequal"
        NameOnlyNameFormat -> "nameonly"
        RevDetailedNameFormat -> "revdetailed"

  fromEBirdString str =
        parseOnly parseRegionNameFormat str
      & left (("Failed to parse RegionNameFormat: " <>) . Text.pack)

-------------------------------------------------------------------------------
-- IsString instances
-------------------------------------------------------------------------------

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString Region where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString RegionType where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString RegionCode where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString RegionNameFormat where
  fromString = unsafeFromEBirdString . Text.pack

-------------------------------------------------------------------------------
-- * attoparsec parsers
-------------------------------------------------------------------------------

-- | Parse an eBird API region code, which is a comma-separated list of one or
-- more regions. To avoid the partial behavior of converting a 'sepBy1' result
-- into a 'NonEmpty', we manually parse the first region followed by an optional
-- tail.
parseRegionCode :: Parser RegionCode
parseRegionCode = do
    r <- parseRegion
    rs <- atEnd >>= \case
      True -> return []
      False -> do
        skip (==',')
        parseRegion `sepBy` char ','
    return $ RegionCode (r :| rs)

-- | Parse an eBird API region. This parser only ensures that the input is
-- somewhat well-formed, in that it is either:
--
--    * A 'Location' region (an \'L\' followed by an integral number)
--    * The 'World' region (just the string "world")
--    * A 'Subnational2' region (formatted as "LETTERS-LETTERS-NUMBER" where
--      "LETTERS" is one or more letters in any case, and "NUMBERS" is an
--      integral number)
--    * A 'Subnational1' region (formatterd as "LETTERS-LETTERS")
--    * A 'Country' region (just \"LETTERS\")
parseRegion :: Parser Region
parseRegion =
    choice
      [ parseLocationId
      , parseWorld
      , parseSubnational2
      , parseSubnational1
      , parseCountry
      ]
  where
    parseLocationId :: Parser Region
    parseLocationId = do
      "L" *> (Location <$> decimal)

    parseWorld :: Parser Region
    parseWorld = do
      "world" $> World

    parseCountry :: Parser Region
    parseCountry = Country <$> letters

    parseSubnational1 :: Parser Region
    parseSubnational1 = do
      cr <- letters
      skipHyphen
      st <- letters
      return $ Subnational1 cr st

    parseSubnational2 :: Parser Region
    parseSubnational2 = do
      cr <- letters
      skipHyphen
      st <- letters
      skipHyphen
      cy <- choice [letters, digits]
      return $ Subnational2 cr st cy

    letters :: Parser Text
    letters = Text.pack <$> many1 letter

    digits :: Parser Text
    digits = Text.pack <$> many1 digit

    skipHyphen :: Parser ()
    skipHyphen = skip (=='-')

-- | Parse an eBird API 'RegionNameFormat'.
parseRegionNameFormat :: Parser RegionNameFormat
parseRegionNameFormat =
    choice
      [ "detailednoqual" $> DetailedNoQualNameFormat
      , "detailed" $> DetailedNameFormat
      , "full" $> FullNameFormat
      , "namequal" $> NameQualNameFormat
      , "nameonly" $> NameOnlyNameFormat
      , "revdetailed" $> RevDetailedNameFormat
      ]
  where
    _casesCovered :: RegionNameFormat -> ()
    _casesCovered =
      \case
        DetailedNoQualNameFormat -> ()
        DetailedNameFormat -> ()
        FullNameFormat -> ()
        NameQualNameFormat -> ()
        NameOnlyNameFormat -> ()
        RevDetailedNameFormat -> ()

-- | Parse an eBird API 'RegionType'.
parseRegionType :: Parser RegionType
parseRegionType =
    choice
      [ "country" $> CountryType
      , "subnational1" $> Subnational1Type
      , "subnational2" $> Subnational2Type
      ]
  where
    _casesCovered :: RegionType -> ()
    _casesCovered =
      \case
        CountryType -> ()
        Subnational1Type -> ()
        Subnational2Type -> ()

-------------------------------------------------------------------------------
-- 'ToHttpApiData' instances
-------------------------------------------------------------------------------

instance ToHttpApiData Region where
  toUrlPiece = toEBirdString

instance ToHttpApiData RegionType where
  toUrlPiece = toEBirdString

instance ToHttpApiData RegionCode where
  toUrlPiece = toEBirdString

instance ToHttpApiData RegionNameFormat where
  toUrlPiece = toEBirdString

{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

-- |
-- Module      : Data.EBird.API.Util.Time
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Utilities for parsing and printing dates and times that the eBird API
-- provides.

module Data.EBird.API.Util.Time (
    -- * Date and time types
    EBirdDate(..)
  , EBirdTime(..)
  , EBirdDateTime(..)

    -- * Conversions
  , eBirdDateToGregorian

    -- * attoparsec parsers
  , parseEBirdDate
  , parseEBirdTime
  , parseEBirdDateTime
  ) where

import Control.Applicative
import Control.Arrow
import Data.Aeson
import Data.Attoparsec.Text
import Data.Attoparsec.Time
import Data.Function
import Data.String
import Data.Text qualified as Text
import Data.Time

import Data.EBird.API.EBirdString

-------------------------------------------------------------------------------
-- Date and time types
-------------------------------------------------------------------------------

-- | An 'EBirdDate' is simply a 'Day'.
newtype EBirdDate = EBirdDate { eBirdDate :: Day }
  deriving (Show, Read, Eq, Ord)
  deriving newtype (Enum)

-- | Since times that come from the eBird API are not provided with a time zone,
-- an 'EBirdTime' is simply a 'TimeOfDay'. Since eBird times are only provided
-- up to the minute, the 'todSec' value will always be 0.
newtype EBirdTime = EBirdTime { eBirdTime :: TimeOfDay }
  deriving (Show, Read, Eq, Ord)

-- | Dates and times that come from the eBird API are not provided with a time
-- zone. All we can do is track the 'Data.Time.Day' and 'Data.Time.TimeOfDay'
-- with a 'Data.Time.LocalTime'. Comparison of, for example,
-- 'Data.EBird.API.Observation's that happened in different time zones must therefore
-- be done carefully.
newtype EBirdDateTime = EBirdDateTime { eBirdDateTime :: LocalTime }
  deriving (Show, Read, Eq, Ord)

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Convert an 'EBirdDate' to a gregorian representation. The first element is
-- the year, the second is the month in the year (1 - 12), and the third is the
-- day in the month.
eBirdDateToGregorian :: EBirdDate -> (Integer, Integer, Integer)
eBirdDateToGregorian EBirdDate{..} =
    (y, fromIntegral m, fromIntegral d)
  where
    (y, m, d) = toGregorian eBirdDate

-------------------------------------------------------------------------------
-- aeson instances
-------------------------------------------------------------------------------

instance FromJSON EBirdDate where
  parseJSON = withText "EBirdDate" $ \t ->
      case parseOnly parseEBirdDate t of
        Left _ -> fail "failed to parse eBird date"
        Right r -> return r

instance ToJSON EBirdDate where
  toJSON = String . toEBirdString

instance FromJSON EBirdTime where
  parseJSON = withText "EBirdTime" $ \t ->
      case parseOnly parseEBirdTime t of
        Left _ -> fail "failed to parse eBird time"
        Right r -> return r

instance ToJSON EBirdTime where
  toJSON = String . toEBirdString

instance FromJSON EBirdDateTime where
  parseJSON = withText "EBirdDateTime" $ \t ->
      case parseOnly parseEBirdDateTime t of
        Left _ -> fail "failed to parse eBird datetime"
        Right r -> return r

instance ToJSON EBirdDateTime where
  toJSON = String . toEBirdString

-------------------------------------------------------------------------------
-- EBirdString instances
-------------------------------------------------------------------------------

-- | eBird dates are formatted as YYYY-MM-DD, with 0 padding where necessary.
instance EBirdString EBirdDate where
  toEBirdString =
      Text.pack . formatTime defaultTimeLocale "%04Y-%02m-%02d" . eBirdDate

  fromEBirdString str =
        parseOnly parseEBirdDate str
      & left (("Failed to parse EBirdDate: " <>) . Text.pack)

-- | eBird times are formatted as HH:MM, with 0 padding where necessary.
instance EBirdString EBirdTime where
  toEBirdString =
      Text.pack . formatTime defaultTimeLocale "%02H:%02M" . eBirdTime

  fromEBirdString str =
        parseOnly parseEBirdTime str
      & left (("Failed to parse EBirdTime: " <>) . Text.pack)

-- | eBird datetimes are formatted as YYYY-MM-DD HH:MM, with 0 padding where
-- necessary.
instance EBirdString EBirdDateTime where
  toEBirdString =
        Text.pack
      . formatTime defaultTimeLocale "%04Y-%02m-%02d %02H:%02M"
      . eBirdDateTime

  fromEBirdString str =
        parseOnly parseEBirdDateTime str
      & left (("Failed to parse EBirdDateTime: " <>) . Text.pack)

-------------------------------------------------------------------------------
-- IsString instances
-------------------------------------------------------------------------------

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString EBirdDate where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString EBirdTime where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString EBirdDateTime where
  fromString = unsafeFromEBirdString . Text.pack

-------------------------------------------------------------------------------
-- attoparsec parsers
-------------------------------------------------------------------------------

-- | Parse an eBird date. Most eBird dates are formatted as YYYY-MM-DD, but the
-- 'Data.EBird.API.ChecklistFeedAPI' gives dates in a format like "19 Jul 2023". So,
-- we try parsing the first format using 'day', and then use a custom
-- 'parseTimeM' format for the latter format if that fails.
parseEBirdDate :: Parser EBirdDate
parseEBirdDate = tryDay <|> tryParseTimeM
  where
    tryDay :: Parser EBirdDate
    tryDay = EBirdDate <$> day

    tryParseTimeM :: Parser EBirdDate
    tryParseTimeM = do
      input <- takeText
      d <- parseTimeM
        False
        defaultTimeLocale
        "%e %b %Y"
        (Text.unpack input)
      return (EBirdDate d)

-- | Parse an eBird time (just uses 'timeOfDay').
parseEBirdTime :: Parser EBirdTime
parseEBirdTime = EBirdTime <$> timeOfDay

-- | Parse an eBird datetime (just uses 'localTime').
parseEBirdDateTime :: Parser EBirdDateTime
parseEBirdDateTime = EBirdDateTime <$> localTime

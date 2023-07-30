{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}

-- |
-- Module      : Data.EBird.CLI
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Functions used to implement a command-line utility for interacting with the
-- [eBird API](https://documenter.getpostman.com/view/664302/S1ENwy59).

module Data.EBird.CLI where

import Control.Exception
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Attoparsec.Text qualified as A
#if !MIN_VERSION_bytestring(0,11,0)
-- Data.ByteString.Char8 does not export 'toStrict' until 0.11.0.0
import Data.ByteString.Lazy qualified as BS (toStrict)
#endif
import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Text.Printf

import Data.EBird.API
import Data.EBird.Client

-- | Entry point for the @ebird@ CLI. Parses the command arguments, selects an
-- API key, and executes the command.
--
-- The API key may be provided as a command-line option. If the key option is
-- not provided, it is read from the file @~\/.ebird\/key.txt@. If that file is
-- unavailable for reading, and no key option is provided, the application
-- exits (if the command requires a key).
eBirdCli :: IO ()
eBirdCli = do
    (optKey, c) <- execParser opts
    fileKey <- readEBirdAPIKey
    let getKey =
          case optKey <|> fileKey of
            Just k -> pure k
            Nothing -> do
              eBirdFail
                ( "An API key is required for this command, but no API key\n" <>
                  "was provided via the `-k` option and no API key file was\n" <>
                  "found at ~/.ebird/key.txt. Exiting."
                )
    runEBirdCommand getKey c
  where
    opts :: ParserInfo (Maybe Text, EBirdCommand)
    opts =
      info
        (eBirdCommand <**> helper)
        (    header "ebird - Go birding on your command line!"
          <> progDesc "Query the official eBird API"
        )

-- | Read an eBird API key from @~\/.ebird\/key.txt@. If the file exists and is
-- available for reading, the result is 'Just' the contents of the file,
-- stripped of leading/trailing whitespace. Otherwise, the result is 'Nothing'.
readEBirdAPIKey :: IO (Maybe Text)
readEBirdAPIKey = do
    home <- getHomeDirectory
    catch
      ( Just . Text.strip <$>
          Text.readFile (home </> ".ebird" </> "key" <.> "txt")
      )
      ( \(_ :: IOException) -> pure Nothing
      )

-- | Run an 'EBirdCommand' with a given API key.
runEBirdCommand
  :: IO Text
  -- ^ Get an API key (this may fail if not found or provided)
  -> EBirdCommand
  -- ^ Command to execute
  -> IO ()
runEBirdCommand getAPIKey = \case
    RecentObservationsCommand RecentObservationsOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        recentObservations_ apiKey
          recentObservationsRegion
          recentObservationsBack
          recentObservationsCategories
          recentObservationsHotspots
          recentObservationsProvisionals
          recentObservationsMaxResults
          recentObservationsSubRegions
          recentObservationsSPPLocale
      handleResponse res

    RecentNotableObservationsCommand RecentNotableObservationsOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        recentNotableObservations_ apiKey
          recentNotableObservationsRegion
          recentNotableObservationsBack
          recentNotableObservationsDetail
          recentNotableObservationsHotspots
          recentNotableObservationsMaxResults
          recentNotableObservationsSubRegions
          recentNotableObservationsSPPLocale
      handleResponse res

    RecentSpeciesObservationsCommand RecentSpeciesObservationsOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        recentSpeciesObservations_ apiKey
          recentSpeciesObservationsRegion
          recentSpeciesObservationsSpecies
          recentSpeciesObservationsBack
          recentSpeciesObservationsHotspots
          recentSpeciesObservationsProvisionals
          recentSpeciesObservationsMaxResults
          recentSpeciesObservationsSubRegions
          recentSpeciesObservationsSPPLocale
      handleResponse res

    RecentNearbyObservationsCommand RecentNearbyObservationsOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        recentNearbyObservations_ apiKey
          recentNearbyObservationsLatitude
          recentNearbyObservationsLongitude
          recentNearbyObservationsDist
          recentNearbyObservationsBack
          recentNearbyObservationsCategories
          recentNearbyObservationsHotspots
          recentNearbyObservationsProvisionals
          recentNearbyObservationsMaxResults
          recentNearbyObservationsSortBy
          recentNearbyObservationsSPPLocale
      handleResponse res

    RecentNearbySpeciesObservationsCommand RecentNearbySpeciesObservationsOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        recentNearbySpeciesObservations_ apiKey
          recentNearbySpeciesObservationsSpecies
          recentNearbySpeciesObservationsLatitude
          recentNearbySpeciesObservationsLongitude
          recentNearbySpeciesObservationsDist
          recentNearbySpeciesObservationsBack
          recentNearbySpeciesObservationsCategories
          recentNearbySpeciesObservationsHotspots
          recentNearbySpeciesObservationsProvisionals
          recentNearbySpeciesObservationsMaxResults
          recentNearbySpeciesObservationsSortBy
          recentNearbySpeciesObservationsSPPLocale
      handleResponse res

    RecentNearestSpeciesObservationsCommand RecentNearestSpeciesObservationsOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        recentNearestSpeciesObservations_ apiKey
          recentNearestSpeciesObservationsSpecies
          recentNearestSpeciesObservationsLatitude
          recentNearestSpeciesObservationsLongitude
          recentNearestSpeciesObservationsDist
          recentNearestSpeciesObservationsBack
          recentNearestSpeciesObservationsHotspots
          recentNearestSpeciesObservationsProvisionals
          recentNearestSpeciesObservationsMaxResults
          recentNearestSpeciesObservationsSPPLocale
      handleResponse res

    RecentNearbyNotableObservationsCommand RecentNearbyNotableObservationsOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        recentNearbyNotableObservations_ apiKey
          recentNearbyNotableObservationsLatitude
          recentNearbyNotableObservationsLongitude
          recentNearbyNotableObservationsDist
          recentNearbyNotableObservationsDetail
          recentNearbyNotableObservationsBack
          recentNearbyNotableObservationsHotspots
          recentNearbyNotableObservationsMaxResults
          recentNearbyNotableObservationsSPPLocale
      handleResponse res

    HistoricalObservationsCommand HistoricalObservationsOptions{..} -> do
      apiKey <- getAPIKey
      let (y,m,d) = eBirdDateToGregorian historicalObservationsDate
      res <- askEBird $
        historicalObservations_ apiKey
          historicalObservationsRegion
          y m d
          historicalObservationsCategories
          historicalObservationsDetail
          historicalObservationsHotspots
          historicalObservationsProvisionals
          historicalObservationsMaxResults
          historicalObservationsRank
          historicalObservationsSubRegions
          historicalObservationsSPPLocale
      handleResponse res

    RecentChecklistsCommand RecentChecklistsOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        recentChecklists_ apiKey
          recentChecklistsRegion
          recentChecklistsMaxResults
      handleResponse res

    Top100Command Top100Options{..} -> do
      apiKey <- getAPIKey
      let (y,m,d) = eBirdDateToGregorian top100Date
      res <- askEBird $
        top100_ apiKey
          top100Region
          y m d
          top100RankedBy
          top100MaxResults
      handleResponse res

    ChecklistFeedCommand ChecklistFeedOptions{..} -> do
      apiKey <- getAPIKey
      let (y,m,d) = eBirdDateToGregorian checklistFeedDate
      res <- askEBird $
        checklistFeed_ apiKey
          checklistFeedRegion
          y m d
          checklistFeedSortBy
          checklistFeedMaxResults
      handleResponse res

    RegionalStatisticsCommand RegionalStatisticsOptions{..} -> do
      apiKey <- getAPIKey
      let (y,m,d) = eBirdDateToGregorian regionalStatisticsDate
      res <- askEBird $
        regionalStatistics_ apiKey
          regionalStatisticsRegion
          y m d
      handleResponse res

    SpeciesListCommand SpeciesListOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        speciesList_ apiKey speciesListRegion
      handleResponse res

    ViewChecklistCommand ViewChecklistOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        viewChecklist_ apiKey viewChecklistSubId
      handleResponse res

    RegionHotspotsCommand RegionHotspotsOptions{..} -> do
      res <- askEBird $
        regionHotspots_
          regionHotspotsRegion
          regionHotspotsBack
          regionHotspotsFmt
      handleResponse res

    NearbyHotspotsCommand NearbyHotspotsOptions{..} -> do
      res <- askEBird $
        nearbyHotspots_
          nearbyHotspotsLatitude
          nearbyHotspotsLongitude
          nearbyHotspotsDist
          nearbyHotspotsBack
          nearbyHotspotsFmt
      handleResponse res

    HotspotInfoCommand HotspotInfoOptions{..} -> do
      res <- askEBird $ hotspotInfo hotspotInfoLocation
      handleResponse res

    TaxonomyCommand TaxonomyOptions{..} -> do
      res <- askEBird $
        taxonomy_
          taxonomyTaxonomyCategories
          taxonomyFormat
          taxonomySPPLocale
          taxonomySpecies
          taxonomyVersion
      handleResponse res

    TaxonomicFormsCommand TaxonomicFormsOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        taxonomicForms_ apiKey
          taxonomicFormsSpecies
      handleResponse res

    TaxaLocaleCodesCommand TaxaLocaleCodesOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        taxaLocaleCodes_ apiKey
          taxaLocaleCodesAcceptLanguage
      handleResponse res

    TaxonomyVersionsCommand -> askEBird taxonomyVersions >>= handleResponse

    TaxonomicGroupsCommand TaxonomicGroupsOptions{..} -> do
      res <- askEBird $
        taxonomicGroups_
          taxonomicGroupsSPPGrouping
          taxonomicGroupsSPPLocale
      handleResponse res

    RegionInfoCommand RegionInfoOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        regionInfo_ apiKey
          regionInfoRegion
          regionInfoRegionNameFormat
      handleResponse res

    SubRegionListCommand SubRegionListOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        subRegionList apiKey
          subRegionListRegionType
          subRegionListParentRegionCode
      handleResponse res

    AdjacentRegionsCommand AdjacentRegionsOptions{..} -> do
      apiKey <- getAPIKey
      res <- askEBird $
        adjacentRegions apiKey
          adjacentRegionsRegion
      handleResponse res
  where
    handleResponse :: ToJSON a => Either ClientError a -> IO ()
    handleResponse = \case
      Right v -> printResJSON v
      Left err ->
        eBirdFail $
          "An error occurred while executing the request:\n" <>
          show err


-- | Simply prints a value as prettified JSON
printResJSON :: ToJSON a => a -> IO ()
printResJSON = BS.putStrLn . BS.toStrict . encodePretty

-- | Print a string to stderr, prepended with a context string, and exit with
-- failure status.
eBirdFail :: String -> IO a
eBirdFail msg = do
  progName <- getProgName
  die
    ( progName <> ": Something went wrong!\n\n"
      <> unlines (map ("  " <>) (lines msg))
    )

-------------------------------------------------------------------------------
-- Command types
-------------------------------------------------------------------------------

-- | Each 'EBirdCommand' corresponds to an endpoint of the eBird API
data EBirdCommand
    = RecentObservationsCommand RecentObservationsOptions
    | RecentNotableObservationsCommand RecentNotableObservationsOptions
    | RecentSpeciesObservationsCommand RecentSpeciesObservationsOptions
    | RecentNearbyObservationsCommand RecentNearbyObservationsOptions
    | RecentNearbySpeciesObservationsCommand RecentNearbySpeciesObservationsOptions
    | RecentNearestSpeciesObservationsCommand RecentNearestSpeciesObservationsOptions
    | RecentNearbyNotableObservationsCommand RecentNearbyNotableObservationsOptions
    | HistoricalObservationsCommand HistoricalObservationsOptions
    | RecentChecklistsCommand RecentChecklistsOptions
    | Top100Command Top100Options
    | ChecklistFeedCommand ChecklistFeedOptions
    | RegionalStatisticsCommand RegionalStatisticsOptions
    | SpeciesListCommand SpeciesListOptions
    | ViewChecklistCommand ViewChecklistOptions
    | RegionHotspotsCommand RegionHotspotsOptions
    | NearbyHotspotsCommand NearbyHotspotsOptions
    | HotspotInfoCommand HotspotInfoOptions
    | TaxonomyCommand TaxonomyOptions
    | TaxonomicFormsCommand TaxonomicFormsOptions
    | TaxaLocaleCodesCommand TaxaLocaleCodesOptions
    | TaxonomyVersionsCommand
    | TaxonomicGroupsCommand TaxonomicGroupsOptions
    | RegionInfoCommand RegionInfoOptions
    | SubRegionListCommand SubRegionListOptions
    | AdjacentRegionsCommand AdjacentRegionsOptions
  deriving (Show, Eq)

-- | Options for the @recent-observations@ command.
data RecentObservationsOptions =
    RecentObservationsOptions
      { recentObservationsRegion :: RegionCode
      , recentObservationsBack :: Maybe Integer
      , recentObservationsCategories :: Maybe TaxonomyCategories
      , recentObservationsHotspots :: Maybe Bool
      , recentObservationsProvisionals :: Maybe Bool
      , recentObservationsMaxResults :: Maybe Integer
      , recentObservationsSubRegions :: Maybe RegionCode
      , recentObservationsSPPLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Options for the @recent-notable-observations@ command.
data RecentNotableObservationsOptions =
    RecentNotableObservationsOptions
      { recentNotableObservationsRegion :: RegionCode
      , recentNotableObservationsBack :: Maybe Integer
      , recentNotableObservationsDetail :: Maybe DetailLevel
      , recentNotableObservationsHotspots :: Maybe Bool
      , recentNotableObservationsMaxResults :: Maybe Integer
      , recentNotableObservationsSubRegions :: Maybe RegionCode
      , recentNotableObservationsSPPLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Options for the @recent-species-observations@ command.
data RecentSpeciesObservationsOptions =
    RecentSpeciesObservationsOptions
      { recentSpeciesObservationsRegion :: RegionCode
      , recentSpeciesObservationsSpecies :: SpeciesCode
      , recentSpeciesObservationsBack :: Maybe Integer
      , recentSpeciesObservationsHotspots :: Maybe Bool
      , recentSpeciesObservationsProvisionals :: Maybe Bool
      , recentSpeciesObservationsMaxResults :: Maybe Integer
      , recentSpeciesObservationsSubRegions :: Maybe RegionCode
      , recentSpeciesObservationsSPPLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Options for the @recent-nearby-observations@ command.
data RecentNearbyObservationsOptions =
    RecentNearbyObservationsOptions
      { recentNearbyObservationsLatitude :: Double
      , recentNearbyObservationsLongitude :: Double
      , recentNearbyObservationsDist :: Maybe Integer
      , recentNearbyObservationsBack :: Maybe Integer
      , recentNearbyObservationsCategories :: Maybe TaxonomyCategories
      , recentNearbyObservationsHotspots :: Maybe Bool
      , recentNearbyObservationsProvisionals :: Maybe Bool
      , recentNearbyObservationsMaxResults :: Maybe Integer
      , recentNearbyObservationsSortBy :: Maybe SortObservationsBy
      , recentNearbyObservationsSPPLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Options for the @recent-nearby-species-observations@ command.
data RecentNearbySpeciesObservationsOptions =
    RecentNearbySpeciesObservationsOptions
      { recentNearbySpeciesObservationsSpecies :: SpeciesCode
      , recentNearbySpeciesObservationsLatitude :: Double
      , recentNearbySpeciesObservationsLongitude :: Double
      , recentNearbySpeciesObservationsDist :: Maybe Integer
      , recentNearbySpeciesObservationsBack :: Maybe Integer
      , recentNearbySpeciesObservationsCategories :: Maybe TaxonomyCategories
      , recentNearbySpeciesObservationsHotspots :: Maybe Bool
      , recentNearbySpeciesObservationsProvisionals :: Maybe Bool
      , recentNearbySpeciesObservationsMaxResults :: Maybe Integer
      , recentNearbySpeciesObservationsSortBy :: Maybe SortObservationsBy
      , recentNearbySpeciesObservationsSPPLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Options for the @recent-nearest-species-observations@ command.
data RecentNearestSpeciesObservationsOptions =
    RecentNearestSpeciesObservationsOptions
      { recentNearestSpeciesObservationsSpecies :: SpeciesCode
      , recentNearestSpeciesObservationsLatitude :: Double
      , recentNearestSpeciesObservationsLongitude :: Double
      , recentNearestSpeciesObservationsDist :: Maybe Integer
      , recentNearestSpeciesObservationsBack :: Maybe Integer
      , recentNearestSpeciesObservationsHotspots :: Maybe Bool
      , recentNearestSpeciesObservationsProvisionals :: Maybe Bool
      , recentNearestSpeciesObservationsMaxResults :: Maybe Integer
      , recentNearestSpeciesObservationsSPPLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Options for the @recent-nearby-notable-observations@ command.
data RecentNearbyNotableObservationsOptions =
    RecentNearbyNotableObservationsOptions
      { recentNearbyNotableObservationsLatitude :: Double
      , recentNearbyNotableObservationsLongitude :: Double
      , recentNearbyNotableObservationsDist :: Maybe Integer
      , recentNearbyNotableObservationsDetail :: Maybe DetailLevel
      , recentNearbyNotableObservationsBack :: Maybe Integer
      , recentNearbyNotableObservationsHotspots :: Maybe Bool
      , recentNearbyNotableObservationsMaxResults :: Maybe Integer
      , recentNearbyNotableObservationsSPPLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Options for the @historical-observations@ command.
data HistoricalObservationsOptions =
    HistoricalObservationsOptions
      { historicalObservationsRegion :: RegionCode
      , historicalObservationsDate :: EBirdDate
      , historicalObservationsCategories :: Maybe TaxonomyCategories
      , historicalObservationsDetail :: Maybe DetailLevel
      , historicalObservationsHotspots :: Maybe Bool
      , historicalObservationsProvisionals :: Maybe Bool
      , historicalObservationsMaxResults :: Maybe Integer
      , historicalObservationsRank :: Maybe SelectObservation
      , historicalObservationsSubRegions :: Maybe RegionCode
      , historicalObservationsSPPLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Options for the @recent-checklists@ command.
data RecentChecklistsOptions =
    RecentChecklistsOptions
      { recentChecklistsRegion :: RegionCode
      , recentChecklistsMaxResults :: Maybe Integer
      }
  deriving (Show, Read, Eq)

-- | Options for the @top-100@ command.
data Top100Options =
    Top100Options
      { top100Region :: Region
      , top100Date :: EBirdDate
      , top100RankedBy :: Maybe RankTop100By
      , top100MaxResults :: Maybe Integer
      }
  deriving (Show, Read, Eq)

-- | Options for the @checklist-feed@ command.
data ChecklistFeedOptions =
    ChecklistFeedOptions
      { checklistFeedRegion :: Region
      , checklistFeedDate :: EBirdDate
      , checklistFeedSortBy :: Maybe SortChecklistsBy
      , checklistFeedMaxResults :: Maybe Integer
      }
  deriving (Show, Read, Eq)

-- | Options for the @regional-statistics@ command.
data RegionalStatisticsOptions =
    RegionalStatisticsOptions
      { regionalStatisticsRegion :: Region
      , regionalStatisticsDate :: EBirdDate
      }
  deriving (Show, Read, Eq)

-- | Options for the @species-list@ command.
newtype SpeciesListOptions =
    SpeciesListOptions
      { speciesListRegion :: Region
      }
  deriving (Show, Read, Eq)

-- | Options for the @view-checklist@ command.
newtype ViewChecklistOptions =
    ViewChecklistOptions
      { viewChecklistSubId :: Text
      }
  deriving (Show, Read, Eq)

-- | Options for the @region-hotspots@ command.
data RegionHotspotsOptions =
    RegionHotspotsOptions
      { regionHotspotsRegion :: RegionCode
      , regionHotspotsBack :: Maybe Integer
      , regionHotspotsFmt :: Maybe CSVOrJSONFormat
      }
  deriving (Show, Read, Eq)

-- | Options for the @nearby-hotspots@ command.
data NearbyHotspotsOptions =
    NearbyHotspotsOptions
      { nearbyHotspotsLatitude :: Double
      , nearbyHotspotsLongitude :: Double
      , nearbyHotspotsBack :: Maybe Integer
      , nearbyHotspotsDist :: Maybe Integer
      , nearbyHotspotsFmt :: Maybe CSVOrJSONFormat
      }
  deriving (Show, Read, Eq)

-- | Options for the @hotspot-info@ command.
newtype HotspotInfoOptions =
    HotspotInfoOptions
      { hotspotInfoLocation :: Text
      }
  deriving (Show, Read, Eq)

-- | Options for the @nearby-hotspots@ command.
data TaxonomyOptions =
    TaxonomyOptions
      { taxonomyTaxonomyCategories :: Maybe TaxonomyCategories
      , taxonomyFormat :: Maybe CSVOrJSONFormat
      , taxonomySPPLocale :: Maybe SPPLocale
      , taxonomySpecies :: Maybe SpeciesCodes
      , taxonomyVersion :: Maybe Text
      }
  deriving (Show, Read, Eq)

-- | Options for the @taxonomic-forms@ command.
newtype TaxonomicFormsOptions =
    TaxonomicFormsOptions
      { taxonomicFormsSpecies :: SpeciesCode
      }
  deriving (Show, Read, Eq)

-- | Options for the @taxa-locale-codes@ command.
newtype TaxaLocaleCodesOptions =
    TaxaLocaleCodesOptions
      { taxaLocaleCodesAcceptLanguage :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Options for the @taxonomic-groups@ command.
data TaxonomicGroupsOptions =
    TaxonomicGroupsOptions
      { taxonomicGroupsSPPGrouping :: SPPGrouping
      , taxonomicGroupsSPPLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Options for the @region-info@ command.
data RegionInfoOptions =
    RegionInfoOptions
      { regionInfoRegion :: Region
      , regionInfoRegionNameFormat :: Maybe RegionNameFormat
      }
  deriving (Show, Read, Eq)

-- | Options for the @sub-regions@ command.
data SubRegionListOptions =
    SubRegionListOptions
      { subRegionListParentRegionCode :: RegionCode
      , subRegionListRegionType :: RegionType
      }
  deriving (Show, Read, Eq)

-- | Options for the @sub-regions@ command.
newtype AdjacentRegionsOptions =
    AdjacentRegionsOptions
      { adjacentRegionsRegion :: Region
      }
  deriving (Show, Read, Eq)

-------------------------------------------------------------------------------
-- Command/option/flag parsers
-------------------------------------------------------------------------------

-- | Parse a command provided to the @ebird@ CLI.
eBirdCommand :: Parser (Maybe Text, EBirdCommand)
eBirdCommand =
        (,)
    <$> optionalAPIKey
    <*>
      (     subparser
              (    commandGroup "Observation commands:"
                <> command "observations" recentObservationsInfo
                <> command "notable-observations" recentNotableObservationsInfo
                <> command "species-observations" recentSpeciesObservationsInfo
                <> command "nearby-observations" recentNearbyObservationsInfo
                <> command "nearby-species-observations" recentNearbySpeciesObservationsInfo
                <> command "nearest-species-observations" recentNearestSpeciesObservationsInfo
                <> command "nearby-notable-observations" recentNearbyNotableObservationsInfo
                <> command "historical-observations" historicalObservationsInfo
              )
        <|> subparser
              (
                commandGroup "Product commands:"
                <> command "recent-checklists" recentChecklistsInfo
                <> command "top-100" top100Info
                <> command "checklist-feed" checklistFeedInfo
                <> command "regional-statistics" regionalStatisticsInfo
                <> command "species-list" speciesListInfo
                <> command "view-checklist" viewChecklistInfo
                <> hidden
              )
        <|> subparser
              (
                   commandGroup "Hotspot commands:"
                <> command "region-hotspots" regionHotspotsInfo
                <> command "nearby-hotspots" nearbyHotspotsInfo
                <> command "hotspot-info" hotspotInfoInfo
                <> hidden
              )
        <|> subparser
              (
                   commandGroup "Taxonomy commands:"
                <> command "taxonomy" taxonomyInfo
                <> command "taxonomic-forms" taxonomicFormsInfo
                <> command "taxa-locale-codes" taxaLocaleCodesInfo
                <> command "taxonomy-versions" taxonomyVersionsInfo
                <> command "taxonomic-groups" taxonomicGroupsInfo
                <> hidden
              )
        <|> subparser
              (
                   commandGroup "Region commands:"
                <> command "region-info" regionInfoInfo
                <> command "sub-regions" subRegionsInfo
                <> command "adjacent-regions" adjacentRegionsInfo
                <> hidden
              )
      )
  where
    optionalAPIKey :: Parser (Maybe Text)
    optionalAPIKey =
      optional
        ( strOption
            (    long "api-key"
              <> short 'k'
              <> metavar "API_KEY"
              <> help "Specify an eBird API key"
            )
        )

    recentObservationsInfo :: ParserInfo EBirdCommand
    recentObservationsInfo =
      info
        (RecentObservationsCommand <$> recentObservationsOptions)
        (progDesc "Get recent observations within a region")

    recentNotableObservationsInfo :: ParserInfo EBirdCommand
    recentNotableObservationsInfo =
      info
        (RecentNotableObservationsCommand <$> recentNotableObservationsOptions)
        (progDesc "Get recent notable observations within a region")

    recentSpeciesObservationsInfo :: ParserInfo EBirdCommand
    recentSpeciesObservationsInfo =
      info
        (RecentSpeciesObservationsCommand <$> recentSpeciesObservationsOptions)
        (progDesc "Get recent observations of a species within a region")

    recentNearbyObservationsInfo :: ParserInfo EBirdCommand
    recentNearbyObservationsInfo =
      info
        (RecentNearbyObservationsCommand <$> recentNearbyObservationsOptions)
        (progDesc "Get recent observations within some radius of a latitude/longitude")

    recentNearbySpeciesObservationsInfo :: ParserInfo EBirdCommand
    recentNearbySpeciesObservationsInfo =
      info
        (RecentNearbySpeciesObservationsCommand <$> recentNearbySpeciesObservationsOptions)
        (progDesc "Get recent observations of a species within some radius of a latitude/longitude")

    recentNearestSpeciesObservationsInfo :: ParserInfo EBirdCommand
    recentNearestSpeciesObservationsInfo =
      info
        (RecentNearestSpeciesObservationsCommand <$> recentNearestSpeciesObservationsOptions)
        (progDesc "Get recent observations of a species nearest to a latitude/longitude")

    recentNearbyNotableObservationsInfo :: ParserInfo EBirdCommand
    recentNearbyNotableObservationsInfo =
      info
        (RecentNearbyNotableObservationsCommand <$> recentNearbyNotableObservationsOptions)
        (progDesc "Get recent notable observations within some radius of a latitude/longitude")

    historicalObservationsInfo :: ParserInfo EBirdCommand
    historicalObservationsInfo =
      info
        (HistoricalObservationsCommand <$> historicalObservationsOptions)
        (progDesc "Get a list of observations for each species seen in a region on a specific date")

    recentChecklistsInfo :: ParserInfo EBirdCommand
    recentChecklistsInfo =
      info
        (RecentChecklistsCommand <$> recentChecklistsOptions)
        (progDesc "Get recent checklists within a region")

    top100Info :: ParserInfo EBirdCommand
    top100Info =
      info
        (Top100Command <$> top100Options)
        (progDesc "Get the top 100 contributors in a region for a given date")

    checklistFeedInfo :: ParserInfo EBirdCommand
    checklistFeedInfo =
      info
        (ChecklistFeedCommand <$> checklistFeedOptions)
        (progDesc "Get the checklist feed in a region for a given date")

    regionalStatisticsInfo :: ParserInfo EBirdCommand
    regionalStatisticsInfo =
      info
        (RegionalStatisticsCommand <$> regionalStatisticsOptions)
        (progDesc "Get the regional statistics for a region on a given date")

    speciesListInfo :: ParserInfo EBirdCommand
    speciesListInfo =
      info
        (SpeciesListCommand <$> speciesListOptions)
        (progDesc "Get the list of all species ever observed in a region")

    viewChecklistInfo :: ParserInfo EBirdCommand
    viewChecklistInfo =
      info
        (ViewChecklistCommand <$> viewChecklistOptions)
        (progDesc "Get information about a particular checklist")

    regionHotspotsInfo :: ParserInfo EBirdCommand
    regionHotspotsInfo =
      info
        (RegionHotspotsCommand <$> regionHotspotsOptions)
        (progDesc "Get a list of hotspots in one or more regions")

    nearbyHotspotsInfo :: ParserInfo EBirdCommand
    nearbyHotspotsInfo =
      info
        (NearbyHotspotsCommand <$> nearbyHotspotsOptions)
        (progDesc "Get a list of hotspots within some radius of a latitude/longitude")

    hotspotInfoInfo :: ParserInfo EBirdCommand
    hotspotInfoInfo =
      info
        (HotspotInfoCommand <$> hotspotInfoOptions)
        (progDesc "Get information about a hotspot")

    taxonomyInfo :: ParserInfo EBirdCommand
    taxonomyInfo =
      info
        (TaxonomyCommand <$> taxonomyOptions)
        (progDesc "Get any version of the eBird taxonomy")

    taxonomicFormsInfo :: ParserInfo EBirdCommand
    taxonomicFormsInfo =
      info
        (TaxonomicFormsCommand <$> taxonomicFormsOptions)
        (progDesc "Get the subspecies of a given species recognized by the taxonomy")

    taxaLocaleCodesInfo :: ParserInfo EBirdCommand
    taxaLocaleCodesInfo =
      info
        (TaxaLocaleCodesCommand <$> taxaLocaleCodesOptions)
        (progDesc "Get the supported locale codes and names for species common names")

    taxonomyVersionsInfo :: ParserInfo EBirdCommand
    taxonomyVersionsInfo =
      info
        (pure TaxonomyVersionsCommand <**> helper)
        (progDesc "Get the complete list of taxonomy versions, with a flag indicating which is the latest")

    taxonomicGroupsInfo :: ParserInfo EBirdCommand
    taxonomicGroupsInfo =
      info
        (TaxonomicGroupsCommand <$> taxonomicGroupsOptions)
        (progDesc "Get the list of species groups in either Merlin or eBird grouping")

    regionInfoInfo :: ParserInfo EBirdCommand
    regionInfoInfo =
      info
        (RegionInfoCommand <$> regionInfoOptions)
        (progDesc "Get information about a region")

    subRegionsInfo :: ParserInfo EBirdCommand
    subRegionsInfo =
      info
        (SubRegionListCommand <$> subRegionListOptions)
        (progDesc "Get the list of sub-regions within a region")

    adjacentRegionsInfo :: ParserInfo EBirdCommand
    adjacentRegionsInfo =
      info
        (AdjacentRegionsCommand <$> adjacentRegionsOptions)
        (progDesc "Get the list of regions that are adjacent to a region")

-------------------------------------------------------------------------------
-- Parsers for observation command options
-------------------------------------------------------------------------------

-- | Parse the options for the @recent-observations@ command.
recentObservationsOptions :: Parser RecentObservationsOptions
recentObservationsOptions =
        RecentObservationsOptions
    <$> regionCodeOpt "observations"
    <*> optional (backOpt "observations" "submitted" (Just 14))
    <*> optional (taxonomyCategoriesOpt "observations of")
    <*> optional observationOnlyHotspotsOpt
    <*> optional observationIncludeProvisionalOpt
    <*> optional observationMaxResultsOpt
    <*> optional extraRegionsOpt
    <*> optional (sppLocaleOpt "common")
    <**> helper

-- | Parse the options for a @recent-notable-observations@ command.
recentNotableObservationsOptions :: Parser RecentNotableObservationsOptions
recentNotableObservationsOptions =
        RecentNotableObservationsOptions
    <$> regionCodeOpt "observations"
    <*> optional (backOpt "observations" "submitted" (Just 14))
    <*> optional observationDetailLevelOpt
    <*> optional observationOnlyHotspotsOpt
    <*> optional observationMaxResultsOpt
    <*> optional extraRegionsOpt
    <*> optional (sppLocaleOpt "common")
    <**> helper

-- | Parse the options for a @recent-species-observations@ command.
recentSpeciesObservationsOptions :: Parser RecentSpeciesObservationsOptions
recentSpeciesObservationsOptions =
        RecentSpeciesObservationsOptions
    <$> regionCodeOpt "observations"
    <*> speciesCodeOpt "observations"
    <*> optional (backOpt "observations" "submitted" (Just 14))
    <*> optional observationOnlyHotspotsOpt
    <*> optional observationIncludeProvisionalOpt
    <*> optional observationMaxResultsOpt
    <*> optional extraRegionsOpt
    <*> optional (sppLocaleOpt "common")
    <**> helper

-- | Parse the options for a @recent-nearby-observations@ command.
recentNearbyObservationsOptions :: Parser RecentNearbyObservationsOptions
recentNearbyObservationsOptions =
        RecentNearbyObservationsOptions
    <$> latLngOpt "latitude" "observations"
    <*> latLngOpt "longitude" "observations"
    <*> optional (searchRadiusOpt "observations" 50 (Just 25))
    <*> optional (backOpt "observations" "submitted" (Just 14))
    <*> optional (taxonomyCategoriesOpt "observations of")
    <*> optional observationOnlyHotspotsOpt
    <*> optional observationIncludeProvisionalOpt
    <*> optional observationMaxResultsOpt
    <*> optional observationSortByOpt
    <*> optional (sppLocaleOpt "common")
    <**> helper

-- | Parse the options for a @recent-nearby-species-observations@ command.
recentNearbySpeciesObservationsOptions :: Parser RecentNearbySpeciesObservationsOptions
recentNearbySpeciesObservationsOptions =
        RecentNearbySpeciesObservationsOptions
    <$> speciesCodeOpt "observations"
    <*> latLngOpt "latitude" "observations"
    <*> latLngOpt "longitude" "observations"
    <*> optional (searchRadiusOpt "observations" 50 (Just 25))
    <*> optional (backOpt "observations" "submitted" (Just 14))
    <*> optional (taxonomyCategoriesOpt "observations of")
    <*> optional observationOnlyHotspotsOpt
    <*> optional observationIncludeProvisionalOpt
    <*> optional observationMaxResultsOpt
    <*> optional observationSortByOpt
    <*> optional (sppLocaleOpt "common")
    <**> helper

-- | Parse the options for a @recent-nearest-species-observations@ command.
recentNearestSpeciesObservationsOptions :: Parser RecentNearestSpeciesObservationsOptions
recentNearestSpeciesObservationsOptions =
        RecentNearestSpeciesObservationsOptions
    <$> speciesCodeOpt "observations"
    <*> latLngOpt "latitude" "observations"
    <*> latLngOpt "longitude" "observations"
    <*> optional (searchRadiusOpt "observations" 50 Nothing)
    <*> optional (backOpt "observations" "submitted" (Just 14))
    <*> optional observationOnlyHotspotsOpt
    <*> optional observationIncludeProvisionalOpt
    <*> optional observationMaxResultsOpt
    <*> optional (sppLocaleOpt "common")
    <**> helper

-- | Parse the options for a @recent-nearest-species-observations@ command.
recentNearbyNotableObservationsOptions :: Parser RecentNearbyNotableObservationsOptions
recentNearbyNotableObservationsOptions =
        RecentNearbyNotableObservationsOptions
    <$> latLngOpt "latitude" "observations"
    <*> latLngOpt "longitude" "observations"
    <*> optional (searchRadiusOpt "observations" 50 (Just 25))
    <*> optional observationDetailLevelOpt
    <*> optional (backOpt "observations" "submitted" (Just 14))
    <*> optional observationOnlyHotspotsOpt
    <*> optional observationMaxResultsOpt
    <*> optional (sppLocaleOpt "common")
    <**> helper

-- | Parse the options for a @historical-observations@ command.
historicalObservationsOptions :: Parser HistoricalObservationsOptions
historicalObservationsOptions =
        HistoricalObservationsOptions
    <$> regionCodeOpt "observations"
    <*> dateOpt
    <*> optional (taxonomyCategoriesOpt "observations of")
    <*> optional observationDetailLevelOpt
    <*> optional observationOnlyHotspotsOpt
    <*> optional observationIncludeProvisionalOpt
    <*> optional observationMaxResultsOpt
    <*> optional rankOpt
    <*> optional extraRegionsOpt
    <*> optional (sppLocaleOpt "common")
    <**> helper
  where
    dateOpt :: Parser EBirdDate
    dateOpt =
      option (attoReadM parseEBirdDate)
        (    long "date"
          <> metavar "YYYY-MM-DD"
          <> help ( "Specify the date to fetch observations from (year " ++
                    "1800 to present)"
                  )
        )

    rankOpt :: Parser SelectObservation
    rankOpt =
      option (attoReadM parseSelectObservation)
        (    long "select"
          <> metavar "SELECT"
          <> help ( "Specify whether to select the first or last " ++
                    "observation of a species if there are multiple " ++
                    "(\"first\" or \"last\") (default: last)"
                  )
        )

-- | Parse a 'GHC.Types.Bool', intended to be used as an option determining whether to
-- include observations from hotspots in the response.
observationOnlyHotspotsOpt :: Parser Bool
observationOnlyHotspotsOpt =
    switch
      (    long "only-hotspots"
        <> help "Only include observations from hotspots"
      )

-- | Parse a 'GHC.Types.Bool', intended to be used as an option determining whether to
-- include unreviewed observations in the response.
observationIncludeProvisionalOpt :: Parser Bool
observationIncludeProvisionalOpt =
    switch
      (    long "include-provisional"
        <> help "Include observations which have not yet been reviewed"
      )

-- | Parse a 'Integer' option indicating the number of results to include in the
-- response.
observationMaxResultsOpt :: Parser Integer
observationMaxResultsOpt =
    option (attoReadM A.decimal)
      (    long "max-results"
        <> metavar "N"
        <> help ( "Specify the max number of observations to include " ++
                  "(1 to 10000, default: all)"
                )
      )

-- | Parse a 'RegionCode' as a generic command option, intended for use with the
-- observation commands.
regionCodeOpt
  :: String
  -- ^ What are we fetching? (e.g. "observations")
  -> Parser RegionCode
regionCodeOpt thing =
    option (attoReadM parseRegionCode)
      (    long "region"
        <> metavar "REGION_CODE"
        <> help helpStr
      )
  where
    helpStr :: String
    helpStr =
      printf
        ( "Specify the regions to fetch %s from " ++
          "(e.g. \"US-WY,US-CO,US-ID\" or \"US-CA-037\")"
        )
        thing

-- | Configurable 'TaxonomyCategories' parser
taxonomyCategoriesOpt
  :: String
  -- ^ String to include after "one or more taxonomy categories to include ..."
  -> Parser TaxonomyCategories
taxonomyCategoriesOpt desc =
    option (attoReadM parseTaxonomyCategories)
      (    long "taxonomy-categories"
        <> metavar "CATEGORIES"
        <> help helpStr
      )
  where
    helpStr :: String
    helpStr =
      printf
        ( "Specify a list of one or more taxonomy categories to include " ++
          "%s (e.g. \"issf\" or \"hybrid\") (default: all categories)"
        )
        desc

-- | Parse a 'SortObservationsBy' as an option determining how returned
-- observations will be sorted.
observationSortByOpt :: Parser SortObservationsBy
observationSortByOpt =
    option (attoReadM parseSortObservationsBy)
      (    long "sort-by"
        <> metavar "SORT_BY"
        <> help ( "Specify the ordering to use for the resulting " ++
                  "observations (\"date\" or \"species\") (default: date, " ++
                  "earliest first)"
                )
      )

-- | Parse a 'DetailLevel' as an option determining the detail level of
-- observations in the result.
observationDetailLevelOpt :: Parser DetailLevel
observationDetailLevelOpt =
    option (attoReadM parseDetailLevel)
      (    long "detail"
        <> metavar "DETAIL_LEVEL"
        <> help ( "Specify the detail level of the returned observations " ++
                  "(\"simple\" or \"full\") (default: simple)"
                )
      )

-- | Parse a 'RegionCode' as extra regions to fetch observations from
extraRegionsOpt :: Parser RegionCode
extraRegionsOpt =
  option (attoReadM parseRegionCode)
    (    long "extra-regions"
      <> metavar "REGION_CODE"
      <> help "Up to 10 extra regions to fetch observations from"
    )

-------------------------------------------------------------------------------
-- Parsers for product command options
-------------------------------------------------------------------------------

-- | Parse the options for a @recent-checklists@ command.
recentChecklistsOptions :: Parser RecentChecklistsOptions
recentChecklistsOptions =
        RecentChecklistsOptions
    <$> checklistRegionCodeOpt
    <*> optional checklistMaxResultsOpt
    <**> helper

-- | Parse the options for a @top-100@ command.
top100Options :: Parser Top100Options
top100Options =
        Top100Options
    <$> regionOpt
    <*> dateOpt
    <*> optional rankedByOpt
    <*> optional maxResultsOpt
    <**> helper
  where
    regionOpt :: Parser Region
    regionOpt =
      option (attoReadM parseRegion)
        (    long "region"
          <> metavar "REGION"
          <> help ( "Specify the region to fetch the top 100 contributors " ++
                    "for (e.g. \"US-WY\" or \"US-CA-037\")"
                  )
        )

    dateOpt :: Parser EBirdDate
    dateOpt =
      option (attoReadM parseEBirdDate)
        (    long "date"
          <> metavar "YYYY-MM-DD"
          <> help ( "Specify the date to fetch the top contributors on " ++
                    "(year 1800 to present)"
                  )
        )

    rankedByOpt :: Parser RankTop100By
    rankedByOpt =
      option (attoReadM parseRankTop100By)
        (    long "rank-by"
          <> metavar "RANK_BY"
          <> help ( "Specify whether to rank contributors by number of " ++
                    "species observed (\"spp\") or number of checklists " ++
                    "completed (\"cl\") (default: spp)"
                  )
        )

    maxResultsOpt :: Parser Integer
    maxResultsOpt =
      option (attoReadM A.decimal)
        (    long "max-results"
          <> metavar "N"
          <> help ( "Specify the max number of contributors to include " ++
                    "(1 to 100, default: 100)"
                  )
        )

-- | Parse the options for a @top-100@ command.
checklistFeedOptions :: Parser ChecklistFeedOptions
checklistFeedOptions =
        ChecklistFeedOptions
    <$> regionOpt
    <*> dateOpt
    <*> optional sortByOpt
    <*> optional maxResultsOpt
    <**> helper
  where
    regionOpt :: Parser Region
    regionOpt =
      option (attoReadM parseRegion)
        (    long "region"
          <> metavar "REGION"
          <> help ( "Specify the region to fetch the checklist feed " ++
                    "for (e.g. \"US-WY\" or \"US-CA-037\")"
                  )
        )

    dateOpt :: Parser EBirdDate
    dateOpt =
      option (attoReadM parseEBirdDate)
        (    long "date"
          <> metavar "YYYY-MM-DD"
          <> help ( "Specify the date to fetch the checklist feed on " ++
                    "(year 1800 to present)"
                  )
        )

    sortByOpt :: Parser SortChecklistsBy
    sortByOpt =
      option (attoReadM parseSortChecklistsBy)
        (    long "sort-by"
          <> metavar "SORT_BY"
          <> help ( "Specify whether to sort the checklist fee by date of " ++
                    "creation (\"obs_dt\") or date of submission " ++
                    "(\"creation_dt\") (default: obs_dt)"
                  )
        )

    maxResultsOpt :: Parser Integer
    maxResultsOpt =
      option (attoReadM A.decimal)
        (    long "max-results"
          <> metavar "N"
          <> help ( "Specify the max number of checklists to include " ++
                    "(1 to 200, default: 10)"
                  )
        )

-- | Parse the options for a @regional-statistics@ command.
regionalStatisticsOptions :: Parser RegionalStatisticsOptions
regionalStatisticsOptions =
        RegionalStatisticsOptions
    <$> regionOpt
    <*> dateOpt
    <**> helper
  where
    regionOpt :: Parser Region
    regionOpt =
      option (attoReadM parseRegion)
        (    long "region"
          <> metavar "REGION"
          <> help ( "Specify the region to fetch the statistics for " ++
                    "(e.g. \"US-WY\" or \"US-CA-037\")"
                  )
        )

    dateOpt :: Parser EBirdDate
    dateOpt =
      option (attoReadM parseEBirdDate)
        (    long "date"
          <> metavar "YYYY-MM-DD"
          <> help ( "Specify the date to fetch the statistics on (year " ++
                    "1800 to present)"
                  )
        )

-- | Parse the options for a @regional-statistics@ command.
speciesListOptions :: Parser SpeciesListOptions
speciesListOptions =
        SpeciesListOptions
    <$> regionOpt
    <**> helper
  where
    regionOpt :: Parser Region
    regionOpt =
      option (attoReadM parseRegion)
        (    long "region"
          <> metavar "REGION"
          <> help ( "Specify the region to fetch the species list for " ++
                    "(e.g. \"US-WY\" or \"US-CA-037\")"
                  )
        )

-- | Parse the options for a @regional-statistics@ command.
viewChecklistOptions :: Parser ViewChecklistOptions
viewChecklistOptions =
        ViewChecklistOptions
    <$> subIdOpt
    <**> helper
  where
    subIdOpt :: Parser Text
    subIdOpt =
      option (attoReadM A.takeText)
        (    long "submission-id"
          <> metavar "SUBMISSION_ID"
          <> help ( "Specify the submission ID of the checklist to view " ++
                    "(e.g. \"S144646447\")"
                  )
        )

-- | Parse a 'RegionCode' as a generic command option, intended for use with the
-- checklists commands.
checklistRegionCodeOpt :: Parser RegionCode
checklistRegionCodeOpt =
    option (attoReadM parseRegionCode)
      (    long "region"
        <> metavar "REGION_CODE"
        <> help ( "Specify the regions to fetch checklists from " ++
                  "(e.g. \"US-WY,US-CO\" or \"US-CA-037\")"
                )
      )

-- | Parse a 'Integer' as a generic command option, intended for use with the
-- checklists commands for determining max results to include.
checklistMaxResultsOpt :: Parser Integer
checklistMaxResultsOpt =
    option (attoReadM A.decimal)
      (    long "max-results"
        <> metavar "N"
        <> help ( "Specify the max number of checklists to include " ++
                  "(1 to 200, default: 10)"
                )
      )

-------------------------------------------------------------------------------
-- Parsers for hotspot command options
-------------------------------------------------------------------------------

-- | Parse the options for a @region-hotspots@ command.
regionHotspotsOptions :: Parser RegionHotspotsOptions
regionHotspotsOptions =
        RegionHotspotsOptions
    <$> regionCodeOpt "hotspots"
    <*> optional (backOpt "hotspots" "visited" Nothing)
    <*> pure (Just JSONFormat)
    <**> helper

-- | Parse the options for a @nearby-hotspots@ command.
nearbyHotspotsOptions :: Parser NearbyHotspotsOptions
nearbyHotspotsOptions =
        NearbyHotspotsOptions
    <$> latLngOpt "latitude" "hotspots"
    <*> latLngOpt "longitude" "hotspots"
    <*> optional (backOpt "hotspots" "visited" Nothing)
    <*> optional (searchRadiusOpt "hotspots" 50 (Just 25))
    <*> pure (Just JSONFormat)
    <**> helper

-- | Parse the options for a @hotspot-info@ command.
hotspotInfoOptions :: Parser HotspotInfoOptions
hotspotInfoOptions =
        HotspotInfoOptions
    <$> locationCodeOpt
    <**> helper
  where
    locationCodeOpt :: Parser Text
    locationCodeOpt =
      option (attoReadM A.takeText)
        (    long "location"
          <> metavar "LOCATION"
          <> help "Location code of the hotspot (e.g. \"L5044136\")"
        )
-------------------------------------------------------------------------------
-- Parsers for taxonomy command options
-------------------------------------------------------------------------------

-- | Parse the options for a @nearby-hotspots@ command.
taxonomyOptions :: Parser TaxonomyOptions
taxonomyOptions =
        TaxonomyOptions
    <$> optional (taxonomyCategoriesOpt "in the taxonomy")
    <*> pure (Just JSONFormat)
    <*> optional (sppLocaleOpt "common")
    <*> optional speciesCodesOpt
    <*> optional taxonomyVersionOpt
    <**> helper
  where
    speciesCodesOpt :: Parser SpeciesCodes
    speciesCodesOpt =
      option (attoReadM parseSpeciesCodes)
        (    long "species"
          <> metavar "SPECIES_CODES"
          <> help "Only include entries for these species (default: all)"
        )

    taxonomyVersionOpt :: Parser Text
    taxonomyVersionOpt =
      option (attoReadM A.takeText)
        (    long "version"
          <> metavar "VERSION"
          <> help "Taxonomy version to fetch (default: latest)"
        )

-- | Parse the options for a @taxonomic-forms@ command.
taxonomicFormsOptions :: Parser TaxonomicFormsOptions
taxonomicFormsOptions =
        TaxonomicFormsOptions
    <$> speciesCodeOpt "subspecies"
    <**> helper

-- | Parse the options for a @taxa-locale-codes@ command.
taxaLocaleCodesOptions :: Parser TaxaLocaleCodesOptions
taxaLocaleCodesOptions =
        TaxaLocaleCodesOptions
    <$> optional acceptLanguageOpt
    <**> helper
  where
    acceptLanguageOpt :: Parser SPPLocale
    acceptLanguageOpt =
      option (attoReadM parseSPPLocale)
        (    long "accept-language"
          <> metavar "LOCALE"
          <> help ( "Get language names translated to this locale, when " ++
                    "available (default: en)"
                  )
        )

-- | Parse the options for a @taxa-locale-codes@ command.
taxonomicGroupsOptions :: Parser TaxonomicGroupsOptions
taxonomicGroupsOptions =
        TaxonomicGroupsOptions
    <$> sppGroupingOpt
    <*> optional (sppLocaleOpt "group")
    <**> helper
  where
    sppGroupingOpt :: Parser SPPGrouping
    sppGroupingOpt =
      option (attoReadM parseSPPGrouping)
        (    long "spp-grouping"
          <> metavar "GROUPING"
          <> help ( "Group species using Merlin or eBird grouping " ++
                    "(\"merlin\" or \"ebird\")"
                  )
        )

-------------------------------------------------------------------------------
-- Parsers for region command options
-------------------------------------------------------------------------------

-- | Parse the options for the @region-info@ command.
regionInfoOptions :: Parser RegionInfoOptions
regionInfoOptions =
        RegionInfoOptions
    <$> region
    <*> optional regionNameFormat
    <**> helper

-- | Parse the options for the @sub-regions@ command.
subRegionListOptions :: Parser SubRegionListOptions
subRegionListOptions =
        SubRegionListOptions
    <$> regionCode
    <*> regionType
    <**> helper

-- | Parse the options for the @sub-regions@ command.
adjacentRegionsOptions :: Parser AdjacentRegionsOptions
adjacentRegionsOptions =
        AdjacentRegionsOptions
    <$> regionOpt
    <**> helper
  where
    regionOpt :: Parser Region
    regionOpt =
      option (attoReadM parseRegion)
        (    long "region"
          <> metavar "REGION"
          <> help ( "Specify the region to get adjacent regions of " ++
                    "(e.g. \"US-WY\")"
                  )
        )

-------------------------------------------------------------------------------
-- Generic single option parsers
-------------------------------------------------------------------------------

-- | Parse a 'SPPLocale' as a generic command option.
sppLocaleOpt :: String -> Parser SPPLocale
sppLocaleOpt nameType =
  option (attoReadM parseSPPLocale)
    (    long "spp-locale"
      <> metavar "LOCALE"
      <> help helpStr
    )
  where
    helpStr :: String
    helpStr =
      printf
        "Specify a locale to use for %s names"
        nameType

-- | Parse a 'Region' as a generic command option.
region :: Parser Region
region = option (attoReadM parseRegion)
    (    long "region"
      <> metavar "REGION"
      <> help "Specify a region (e.g. \"world\" or \"US-WY\")"
    )

-- | Parse a 'RegionCode' as a generic command option.
regionCode :: Parser RegionCode
regionCode = option (attoReadM parseRegionCode)
    (    long "region"
      <> metavar "REGION_CODE"
      <> help "Specify a region code (e.g. \"world\" or \"US-MT,US-WY\")"
    )

-- | Parse a 'RegionNameFormat' as a generic command option.
regionNameFormat :: Parser RegionNameFormat
regionNameFormat = option (attoReadM parseRegionNameFormat)
    (    long "region-name-format"
      <> metavar "REGION_NAME_FORMAT"
      <> help ( "Specify a region name format for the result. Must be one " ++
                "of \"detailed\", \"detailednoqual\", \"full\", " ++
                "\"namequal\", \"nameonly\", or \"revdetailed\" " ++
                "(default: full)"
              )
    )

-- | Parse a 'RegionType' as a generic command option.
regionType :: Parser RegionType
regionType = option (attoReadM parseRegionType)
    (    long "region-type"
      <> metavar "REGION_TYPE"
      <> help ( "Specify a region type (\"country\", \"subnational1\", " ++
                "\"subnational2\")"
              )
    )

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | Configurable 'SpeciesCode' option
speciesCodeOpt :: String -> Parser SpeciesCode
speciesCodeOpt thing =
  option (attoReadM parseSpeciesCode)
    (    long "species-code"
      <> metavar "SPECIES_CODE"
      <> help helpStr
    )
  where
    helpStr :: String
    helpStr =
      printf
        ( "Specify a species code to fetch %s for (e.g. \"barswa\" for Barn " ++
          "Swallow)"
        )
        thing

-- | Configurable "search radius" option
searchRadiusOpt
  :: String
  -- ^ What are we searching for (e.g. "observations")
  -> Integer
  -- ^ Maximum allowed value by the API
  -> Maybe Integer
  -- ^ Default value of the API ('Nothing' for "no limit")
  -> Parser Integer
searchRadiusOpt thing maxRadius mDefaultRadius =
    option (attoReadM A.decimal)
      (    long "radius"
        <> metavar "KILOMETERS"
        <> help helpStr
      )
  where
    helpStr :: String
    helpStr =
      printf
        "Specify the search radius to fetch %s within (0 to %d, default: %s)"
        thing
        maxRadius
        defStr

    defStr = maybe "no limit" show mDefaultRadius

-- | Configurable "back" option
backOpt
  :: String
  -- ^ What are we fetching (e.g. "hotspots")
  -> String
  -- ^ Verb we are filtering on (e.g. "submitted", or "visited")
  -> Maybe Integer
  -- ^ Default value ('Nothing' for "no limit")
  -> Parser Integer
backOpt thing verb mmax =
    option (attoReadM A.decimal)
      (    long "back"
        <> metavar "N"
        <> help helpStr
      )
  where
    helpStr :: String
    helpStr =
      printf
        "Only fetch %s %s within the last N days (1 - 30, default: %s)"
        thing
        verb
        defStr

    defStr = maybe "no limit" show mmax

-- | Configurable lat/lng option
latLngOpt
  :: String
  -- ^ "latitude" or "longitude"
  -> String
  -- ^ What are we looking for
  -> Parser Double
latLngOpt latLng thing =
    option (attoReadM A.double)
      (    long latLng
        <> metavar (map toUpper latLng)
        <> help helpStr
      )
  where
    helpStr :: String
    helpStr =
      printf
        "Specify the %s of the location to fetch %s near"
        latLng
        thing

-- Attoparsec utilities

-- | Convert an attoparsec parser into an optparse-applicative parser.
attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . Text.pack)

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- |
-- Module      : Data.EBird.API.Internal.Types.Taxonomy
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Types related to eBird taxonomy-related API values.

module Data.EBird.API.Taxonomy where

import Control.Arrow
import Data.Aeson
import Data.Attoparsec.Text
import Data.Char
import Data.Function
import Data.Functor
import Data.List.NonEmpty (NonEmpty(..))
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Optics
import GHC.Exts
import Servant.API (ToHttpApiData(..))

import Data.EBird.API.EBirdString

-------------------------------------------------------------------------------
-- * Taxonomy types
-------------------------------------------------------------------------------

-- | Taxa in the eBird taxonomy.
data Taxon =
    Taxon
      { -- | Scientific name, e.g. "Bombycilla garrulus/cedrorum"
        _taxonScientificName :: Text

        -- | Common name, e.g. "Bohemian/Cedar Waxwing"
      , _taxonCommonName :: Text

        -- | eBird species code, e.g. "waxwin"
      , _taxonSpeciesCode :: SpeciesCode

        -- | eBird species category, e.g. "slash"
        --
        -- See the [eBird
        -- documentation](https://science.ebird.org/en/use-ebird-data/the-ebird-taxonomy)
        -- for more information on species categories
      , _taxonCategory :: TaxonomyCategory

        -- | A numeric value that determines the location of this taxon in the
        -- taxonomy list, e.g. 29257.0
      , _taxonTaxonOrder :: Double

        -- | Banding codes, e.g. [\"BOWA\"] for Bohemian Waxwing.
      , _taxonBandingCodes :: [Text]

        -- | Common name codes, e.g. [\"BOWA\",\"CEDW\",\"CEWA\"]
      , _taxonCommonNameCodes :: [Text]

        -- | Scientific name codes, e.g. [\"BOCE\",\"BOGA\"]
      , _taxonScientificNameCodes :: [Text]

        -- | Order, e.g. \"Passeriformes\"
      , _taxonOrder :: Text

        -- | Family code, e.g. "bombyc1"
      , _taxonFamilyCode :: Maybe Text

        -- | Family common name, e.g. \"Waxwings\"
      , _taxonFamilyCommonName :: Maybe Text

        -- | Family scientific name, e.g. \"Bombycillidae\"
      , _taxonFamilyScientificName :: Maybe Text
      }
  deriving (Show, Read, Eq)

-- | eBird species codes, simply 'Text'; e.g. Gray Vireo is "gryvir", Field
-- Sparrow is "fiespa".
newtype SpeciesCode = SpeciesCode { speciesCode :: Text }
  deriving (Eq, Show, Read)

-- | A list of eBird 'SpeciesCode's.
newtype SpeciesCodes = SpeciesCodes { speciesCodes :: [SpeciesCode] }
  deriving (Eq, Show, Read)

-- | The taxonomy categories are explained in the
-- [eBird documentation](https://science.ebird.org/en/use-ebird-data/the-ebird-taxonomy).
-- Their examples are echoed in the documentation of the constructors of this
-- type.
data TaxonomyCategory =
      -- | The 'Species' category simply identifies species, e.g. "Tundra Swan
      -- /Cygnus columbianus/"
      Species

      -- | Genus or broad identification, e.g. "swan sp. /Cygnus sp./"
    | Spuh

      -- | Identifiable subspecies or group of subspecies, e.g. "Tundra Swan
      -- (Bewick’s) /Cygnus columbianus bewickii/" or "Tundra Swan (Whistling)
      -- /Cygnus columbianus columbianus/"
    | ISSF

      -- | Identification to species pair, e.g. "Tundra/Trumpeter Swan
      -- /Cygnus columbianus\/buccinator/"
    | Slash

      -- | Hybrid between two species, e.g. "Tundra x Trumpeter Swan (hybrid)"
    | Hybrid

      -- | Hybrid between two ISSF (subspecies or subspecies groups), e.g.
      -- "Tundra Swan (Whistling x Bewick’s)
      -- /Cygnus columbianus columbianus x bewickii/"
    | Intergrade

      -- | Distinctly-plumaged domesticated varieties that may be free-flying
      -- (these do not count on personal lists), e.g. "Mallard (Domestic type)"
    | Domestic

      -- | Miscellaneous other taxa, including recently-described species yet to
      -- be accepted or distinctive forms that are not universally accepted,
      -- e.g. Red-tailed Hawk (abieticola), Upland Goose (Bar-breasted).
    | Form
  deriving (Show, Read, Eq)

-- | 'TaxonomyCategories' values contain a 'NonEmpty' list of
-- 'TaxonomyCategory's.
newtype TaxonomyCategories =
    TaxonomyCategories
      { taxonomyCategoriesCategories :: NonEmpty TaxonomyCategory
      }
  deriving (Show, Read, Eq)

-- ** Optics for taxonomy types

makeLenses ''Taxon
makeFieldLabels ''Taxon

-------------------------------------------------------------------------------
-- * Auxiliary eBird taxonomy-related API types
-------------------------------------------------------------------------------

-- | eBird maintains many common name translations. See their
-- ["Bird Names in eBird"](https://support.ebird.org/en/support/solutions/articles/48000804865-bird-names-in-ebird)
-- documentation for a discussion of the languages they support.
--
-- This type is an enumeration of those languages, and is used to support the
-- [eBird API](https://documenter.getpostman.com/view/664302/S1ENwy59)
-- endpoints which allow a locale to be specified.
data SPPLocale =
      Af     -- ^ Afrikaans
    | Sq     -- ^ Albanians
    | Ar     -- ^ Arabic
    | Hy     -- ^ Armenian
    | As     -- ^ Assamese
    | Ast    -- ^ Asturian
    | Az     -- ^ Azerbaijani
    | Eu     -- ^ Basque
    | Bn     -- ^ Bengali
    | Bg     -- ^ Bulgarian
    | Ca     -- ^ Catalan
    | Zh     -- ^ Chinese, Mandarin (traditional)
    | Zh_SIM -- ^ Chinese, Simple
    | Ht_HT  -- ^ Creole, Haiti
    | Hr     -- ^ Croatian
    | Cs     -- ^ Czech
    | Da     -- ^ Danish
    | Nl     -- ^ Dutch
    | En     -- ^ English
    | En_AU  -- ^ English, Australia
    | En_BD  -- ^ English, Bangladesh
    | En_HAW -- ^ English, Hawaii
    | En_HBW -- ^ English, HBW
    | En_IN  -- ^ English, India
    | En_IOC -- ^ English, IOC
    | En_KE  -- ^ English, Kenya
    | En_MY  -- ^ English, Malaysia
    | En_NZ  -- ^ English, New Zealand
    | En_PH  -- ^ English, Philippines
    | En_ZA  -- ^ English, South Africa
    | En_AE  -- ^ English, UAE
    | En_UK  -- ^ English, United Kingdon
    | En_US  -- ^ English, United States
    | Fo     -- ^ Faroese
    | Fi     -- ^ Finnish
    | Fr     -- ^ French
    | Fr_AOU -- ^ French, AOU
    | Fr_FR  -- ^ French, France
    | Fr_CA  -- ^ French, Canada
    | Fr_GF  -- ^ French, Guiana
    | Fr_GP  -- ^ French, Guadeloupe
    | Fr_HT  -- ^ French, Haiti
    | Gl     -- ^ Gallegan
    | De     -- ^ German
    | El     -- ^ Greek
    | Gu     -- ^ Gujarati
    | He     -- ^ Hebrew
    | Hi     -- ^ Hindi
    | Hu     -- ^ Hungarian
    | Is     -- ^ Icelandic
    | In     -- ^ Indonesian
    | It     -- ^ Italian
    | Ja     -- ^ Japanese
    | Ko     -- ^ Korean
    | Lv     -- ^ Latvian
    | Lt     -- ^ Lithuanian
    | Ml     -- ^ Malayalam
    | Mr     -- ^ Marathi
    | Mn     -- ^ Mongolian
    | No     -- ^ Norwegian
    | Or     -- ^ Odia
    | Fa     -- ^ Persian
    | Pl     -- ^ Polish
    | Pt_AO  -- ^ Portuguese, Angola
    | Pt_RAA -- ^ Portuguese, Azores
    | Pt_Br  -- ^ Portuguese, Brazil
    | Pt_RAM -- ^ Portuguese, Madeira
    | Pt_PT  -- ^ Portuguese, Portugal
    | Ro     -- ^ Romanian
    | Ru     -- ^ Russian
    | Sr     -- ^ Serbian
    | Sk     -- ^ Slovak
    | Sl     -- ^ Slovenian
    | Es     -- ^ Spanish
    | Es_AR  -- ^ Spanish, Argentina
    | Es_CL  -- ^ Spanish, Chile
    | Es_CR  -- ^ Spanish, Costa Rica
    | Es_CU  -- ^ Spanish, Cuba
    | Es_DO  -- ^ Spanish, Dominican Republic
    | Es_EC  -- ^ Spanish, Ecuador
    | Es_HN  -- ^ Spanish, Honduras
    | Es_MX  -- ^ Spanish, Mexico
    | Es_PA  -- ^ Spanish, Panama
    | Es_PY  -- ^ Spanish, Paraguay
    | Es_PE  -- ^ Spanish, Peru
    | Es_PR  -- ^ Spanish, Puerto Rico
    | Es_ES  -- ^ Spanish, Spain
    | Es_UY  -- ^ Spanish, Uruguay
    | Es_VE  -- ^ Spanish, Venezuela
    | Sv     -- ^ Swedish
    | Te     -- ^ Telugu
    | Th     -- ^ Thai
    | Tr     -- ^ Turkish
    | Uk     -- ^ Ukrainian
  deriving (Show, Read, Eq)

-- | Values returned from the 'Data.EBird.API.TaxaLocaleCodesAPI'.
data SPPLocaleListEntry =
    SPPLocaleListEntry
      { -- | The code of the locale, e.g. 'En_US'
        _sppLocaleListEntryCode :: SPPLocale

        -- | The name, e.g. "English (United States)"
      , _sppLocaleListEntryName :: Text

        -- | The date and time of the last update for this locale
      , _sppLocaleListEntryLastUpdate :: Text
      }
  deriving (Show, Read, Eq)

-- | Values represent the different ways that taxonomic groups may be grouped.
-- 'MerlinGrouping' puts like birds together, with falcons next to hawks.
-- 'EBirdGrouping' follows taxonomic order.
data SPPGrouping = MerlinGrouping | EBirdGrouping
  deriving (Show, Read, Eq)

-- | Values returned by the 'Data.EBird.API.TaxonomicGroupsAPI'.
data TaxonomicGroupListEntry =
    TaxonomicGroupListEntry
      { -- | Name of the group, e.g. \"Waterfowl\"
        _taxonomicGroupListEntryName :: Text

        -- | Numeric value determining the location of this group in the list
      , _taxonomicGroupListEntryOrder :: Integer

        -- | The bounds of the ordering, depending on the grouping
      , _taxonomicGroupListEntryOrderBounds :: [(Integer, Integer)]
      }
  deriving (Show, Read, Eq)

-- | Values returned by the 'Data.EBird.API.TaxonomyVersionsAPI'.
data TaxonomyVersionListEntry =
    TaxonomyVersionListEntry
      { _taxonomyVersionAuthorityVersion :: Double
      , _taxonomyVersionLatest :: Bool
      }
  deriving (Show, Read, Eq)

-- ** Optics for taxonomy-related types

makeLenses ''SPPLocaleListEntry
makeFieldLabels ''SPPLocaleListEntry
makeLenses ''TaxonomicGroupListEntry
makeFieldLabels ''TaxonomicGroupListEntry
makeLenses ''TaxonomyVersionListEntry
makeFieldLabels ''TaxonomyVersionListEntry

-------------------------------------------------------------------------------
-- aeson instances
-------------------------------------------------------------------------------

-- | Explicit instance for compatibility with their field names
instance FromJSON Taxon where
  parseJSON = withObject "Taxon" $ \v ->
          Taxon
      <$> v .: "sciName"
      <*> v .: "comName"
      <*> v .: "speciesCode"
      <*> v .: "category"
      <*> v .: "taxonOrder"
      <*> v .: "bandingCodes"
      <*> v .: "comNameCodes"
      <*> v .: "sciNameCodes"
      <*> v .: "order"
      <*> v .:? "familyCode"
      <*> v .:? "familyComName"
      <*> v .:? "familySciName"


-- | Explicit instance for compatibility with their field names
instance ToJSON Taxon where
  toJSON Taxon{..} =
      object $
        [ "sciName" .= _taxonScientificName
        , "comName" .= _taxonCommonName
        , "speciesCode" .= _taxonSpeciesCode
        , "category" .= _taxonCategory
        , "taxonOrder" .= _taxonTaxonOrder
        , "bandingCodes" .= _taxonBandingCodes
        , "comNameCodes" .= _taxonCommonNameCodes
        , "sciNameCodes" .= _taxonScientificNameCodes
        , "order" .= _taxonOrder
        , "familyComName" .= _taxonFamilyCommonName
        , "familySciName" .= _taxonFamilyScientificName
        ]
        -- Fields that may or may not be included
        <> [ "familyCode" .= c | Just c <- [_taxonFamilyCode]]
        <> [ "familyComName" .= n | Just n <- [_taxonFamilyCommonName]]
        <> [ "familySciName" .= n | Just n <- [_taxonFamilyScientificName]]

instance FromJSON SpeciesCode where
  parseJSON = withText "SpeciesCode" (pure . SpeciesCode)

instance ToJSON SpeciesCode where
  toJSON SpeciesCode{..} = String speciesCode

instance FromJSON SpeciesCodes where
  parseJSON = withArray "SpeciesCodes" $
      fmap (SpeciesCodes . toList) . traverse parseJSON

instance ToJSON SpeciesCodes where
  toJSON = Array . fromList . map toJSON . speciesCodes

instance FromJSON TaxonomyCategory where
  parseJSON = withText "TaxonomyCategory" $ \t ->
      case parseOnly parseTaxonomyCategory t of
        Left _ -> fail "failed to parse taxonomy category"
        Right r -> return r

instance ToJSON TaxonomyCategory where
  toJSON = String . toEBirdString

-- | Explicit instance for compatibility with their field names
instance FromJSON TaxonomyVersionListEntry where
  parseJSON = withObject "TaxonomyVersionListEntry" $ \v ->
          TaxonomyVersionListEntry
      <$> v .: "authorityVer"
      <*> v .: "latest"

-- | Explicit instance for compatibility with their field names
instance ToJSON TaxonomyVersionListEntry where
  toJSON TaxonomyVersionListEntry{..} =
      object
        [ "authorityVer" .= _taxonomyVersionAuthorityVersion
        , "latest" .= _taxonomyVersionLatest
        ]

instance FromJSON SPPLocale where
  parseJSON = withText "SPPLocale" $ \t ->
      case parseOnly parseSPPLocale t of
        Left _ -> fail $ "failed to parse spp locale: " <> Text.unpack t
        Right r -> return r

instance ToJSON SPPLocale where
  toJSON = String . toEBirdString

-- | Explicit instance for compatibility with their field names
instance FromJSON SPPLocaleListEntry where
  parseJSON = withObject "SPPLocaleListEntry" $ \v ->
          SPPLocaleListEntry
      <$> v .: "code"
      <*> v .: "name"
      <*> v .: "lastUpdate"

-- | Explicit instance for compatibility with their field names
instance ToJSON SPPLocaleListEntry where
  toJSON SPPLocaleListEntry{..} =
      object
        [ "code" .= _sppLocaleListEntryCode
        , "name" .= _sppLocaleListEntryName
        , "lastUpdate" .= _sppLocaleListEntryLastUpdate
        ]

-- | Explicit instance for compatibility with their field names
instance FromJSON TaxonomicGroupListEntry where
  parseJSON = withObject "TaxonomicGroupListEntry" $ \v ->
          TaxonomicGroupListEntry
      <$> v .: "groupName"
      <*> v .: "groupOrder"
      <*> v .: "taxonOrderBounds"

-- | Explicit instance for compatibility with their field names
instance ToJSON TaxonomicGroupListEntry where
  toJSON TaxonomicGroupListEntry{..} =
      object
        [ "groupName" .= _taxonomicGroupListEntryName
        , "groupOrder" .= _taxonomicGroupListEntryOrder
        , "taxonOrderBounds" .= _taxonomicGroupListEntryOrderBounds
        ]

-------------------------------------------------------------------------------
-- 'EBirdString' instances
-------------------------------------------------------------------------------

-- | The eBird strings of the taxonomy categories are simply the lowercase
-- constructor names.
instance EBirdString TaxonomyCategory where
  toEBirdString =
      \case
        Species -> "species"
        ISSF -> "issf"
        Spuh -> "spuh"
        Slash -> "slash"
        Hybrid -> "hybrid"
        Intergrade -> "intergrade"
        Domestic -> "domestic"
        Form -> "form"

  fromEBirdString str =
        parseOnly parseTaxonomyCategory str
      & left (("Failed to parse TaxonomyCategory: " <>) . Text.pack)

-- | The eBird string of a 'TaxonomyCategories' is the comma-separated list of
-- category strings.
instance EBirdString TaxonomyCategories where
  toEBirdString (TaxonomyCategories (c :| cs)) =
      Text.intercalate "," $ map toEBirdString (c : cs)

  fromEBirdString str =
        parseOnly parseTaxonomyCategories str
      & left (("Failed to parse TaxonomyCategories: " <>) . Text.pack)

-- | The eBird string of a 'SpeciesCode' is simply the literal string
instance EBirdString SpeciesCode where
  toEBirdString (SpeciesCode c) = c

  fromEBirdString str =
        parseOnly parseSpeciesCode str
      & left (("Failed to parse SpeciesCode: " <>) . Text.pack)

-- | The eBird string of a 'SpeciesCodes' is simply the comma-separated
-- 'SpeciesCode's
instance EBirdString SpeciesCodes where
  toEBirdString (SpeciesCodes cs) = Text.intercalate "," $ map toEBirdString cs

  fromEBirdString str =
        parseOnly parseSpeciesCodes str
      & left (("Failed to parse SpeciesCodes: " <>) . Text.pack)

-- | The eBird strings of the species locales are simply the lowercase
-- constructor names.
instance EBirdString SPPLocale where
  toEBirdString =
      \case
        Af -> "af"
        Sq -> "sq"
        Ar -> "ar"
        Hy -> "hy"
        As -> "as"
        Ast -> "ast"
        Az -> "az"
        Eu -> "eu"
        Bn -> "bn"
        Bg -> "bg"
        Ca -> "ca"
        Zh -> "zh"
        Zh_SIM -> "zh_SIM"
        Ht_HT -> "ht_HT"
        Hr -> "hr"
        Cs -> "cs"
        Da -> "da"
        Nl -> "nl"
        En -> "en"
        En_AU -> "en_AU"
        En_BD -> "en_BD"
        En_HAW -> "en_HAW"
        En_HBW -> "en_HBW"
        En_IN -> "en_IN"
        En_IOC -> "en_IOC"
        En_KE -> "en_KE"
        En_MY -> "en_MY"
        En_NZ -> "en_NZ"
        En_PH -> "en_PH"
        En_ZA -> "en_ZA"
        En_AE -> "en_AE"
        En_UK -> "en_UK"
        En_US -> "en_US"
        Fo -> "fo"
        Fi -> "fi"
        Fr -> "fr"
        Fr_AOU -> "fr_AOU"
        Fr_FR -> "fr_FR"
        Fr_CA -> "fr_CA"
        Fr_GF -> "fr_GF"
        Fr_GP -> "fr_GP"
        Fr_HT -> "fr_HT"
        Gl -> "gl"
        De -> "de"
        El -> "el"
        Gu -> "gu"
        He -> "he"
        Hi -> "hi"
        Hu -> "hu"
        Is -> "is"
        In -> "in"
        It -> "it"
        Ja -> "ja"
        Ko -> "ko"
        Lv -> "lv"
        Lt -> "lt"
        Ml -> "ml"
        Mr -> "mr"
        Mn -> "mn"
        No -> "no"
        Or -> "or"
        Fa -> "fa"
        Pl -> "pl"
        Pt_AO -> "pt_AO"
        Pt_RAA -> "pt_RAA"
        Pt_Br -> "pt_BR"
        Pt_RAM -> "pt_RAM"
        Pt_PT -> "pt_PT"
        Ro -> "ro"
        Ru -> "ru"
        Sr -> "sr"
        Sk -> "sk"
        Sl -> "sl"
        Es -> "es"
        Es_AR -> "es_AR"
        Es_CL -> "es_CL"
        Es_CR -> "es_CR"
        Es_CU -> "es_CU"
        Es_DO -> "es_DO"
        Es_EC -> "es_EC"
        Es_HN -> "es_HN"
        Es_MX -> "es_MX"
        Es_PA -> "es_PA"
        Es_PY -> "es_PY"
        Es_PE -> "es_PE"
        Es_PR -> "es_PR"
        Es_ES -> "es_ES"
        Es_UY -> "es_UY"
        Es_VE -> "es_VE"
        Sv -> "sv"
        Te -> "te"
        Th -> "th"
        Tr -> "tr"
        Uk -> "uk"

  fromEBirdString str =
        parseOnly parseSPPLocale str
      & left (("Failed to parse SPPLocale: " <>) . Text.pack)

-- | The eBird string of an 'SPPGrouping' is either "merlin" or "ebird"
instance EBirdString SPPGrouping where
  toEBirdString =
      \case
        MerlinGrouping -> "merlin"
        EBirdGrouping -> "ebird"

  fromEBirdString str =
        parseOnly parseSPPGrouping str
      & left (("Failed to parse SPPGrouping: " <>) . Text.pack)

-------------------------------------------------------------------------------
-- IsString instances
-------------------------------------------------------------------------------

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString TaxonomyCategory where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString TaxonomyCategories where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString SpeciesCode where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString SpeciesCodes where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString SPPLocale where
  fromString = unsafeFromEBirdString . Text.pack

-- | Use this instance carefully! It throws runtime exceptions if the string is
-- malformatted.
instance IsString SPPGrouping where
  fromString = unsafeFromEBirdString . Text.pack

-------------------------------------------------------------------------------
-- * attoparsec parsers
-------------------------------------------------------------------------------

-- | Parse an eBird species code, which we loosely assume is a string of one or
-- more alphanumeric characters.
parseSpeciesCode :: Parser SpeciesCode
parseSpeciesCode = SpeciesCode . Text.pack <$> many1 (satisfy isAlphaNum)

-- | Parse a comma separated list of zero or more 'SpeciesCode's
parseSpeciesCodes :: Parser SpeciesCodes
parseSpeciesCodes = SpeciesCodes <$> parseSpeciesCode `sepBy` char ','

-- | Parse an eBird 'TaxonomyCategory'.
parseTaxonomyCategory :: Parser TaxonomyCategory
parseTaxonomyCategory =
    choice
      [ "species" $> Species
      , "spuh" $> Spuh
      , "issf" $> ISSF
      , "slash" $> Slash
      , "hybrid" $> Hybrid
      , "intergrade" $> Intergrade
      , "domestic" $> Domestic
      , "form" $> Form
      ]
  where
    _casesCovered :: TaxonomyCategory -> ()
    _casesCovered =
      \case
        Species -> ()
        Spuh -> ()
        ISSF -> ()
        Slash -> ()
        Hybrid -> ()
        Intergrade -> ()
        Domestic -> ()
        Form -> ()

-- | Parse a list of eBird API taxononomy categories. To avoid the partial
-- behavior of converting a 'sepBy1' result into a 'NonEmpty', we manually parse
-- the first category followed by an optional tail.
parseTaxonomyCategories :: Parser TaxonomyCategories
parseTaxonomyCategories = do
    c <- parseTaxonomyCategory
    cs <- atEnd >>= \case
      True -> return []
      False -> do
        skip (==',')
        parseTaxonomyCategory `sepBy` char ','
    return $ TaxonomyCategories (c :| cs)

-- | Parse an eBird 'SPPLocale'.
parseSPPLocale :: Parser SPPLocale
parseSPPLocale =
    choice
      [ "af" $> Af
      , "sq" $> Sq
      , "ar" $> Ar
      , "hy" $> Hy
      , "as" $> As
      , "ast" $> Ast
      , "az" $> Az
      , "eu" $> Eu
      , "bn" $> Bn
      , "bg" $> Bg
      , "ca" $> Ca
      , "zh" $> Zh
      , "zh_SIM" $> Zh_SIM
      , "ht_HT" $> Ht_HT
      , "hr" $> Hr
      , "cs" $> Cs
      , "da" $> Da
      , "nl" $> Nl
      , "en" $> En
      , "en_AU" $> En_AU
      , "en_BD" $> En_BD
      , "en_HAW" $> En_HAW
      , "en_HBW" $> En_HBW
      , "en_IN" $> En_IN
      , "en_IOC" $> En_IOC
      , "en_KE" $> En_KE
      , "en_MY" $> En_MY
      , "en_NZ" $> En_NZ
      , "en_PH" $> En_PH
      , "en_ZA" $> En_ZA
      , "en_AE" $> En_AE
      , "en_UK" $> En_UK
      , "en_US" $> En_US
      , "fo" $> Fo
      , "fi" $> Fi
      , "fr" $> Fr
      , "fr_AOU" $> Fr_AOU
      , "fr_FR" $> Fr_FR
      , "fr_CA" $> Fr_CA
      , "fr_GF" $> Fr_GF
      , "fr_GP" $> Fr_GP
      , "fr_HT" $> Fr_HT
      , "gl" $> Gl
      , "de" $> De
      , "el" $> El
      , "gu" $> Gu
      , "he" $> He
      , "hi" $> Hi
      , "hu" $> Hu
      , "is" $> Is
      , "in" $> In
      , "it" $> It
      , "ja" $> Ja
      , "ko" $> Ko
      , "lv" $> Lv
      , "lt" $> Lt
      , "ml" $> Ml
      , "mr" $> Mr
      , "mn" $> Mn
      , "no" $> No
      , "or" $> Or
      , "fa" $> Fa
      , "pl" $> Pl
      , "pt_AO" $> Pt_AO
      , "pt_RAA" $> Pt_RAA
      , "pt_BR" $> Pt_Br
      , "pt_RAM" $> Pt_RAM
      , "pt_PT" $> Pt_PT
      , "ro" $> Ro
      , "ru" $> Ru
      , "sr" $> Sr
      , "sk" $> Sk
      , "sl" $> Sl
      , "es" $> Es
      , "es_AR" $> Es_AR
      , "es_CL" $> Es_CL
      , "es_CR" $> Es_CR
      , "es_CU" $> Es_CU
      , "es_DO" $> Es_DO
      , "es_EC" $> Es_EC
      , "es_HN" $> Es_HN
      , "es_MX" $> Es_MX
      , "es_PA" $> Es_PA
      , "es_PY" $> Es_PY
      , "es_PE" $> Es_PE
      , "es_PR" $> Es_PR
      , "es_ES" $> Es_ES
      , "es_UY" $> Es_UY
      , "es_VE" $> Es_VE
      , "sv" $> Sv
      , "te" $> Te
      , "th" $> Th
      , "tr" $> Tr
      , "uk" $> Uk
      ]
  where
    _casesCovered :: SPPLocale -> ()
    _casesCovered =
      \case
        Af -> ()
        Sq -> ()
        Ar -> ()
        Hy -> ()
        As -> ()
        Ast -> ()
        Az -> ()
        Eu -> ()
        Bn -> ()
        Bg -> ()
        Ca -> ()
        Zh -> ()
        Zh_SIM -> ()
        Ht_HT -> ()
        Hr -> ()
        Cs -> ()
        Da -> ()
        Nl -> ()
        En -> ()
        En_AU -> ()
        En_BD -> ()
        En_HAW -> ()
        En_HBW -> ()
        En_IN -> ()
        En_IOC -> ()
        En_KE -> ()
        En_MY -> ()
        En_NZ -> ()
        En_PH -> ()
        En_ZA -> ()
        En_AE -> ()
        En_UK -> ()
        En_US -> ()
        Fo -> ()
        Fi -> ()
        Fr -> ()
        Fr_AOU -> ()
        Fr_FR -> ()
        Fr_CA -> ()
        Fr_GF -> ()
        Fr_GP -> ()
        Fr_HT -> ()
        Gl -> ()
        De -> ()
        El -> ()
        Gu -> ()
        He -> ()
        Hi -> ()
        Hu -> ()
        Is -> ()
        In -> ()
        It -> ()
        Ja -> ()
        Ko -> ()
        Lv -> ()
        Lt -> ()
        Ml -> ()
        Mr -> ()
        Mn -> ()
        No -> ()
        Or -> ()
        Fa -> ()
        Pl -> ()
        Pt_AO -> ()
        Pt_RAA -> ()
        Pt_Br -> ()
        Pt_RAM -> ()
        Pt_PT -> ()
        Ro -> ()
        Ru -> ()
        Sr -> ()
        Sk -> ()
        Sl -> ()
        Es -> ()
        Es_AR -> ()
        Es_CL -> ()
        Es_CR -> ()
        Es_CU -> ()
        Es_DO -> ()
        Es_EC -> ()
        Es_HN -> ()
        Es_MX -> ()
        Es_PA -> ()
        Es_PY -> ()
        Es_PE -> ()
        Es_PR -> ()
        Es_ES -> ()
        Es_UY -> ()
        Es_VE -> ()
        Sv -> ()
        Te -> ()
        Th -> ()
        Tr -> ()
        Uk -> ()

-- | Parse an eBird 'SPPGrouping'.
parseSPPGrouping :: Parser SPPGrouping
parseSPPGrouping =
    choice
      [ "merlin" $> MerlinGrouping
      , "ebird" $> EBirdGrouping
      ]
  where
    _casesCovered :: SPPGrouping -> ()
    _casesCovered =
      \case
        MerlinGrouping -> ()
        EBirdGrouping -> ()

-------------------------------------------------------------------------------
-- 'ToHttpApiData' instances
-------------------------------------------------------------------------------

instance ToHttpApiData SpeciesCode where
  toUrlPiece = toEBirdString

instance ToHttpApiData SpeciesCodes where
  toUrlPiece = Text.intercalate "," . map toEBirdString . speciesCodes

instance ToHttpApiData TaxonomyCategories where
  toUrlPiece = toEBirdString

instance ToHttpApiData SPPLocale where
  toUrlPiece = toEBirdString

instance ToHttpApiData SPPGrouping where
  toUrlPiece = toEBirdString

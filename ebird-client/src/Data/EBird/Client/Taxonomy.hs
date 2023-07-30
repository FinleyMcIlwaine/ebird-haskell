{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Data.EBird.Client.Taxonomy
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- Types and functions for taxonomy-related eBird API queries.

module Data.EBird.Client.Taxonomy where

import Data.Default
import Data.Text
import Optics.TH
import Servant.Client

import Data.EBird.API
import Data.EBird.Client.Generated

-------------------------------------------------------------------------------
-- Taxonomy
-------------------------------------------------------------------------------

-- | Get any version of the eBird taxonomy, with optional filtering based on
-- taxonomy categories and species.
--
-- For example, get the taxa for species in the "hybrid" category:
--
-- @
-- askEBird $ taxonomy (def & #categories ?~ "hybrid")
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#952a4310-536d-4ad1-8f3e-77cfb624d1bc).
taxonomy
  :: TaxonomyParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultTaxonomyParams'/
  -> ClientM [Taxon]
taxonomy TaxonomyParams{..} =
    taxonomy_
      _taxonomyParamsCategories
      -- Hard coded to JSONFormat because it makes no difference and CSVFormat
      -- does not work like it should. See the note on the generated function's
      -- parameter documentation.
      (Just JSONFormat)
      _taxonomyParamsLocale
      _taxonomyParamsSpecies
      _taxonomyParamsVersion

-- | Optional parameters accepted by the 'TaxonomyAPI'.
--
-- Note that 'defaultTaxonomyParams' (or the 'Default' instance's 'def' value)
-- may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_taxonomyParamsSpecies' field to "bohwax":
--
-- > def & taxonomyParamsSpecies ?~ "bohwax"
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #species ?~ "bohwax"
data TaxonomyParams =
    TaxonomyParams
      { -- | Only include species of these 'TaxonomyCategory's in the taxonomy
        --
        -- /default: all categories/
        _taxonomyParamsCategories :: Maybe TaxonomyCategories

        -- | Use this locale for common names
        --
        -- /default: 'En'/
      , _taxonomyParamsLocale :: Maybe SPPLocale

        -- | Only fetch records for these species
        --
        -- /default: all/
      , _taxonomyParamsSpecies :: Maybe SpeciesCodes

        -- | Fetch this version of the eBird taxonomy
        --
        -- /default: latest/
      , _taxonomyParamsVersion :: Maybe Text
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultTaxonomyParams :: TaxonomyParams
defaultTaxonomyParams =
    TaxonomyParams
      { _taxonomyParamsCategories = Nothing
      , _taxonomyParamsLocale = Nothing
      , _taxonomyParamsSpecies = Nothing
      , _taxonomyParamsVersion = Nothing
      }

instance Default TaxonomyParams where
  def = defaultTaxonomyParams

-- ** Optics for 'TaxonomyParams'
makeLenses ''TaxonomyParams
makeFieldLabels ''TaxonomyParams

-------------------------------------------------------------------------------
-- Taxonomic forms
-------------------------------------------------------------------------------

-- | Get the list of subspecies of a given species recognized in the eBird
-- taxonomy.
--
-- For example, get subspecies of Canada Goose (using
-- @-XOverloadedStrings@):
--
-- @
-- askEBird $ taxonomicForms key "cangoo"
-- @
--
-- Note that the endpoint for this query is simple enough that 'taxonomicForms'
-- is equivalent to the generated 'taxonomicForms_'.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#e338e5a6-919d-4603-a7db-6c690fa62371).
taxonomicForms
  :: Text
  -- ^ eBird API key
  -> SpeciesCode
  -- ^ The species to get subspecies of
  -> ClientM SpeciesCodes
taxonomicForms = taxonomicForms_

-------------------------------------------------------------------------------
-- Taxa locale codes
-------------------------------------------------------------------------------

-- | Get the supported locale codes and names for species common names, with the
-- last time they were updated.
--
-- For example:
--
-- @
-- askEBird $ taxaLocaleCodes key def
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#3ea8ff71-c254-4811-9e80-b445a39302a6).
taxaLocaleCodes
  :: Text
  -- ^ eBird API key
  -> TaxaLocaleCodesParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultTaxaLocaleCodesParams'/
  -> ClientM [SPPLocaleListEntry]
taxaLocaleCodes k TaxaLocaleCodesParams{..} =
    taxaLocaleCodes_ k _taxaLocaleCodesParamsLocale

-- | Optional parameters accepted by the 'TaxaLocaleCodesAPI'.
--
-- Note that 'defaultTaxaLocaleCodesParams' (or the 'Default' instance's 'def'
-- value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_taxaLocaleCodesParamsLocale' field to 'Es':
--
-- > def & taxaLocaleCodesParamsLocale ?~ Es
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #locale ?~ Es
newtype TaxaLocaleCodesParams =
    TaxaLocaleCodesParams
      { -- | Value for the "Accept-Language" header, for translated language
        -- names, when available
        --
        -- /default: 'En'/
        _taxaLocaleCodesParamsLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultTaxaLocaleCodesParams :: TaxaLocaleCodesParams
defaultTaxaLocaleCodesParams =
    TaxaLocaleCodesParams
      { _taxaLocaleCodesParamsLocale = Nothing
      }

instance Default TaxaLocaleCodesParams where
  def = defaultTaxaLocaleCodesParams

-- ** Optics for 'TaxaLocaleCodesParams'
makeLenses ''TaxaLocaleCodesParams
makeFieldLabels ''TaxaLocaleCodesParams

-------------------------------------------------------------------------------
-- * Taxonomy versions
-------------------------------------------------------------------------------

-- | Get all versions of the taxonomy, with a flag indicating which is latest.
--
-- For example:
--
-- @
-- askEBird taxonomyVersions
-- @
--
-- Note that the endpoint for this query is simple enough that 'taxonomyVersions'
-- is equivalent to the generated 'taxonomyVersions_'.
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#9bba1ff5-6eb2-4f9a-91fd-e5ed34e51500).
taxonomyVersions :: ClientM [TaxonomyVersionListEntry]
taxonomyVersions = taxonomyVersions_

-------------------------------------------------------------------------------
-- * Taxonomic groups
-------------------------------------------------------------------------------

-- | Get the list of species groups, in either Merlin or eBird grouping.
--
-- For example, get the taxonomic groups using eBird grouping order (using
-- @-XOverloadedStrings@):
--
-- @
-- askEBird $ taxonomicGroups "ebird" def
-- @
--
-- See the [eBird API documentation for the corresponding
-- endpoint](https://documenter.getpostman.com/view/664302/S1ENwy59#aa9804aa-dbf9-4a53-bbf4-48e214e4677a).
taxonomicGroups
  :: SPPGrouping
  -- ^ 'MerlinGrouping' groups like birds together, with falcons next to hawks,
  -- while 'EBirdGrouping' groups in taxonomy order
  -> TaxonomicGroupsParams
  -- ^ Optional parameters
  --
  -- /default: 'defaultTaxonomicGroupsParams'/
  -> ClientM [TaxonomicGroupListEntry]
taxonomicGroups r TaxonomicGroupsParams{..} =
    taxonomicGroups_ r _taxonomicGroupsParamsLocale

-- | Optional parameters accepted by the 'TaxonomicGroupsAPI'.
--
-- Note that 'defaultTaxonomicGroupsParams' (or the 'Default' instance's 'def'
-- value) may be used to accept the defaults of the eBird API.
--
-- Additionally, note that there are optics available for manipulating this
-- type. For example, if you would like to just set the
-- '_taxonomicGroupsParamsLocale' field to 'Es':
--
-- > def & taxonomicGroupsParamsLocale ?~ Es
--
-- Or, using @-XOverloadedLabels@:
--
-- > def & #locale ?~ Es
newtype TaxonomicGroupsParams =
    TaxonomicGroupsParams
      { -- | Locale to use for species group names. 'En' is used for any locale
        -- whose translations are unavailable at this endpoint
        --
        -- /default: 'En'/
        _taxonomicGroupsParamsLocale :: Maybe SPPLocale
      }
  deriving (Show, Read, Eq)

-- | Note that this value does not actually use the eBird API default values.
-- It simply sets every option to 'Nothing', which means we just don't send any
-- of these parameters to the eBird API and they will use /their own/ defaults.
defaultTaxonomicGroupsParams :: TaxonomicGroupsParams
defaultTaxonomicGroupsParams =
    TaxonomicGroupsParams
      { _taxonomicGroupsParamsLocale = Nothing
      }

instance Default TaxonomicGroupsParams where
  def = defaultTaxonomicGroupsParams

-- ** Optics for 'TaxonomicGroupsParams'
makeLenses ''TaxonomicGroupsParams
makeFieldLabels ''TaxonomicGroupsParams

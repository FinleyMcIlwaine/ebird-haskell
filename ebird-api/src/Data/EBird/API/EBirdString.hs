-- |
-- Module      : Data.EBird.API.EBirdString
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- The 'EBirdString' class contains types whose values may be represented as
-- strings compatible with the eBird API.

module Data.EBird.API.EBirdString
  ( -- * The class
    EBirdString(..)

    -- * Unsafe interface
  , unsafeFromEBirdString
  ) where

import Data.Text (Text)
import GHC.Stack

-- | A convenience class for converting the litany of eBird API types to and
-- from their respective eBird API compatible string representations.
class EBirdString a where
  -- | Convert a value to an eBird string.
  toEBirdString :: a -> Text

  -- | Parse a string into an eBird value. If parsing fails, this should result
  -- in 'Left' with an error message.
  fromEBirdString :: Text -> Either Text a

-- | Parse a string into an eBird value unsafely.
--
-- __Be careful!__ This can result in runtime errors if the string is
-- malformatted.
unsafeFromEBirdString :: (HasCallStack, EBirdString a) => Text -> a
unsafeFromEBirdString str = case fromEBirdString str of
    Left _ -> error "Failed to parse eBird string"
    Right x -> x

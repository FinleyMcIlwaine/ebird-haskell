module Main where

-- |
-- Module      : Main
-- Copyright   : (c) 2023 Finley McIlwaine
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Finley McIlwaine <finleymcilwaine@gmail.com>
--
-- An executable command-line utility for interacting with the
-- [eBird API](https://documenter.getpostman.com/view/664302/S1ENwy59).

import Data.EBird.CLI (eBirdCli)

main :: IO ()
main = eBirdCli

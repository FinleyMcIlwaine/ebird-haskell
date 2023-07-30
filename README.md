# eBird Haskell

![CI Status Badge](https://github.com/FinleyMcIlwaine/ebird-haskell/actions/workflows/haskell-ci.yml/badge.svg)

eBird libraries and tools written in Haskell, for Haskell.

## What is eBird?

[eBird](https://ebird.org/home) is a massive collection of ornithological
science projects developed by the [Cornell Lab of
Ornithology](https://www.birds.cornell.edu/home/). The [eBird
API](https://documenter.getpostman.com/view/664302/S1ENwy59) offers programmatic
access to the incredible dataset backing these projects.

## What is included?

This repository hosts several libraries and tools, all centered around accessing
and processing eBird data from the public eBird web API.

### [`./ebird-cli`](./ebird-cli/)

A command-line interface for querying the official eBird API.

### [`./ebird-client`](./ebird-client/)

A Haskell library for querying the official eBird API.

### [`./ebird-api`](./ebird-api/)

A Haskell library that defines the eBird API as a [servant][servant] API type.
This library is intended for use by those who wish to write their own eBird API
clients using [servant-client][servant-client], or who wish to do custom
processing of eBird data using the types defined in the library.

## Contribute

Please don't hesitate to [open an
issue](https://github.com/FinleyMcIlwaine/ebird-haskell/issues) (or a [pull
request](https://github.com/FinleyMcIlwaine/ebird-haskell/pulls)!) if you have
any questions or something doesn't work as expected.

<!-- LINKS -->
[servant]: https://docs.servant.dev/en/stable/
[servant-client]: https://hackage.haskell.org/package/servant-client

# ebird-api

A Haskell description of the [eBird API][api-docs].

## Installation

In your cabal file:
```cabal
  build-depends:
      ebird-api
```

## Usage

> Note: If you are interested in *querying* the eBird API, use
> [ebird-client](../ebird-client/) instead!

This library is intended for those who want to write their own clients for the
[eBird API][api-docs], or do some custom processing of eBird data using the
types defined here.

Definitions for all major types of values that the [eBird API][api-docs]
communicates in are provided, including
[observations](./src/EBird/API/Observations.hs) and
[checklists](./src/EBird/API/Checklists.hs). [servant] API types for all
endpoints of the [eBird API][api-docs] are also provided.

Please don't hesitate to

For more documentation, see the library's [Hackage documentation][ebird-api].


<!-- LINKS -->
[api-docs]: https://documenter.getpostman.com/view/664302/S1ENwy59
[ebird-api]: https://hackage.haskell.org/package/ebird-api
[servant]: https://hackage.haskell.org/package/servant

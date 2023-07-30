# ebird-client

Query the official eBird API from Haskell.

## Installation

In your cabal file:
```cabal
  build-depends:
      ebird-client
```

## Usage

Every eBird API endpoint (as listed in [their documentation][api-docs]) is
supported. Use `askEBird` to send requests to the official eBird API. Many
requests require an API key, which can be obtained
[here](https://ebird.org/api/keygen).

For example, to get recent observations of Peregrine Falcons in Park County,
Wyoming (using `-XOverloadedStrings`):

```haskell
askEBird $ recentSpeciesObservations apiKey "US-WY-029" "perfal" def
```

For more examples and documentation, see the library's [Hackage
documentation][ebird-client].


<!-- LINKS -->
[api-docs]: https://documenter.getpostman.com/view/664302/S1ENwy59
[ebird-client]: https://hackage.haskell.org/package/ebird-client

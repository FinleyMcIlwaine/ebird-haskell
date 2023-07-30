# ebird-cli

*Go birding on your command line!*

## Installation

Using cabal:

```
cabal install ebird-cli
```

This will install the `ebird` executable. To see the list of commands, try:

```
ebird --help
```

## Usage

There is one command per endpoint of the official eBird API, as listed in [their
documentation][api-docs]. Some commands require an API key, which can be
obtained [here](https://ebird.org/api/keygen). If a command requires an API key,
the key can be provided via the `--key` or `-k` option. If no key is provided
via the options, the CLI will attempt to read a key from a file located at
`$HOME/.ebird/key.txt`. If no key is available there (and none was provided via
the options), the command will fail.

Please don't hesitate to [open an
issue](https://github.com/FinleyMcIlwaine/ebird-haskell/issues) (or a [pull request](https://github.com/FinleyMcIlwaine/ebird-haskell/pulls)!) if something
doesn't work as expected.

<!-- LINKS -->
[api-docs]: https://documenter.getpostman.com/view/664302/S1ENwy59

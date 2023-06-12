# tornadano-deposit

## Development shell

You can use cabal, haskell-language-server and other dev tools inside.

```sh
nix develop
```

## Usage

Example in nix dev shell:

```sh
cabal run dump-policy -- --help
cabal run dump-policy -- -h bbd65a4af3dd5bb07b11cfb66418cdffc6bd26817559e0c5a80f405d -x 1
```

```sh
cabal run dump-script -- --help
cabal run dump-script -- -s bbd65a4af3dd5bb07b11cfb66418cdffc6bd26817559e0c5a80f405d -h 7
```

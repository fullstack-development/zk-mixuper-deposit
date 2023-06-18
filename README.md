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
cabal run dump-policy -- -h 93c7527ee2d0b86de5b73baa04728bc8cdf76cf055e4bf30f495d101fa37ff8d -x 1
```

```sh
cabal run dump-script -- --help
cabal run dump-script -- -s f55e65860d9b3e311588219fd0facf0d09bab8393bfceaa027cebacb -h 7
```

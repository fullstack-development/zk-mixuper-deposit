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
cabal run dump-policy -- -h 2a1e9666d57a0b524b11fb67eeca2dcb80ea289a6d3f337fe1b624f0381e289e -x 1
```

```sh
cabal run dump-script -- --help
cabal run dump-script -- -s 9034c0e636ba7e1da6afed7eca347d7949982f45739a827780d9cfd6 -h 7
```

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
cabal run dump-policy -- -h 8f1efed2e61abd2ece188f7f98fcf0a89ccb5df1bcc7d1f6acb0f01bd2676070 -x 1
```

```sh
cabal run dump-script -- --help
cabal run dump-script -- -s cd4ecd8b80466c7325e9d2f76fce6eb8a236667734eb1646bcfdcb51 -h 7
```

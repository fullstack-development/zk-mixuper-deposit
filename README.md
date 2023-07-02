# zk-mixuper-deposit

This project is inspired/based on previous open source work: [tornado](https://github.com/tornadocash/tornado-core), [hydra-merkle-tree](https://github.com/input-output-hk/hydra/tree/master/plutus-merkle-tree).

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

## On-chain limitations

1. Due to transaction size limits max height of merkle tree would be 8.

2. Due to memory execution units limit max height of merkle tree is 4.

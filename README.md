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
cabal run dump-policy -- -h 2dba01b0cb2aba71b026d8aeac057ce9249f82dacb78a216b34fee4d52bfb909 -x 2
```

```sh
cabal run dump-script -- --help
cabal run dump-script -- -s 75a07ecddfcd14b0b5ac5b3ca3d03ee8337145166bc522a5ec1529c0 -h 7
```

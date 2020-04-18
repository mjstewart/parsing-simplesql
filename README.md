# parsing

## project setup tips

without having cabal-install globally

```
mkdir simple-parser
cd simple-parser
nix-shell --pure -p ghc cabal-install --run "cabal init"
```

note: cabal-install gives you `cabal` cli tool so if you have this installed globally via nix-env you can do

```
mkdir simple-parser
cd simple-parser
cabal init
```

# haskell-music-engine

A a very simple command-line user interface built on top of the [Vivid](https://hackage.haskell.org/package/vivid) package, a Haskell [SuperCollider](https://supercollider.github.io/) client. This project is used as a short demonstration of how a "real-life" Haskell project interacts with the outside world.

## Prequisites

If you want to run the SuperCollider server yourself, you need to install it and find the `scsynth` binary.

For example, with a default Mac installation, to start the server run:
```
/Applications/SuperCollider.app/Contents/Resources/scsynth -u 57110
```

The Haskell project requires [Nix](https://nixos.org/download/).

## Building

```
nix develop
cabal build haskell-music-engine
```

## Quickstarting the app

The SuperCollider server needs to be running at the default address and port.

```
cabal exec haskell-music-engine
```

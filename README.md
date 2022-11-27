# haskell-music-engine

A command-line user interface built on top of the [Vivid](https://hackage.haskell.org/package/vivid) package, a Haskell [SuperCollider](https://supercollider.github.io/) client.

## Prequisites

If you want to run the SuperCollider server yourself, you need to install it and find the `scsynth` binary.

For example, to start the server run:
```
$ /Applications/SuperCollider.app/Contents/Resources/scsynth -u 57110
```
## Building

```
$ stack build
```

## Quickstarting the app

The SuperCollider server needs to be running at the default address and port.

```
$ stack exec -- haskell-music-engine
```

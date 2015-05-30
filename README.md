# Epidemic -- a game about exponential growth written in Haskell

## Want to help beta test the game?

This particular repo contains the last version of the game that I'll be releasing to the public
(at least for now). I've got big plans for the final game, including significant changes to the
game play. If you'd like to stay in the loop then please [subscribe](http://eepurl.com/boW1vz).

## Installation

Epidemic can be built for two targets:
  * Android
  * iOS
  * Mac OS X

However, you're probably here to build it for Android. In that case see the next section.

### Building for Android

A complete development environment for Android has been developed with the aid of
[Docker](https://www.docker.com/). The complete instructions are available in the `README.md` files
of the following two repos:

* [`docker-epidemic-build-env`](https://github.com/sseefried/docker-epidemic-build-env)
* [`android-build-epidemic-apk`](https://github.com/sseefried/android-build-epidemic-apk)

### Building for iOS

*Coming soon*

### Building for Mac OS X

We are using Stackage to build all the requisite libraries. Specifically, we are using the
snapshot [2014-12-04-ghc78hp-inc](http://www.stackage.org/snapshot/2014-12-04-ghc78hp-inc).

Please ensure you have followed the instruction for setting it up, and then run:

    ./setup.sh
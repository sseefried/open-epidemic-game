# Epidemic -- a game about exponential growth written in Haskell

## Introduction

For the past few months I've been working on a simple mobile game written in Haskell. This repo
contains everything I've done up to this point. It is my gift to the Haskell community and I hope
it inspires some of you to write more games in Haskell for mobile devices.

I've now forked to a private repo and hope to continue development. I've got big plans for the final
game, including significant changes to the game play. Hint: it will no longer just be a one-tap
wonder, and will involve more strategic elements.

## Want to help beta test the game?

If you'd like to stay in the loop then please [subscribe](http://eepurl.com/boW1vz).

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
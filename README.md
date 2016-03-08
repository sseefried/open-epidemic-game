# Epidemic -- a game about exponential growth written in Haskell

## Introduction

In 2014 I worked on a simple mobile game written in Haskell. This repo contains everything that came
out of that effort. It is my gift to the Haskell community and I hope it inspires some of you to
write more games in Haskell for mobile devices.

## Credits

While the programming was done solely by me I had some help. First, the music was done by a talented
composer/musician called Anton Kholomiov using his
Haskell library [`csound-expression`](https://github.com/anton-k/csound-expression),
a fantastic combinator library that uses [Csound](http://en.wikipedia.org/wiki/Csound) as a backend.

The artistic style of the game was inspired by the work of [Rauri Rochford](http://esquemedia.com/).
All failings are mine alone. Had he been further involved it would doubtless have been stunning.


## Installation

Epidemic was designed to be be built for several targets:
  * Android
  * iOS
  * Mac OS X

However, you're probably here to build it for Android. In that case see the next section.

Building on Mac OS X is fairly straightforward but building for iOS was always a long and
detailed process that I did not get around to automating in the same way that I automated the
Android build process.

### Building for Android

A complete development environment for Android has been developed with the aid of
[Docker](https://www.docker.com/). The complete instructions are available in the `README.md` files
of the following two repos:

* [`docker-epidemic-build-env`](https://github.com/sseefried/docker-epidemic-build-env)
* [`android-build-epidemic-apk`](https://github.com/sseefried/android-build-epidemic-apk)

### Building for iOS

I may add instructions for doing this one day, but at the moment it seems unlikely.

### Building for Mac OS X

We are using Stackage to build all the requisite libraries. Specifically, we are using the
snapshot [2014-12-04-ghc78hp-inc](http://www.stackage.org/snapshot/2014-12-04-ghc78hp-inc).

Please ensure you have followed the instruction for setting it up, and then run:

    ./setup.sh
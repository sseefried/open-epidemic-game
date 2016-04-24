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
  * Linux

However, you're probably here to build it for Android. In that case see the next section.

Building on Mac OS X is fairly straightforward but building for iOS was always a long and
detailed process that I did not get around to automating in the same way that I automated the
Android build process.

### Building for Android

A complete development environment for Android has been developed with the aid of
[Docker](https://www.docker.com/). The complete instructions are available in the `README.md` files
of the following two repos:

* [`docker-game-build-env`](https://github.com/sseefried/docker-game-build-env)
* [`android-build-game-apk`](https://github.com/sseefried/android-build-game-apk)

### Building for iOS

I may add instructions for doing this one day, but at the moment it seems unlikely.

### Building for Mac OS X and Linux

We are using [`stack`](http://docs.haskellstack.org/en/stable/README/) to build all the
requisite libraries.

It should be as simple as

    $ stack build
    $ stack exec Epidemic

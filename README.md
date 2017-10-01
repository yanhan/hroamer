# hroamer

A plain text file manager using your text editor for manipulations.

Writing this so I can work on something more real world using Haskell.

Inspired by roamer: https://github.com/abaldwin88/roamer


## Warnings about the code

Because this is the first non-trivial Haskell project I've worked on

- The code does not look good at all
- There are no tests. These will come in future patches
- Usage of cabal file may be wrong in that I didn't specify library versions
- Possibly many non idiomatic usages of Haskell. Most of the code is essentially written in an imperative style. I have tried to make the code as Haskell as possible, to the best of my ability
- Code organization is far from ideal. Currently all the code that does the hard work is in one file


## Building

This project makes use of [Stack](https://docs.haskellstack.org/en/stable/README/) for building.

To build:

    stack build

To run:

    stack exec hroamer

I am not too familiar with Stack so I cannot give the correct instructions for installing this to a location on your PATH. However, you can find the binary in `.stack-work/install/<ARCH>-<OS>/lts-<LTS-VERSION>/<GHC-VERSION>/bin/hroamer` after building it.


## License

[MIT License](/LICENSE), Copyright (c) 2017 Pang Yan Han

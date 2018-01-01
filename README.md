# hroamer

A file manager that lets you use your text editor for manipulations.

Writing this so I can work on something more real world using Haskell.

Inspired by roamer: https://github.com/abaldwin88/roamer


## Warnings about the code

Because this is the first non-trivial Haskell project I've worked on

- Some of the code looks awful and there are probably a good number of non idiomatic usages of Haskell. I have tried to make the code as Haskell as possible, to the best of my ability
- Usage of cabal file may be wrong in that I didn't specify library versions


## Building

This project makes use of [Stack](https://docs.haskellstack.org/en/stable/README/) for building.

To build:

    stack build


## Installing

    stack install hroamer

This will install the `hroamer` binary to `~/.local/bin/hroamer`


## Tests

To run the tests:

    stack test


## TODO


## License

[MIT License](/LICENSE), Copyright (c) 2017 Pang Yan Han

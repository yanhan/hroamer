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

```
stack build
```


## Installing

```
stack install hroamer
```

This will install the `hroamer` binary to `~/.local/bin/hroamer`


## Quickstart

Suppose you want to shift some files from `/home/linda/workspace/poseidon` to `/home/linda/workspace/zeus`.

In 2 different terminals, use 1 terminal to cd to the first directory and use the other terminal to cd to the second directory. Then launch `~/.local/bin/hroamer` on both terminals.

Suppose you see the following contents in your text editor for `/home/linda/workspace/poseidon`:

```
" /home/linda/workspace/poseidon
.git/ | 59472a7b-4b52-456b-bcb7-4c36f048a6bc  | /home/linda/workspace/poseidon/.git
.gitignore | 68900438-f70e-4004-ad9f-6d32cc386178  | /home/linda/workspace/poseidon/.gitignore
README.md | 76a1aa25-7c70-4774-84c3-36ae495d42a0  | /home/linda/workspace/poseidon/README.md
hash.py | af48298a-5d8c-43f1-97a6-595e08e0783f  | /home/linda/workspace/poseidon/hash.py
main.c | ee8a97dd-e269-4f6e-9565-c1f3fd0457b1  | /home/linda/workspace/poseidon/main.c
scripts/ | e4f0c1ab-2cb2-467b-b4f8-915fb82e5053  | /home/linda/workspace/poseidon/scripts/
```

Suppose you want to **copy** the `hash.py` file and the `scripts` directory to `/home/linda/workspace/zeus`. Simply copy and paste the 2 lines beginning with `hash.py` and `scripts/` to the text editor launched by hroamer in the `/home/linda/workspace/zeus` directory, then **save** and exit both text editors (in any order).


## Tests

To run the tests:

```
stack test
```


## TODO


## License

[MIT License](/LICENSE), Copyright (c) 2017 Pang Yan Han

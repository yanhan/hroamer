# hroamer

hroamer lets you use a text editor specified by the `EDITOR` environment variable (defaults to `vim` if not set) to copy / move files from one directory to another.

My original intent of writing hroamer is for me to work on something more real world using Haskell.

Inspired by roamer: https://github.com/abaldwin88/roamer


## Installing

This project makes use of [Stack](https://docs.haskellstack.org/en/stable/README/).

To build and install:

```
stack build
stack install hroamer
```

This will build and install the `hroamer` binary to `~/.local/bin/hroamer`. For convenience, you may want to append the `${HOME}/.local/bin` directory to your `PATH` environment variable.


## Quickstart

Suppose you want to shift some files from `/home/linda/workspace/poseidon` to `/home/linda/workspace/zeus`.

In 2 different terminals, use 1 terminal to cd to the first directory and use the other terminal to cd to the second directory. Then launch `~/.local/bin/hroamer` on both terminals.

Suppose you see the following contents in your text editor for `/home/linda/workspace/poseidon`:

```
" /home/linda/workspace/poseidon
.git/ | 59472a7b-4b52-456b-bcb7-4c36f048a6bc | /home/linda/workspace/poseidon/.git
.gitignore | 68900438-f70e-4004-ad9f-6d32cc386178 | /home/linda/workspace/poseidon/.gitignore
README.md | 76a1aa25-7c70-4774-84c3-36ae495d42a0 | /home/linda/workspace/poseidon/README.md
hash.py | af48298a-5d8c-43f1-97a6-595e08e0783f | /home/linda/workspace/poseidon/hash.py
main.c | ee8a97dd-e269-4f6e-9565-c1f3fd0457b1 | /home/linda/workspace/poseidon/main.c
scripts/ | e4f0c1ab-2cb2-467b-b4f8-915fb82e5053 | /home/linda/workspace/poseidon/scripts/
```

This is a listing of the files in `/home/linda/workspace/poseidon` that can be moved / copied by hroamer. You will see something similar in the text editor for the other directory `/home/linda/workspace/zeus`.

Suppose you want to **copy** the `hash.py` file and the `scripts` directory to `/home/linda/workspace/zeus`. Simply copy and paste the line beginning with `hash.py` and the line beginning with `scripts/` to the text editor launched by hroamer in the `/home/linda/workspace/zeus` directory. Then **save** and exit both text editors (in any order).

In the terminal for `/home/linda/workspace/zeus`, you should see the following output:

```
hroamer: sigaction
cp /home/linda/workspace/poseidon/hash.py /home/linda/workspace/zeus/hash.py
cp -R /home/linda/workspace/poseidon/scripts /home/linda/workspace/zeus/scripts
```

In `/home/linda/workspace/zeus`, you should see a copy of the `hash.py` file and the `scripts` directory from `/home/linda/workspace/poseidon`.

Ignore the `hroamer: sigaction` line; it is output by the [unix library](https://hackage.haskell.org/package/unix-2.7.2.2) because hroamer uses some signal handlers to perform cleanup of temporary files. So far, I have been unable to prevent the library from outputting this.


## Usage details

### Anatomy of a line

All lines beginning with a `"` are treated as comments. Hence, the first line we see in the text editor of the [Quickstart section](#quickstart):

```
" /home/linda/workspace/poseidon
```

will be ignored by hroamer and is only there to show the user what the cwd is.

This line:

```
main.c | ee8a97dd-e269-4f6e-9565-c1f3fd0457b1 | /home/linda/workspace/poseidon/main.c
```

has 3 parts, each separated by a ` | `.

The first part is `main.c`. This is the filename. Directory names will end with a slash to make it easy to distinguish between files and directories. This is a mandatory component of the line.

The second part is `ee8a97dd-e269-4f6e-9565-c1f3fd0457b1`. This is the randomly generated UUID of the file / directory and is unique. This is mandatory component of the line.

The third part is `/home/linda/workspace/poseidon/main.c`. This is the original path to the file / directory. This is not a mandatory component of the line.

hroamer uses the UUID to identify the source file / directory that needs to be copied to the destination. This UUID is not dependent on the contents of the file / directory and is globally unique. The name before the UUID allows the user to specify the destination filename. If unchanged, the destination file / directory will have the same name as the source.

We mentioned that the part after the second ` | ` is not important - that is true and it is something I added for improved clarity on where a file was originally from, because UUIDs are not exactly human-friendly. Therefore, to hroamer, the following 2 lines are equivalent:

```
main.c | ee8a97dd-e269-4f6e-9565-c1f3fd0457b1
main.c | ee8a97dd-e269-4f6e-9565-c1f3fd0457b1 | /home/linda/workspace/poseidon/main.c
```

### Moving files / directories

Instead of copy and paste, use cut and paste.

### Use different name for destination file / directory

Just modify the name before the first ` | ` to what you want. Using the example in our [Quickstart section](#quickstart), if we want to copy the `hash.py` file but name it as `run.py`, we will modify the line that was copy-and-pasted to:

```
run.py | af48298a-5d8c-43f1-97a6-595e08e0783f | /home/linda/workspace/poseidon/hash.py
```

### Delete files / directories

Just delete the line from the text editor.

However, the file / directory will not truly be deleted. A new directory (whose name is the uuid after the first ` | `) will be created in `~/.local/share/hroamer/trash-copy` and the file / directory will be shifted there.

### Relative paths and absolute paths

If you are too lazy to open a second `hroamer` instance, you can do the same work in just 1 window by using relative paths and absolute paths. Using the same example as in our [Quickstart section](#quickstart), you will launch `hroamer` in `/home/linda/workspace/poseidon` and edit the contents to a state similar to the following (take note of the final 2 lines):

```
" /home/linda/workspace/poseidon
.git/ | 59472a7b-4b52-456b-bcb7-4c36f048a6bc | /home/linda/workspace/poseidon/.git
.gitignore | 68900438-f70e-4004-ad9f-6d32cc386178 | /home/linda/workspace/poseidon/.gitignore
README.md | 76a1aa25-7c70-4774-84c3-36ae495d42a0 | /home/linda/workspace/poseidon/README.md
hash.py | af48298a-5d8c-43f1-97a6-595e08e0783f | /home/linda/workspace/poseidon/hash.py
main.c | ee8a97dd-e269-4f6e-9565-c1f3fd0457b1 | /home/linda/workspace/poseidon/main.c
scripts/ | e4f0c1ab-2cb2-467b-b4f8-915fb82e5053 | /home/linda/workspace/poseidon/scripts/
../zeus/hash.py | af48298a-5d8c-43f1-97a6-595e08e0783f | /home/linda/workspace/poseidon/hash.py
/home/linda/workspace/zeus/scripts | e4f0c1ab-2cb2-467b-b4f8-915fb82e5053 | /home/linda/workspace/poseidon/scripts/
```

Then save and quit the text editor. This uses:

- the relative path `../zeus/hash.py` to copy `hash.py` to `/home/linda/workspace/zeus/hash.py`
- the absolute path `/home/linda/workspace/zeus/scripts` to copy the `scripts` directory there

For moving files / directory, simply edit the original line in place.

Note that the order of the lines **do not matter**. You can also specify as many different relative / absolute paths as you like.


## Paths that are not supported by hroamer

- Any file or directory with whitespace characters anywhere in its absolute path. Whitespace characters include but are not limited to the space character, the tab character, the newline character
- Any file or directory beginning with a `"` character

Other violations:

- Using hroamer to manage any directory under `$XDG_DATA_HOME/hroamer` (defaults to `~/.local/share/hroamer`). This is forbidden because hroamer uses that directory to store some state
- Duplicate filenames in the same text editor
- Trying to override the directory or any of its ancestors for the currently active hroamer session. But this does not prevent you from opening another hroamer session to do so


## Tests

To run the tests:

```
stack test
```


## Warnings about the code

Because this is the first non-trivial Haskell project I've worked on

- Some of the code looks awful and there are probably a good number of non idiomatic usages of Haskell. I have tried to make the code as Haskell as possible, to the best of my ability
- Usage of cabal file may be wrong in that I didn't specify library versions


## TODO


## License

[MIT License](/LICENSE), Copyright (c) 2017-2018 Pang Yan Han

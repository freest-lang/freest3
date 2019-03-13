# Context-Free Session Types - A compiler

## Requirements

This section contains the requirements for running the compiler.
The following tools are required:

  * The Haskell platform (See [`Haskell Website`](https://www.haskell.org/platform/))
  * Cabal

    ```
    $ cabal update
    $ cabal install --enable-tests --dependencies-only
    $ cabal build
    ```

## Compiling

Run ``` $ cabal build ```.

To run the compiler just create a file (with ".fst" extension) and then run ``` cabal run [filepath] ```

For example, to compile the file named "test.fst" located in the directory "dir" you just have to run
```
$ cabal run dir/test.hs
```

### Samples
There are some code examples that are available on the directory "test/Programs/ValidTests/"

## Run tests

See [`test/README.md`](test/README.md).

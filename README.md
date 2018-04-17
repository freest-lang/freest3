# Context-Free Session Types - A compiler

## Requirements

This section contains the requirements for running the compiler.
It's necessary to have the following tools installed:

  * The Haskell platform (See [`Haskell Website`](https://www.haskell.org/platform/))
  * Build cabal

    ```
    $ cabal install --dependencies-only
    $ cabal build
    ```

## Compiling

Run ``` $ cabal build ```.

To run the compiler just create a file (with ".hs" extension) and then run ``` cabal run [filepath] ```

For example, to compile the file named "test.hs" located in the directory "dir" you just have to run
```
$ cabal run dir/test.hs
```

### Samples
There are some code examples that are available on the directory "test/Programs/ValidTests/"

## Run tests

See [`test/README.md`](test/README.md).
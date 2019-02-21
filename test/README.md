## Context Free Session Types - Test framework

The test infrastructure contains a collection of tools for testing the Context Free Session Types compiler and displaying the results.

## Requirements

This section contains the requirements for the test framework.
Requirements:

  * The Haskell platform (See [`Haskell Website`](https://www.haskell.org/platform/))
  * See [`README.md`](../README.md) for installation details

## Automated testing - Run Tests

To be able to run the tests you need to be in the root folder of the project (ContextFreeSession directory) and run

```
$ cabal update
$ cabal install --dependencies-only
$ cabal build
```

After building the compiler you can choose one of the following options:

  * To run all the tests:
    ```
    $ cabal test
    ```
  * To run only the Unit tests:
    ```
    $ cabal test unit-tests
    ```
  * To run only the Programs tests:
    ```
    $ cabal test program-tests
    ```

There are also available options to run valid and invalid tests separately for all the units:

By running one of the following:

  * Parser valid tests: ```$ make testParserValid ```
  * Parser invalid tests: ```$ make testParserInvalid ```
  * Types valid tests:  ```$ make testTypesValid ```
  * Types invalid tests: ```$ make testTypesInvalid ```
  * Kinding valid tests: ```$ make testKindingValid ```
  * Type Equivalence valid tests: ```$ make testEquivalenceValid ```
  * Type Equivalence invalid tests: ```$ make testEquivalenceInvalid ```
  * Test Show (only valid): ```$ make testShow ```

Or, by running:
  ```
  $ cabal build && cabal exec -- runhaskell -isrc -itest/UnitTests/ test/UnitTests/Types/[TESTNAME]
  ```

It's absolutely necessary to have installed the ghc, parsec, hspec and hspec-discover packages.

### Viewing test results

The test results will be displayed on the console.

### Viewing test coverage

To view the test coverage run ``` make coverage ```. The output files will be stored at [test/outputs](test/outputs) folder. (The main file is hpc_index.html that summarizes the information available and contains links for more specific information)

Note: It's mandatory to have the hpc package installed.

## Automated testing - Add tests

To add test to the infrastructure just edit the correspondent txt of the desired file. (i.e. to add valid kinding tests just edit the TestValidKinding.txt)

The txt file is structured as follows :

    Testing language phrase
        Expected


## Profiling

To profile a test suite we need to add "ghc-options" to the ContextFreeSession.cabal file.

For example:

```
test-suite unit-tests
  main-is:             UnitSpec.hs
  ghc-options:
             -fprof-auto-top
             "-with-rtsopts=-N -p -s -h -i0.1"
  Type:                exitcode-stdio-1.0
  ...
```

The compiler options for profiling can be found in this page: https://downloads.haskell.org/~ghc/7.6.2/docs/html/users_guide/prof-compiler-options.html with useful explanations.

With this setup, run:

```
$ cabal clean
$ cabal configure --enable-library-profiling --enable-profiling --enable-tests --enable-benchmarks
$ cabal build
$ cabal test unit-tests
```

Then, it will be generated a file unit-tests.prof (in this case) that contains the data gathered.
An useful explanation to interpret the data collected can be found in the following page: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html

## Test suite profiling
To collect the run times and allocated memory of a test suite with 10 runs, use the command:

```
$ bash ./testSuiteProf.sh
```

This will generate a file testsProf.prof with the running times and allocated memory.

## Context Free Session Types - Test framework

The test infrastructure contains a collection of tools for testing the Context Free Session Types compiler and displaying the results.
(See also [README.md](../README.md) for more information about the compiler).

## Requirements

This section contains the requirements for the test framework.
It's necessary to have the following tools installed:

  * The Haskell platform (TODO See [`Haskell Website`](https://www.haskell.org/platform/))
  * Hspec & Hspec-discover

## Automated testing - Run Tests

To be able to run the tests you need to be on the root folder of the project (ContextFreeSession directory) and then:

  * Run all tests: ``` make test ```
  * Run valid tests: ``` make testValid ```
  * Run invalid tests: ``` make testInvalid ```

There are also available options to run valid and invalid tests separately for all the units:

  * Parser valid tests: ``` make testValidParser ```
  * Parser invalid tests: ``` make testInvalidParser ```
  * Types valid tests: ``` make testValidTypes ```
  * Types invalid tests: ``` make testInvalidTypes ```
  * Kinding valid tests: ``` make testValidKinding ```
  * Kinding invalid tests: ``` make testInvalidKinding ```


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
        Expected result

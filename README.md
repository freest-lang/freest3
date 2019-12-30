```
  ______              _____ _______ 
 |  ____|            / ____|__   __|
 | |__ _ __ ___  ___| (___    | |
 |  __| '__/ _ \/ _ \\___ \   | |
 | |  | | |  __/  __/____) |  | |
 |_|  |_|  \___|\___|_____/   |_|
```


# Table of Contents
1. [Install stack](#stack)
2. [Building FreeST](#buildFreeST)
2. [Cleaning the project](#cleanProj)
3. [Compile programs](#compileProgs)
   1. [FreeST Samples](#freeSTSamples)
4. [Tests](#tests)
   1. [Unit tests](#unitTests)
      1. [Run unit tests](#runUnitTests)
      2. [Add unit tests](#addUnitTests)
   2. [Program tests](#programTests)
      1. [Run program tests](#runProgTests)
      2. [Add program tests](#addProgTests)
   3. [Run each spec separately](#runSpecs)
      1. [Unit test specs](#unitSpec)
      2. [Quickcheck spec](#quickCheckSpec)
      3. [Program specs (valid or invalid programs)](#progSpec)
   4. [Profiling (TODO + test)](#profiling)
	  1. Test suite profiling 
	  2. [Test suite profiling (10 times)](#prof10Times)
   5. Line Coverage  (TODO + test)

# Install stack
<a id="stack"></a>
- For Un*x-like operating systems:
```
    $ curl -sSL https://get.haskellstack.org/ | sh
```

- For windows download the installer available at: 
  
  [Get stack for windows](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)

For more information about stack please visit [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)

<a id="buildFreeST"></a>

# Building FreeST

```
    $ stack build :TOOL_NAME
```	

will install (and build) GHC and all the project dependencies on an isolated location. 

To build only FreeST one should run ```stack build :freest```.
If the trailing part is ommited (```:TOOL_NAME```) all the available tools will be built.

The available tools to replace ```TOOL_NAME``` are:
  - freest
  - TACAS2020

<a id="compileProgs"></a>

# Compile programs

To run a program just create a file (with ".fst" extension) and then, 
run ``` stack run [filepath] ```

For example, to compile a FreeST program named "test.fst" located in the directory "dir" run:
```
    $ stack run dir/test.hs
```

<a id="freeSTSamples"></a>

## FreeST Samples

There are some code examples that are available on the directory 
[`test/Programs/ValidTests/`](test/Programs/ValidTests/)



<a id="tests"></a>

# Tests
<a id="unitTests"></a>

## Unit tests
<a id="runUnitTests"></a>

### Run unit tests
```
    $ stack test :unit-tests
```

<a id="addUnitTests"></a>

### Add unit tests
To add tests to the infrastructure, the correspondent txt of the desired unit test. 
For example to add valid kinding tests, select the TestValidKinding.txt file, which is under 
the directory `test/UnitTests/Validation`.

The txt file is structured as follows :

    Testing language phrase
        Expected


<a id="programTests"></a>

## Program tests
<a id="runProgTests"></a>

### Run program tests
```
    $ stack test :program-tests
```

<a id="addProgTests"></a>

### Add program tests

To add a valid program test follow the steps below:

1. Create a new directory under `test/Programs/ValidTests/`
2. Create a FreeST program (i.e. `test.fst`)
3. Create a file with the expected result (`test.expected`)

The process of creating invalid tests is analogous, except for step 3, 
since that `test.fst` is an incorrect program. Also, the tests must be
placed under `test/Programs/InvalidTests/` .

<a id="runSpecs"></a>

## Run each spec separately

<a id="unitSpec"></a>
### Unit test specs
```
    $ stack build :freest && stack runghc -- -itest/UnitTests/ PATH_TO_SPEC
```
where the PATH_TO_SPEC is one of the following:

- test/UnitTests/Equivalence/TestBisimInvalidSpec.hs
- test/UnitTests/Equivalence/TestEquivalenceInvalidSpec.hs
- test/UnitTests/Equivalence/TestGrammarInvalidSpec.hs
- test/UnitTests/Equivalence/TestGrammarValidSpec.hs
- test/UnitTests/Equivalence/TestBisimValidSpec.hs
- test/UnitTests/Equivalence/TestEquivalenceValidSpec.hs
- test/UnitTests/Validation/TestKindingInvalidSpec.hs
- test/UnitTests/Validation/TestKindingValidSpec.hs
- test/UnitTests/Validation/TestTypeSchemeKindingSpec.hs
- test/UnitTests/Parse/TestParserInvalidSpec.hs

<a id="quickCheckSpec"></a>

### Quickcheck spec
```
    $ stack build :freest && stack runghc -- -itest/ test/QuickCheck/QuickCheckSpec.hs
```

<a id="progSpec"></a>

### Program specs (valid or invalid programs)
```
    $ stack build :freest && stack runghc -- -itest/Programs/ PATH_TO_SPEC
```
with one of the following paths:

- test/Programs/CompilerInvalidSpec.hs
- test/Programs/CompilerValidSpec.hs

<a id="profiling"></a>

## Profiling
### Test suite profiling
TODO

<a id="prof10Times"></a>

### Test suite profiling (10 times)
To collect the run times and allocated memory of a test suite with 10 runs, use the command:

```
$ bash ./testSuiteProf.sh
```

This will generate a file testSuiteProf.prof with the running times and allocated memory.

### Test line coverage
To view the test coverage run ``` make coverage ```. The output files will be stored at 
[test/outputs](test/outputs) folder. (The main file is hpc_index.html that summarizes the 
information available and contains links for more specific information)
	

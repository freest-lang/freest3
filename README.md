```
  ______              _____ _______ 
 |  ____|            / ____|__   __|
 | |__ _ __ ___  ___| (___    | |
 |  __| '__/ _ \/ _ \\___ \   | |
 | |  | | |  __/  __/____) |  | |
 |_|  |_|  \___|\___|_____/   |_|
```

# Install stack
- For Un*x-like operating systems:
```
    $ curl -sSL https://get.haskellstack.org/ | sh
```

- For windows download the installer available at: 
  
  [Get stack for windows](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)

For more information about stack please visit [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)

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


# Compile programs

To run a program just create a file (with ".fst" extension) and then, 
run ``` stack run freest [filepath] ```

For example, to compile a FreeST program named "test.fst" located in the directory "dir" run:
```
    $ stack run freest dir/test.hs
```

## FreeST Samples

There are some code examples that are available on the directory 
[`FreeST/test/Programs/ValidTests/`](FreeST/test/Programs/ValidTests/)


# Tests

```
    $ stack test
```

will run both program and unit tests.

## Unit tests

### Run unit tests
```
    $ stack test :units
```

or

```
    $ make units
```

### Add unit tests
To add tests to the infrastructure, the correspondent txt of the desired unit test. 
For example to add valid kinding tests, select the TestValidKinding.txt file, which is under 
the directory `FreeST/test/UnitTests/Validation`.

The txt file is structured as follows :

    Testing language phrase
     Expected

## Program tests

### Run program tests
```
    $ stack test :programs
```

or

```
    $ make programs
```

### Add program tests

To add a valid program test follow the steps below:

1. Create a new directory under `FreeST/test/Programs/ValidTests/`
2. Create a FreeST program (i.e. `test.fst`)
3. Create a file with the expected result (`test.expected`)

The process of creating invalid tests is analogous, except for step 3, 
since that `test.fst` is an incorrect program. Also, the tests must be
placed under `FreeST/test/Programs/InvalidTests/` .


## Run each spec separately

### Unit test specs
```
    $ stack test :units --ta "-m SPEC_NAME"
```

where the `SPEC_NAME` is one of the following:

- Valid equivalence tests: `TestEquivalenceValid`
- Invalid equivalence tests: `TestEquivalenceInvalid`
- All equivalence tests: `TestEquivalence`
- Valid types tests: `TestTypesValid`
- Invalid types tests: `TestTypesInvalid`
- All types tests: `TestTypes`

We can also run them through:

- Valid equivalence tests: `$ make equiv-types`
- Invalid equivalence tests: `$ make nonequiv-types`
- All equivalence tests: `make equiv`
- Valid types tests: `$ make valid-types`
- Invalid types tests: `$ make invalid-types`
- All types tests: `$ make types`


### Quickcheck spec
```
    $ stack test :valid-types-quick
```

or

```
    $ make valid-types-quick
```

### Program specs (valid or invalid programs)
```
    $ stack test :units --ta "-m SPEC_NAME"
```

with one of the following `SPEC_NAME`s:

- CompilerInvalid
- CompilerValid

or also,

```
    $ make valid-programs
```

```
$ make invalid-programs
```

## Profiling
### Test suite profiling
TODO

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


# Build & Run other tools

 - CFSTEquiv: ``` $ stack run CFSTEquiv```

 - SGBisim:  ``` $ stack run SGBisim```

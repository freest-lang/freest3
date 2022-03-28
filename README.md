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
```bash
    $ curl -sSL https://get.haskellstack.org/ | sh
```

- For windows download the installer available at: 
  
  [Get stack for windows](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)

For more information about stack please visit [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)

# Building FreeST

```bash
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
```bash
    $ stack run freest dir/test.fst
```

One can find the suitable compiler options by running 
```bash
    $ stack run -- -h
```

For example, if you want to specify which function should be interpreted as the main function (`fun` in this example) you should run:
```bash
    $ stack run test.fst -- -m fun
```


# Run the interpreter (ghci)

```bash
    $ stack ghci FreeST:exe:freest
    :set prompt  "λ: "
```

## FreeST Samples

Code examples available in directory
[`FreeST/test/Programs/ValidTests/`](FreeST/test/Programs/ValidTests/)


# Tests

```bash
    $ stack test
```

will run program tests, quickcheck, and unit tests.

## Unit tests

### Run unit tests
```bash
    $ stack test :units
```

or

```bash
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
```bash
    $ stack test :programs
```

or

```bash
    $ make programs
```

### Add program tests

To add a valid program test follow the steps below:

1. Create or use a directory under `FreeST/test/Programs/ValidTests/` which represents the test category
2. Create a new directory to add the test files 
3. Create a FreeST program under the directory created on step 2 (i.e. `test.fst`)
4. Create a file, under the directory created on step 2, with the expected result (`test.expected`)

The contents of the ".expected" test file may be:
  - \<divergent\>, if the computation do not end.
  - \<pending\>, can be used as a TODO list. These are tests that need to be taken care in the future.
  - The result of the computation


The process of creating invalid tests is analogous. In the third step, the only
option that makes sense is "<pending>" since these are incorrect programs and
thus they are expected to raise errors. Also, the tests must be
placed under `FreeST/test/Programs/InvalidTests/`.


## Run each spec separately

### Unit test specs
```bash
    $ stack test :units --ta "-m SPEC_NAME"
```

where the `SPEC_NAME` is one of the following:

- Valid equivalence tests: `TestEquivalenceValid`
- Invalid equivalence tests: `TestEquivalenceInvalid`
- Well formed expressions: `TestExpressionInvalid
`
- Non Well formed expressions: `TestExpressionValid`
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
```bash
    $ stack test :valid-types-quick
```

or

```bash
    $ make valid-types-quick
```

### Program specs (valid or invalid programs)
```bash
    $ stack test :programs --ta "-m SPEC_NAME"
```

with one of the following `SPEC_NAME`s:

- CompilerInvalid
- CompilerValid

or also,

```bash
    $ make valid-programs
```

```bash
    $ make invalid-programs
```

**Note:** It is possible run the tests of each category separately by using the
same approach. Try:

```bash
    $ stack test :programs --ta "-m SessionTypes"
```

*Warning!* If there are two categories named "SessionTypes" (one on invalid and
the other on valid test), the previous command will capture both.

If the output from the compiler is important (relevant to invalid
tests to visually inspect error messages). Warning: a bit slow.

```bash
cd FreeST/test/Programs/InvalidTests/
for f in $(find . -name "*\.fst" -print); do stack run $f; done
```

## Profiling
### Test suite profiling
TODO

### Test suite profiling (10 times)
To collect the run times and allocated memory of a test suite with 10 runs, use the command:

```bash
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

Note: For the ease of the development cycle, these two tools are commented out on file `stack.yaml`. 
Uncomment to run them. Having them commented allows running `stack run test.fst` instead of `stack run freest test.fst`.

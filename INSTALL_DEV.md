* Build cabal

```
$ cabal install --enable-tests --only-dependencies
$ cabal build
```

* Running tests (check the entries in file `ContextFreeSession.cabal`)

```
$ cabal test unit-tests
```
or
```
$ cabal test program-tests
```

If you need to configure your package manually, you can also enable tests with
```
$ cabal configure --enable-tests
```

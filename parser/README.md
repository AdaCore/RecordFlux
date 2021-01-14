Parser for the [RecordFlux](https://github.com/Componolit/RecordFlux) language using [Langkit](https://github.com/AdaCore/langkit).

# Tests

As a prerequisite to build the parser, [GNAT Community Edition](https://www.adacore.com/download) >= 2020 needs to be installed. To run the tests, the RecordFlux language package and its dependencies must also be installed. Either use the respective make target:

```Console
$ make install_parser
```

or install the development version manually using `pip`:

```Console
$ pip3 install .[Devel]
```

The tests can then be executed using:

```Console
$ make test
```

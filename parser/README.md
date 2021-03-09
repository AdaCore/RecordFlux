# RecordFlux Parser

This is the parser for the [RecordFlux](https://github.com/Componolit/RecordFlux) language which is based on [Langkit](https://github.com/AdaCore/langkit).

## Development

As a prerequisite to build the parser, [GNAT Community Edition](https://www.adacore.com/download) >= 2020 needs to be installed. To run the tests, the RecordFlux language package and its dependencies must also be installed. Either use the respective make target:

```Console
$ make install_parser
```

or install the development version manually using `pip`:

```Console
$ pip3 install ".[devel]"
```

**Note:** Develop mode of setuptools (`pip -e`) is unsupported. The parser must be reinstalled before changes to the code take effect.

The tests can then be executed using:

```Console
$ make test
```

# RecordFlux support in VS Code 

This project is a prototype of support for *RecordFlux* in *Visual Studio Code*. It groups both the *VS Code* extension (in the `client` folder) and the language server (in the `server` folder).

## Try it out for yourself

Make sure you have [Visual Studio Code](https://code.visualstudio.com/), [Python3.8](https://www.python.org/downloads/), [nvm](https://github.com/nvm-sh/nvm) and an Ada compiler installed and run the the following commands:

```bash
make init
make install
```

You should now be able to use the extension and get highlighted *RecordFlux* specification files in *VS Code*.

## Setup development environment

### Installation

```bash
make init
make install_devel
```

### Usage

To run checks use `make check`, to format the code use `make format` and to run the tests use `make test`.

### Running the extension in debug mode

Once the server is installed, you can run the extension in *VS Code*. To do so, open the `client` folder in *Visual Studio Code* and click `Run > Start Debugging` in the menu.
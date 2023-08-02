# Language Server

Before reading any further, it is important to have in mind a clear understanding of the distinctions between the two kind of types we might want to talk about.

### Variable types

When using a programming language, we are used to declare variable with a type.

```python
list_of_strings: list[str] = []
```
In this case, we declare a variable `list_of_string` and its type is `list[str]`.

### Token and symbol types

When we deal with semantic highlighting, the token types we talk about are different. Let's go back to the previous example:

```python
list_of_strings: list[str] = []
```
This time, in the context of semantic highlighting `list_of_strings` would be a token of type **variable**. To avoid any confusion, these kind of types are refered to as symbol categories throughout this document and the code.

## Model

At its core, the `LSModel` is dictionary mapping lexemes to symbol definitions.

```python
class LSModel:
    def __init__(self):
        self._definitions: dict[str, list[Symbol]] = defaultdict(list)
        ...
```

### Why does the model map lexemes to a list of `Symbol` instead of using a one to one mapping of `ID` to `Symbol`?

Using the lexeme is much more convenient for the lexer as it handles lexemes rather than IDs. With the second approach, the lexer would have to guess a list of ID from each lexeme, this is both inefficient, as most guesses will be wrong, and prone to errors, as its not trivial to generate valid IDs in the current context from a lexeme.

### Why not use *RecordFlux*'s model directly?

1. `LSModel` flattens the model representation provided by *RecordFlux*. This allows to treat most symbols in a universal manner by removing the distinctions irrelevant for semantic highlighting (for example between messages and their fields). This requires to generate an `ID` for the symbols which are not *RecordFlux* types.
2. Contrary to *RecordFlux*'s model, `LSModel` provides an easy access to types knowing the lexeme they are identified with in the specification file.

## Lexer

The role of the lexer is as follows:

- When given a source file as input, store the corresponding list of tokens
- Search for a token given a position in the file

The lexer currently is able to handle invalid specification files as long as they can be parsed by *langkit*. However, the language server as a whole is also limited by the creation of the unchecked model.

## Server

The role of the server is to keep a `LSModel` instance up to date and provide callback functions implementing the logic behind the supported features.

## Areas of improvement

### Include variable types in the model

Currently the semantic highlighting does not acknowledge when a field is being accessed via a message instance.

```rflx
Message.Field
```

This would require for each token to store the type associated to the variable it represents if any.

### Rely more on syntax rather than semantic because this is more robust

For example, definitions should always be highlighted correctly even though other parts of the specification are invalid.

## Debugging the server

For logging debug information, the `logging` module must be used. Using `print` and the standard output in general will not work and will most certainly result in a crash of the server, because the standard output is used to communicated with the client.

## References

[Microsoft's Language Server Protocol Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/) is the best place to get an up-to-date list of all the LSP features. [pygls](https://github.com/openlawlibrary/pygls) has an API very close to the specification.

The following links are good starting points to browse information about the development of the *VS Code* extension:

- [Basis for language server client](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide)
- [Parts of the extension related to virtual environments](https://github.com/microsoft/vscode-python-tools-extension-template)
- [*VS Code* extension API](https://code.visualstudio.com/api)

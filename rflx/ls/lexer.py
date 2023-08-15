from __future__ import annotations

from dataclasses import dataclass
from functools import singledispatchmethod
from typing import Optional, Union, cast

import rflx_lang

from rflx import const
from rflx.identifier import ID

from .model import LSModel, Symbol, SymbolCategory


@dataclass
class Token:
    """
    Data structure produced by the LSLexer.

    Attributes
    ----------
        symbol: The symbol referenced by this token.
        lexeme: A string representation of the token in the file.
        line_number: An integer indicating the line at which the token is located in the file
                     starting from 1.
        character_offset: An integer indicating the character offset from the begining of the line
                          at which the first character of the lexeme is located starting from 1.
    """

    symbol: Optional[Symbol]
    lexeme: str
    line_number: int
    character_offset: int


@dataclass
class State:
    imports: set[ID]
    current_package: Optional[ID]
    declarations: list[ID]
    foreign_package: Optional[ID]
    current_session: Optional[ID]
    top_level: bool


class LSLexer:
    def __init__(self, model: LSModel):
        self._model = model
        self._tokens: list[Token] = []

    @property
    def tokens(self) -> list[Token]:
        """List of tokens resulting from the previous LSLexer.tokenize calls."""
        return self._tokens

    def search_token(self, line_number: int, character_offset: int) -> Optional[Token]:
        """Return the token at the given location if it exists, otherwise return None."""

        if len(self._tokens) == 0:
            return None

        lower_bound = 0
        upper_bound = len(self._tokens)

        # binary search over the lines
        while upper_bound - lower_bound > 1:
            middle = (lower_bound + upper_bound) // 2
            token = self._tokens[middle]

            if line_number < token.line_number:
                upper_bound = middle
            elif line_number > token.line_number:
                lower_bound = middle
            else:
                lower_bound = middle
                break

        # early exit if there are no tokens on requested line_number
        if self._tokens[lower_bound].line_number != line_number:
            return None

        # get to the first token of the line because the binary search
        # is likely to provide one in the middle
        while lower_bound > 0 and self._tokens[lower_bound - 1].line_number == line_number:
            lower_bound -= 1

        # linear search to find the token at the desired character offset if it exists
        while (
            lower_bound < len(self._tokens) and self._tokens[lower_bound].line_number == line_number
        ):
            if self._tokens[lower_bound].character_offset > character_offset:
                break
            if (
                self._tokens[lower_bound].character_offset + len(self._tokens[lower_bound].lexeme)
                >= character_offset
            ):
                return self._tokens[lower_bound]
            lower_bound += 1

        return None

    def tokenize(self, source: str, path: str = "<stdin>") -> None:
        """Convert a string into a list of tokens that can be accessed via the tokens property."""

        unit = rflx_lang.AnalysisContext().get_from_buffer(
            path,
            source,
            rule=rflx_lang.GrammarRule.main_rule_rule,
        )

        state = State(set(), None, [], None, None, top_level=False)
        self._process_ast_node(unit.root, state)

    @singledispatchmethod
    def _process_ast_node(self, node: Optional[rflx_lang.RFLXNode], state: State) -> None:
        if node is None:
            return

        self._process_children(node, state)

    def _process_children(self, node: rflx_lang.RFLXNode, state: State) -> None:
        for child in node.children:
            self._process_ast_node(child, state)

    def _identify_symbol(self, lexeme: str, state: State) -> Optional[Symbol]:
        symbols: list[Symbol] = self._model.get_symbols(lexeme)

        if state.foreign_package is not None and state.foreign_package != ID(lexeme):
            for symbol in symbols:
                if symbol.identifier == state.foreign_package * ID(lexeme):  # pragma: no branch
                    return symbol
            return None

        if not state.top_level:
            for symbol in symbols:
                if symbol.parent in state.declarations:
                    return symbol

        for symbol in filter(lambda s: s.parent is None, symbols):
            if (
                (
                    symbol.category == SymbolCategory.PACKAGE
                    and (
                        symbol.identifier in state.imports
                        or symbol.identifier == state.current_package
                    )
                )
                or (
                    state.current_package is not None
                    and symbol.identifier == state.current_package * ID(lexeme)
                )
                or symbol.identifier == const.BUILTINS_PACKAGE * ID(lexeme)
                or symbol.identifier == const.INTERNAL_PACKAGE * ID(lexeme)
            ):
                return symbol

        return None

    @_process_ast_node.register
    def _(self, node: rflx_lang.ID, state: State) -> None:
        if node.f_package is not None:
            state.foreign_package = ID(node.f_package.text)
        self._process_children(node, state)
        state.foreign_package = None

    @_process_ast_node.register
    def _(self, node: rflx_lang.UnqualifiedID, state: State) -> None:
        symbol = self._identify_symbol(node.text, state)

        self._tokens.append(
            Token(
                symbol,
                node.text,
                node.sloc_range.start.line - 1,
                node.sloc_range.start.column - 1,
            ),
        )

    @_process_ast_node.register
    def _(self, node: rflx_lang.VariableDecl, state: State) -> None:
        self._process_ast_node(node.f_identifier, state)
        state.top_level = True
        self._process_ast_node(node.f_type_identifier, state)
        state.top_level = False

    @_process_ast_node.register
    def _(self, node: rflx_lang.Parameter, state: State) -> None:
        self._process_ast_node(node.f_identifier, state)
        state.top_level = True
        self._process_ast_node(node.f_type_identifier, state)
        state.top_level = False

    @_process_ast_node.register
    def _(self, node: rflx_lang.ContextItem, state: State) -> None:
        state.imports.add(ID(node.f_item.text))
        self._process_children(node, state)

    @_process_ast_node.register
    def _(self, node: rflx_lang.PackageNode, state: State) -> None:
        state.current_package = ID(node.f_identifier.text)
        self._process_children(node, state)
        state.current_package = None

    # Python 3.11 directly supports single dispatch with typing.Union
    @_process_ast_node.register(rflx_lang.TypeDecl)
    @_process_ast_node.register(rflx_lang.SessionDecl)
    def _(self, node: Union[rflx_lang.TypeDecl, rflx_lang.SessionDecl], state: State) -> None:
        partial_identifier = ID(node.f_identifier.text)
        identifier = (
            state.current_package * partial_identifier
            if state.current_package is not None
            else partial_identifier
        )

        if node.is_a(rflx_lang.SessionDecl):
            state.current_session = identifier

        state.declarations.append(identifier)
        self._process_children(node, state)
        state.declarations.pop()

        if node.is_a(rflx_lang.SessionDecl):
            state.current_session = None

    @_process_ast_node.register
    def _(self, node: rflx_lang.MessageField, state: State) -> None:
        self._process_ast_node(node.f_identifier, state)
        state.top_level = True
        self._process_ast_node(node.f_type_identifier, state)
        state.top_level = False
        self._process_ast_node(node.f_aspects, state)
        self._process_ast_node(node.f_condition, state)
        self._process_ast_node(node.f_thens, state)
        self._process_ast_node(node.f_type_identifier, state)

    @_process_ast_node.register
    def _(self, node: rflx_lang.RefinementDecl, state: State) -> None:
        name = node.f_pdu.f_name
        # TODO(eng/recordflux/RecordFlux#1371): Invalid type annotation for optional field
        if cast(Optional[rflx_lang.UnqualifiedID], node.f_pdu.f_package) is None:
            message_identifier = (
                state.current_package * ID(name.text)
                if state.current_package is not None
                else ID(name.text)
            )
        else:
            message_identifier = ID(node.f_pdu.f_package.text) * ID(name.text)
        state.declarations.append(message_identifier)
        self._process_children(node, state)
        state.declarations.pop()

    @_process_ast_node.register
    def _(self, node: rflx_lang.FormalFunctionDecl, state: State) -> None:
        assert state.current_session is not None
        state.declarations.append(state.current_session * ID(node.f_identifier.text))
        self._process_children(node, state)
        state.declarations.pop()

    @_process_ast_node.register
    def _(self, node: rflx_lang.State, state: State) -> None:
        assert state.current_session is not None
        state.declarations.append(state.current_session * ID(node.f_identifier.text))
        self._process_children(node, state)
        state.declarations.pop()

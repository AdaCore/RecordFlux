from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass
from enum import Enum, auto
from typing import Optional

from rflx.common import assert_never
from rflx.error import Location
from rflx.identifier import ID
from rflx.model import UncheckedModel
from rflx.model.declaration import ChannelDeclaration, FunctionDeclaration
from rflx.model.message import UncheckedMessage
from rflx.model.session import UncheckedSession
from rflx.model.top_level_declaration import UncheckedTopLevelDeclaration
from rflx.model.type_ import (
    UncheckedEnumeration,
    UncheckedInteger,
    UncheckedSequence,
)


class SymbolCategory(Enum):
    UNDEFINED = auto()
    KEYWORD = auto()
    NUMERIC = auto()
    ENUMERATION = auto()
    ENUMERATION_LITERAL = auto()
    SEQUENCE = auto()
    MESSAGE = auto()
    MESSAGE_FIELD = auto()
    SESSION = auto()
    SESSION_MEMBER = auto()
    SESSION_CHANNEL = auto()
    SESSION_FUNCTION = auto()
    SESSION_FUNCTION_PARAMETER = auto()
    SESSION_STATE = auto()
    SESSION_STATE_VARIABLE = auto()
    PACKAGE = auto()

    def to_lsp_token(self) -> str:  # noqa: PLR0911
        if self is SymbolCategory.KEYWORD:
            return "keyword"
        if self is SymbolCategory.NUMERIC or self is SymbolCategory.SEQUENCE:
            return "type"
        if self is SymbolCategory.ENUMERATION:
            return "enum"
        if self is SymbolCategory.ENUMERATION_LITERAL:
            return "enumMember"
        if self is SymbolCategory.MESSAGE:
            return "struct"
        if self is SymbolCategory.MESSAGE_FIELD or self is SymbolCategory.SESSION_MEMBER:
            return "property"
        if self is SymbolCategory.SESSION:
            return "class"
        if self is SymbolCategory.SESSION_STATE or self is SymbolCategory.SESSION_CHANNEL:
            return "event"
        if self is SymbolCategory.SESSION_STATE_VARIABLE:
            return "variable"
        if self is SymbolCategory.SESSION_FUNCTION:
            return "method"
        if self is SymbolCategory.PACKAGE:
            return "namespace"
        if self is SymbolCategory.SESSION_FUNCTION_PARAMETER:
            return "parameter"
        assert self is not SymbolCategory.UNDEFINED
        assert_never(self)  # pragma: no cover


@dataclass(eq=True, frozen=True)
class Symbol:
    """
    Information about the definition of a symbol.

    Arguments:
    ---------
        identifier: The unique identifier associated to the symbol.
        category: A SymbolCategory indicating the type (message, session, numeric, enumeration...)
                  of the symbol.
        definition_location: A Location indicating where the symbol is defined.
        parent: A optional ID indicating if the cur
    """

    identifier: ID
    category: SymbolCategory
    definition_location: Optional[Location]
    parent: Optional[ID]


class LSModel:
    def __init__(self, unchecked_model: UncheckedModel):
        self._symbols: dict[str, list[Symbol]] = defaultdict(list)

        for declaration in unchecked_model.declarations:
            symbols = LSModel._to_symbols(declaration)
            if len(symbols) == 0:
                continue
            self._append_package_of(declaration)
            for symbol in symbols:
                self._append_symbol(symbol)

    def __contains__(self, lexeme: str) -> bool:
        return lexeme in self._symbols

    def get_symbols(self, lexeme: str) -> list[Symbol]:
        return self._symbols[lexeme]

    def _package_already_registered(self, package: Symbol) -> bool:
        return any(
            symbol.category == SymbolCategory.PACKAGE
            for lexeme, symbols in self._symbols.items()
            if lexeme == str(package.identifier)
            for symbol in symbols
        )

    def _append_package_of(self, declaration: UncheckedTopLevelDeclaration) -> None:
        package_identifier = declaration.package
        package = LSModel._to_symbol(package_identifier)
        if not self._package_already_registered(package):
            self._append_symbol(package)

    def _append_symbol(self, symbol: Symbol) -> None:
        self._symbols[str(symbol.identifier.name)].append(symbol)

    @staticmethod
    def _to_symbols(declaration: UncheckedTopLevelDeclaration) -> list[Symbol]:
        if isinstance(declaration, UncheckedInteger):
            return [
                Symbol(declaration.identifier, SymbolCategory.NUMERIC, declaration.location, None),
            ]

        if isinstance(declaration, UncheckedEnumeration):
            result = [
                Symbol(
                    declaration.package * literal[0],
                    SymbolCategory.ENUMERATION_LITERAL,
                    declaration.location,
                    None,
                )
                for literal in declaration.literals
            ]
            result.append(
                Symbol(
                    declaration.identifier,
                    SymbolCategory.ENUMERATION,
                    declaration.location,
                    None,
                ),
            )
            return result

        if isinstance(declaration, UncheckedSequence):
            return [
                Symbol(declaration.identifier, SymbolCategory.SEQUENCE, declaration.location, None),
            ]

        if isinstance(declaration, UncheckedMessage):
            result = [
                Symbol(
                    declaration.identifier * field[0].identifier,
                    SymbolCategory.MESSAGE_FIELD,
                    field[0].identifier.location,
                    declaration.identifier,
                )
                for field in declaration.field_types
            ]
            result.append(
                Symbol(declaration.identifier, SymbolCategory.MESSAGE, declaration.location, None),
            )
            return result

        if isinstance(declaration, UncheckedSession):
            channels = [
                Symbol(
                    declaration.identifier * channel.identifier,
                    SymbolCategory.SESSION_CHANNEL,
                    channel.location,
                    declaration.identifier,
                )
                for channel in declaration.parameters
                if isinstance(channel, ChannelDeclaration)
            ]
            functions = [
                Symbol(
                    declaration.identifier * function.identifier,
                    SymbolCategory.SESSION_FUNCTION,
                    function.location,
                    declaration.identifier,
                )
                for function in declaration.parameters
                if isinstance(function, FunctionDeclaration)
            ]
            function_arguments = [
                Symbol(
                    declaration.identifier * function.identifier * argument.identifier,
                    SymbolCategory.SESSION_FUNCTION_PARAMETER,
                    argument.identifier.location,
                    declaration.identifier * function.identifier,
                )
                for function in declaration.parameters
                if isinstance(function, FunctionDeclaration)
                for argument in function.arguments
            ]
            declarations = [
                Symbol(
                    declaration.identifier * session_declaration.identifier,
                    SymbolCategory.SESSION_MEMBER,
                    session_declaration.location,
                    declaration.identifier,
                )
                for session_declaration in declaration.declarations
            ]
            states = [
                Symbol(
                    declaration.identifier * state.identifier,
                    SymbolCategory.SESSION_STATE,
                    state.location,
                    declaration.identifier,
                )
                for state in declaration.states
            ]
            state_declarations = [
                Symbol(
                    declaration.identifier * state.identifier * state_declaration.name,
                    SymbolCategory.SESSION_STATE_VARIABLE,
                    state_declaration.location,
                    declaration.identifier * state.identifier,
                )
                for state in declaration.states
                for state_declaration in state.declarations
            ]
            result = [
                Symbol(
                    declaration.identifier,
                    SymbolCategory.SESSION,
                    declaration.location,
                    None,
                ),
            ]
            result.extend(channels)
            result.extend(functions)
            result.extend(function_arguments)
            result.extend(declarations)
            result.extend(states)
            result.extend(state_declarations)

            return result

        return []

    @staticmethod
    def _to_symbol(package: ID) -> Symbol:
        return Symbol(package.name, SymbolCategory.PACKAGE, package.location, None)

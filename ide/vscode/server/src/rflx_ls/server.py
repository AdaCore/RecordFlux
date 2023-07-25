from __future__ import annotations

import multiprocessing
from pathlib import Path
from typing import Final, Optional
from urllib.parse import unquote, urlparse

import rflx.error
import rflx.model
from lsprotocol.types import (
    TEXT_DOCUMENT_CODE_LENS,
    TEXT_DOCUMENT_DEFINITION,
    TEXT_DOCUMENT_DIAGNOSTIC,
    TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL,
    CodeLens,
    CodeLensParams,
    Command,
    DefinitionParams,
    Diagnostic,
    DiagnosticSeverity,
    DocumentDiagnosticParams,
    Location,
    MessageType,
    Position,
    Range,
    SemanticTokens,
    SemanticTokensLegend,
    SemanticTokensParams,
)
from pygls.server import LanguageServer
from rflx.graph import create_message_graph, write_graph
from rflx.model.cache import Cache
from rflx.specification import Parser

from rflx_ls.lexer import LSLexer
from rflx_ls.model import LSModel, SymbolCategory

# workaround to prevent a deadlock while calling create_model()
# on rflx.specification.Parser after parsing a file containing
# a session
multiprocessing.set_start_method("spawn", force=True)

CACHE_PATH: Final = Path.home() / ".cache" / "RecordFlux"

LSP_TOKEN_CATEGORIES: Final = {
    "type": 0,
    "enum": 1,
    "enumMember": 2,
    "struct": 3,
    "property": 4,
    "keyword": 5,
    "class": 6,
    "namespace": 7,
    "event": 8,
    "method": 9,
    "parameter": 10,
    "variable": 11,
}


def rflx_type_to_lsp_token(type: SymbolCategory) -> str:  # noqa: PLR0911
    if type == SymbolCategory.KEYWORD:
        return "keyword"
    if type in {SymbolCategory.NUMERIC, SymbolCategory.SEQUENCE}:
        return "type"
    if type == SymbolCategory.ENUMERATION:
        return "enum"
    if type == SymbolCategory.ENUMERATION_LITERAL:
        return "enumMember"
    if type == SymbolCategory.MESSAGE:
        return "struct"
    if type in {SymbolCategory.MESSAGE_FIELD, SymbolCategory.SESSION_MEMBER}:
        return "property"
    if type == SymbolCategory.SESSION:
        return "class"
    if type in {SymbolCategory.SESSION_STATE, SymbolCategory.SESSION_CHANNEL}:
        return "event"
    if type == SymbolCategory.SESSION_STATE_VARIABLE:
        return "variable"
    if type == SymbolCategory.SESSION_FUNCTION:
        return "method"
    if type == SymbolCategory.PACKAGE:
        return "namespace"
    if type == SymbolCategory.SESSION_FUNCTION_PARAMETER:
        return "parameter"
    return "you probably created a new type without modifying lsp_token_types and/or rflx_type_to_lsp in main.py"


def rflx_location_to_lsp(location: Optional[rflx.error.Location]) -> Optional[Location]:
    if location is None or location.source is None:
        return None

    source = location.source.absolute().as_uri()
    start = location.start
    end = location.end if location.end is not None else location.start

    return Location(
        source, Range(Position(start[0] - 1, start[1] - 1), Position(end[0] - 1, end[1] - 1))
    )


def rflx_severity_to_lsp(severity: rflx.error.Severity) -> Optional[DiagnosticSeverity]:
    if severity == rflx.error.Severity.ERROR:
        return DiagnosticSeverity.Error
    if severity == rflx.error.Severity.INFO:
        return DiagnosticSeverity.Information
    if severity == rflx.error.Severity.WARNING:
        return DiagnosticSeverity.Warning
    return None


def initialize_lexer(language_server: RecordFluxLanguageServer, uri: str) -> LSLexer:
    language_server.update_model()
    document = language_server.workspace.get_document(uri)
    lexer = LSLexer(language_server.model)
    lexer.tokenize(document.source, document.path)
    return lexer


class RecordFluxLanguageServer(LanguageServer):
    CMD_SHOW_MESSAGE_GRAPH: Final = "showMessageGraph"

    def __init__(self, name: str, version: str) -> None:
        super().__init__(name, version)
        self.rflx_model = rflx.model.UncheckedModel([], rflx.error.RecordFluxError())
        self.model = LSModel(self.rflx_model)
        self.rflx_model_cache = Cache()

    def update_model(self) -> None:
        files: list[Path] = []

        for folder_uri in list(self.workspace.folders.keys()):
            folder_path = Path(unquote(urlparse(folder_uri).path))
            files.extend([file for file in folder_path.rglob("*.rflx") if file.is_file()])

        if len(self.workspace.folders.keys()) == 0:
            for document_uri in self.workspace.documents.keys():
                document_path = Path(unquote(urlparse(document_uri).path))
                files.append(document_path)

        errors = rflx.error.RecordFluxError([])
        parser = Parser(skip_verification=False, cached=True, workers=1, integration_files_dir=None)

        for path in files:
            document = self.workspace.get_document(path.as_uri())
            try:
                parser.parse_string(document.source, path)
            except rflx.error.BaseError as local_errors:
                errors.extend(local_errors)

        self.rflx_model = parser.create_unchecked_model()

        errors.extend(self.rflx_model.error)
        self._publish_errors_as_diagnostics(files, errors)

        self.model = LSModel(self.rflx_model)

    def _publish_errors_as_diagnostics(
        self, files: list[Path], errors: rflx.error.BaseError
    ) -> None:
        for file in files:
            diagnostics = [
                Diagnostic(
                    lsp_location.range,
                    error.message,
                    rflx_severity_to_lsp(error.severity),
                )
                for error in errors.messages
                for lsp_location in [rflx_location_to_lsp(error.location)]
                if lsp_location is not None and lsp_location.uri == file.absolute().as_uri()
            ]
            self.publish_diagnostics(file.absolute().as_uri(), diagnostics)


server = RecordFluxLanguageServer("recordflux-ls", "v0.1")


@server.feature(TEXT_DOCUMENT_DIAGNOSTIC)
async def diagnostics(
    ls: RecordFluxLanguageServer, params: DocumentDiagnosticParams
) -> list[Diagnostic]:
    ls.show_message(f"Diagnostics {params.text_document.uri}")
    return []


@server.feature(TEXT_DOCUMENT_DEFINITION)
async def goto_definition(
    ls: RecordFluxLanguageServer, params: DefinitionParams
) -> Optional[list[Location]]:
    lexer = initialize_lexer(ls, params.text_document.uri)
    token = lexer.search_token(params.position.line, params.position.character)

    if token is None or token.symbol is None:
        return None

    location = rflx_location_to_lsp(token.symbol.definition_location)

    if location is None:
        return None

    return [location]


@server.feature(
    TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL,
    SemanticTokensLegend(token_types=list(LSP_TOKEN_CATEGORIES.keys()), token_modifiers=[]),
)
async def semantic_tokens(
    ls: RecordFluxLanguageServer, params: SemanticTokensParams
) -> SemanticTokens:
    lexer = initialize_lexer(ls, params.text_document.uri)

    result: list[int] = []

    previous_line = 0
    previous_offset = 0

    for token in lexer.tokens:
        if token.symbol is None:
            continue

        if token.line_number != previous_line:
            previous_offset = 0

        relative_line = token.line_number - previous_line
        relative_offset = token.character_offset - previous_offset
        length = len(token.lexeme)
        token_category = LSP_TOKEN_CATEGORIES[rflx_type_to_lsp_token(token.symbol.category)]

        previous_line = token.line_number
        previous_offset = token.character_offset

        result.extend([relative_line, relative_offset, length, token_category, 0])

    return SemanticTokens(data=result)


@server.command(RecordFluxLanguageServer.CMD_SHOW_MESSAGE_GRAPH)
async def display_message_graph(ls: RecordFluxLanguageServer, parameters: list[int]) -> None:
    try:
        model = ls.rflx_model.checked(skip_verification=True, workers=1, cache=ls.rflx_model_cache)
    except rflx.error.BaseError:
        ls.show_message("Invalid specification", MessageType.Error)
        return

    message_index = parameters[0]
    proven_message = model.declarations[message_index]
    assert isinstance(proven_message, rflx.model.Message)

    graph_cache = CACHE_PATH / "graphs"
    graph_cache.mkdir(parents=True, exist_ok=True)

    graph = create_message_graph(proven_message)
    write_graph(graph, graph_cache / f"{proven_message.name}.svg")


@server.feature(TEXT_DOCUMENT_CODE_LENS)
async def code_lens(ls: RecordFluxLanguageServer, params: CodeLensParams) -> list[CodeLens]:
    ls.update_model()

    result: list[CodeLens] = []

    for index, declaration in enumerate(ls.rflx_model.declarations):
        if not isinstance(declaration, rflx.model.message.UncheckedMessage):
            continue

        location = rflx_location_to_lsp(declaration.location)

        if location is None or location.uri != params.text_document.uri:
            continue

        result.append(
            CodeLens(
                location.range,
                Command(
                    "Show message graph", RecordFluxLanguageServer.CMD_SHOW_MESSAGE_GRAPH, [index]
                ),
            )
        )

    return result

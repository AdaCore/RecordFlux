from __future__ import annotations

import uuid
from collections import defaultdict
from pathlib import Path
from typing import Final, Optional
from urllib.parse import unquote, urlparse

from lsprotocol.types import (
    TEXT_DOCUMENT_CODE_LENS,
    TEXT_DOCUMENT_DEFINITION,
    TEXT_DOCUMENT_DID_CHANGE,
    TEXT_DOCUMENT_DID_OPEN,
    TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL,
    CodeLens,
    CodeLensParams,
    Command,
    DefinitionParams,
    Diagnostic,
    DiagnosticSeverity,
    DidChangeTextDocumentParams,
    DidOpenTextDocumentParams,
    Location,
    Position,
    Range,
    SemanticTokens,
    SemanticTokensLegend,
    SemanticTokensParams,
    WorkDoneProgressBegin,
    WorkDoneProgressEnd,
)
from pygls.server import LanguageServer

from rflx import __version__, error
from rflx.common import assert_never
from rflx.const import CACHE_PATH
from rflx.graph import create_message_graph, write_graph
from rflx.model import Message, Model, UncheckedMessage, UncheckedModel
from rflx.model.cache import Cache
from rflx.specification import Parser

from .lexer import LSLexer
from .model import LSModel

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


def to_lsp_location(location: Optional[error.Location]) -> Optional[Location]:
    if location is None or location.source is None:
        return None

    source = location.source.absolute().as_uri()
    start = location.start
    end = location.end if location.end is not None else location.start

    return Location(
        source,
        Range(Position(start[0] - 1, start[1] - 1), Position(end[0] - 1, end[1] - 1)),
    )


def to_lsp_severity(severity: error.Severity) -> Optional[DiagnosticSeverity]:
    if severity is error.Severity.ERROR:
        return DiagnosticSeverity.Error
    if severity is error.Severity.INFO:
        return DiagnosticSeverity.Information
    if severity is error.Severity.WARNING:
        return DiagnosticSeverity.Warning
    if severity is error.Severity.NONE:
        return None
    assert_never(severity)


def initialize_lexer(language_server: RecordFluxLanguageServer, uri: str) -> LSLexer:
    language_server.update_model()
    document = language_server.workspace.get_document(uri)
    lexer = LSLexer(language_server.model)
    lexer.tokenize(document.source, document.path)
    return lexer


class RecordFluxLanguageServer(LanguageServer):
    CMD_SHOW_MESSAGE_GRAPH: Final = "showMessageGraph"

    def __init__(self, workers: int = 1) -> None:
        super().__init__("RecordFlux Language Server", __version__)
        self.workers = workers
        self.unchecked_model = UncheckedModel([], error.RecordFluxError())
        self.checked_model = Model()
        self.model = LSModel(self.unchecked_model)
        self.cache = Cache()

        self._parser: Parser
        self._error: error.RecordFluxError

    def update_model(self) -> None:
        token = str(uuid.uuid4())
        self.progress.create(token)
        self.progress.begin(
            token,
            WorkDoneProgressBegin(title="Update", percentage=0, cancellable=False),
        )

        files: list[Path] = []

        for folder_uri in list(self.workspace.folders):
            folder_path = Path(unquote(urlparse(folder_uri).path))
            for file in folder_path.rglob("*.rflx"):
                if file.is_file():
                    files.append(file)
                    self.publish_diagnostics(file.as_uri(), [])

        if len(self.workspace.folders) == 0:
            for document_uri in self.workspace.documents:
                document_path = Path(unquote(urlparse(document_uri).path))
                files.append(document_path)
                self.publish_diagnostics(document_uri, [])

        self._parser = Parser(cached=True, workers=self.workers)

        self._error = error.RecordFluxError()

        for path in files:
            document = self.workspace.get_document(path.as_uri())
            try:
                self._parser.parse_string(document.source, path)
            except error.RecordFluxError as e:
                self._error.extend(e)

        self._publish_errors_as_diagnostics(self._error)

        self.unchecked_model = self._parser.create_unchecked_model()

        self._publish_errors_as_diagnostics(self._error + self.unchecked_model.error)

        self.model = LSModel(self.unchecked_model)

        self.progress.end(token, WorkDoneProgressEnd(message="Completed"))

    def verify(self) -> None:
        token = str(uuid.uuid4())
        self.progress.create(token)
        self.progress.begin(
            token,
            WorkDoneProgressBegin(title="Verification", percentage=0, cancellable=True),
        )

        try:
            self.checked_model = self.unchecked_model.checked(self.cache, workers=self.workers)
        except error.RecordFluxError as e:
            self._error.extend(e)

        self._publish_errors_as_diagnostics(self._error)

        self.progress.end(token, WorkDoneProgressEnd(message="Completed"))

    def _publish_errors_as_diagnostics(self, errors: error.RecordFluxError) -> None:
        diagnostics = defaultdict(list)

        for msg in errors.messages:
            location = to_lsp_location(msg.location)
            severity = to_lsp_severity(msg.severity)

            if location is not None:
                diagnostics[location.uri].append(Diagnostic(location.range, msg.message, severity))

        for uri, diag in diagnostics.items():
            self.publish_diagnostics(uri, diag)


server = RecordFluxLanguageServer()


@server.feature(TEXT_DOCUMENT_DID_OPEN)
async def did_open(ls: RecordFluxLanguageServer, _params: DidOpenTextDocumentParams) -> None:
    ls.update_model()
    ls.thread_pool_executor.submit(ls.verify)


@server.feature(TEXT_DOCUMENT_DID_CHANGE)
async def did_change(ls: RecordFluxLanguageServer, _params: DidChangeTextDocumentParams) -> None:
    ls.update_model()
    ls.thread_pool_executor.submit(ls.verify)


@server.feature(TEXT_DOCUMENT_DEFINITION)
async def go_to_definition(
    ls: RecordFluxLanguageServer,
    params: DefinitionParams,
) -> Optional[list[Location]]:
    lexer = initialize_lexer(ls, params.text_document.uri)
    token = lexer.search_token(params.position.line, params.position.character)

    if token is None or token.symbol is None:
        return None

    location = to_lsp_location(token.symbol.definition_location)

    if location is None:
        return None

    return [location]


@server.feature(
    TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL,
    SemanticTokensLegend(token_types=list(LSP_TOKEN_CATEGORIES), token_modifiers=[]),
)
async def semantic_tokens(
    ls: RecordFluxLanguageServer,
    params: SemanticTokensParams,
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
        token_category = LSP_TOKEN_CATEGORIES[token.symbol.category.to_lsp_token()]

        previous_line = token.line_number
        previous_offset = token.character_offset

        result.extend([relative_line, relative_offset, length, token_category, 0])

    return SemanticTokens(data=result)


@server.command(RecordFluxLanguageServer.CMD_SHOW_MESSAGE_GRAPH)
async def show_message_graph(ls: RecordFluxLanguageServer, parameters: list[int]) -> None:
    message = ls.checked_model.declarations[parameters[0]]

    assert isinstance(message, Message)

    graph_cache = CACHE_PATH / "graphs"
    graph_cache.mkdir(parents=True, exist_ok=True)

    graph = create_message_graph(message)
    write_graph(graph, graph_cache / f"{message.name}.svg")


@server.feature(TEXT_DOCUMENT_CODE_LENS)
async def code_lens(ls: RecordFluxLanguageServer, params: CodeLensParams) -> list[CodeLens]:
    ls.update_model()

    result: list[CodeLens] = []

    for index, declaration in enumerate(ls.unchecked_model.declarations):
        if not isinstance(declaration, UncheckedMessage):
            continue

        location = to_lsp_location(declaration.location)

        if location is None or location.uri != params.text_document.uri:
            continue

        result.append(
            CodeLens(
                location.range,
                Command(
                    "Show message graph",
                    RecordFluxLanguageServer.CMD_SHOW_MESSAGE_GRAPH,
                    [index],
                ),
            ),
        )

    return result

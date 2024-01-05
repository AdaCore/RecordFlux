from __future__ import annotations

import functools
import inspect
import threading
import uuid
from collections import defaultdict
from pathlib import Path
from typing import Callable, Final, Iterable, Mapping, Optional
from urllib.parse import unquote, urlparse

from lsprotocol.types import (
    TEXT_DOCUMENT_CODE_LENS,
    TEXT_DOCUMENT_DEFINITION,
    TEXT_DOCUMENT_DID_CHANGE,
    TEXT_DOCUMENT_DID_OPEN,
    TEXT_DOCUMENT_DID_SAVE,
    TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL,
    CodeLens,
    CodeLensParams,
    Command,
    DefinitionParams,
    Diagnostic,
    DiagnosticSeverity,
    DidChangeTextDocumentParams,
    DidOpenTextDocumentParams,
    DidSaveTextDocumentParams,
    Location,
    Position,
    Range,
    SemanticTokens,
    SemanticTokensLegend,
    SemanticTokensParams,
    TextDocumentItem,
    WorkDoneProgressBegin,
    WorkDoneProgressEnd,
)
from pygls.server import LanguageServer

from rflx import __version__, error
from rflx.common import assert_never
from rflx.const import BUILTINS_PACKAGE, CACHE_PATH, INTERNAL_PACKAGE
from rflx.graph import create_message_graph, write_graph
from rflx.model import Message, Model, UncheckedModel
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
    if (
        location is None
        or location.source is None
        or location.source.name in [str(BUILTINS_PACKAGE), str(INTERNAL_PACKAGE)]
    ):
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
    document = language_server.workspace.get_text_document(uri)
    lexer = LSLexer(language_server.models[Path(document.path).parent].ls_model)
    lexer.tokenize(document.source, document.path)
    return lexer


class Models:
    def __init__(self, unchecked_model: UncheckedModel) -> None:
        self.unchecked_model = unchecked_model
        self.checked_model = Model()
        self.ls_model = LSModel(unchecked_model)

    def verify(self, cache: Cache, workers: int) -> None:
        self.checked_model = Model()
        self.checked_model = self.unchecked_model.checked(cache, workers=workers)


class RecordFluxLanguageServer(LanguageServer):
    CMD_SHOW_MESSAGE_GRAPH: Final = "showMessageGraph"

    def __init__(self, workers: int = 1) -> None:
        super().__init__("RecordFlux Language Server", __version__)
        self.workers = workers
        self._cache = Cache()
        self._models: dict[Path, Models] = {}
        self._document_state: dict[str, int] = {}
        self._error: error.RecordFluxError

    @property
    def models(self) -> Mapping[Path, Models]:
        return self._models

    def needs_update_for_document(self, document: TextDocumentItem) -> bool:
        return hash(document.text) != self._document_state.get(document.uri, None)

    def update(self, document_uri: str) -> None:
        token = str(uuid.uuid4())
        self.progress.create(token)
        self.progress.begin(
            token,
            WorkDoneProgressBegin(title="RecordFlux Update", percentage=0, cancellable=False),
        )

        document_path = Path(unquote(urlparse(document_uri).path))
        directory = document_path.parent
        workspace_files = self._workspace_files()
        parser = Parser(self._cache, workers=self.workers)

        self._error = error.RecordFluxError()

        for path in workspace_files:
            document = self.workspace.get_text_document(path.as_uri())
            self._document_state[document.uri] = hash(document.source)
            try:
                parser.parse_string(document.source, path)
            except error.RecordFluxError as e:
                self._error.extend(e)

        unchecked_model = parser.create_unchecked_model()

        self._error.extend(unchecked_model.error)

        self._publish_errors_as_diagnostics(self._error)
        self._reset_diagnostics(
            set(workspace_files) - {e.location.source for e in self._error.errors if e.location},
        )
        self._models[directory] = Models(unchecked_model)

        self.progress.end(token, WorkDoneProgressEnd(message="RecordFlux Update Completed"))

    def verify(self, document_uri: str) -> None:
        token = str(uuid.uuid4())
        self.progress.create(token)
        self.progress.begin(
            token,
            WorkDoneProgressBegin(title="RecordFlux Verification", percentage=0, cancellable=True),
        )

        directory = Path(unquote(urlparse(document_uri).path)).parent

        try:
            self._models[directory].verify(self._cache, workers=self.workers)
        except error.RecordFluxError as e:
            self._error.extend(e)

        self._publish_errors_as_diagnostics(self._error)

        self.progress.end(token, WorkDoneProgressEnd(message="RecordFlux Verification Completed"))

    def _publish_errors_as_diagnostics(self, errors: error.RecordFluxError) -> None:
        diagnostics = defaultdict(list)

        for msg in errors.messages:
            location = to_lsp_location(msg.location)
            severity = to_lsp_severity(msg.severity)

            if location is not None:
                diagnostics[location.uri].append(Diagnostic(location.range, msg.message, severity))

        for uri, diag in diagnostics.items():
            self.publish_diagnostics(uri, diag)

    def _reset_diagnostics(self, files: Iterable[Path]) -> None:
        for f in files:
            self.publish_diagnostics(f.as_uri(), [])

    def _workspace_files(self) -> list[Path]:
        workspace_files = [
            file
            for folder_uri in list(self.workspace.folders)
            for file in Path(unquote(urlparse(folder_uri).path)).rglob("*.rflx")
            if file.is_file()
        ]

        if len(self.workspace.folders) == 0:
            for document_uri in self.workspace.documents:
                document_path = Path(unquote(urlparse(document_uri).path))
                workspace_files.append(document_path)

        return workspace_files


# TODO(eng/recordflux/RecordFlux#1424): Use typing.ParamSpec instead of ... and object
def debounce(  # type: ignore[misc]
    interval_s: int,
    keyed_by: Optional[str] = None,
) -> Callable[[Callable[..., None]], Callable[..., None]]:
    """
    Debounce calls to this function until interval_s seconds have passed.

    Based on decorator from https://github.com/python-lsp/python-lsp-server.
    """

    def wrapper(func: Callable[..., None]) -> Callable[..., None]:  # type: ignore[misc]
        timers: dict[object, threading.Timer] = {}
        lock = threading.Lock()

        @functools.wraps(func)
        def debounced(*args: object, **kwargs: object) -> None:
            sig = inspect.signature(func)
            call_args = sig.bind(*args, **kwargs)
            key = call_args.arguments[keyed_by] if keyed_by else None

            def run() -> None:
                with lock:
                    del timers[key]
                return func(*args, **kwargs)

            with lock:
                old_timer = timers.get(key)
                if old_timer:
                    old_timer.cancel()

                timer = threading.Timer(interval_s, run)
                timers[key] = timer
                timer.start()

        return debounced

    return wrapper


@debounce(1, keyed_by="uri")
def update_model_debounced(ls: RecordFluxLanguageServer, uri: str) -> None:
    ls.update(uri)


@debounce(1, keyed_by="uri")
def update_model_and_verify_debounced(ls: RecordFluxLanguageServer, uri: str) -> None:
    ls.update(uri)
    ls.thread_pool_executor.submit(lambda: ls.verify(uri))


server = RecordFluxLanguageServer()


@server.feature(TEXT_DOCUMENT_DID_OPEN)
async def did_open(ls: RecordFluxLanguageServer, params: DidOpenTextDocumentParams) -> None:
    if ls.needs_update_for_document(params.text_document):
        ls.update(params.text_document.uri)
        ls.thread_pool_executor.submit(lambda: ls.verify(params.text_document.uri))


@server.feature(TEXT_DOCUMENT_DID_SAVE)
async def did_save(ls: RecordFluxLanguageServer, params: DidSaveTextDocumentParams) -> None:
    update_model_and_verify_debounced(ls, params.text_document.uri)


@server.feature(TEXT_DOCUMENT_DID_CHANGE)
async def did_change(ls: RecordFluxLanguageServer, params: DidChangeTextDocumentParams) -> None:
    update_model_debounced(ls, params.text_document.uri)


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
async def show_message_graph(ls: RecordFluxLanguageServer, parameters: list[object]) -> None:
    assert isinstance(parameters[0], str)
    assert isinstance(parameters[1], int)

    directory = Path(parameters[0])
    message = ls.models[directory].checked_model.declarations[parameters[1]]

    assert isinstance(message, Message)

    graph_cache = CACHE_PATH / "graphs"
    graph_cache.mkdir(parents=True, exist_ok=True)

    graph = create_message_graph(message)
    write_graph(graph, graph_cache / f"{message.name}.svg")


@server.feature(TEXT_DOCUMENT_CODE_LENS)
async def code_lens(ls: RecordFluxLanguageServer, params: CodeLensParams) -> list[CodeLens]:
    directory = Path(unquote(urlparse(params.text_document.uri).path)).parent

    if directory not in ls.models:
        return []

    result: list[CodeLens] = []

    for index, declaration in enumerate(ls.models[directory].checked_model.declarations):
        if not isinstance(declaration, Message):
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
                    [directory, index],
                ),
            ),
        )

    return result

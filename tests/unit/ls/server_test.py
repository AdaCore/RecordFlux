from __future__ import annotations

from pathlib import Path
from typing import Final, Optional

import pytest
from lsprotocol.types import (
    CodeLens,
    CodeLensParams,
    Command,
    DefinitionParams,
    Diagnostic,
    DiagnosticSeverity,
    Location,
    Position,
    Range,
    SemanticTokensParams,
    TextDocumentIdentifier,
    TextDocumentItem,
    WorkspaceFolder,
)
from pygls.workspace import Workspace

from rflx import error
from rflx.ls import model, server
from rflx.model import Message

DATA_DIR = Path("tests/unit/ls/data")

VALID_LSP_TOKEN_CATEGORIES: Final = {
    "namespace",
    "type",
    "class",
    "enum",
    "interface",
    "struct",
    "typeParameter",
    "parameter",
    "variable",
    "property",
    "enumMember",
    "event",
    "function",
    "method",
    "macro",
    "keyword",
    "modifier",
    "comment",
    "string",
    "number",
    "regexp",
    "operator",
    "decorator",
}


@pytest.fixture()
def language_server() -> server.RecordFluxLanguageServer:
    language_server = server.RecordFluxLanguageServer()
    language_server.lsp.workspace = Workspace(
        DATA_DIR.absolute().as_uri(),
        None,
        workspace_folders=[WorkspaceFolder(DATA_DIR.absolute().as_uri(), "data")],
    )  # type: ignore[no-untyped-call]
    return language_server


def mock_publish_diagnostics(
    result: list[tuple[str, list[Diagnostic]]],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(
        server.RecordFluxLanguageServer,
        "publish_diagnostics",
        lambda self, uri, diagnostics, **kwargs: result.append((uri, diagnostics)),  # noqa: ARG005
    )


def test_lsp_tokens_categories() -> None:
    assert all(
        token_type in VALID_LSP_TOKEN_CATEGORIES for token_type in server.LSP_TOKEN_CATEGORIES
    )

    seen: set[int] = set()

    for token_value in server.LSP_TOKEN_CATEGORIES:
        assert server.LSP_TOKEN_CATEGORIES[token_value] not in seen
        seen.add(server.LSP_TOKEN_CATEGORIES[token_value])


def test_to_lsp_token() -> None:
    assert all(
        category.to_lsp_token() in server.LSP_TOKEN_CATEGORIES
        for category in model.SymbolCategory
        if category != model.SymbolCategory.UNDEFINED
    )
    assert set(server.LSP_TOKEN_CATEGORIES.keys()) == {
        category.to_lsp_token()
        for category in model.SymbolCategory
        if category != model.SymbolCategory.UNDEFINED
    }


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_to_lsp_token_error() -> None:
    with pytest.raises(AssertionError):
        assert model.SymbolCategory.UNDEFINED.to_lsp_token()


def test_to_lsp_location() -> None:
    assert server.to_lsp_location(error.Location((1, 2), Path("test"), (3, 4))) == Location(
        Path("test").absolute().as_uri(),
        Range(Position(0, 1), Position(2, 3)),
    )
    assert server.to_lsp_location(error.Location((1, 2), Path("test"))) == Location(
        Path("test").absolute().as_uri(),
        Range(Position(0, 1), Position(0, 1)),
    )
    assert server.to_lsp_location(None) is None


@pytest.mark.parametrize(
    ("severity", "expected"),
    [
        (error.Severity.ERROR, DiagnosticSeverity.Error),
        (error.Severity.WARNING, DiagnosticSeverity.Warning),
        (error.Severity.INFO, DiagnosticSeverity.Information),
        (error.Severity.NONE, None),
    ],
)
def test_to_lsp_severity(severity: error.Severity, expected: Optional[DiagnosticSeverity]) -> None:
    assert server.to_lsp_severity(severity) == expected


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_to_lsp_severity_error() -> None:
    with pytest.raises(AssertionError):
        assert server.to_lsp_severity(None)  # type: ignore[arg-type]


def test_update_model_directory(tmp_path: Path) -> None:
    (tmp_path / "directory.rflx").mkdir()

    ls = server.RecordFluxLanguageServer()
    ls.lsp.workspace = Workspace(
        tmp_path.absolute().as_uri(),
        None,
        workspace_folders=[WorkspaceFolder(tmp_path.absolute().as_uri(), "tmp_path")],
    )  # type: ignore[no-untyped-call]
    ls.update_model()


def test_update_model_no_folders(tmp_path: Path) -> None:
    document = tmp_path / "test.rflx"
    document.write_text("")

    ls = server.RecordFluxLanguageServer()
    ls.lsp.workspace = Workspace(
        tmp_path.absolute().as_uri(),
        None,
        workspace_folders=[],
    )  # type: ignore[no-untyped-call]
    ls.lsp.workspace.put_document(TextDocumentItem(document.absolute().as_uri(), "", 0, ""))
    ls.update_model()


def test_update_model_error_in_parser(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    published_diagnostics: list[tuple[str, list[Diagnostic]]] = []
    mock_publish_diagnostics(published_diagnostics, monkeypatch)

    document = tmp_path / "test.rflx"
    document.write_text("invalid")

    ls = server.RecordFluxLanguageServer()
    ls.lsp.workspace = Workspace(
        tmp_path.absolute().as_uri(),
        None,
        workspace_folders=[WorkspaceFolder(tmp_path.absolute().as_uri(), "tmp_path")],
    )  # type: ignore[no-untyped-call]
    ls.update_model()

    assert published_diagnostics == [
        (document.absolute().as_uri(), []),
        *(
            3
            * [
                (
                    document.absolute().as_uri(),
                    [
                        Diagnostic(
                            Range(Position(0, 0), Position(0, 7)),
                            'End of input expected, got "Unqualified_Identifier"',
                            DiagnosticSeverity.Error,
                        ),
                    ],
                ),
            ]
        ),
    ]


def test_update_model_error_in_unchecked_model(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    published_diagnostics: list[tuple[str, list[Diagnostic]]] = []
    mock_publish_diagnostics(published_diagnostics, monkeypatch)

    document = tmp_path / "end.rflx"
    document.write_text("package End is end End;")

    ls = server.RecordFluxLanguageServer()
    ls.lsp.workspace = Workspace(
        tmp_path.absolute().as_uri(),
        None,
        workspace_folders=[WorkspaceFolder(tmp_path.absolute().as_uri(), "tmp_path")],
    )  # type: ignore[no-untyped-call]
    ls.update_model()

    assert published_diagnostics == [
        (document.absolute().as_uri(), []),
        *(
            2
            * [
                (
                    document.absolute().as_uri(),
                    [
                        Diagnostic(
                            Range(Position(0, 8), Position(0, 11)),
                            'reserved word "End" used as identifier',
                            DiagnosticSeverity.Error,
                        ),
                    ],
                ),
            ]
        ),
    ]


def test_update_model_error_in_model(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    published_diagnostics: list[tuple[str, list[Diagnostic]]] = []
    mock_publish_diagnostics(published_diagnostics, monkeypatch)

    document = tmp_path / "test.rflx"
    document.write_text(
        "package Test is type T is range 0 .. 2 ** 64 - 1 with Size => 64; end Test;",
    )

    ls = server.RecordFluxLanguageServer()
    ls.lsp.workspace = Workspace(
        tmp_path.absolute().as_uri(),
        None,
        workspace_folders=[WorkspaceFolder(tmp_path.absolute().as_uri(), "tmp_path")],
    )  # type: ignore[no-untyped-call]
    ls.update_model()

    assert published_diagnostics == [
        (document.absolute().as_uri(), []),
        (
            document.absolute().as_uri(),
            [
                Diagnostic(
                    Range(Position(0, 21), Position(0, 64)),
                    'last of "T" exceeds limit (2**63 - 1)',
                    DiagnosticSeverity.Error,
                ),
            ],
        ),
    ]


def test_publish_errors_as_diagnostics(monkeypatch: pytest.MonkeyPatch) -> None:
    published_diagnostics: list[tuple[str, list[Diagnostic]]] = []
    mock_publish_diagnostics(published_diagnostics, monkeypatch)

    ls = server.RecordFluxLanguageServer()
    ls._publish_errors_as_diagnostics(  # noqa: SLF001
        error.RecordFluxError(
            [
                (
                    "foo",
                    error.Subsystem.MODEL,
                    error.Severity.ERROR,
                    error.Location((1, 2), Path("test"), (3, 4)),
                ),
                (
                    "bar",
                    error.Subsystem.MODEL,
                    error.Severity.ERROR,
                    None,
                ),
            ],
        ),
    )

    assert published_diagnostics == [
        (
            Path("test").absolute().as_uri(),
            [
                Diagnostic(
                    Range(Position(0, 1), Position(2, 3)),
                    "foo",
                    DiagnosticSeverity.Error,
                ),
            ],
        ),
    ]


@pytest.mark.asyncio()
async def test_go_to_definition(
    language_server: server.RecordFluxLanguageServer,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    positions: dict[tuple[int, int], Location] = {
        (19, 41): Location(
            (DATA_DIR / "message.rflx").absolute().as_uri(),
            Range(Position(17, 8), Position(17, 88)),
        ),
    }

    for i, position in enumerate(positions):
        params = DefinitionParams(
            TextDocumentIdentifier((DATA_DIR / "message.rflx").absolute().as_uri()),
            Position(position[0], position[1] + (i % 2)),
        )
        assert await server.go_to_definition(language_server, params) == [positions[position]]

    assert (
        await server.go_to_definition(
            language_server,
            DefinitionParams(
                TextDocumentIdentifier((DATA_DIR / "message.rflx").absolute().as_uri()),
                Position(0, 0),
            ),
        )
        is None
    )

    monkeypatch.setattr(server, "to_lsp_location", lambda _: None)

    assert (
        await server.go_to_definition(
            language_server,
            DefinitionParams(
                TextDocumentIdentifier((DATA_DIR / "message.rflx").absolute().as_uri()),
                Position(19, 41),
            ),
        )
        is None
    )


@pytest.mark.asyncio()
async def test_code_lens(language_server: server.RecordFluxLanguageServer) -> None:
    assert await server.code_lens(
        language_server,
        CodeLensParams(TextDocumentIdentifier((DATA_DIR / "message.rflx").absolute().as_uri())),
    ) == [
        CodeLens(
            range=Range(Position(21, 8), Position(32, 17)),
            command=Command(title="Show message graph", command="showMessageGraph", arguments=[8]),
            data=None,
        ),
        CodeLens(
            range=Range(Position(36, 8), Position(76, 17)),
            command=Command(title="Show message graph", command="showMessageGraph", arguments=[10]),
            data=None,
        ),
        CodeLens(
            range=Range(Position(78, 8), Position(82, 17)),
            command=Command(title="Show message graph", command="showMessageGraph", arguments=[11]),
            data=None,
        ),
        CodeLens(
            range=Range(Position(84, 8), Position(90, 43)),
            command=Command(title="Show message graph", command="showMessageGraph", arguments=[12]),
            data=None,
        ),
        CodeLens(
            range=Range(Position(92, 8), Position(97, 43)),
            command=Command(title="Show message graph", command="showMessageGraph", arguments=[13]),
            data=None,
        ),
    ]


@pytest.mark.asyncio()
async def test_show_message_graph(
    language_server: server.RecordFluxLanguageServer,
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    monkeypatch.setattr(server, "CACHE_PATH", tmp_path)

    language_server.update_model()
    index, identifier = next(  # pragma: no branch
        (i, d.identifier.name)
        for i, d in enumerate(language_server.checked_model.declarations)
        if isinstance(d, Message)
    )
    await server.show_message_graph(language_server, [index])

    assert (tmp_path / "graphs" / f"{identifier}.svg").is_file()


@pytest.mark.asyncio()
async def test_semantic_tokens(language_server: server.RecordFluxLanguageServer) -> None:
    params = SemanticTokensParams(
        TextDocumentIdentifier((DATA_DIR / "message.rflx").absolute().as_uri()),
    )
    tokens = await server.semantic_tokens(language_server, params)

    assert tokens.data != [17, 84, 4, 2, 0], "lexer seems to use an empty model"
    assert tokens.data == [
        0,
        8,
        7,
        7,
        0,
        2,
        8,
        12,
        1,
        0,
        0,
        17,
        7,
        2,
        0,
        1,
        25,
        7,
        2,
        0,
        1,
        25,
        8,
        2,
        0,
        1,
        25,
        9,
        2,
        0,
        1,
        25,
        15,
        2,
        0,
        1,
        25,
        10,
        2,
        0,
        1,
        25,
        21,
        2,
        0,
        1,
        25,
        24,
        2,
        0,
        2,
        8,
        6,
        0,
        0,
        2,
        8,
        5,
        0,
        0,
        2,
        8,
        6,
        0,
        0,
        0,
        22,
        5,
        0,
        0,
        2,
        8,
        11,
        1,
        0,
        0,
        16,
        7,
        2,
        0,
        0,
        14,
        7,
        2,
        0,
        0,
        46,
        4,
        2,
        0,
        2,
        8,
        12,
        0,
        0,
        0,
        28,
        11,
        1,
        0,
        2,
        8,
        6,
        3,
        0,
        2,
        9,
        11,
        4,
        0,
        0,
        14,
        11,
        1,
        0,
        2,
        18,
        11,
        4,
        0,
        0,
        14,
        7,
        2,
        0,
        1,
        17,
        6,
        4,
        0,
        1,
        18,
        11,
        4,
        0,
        0,
        14,
        7,
        2,
        0,
        -4,
        23,
        11,
        4,
        0,
        5,
        9,
        6,
        4,
        0,
        0,
        9,
        6,
        0,
        0,
        1,
        17,
        4,
        4,
        0,
        1,
        28,
        6,
        4,
        0,
        -2,
        18,
        6,
        4,
        0,
        3,
        9,
        4,
        4,
        0,
        3,
        8,
        7,
        0,
        0,
        0,
        23,
        6,
        3,
        0,
        2,
        8,
        7,
        7,
        0,
        2,
        9,
        12,
        4,
        0,
        0,
        15,
        12,
        1,
        0,
        2,
        18,
        12,
        4,
        0,
        0,
        15,
        7,
        2,
        0,
        1,
        17,
        4,
        4,
        0,
        1,
        28,
        7,
        7,
        0,
        0,
        15,
        12,
        4,
        0,
        1,
        18,
        12,
        4,
        0,
        0,
        15,
        21,
        2,
        0,
        1,
        17,
        6,
        4,
        0,
        1,
        18,
        12,
        4,
        0,
        0,
        16,
        7,
        2,
        0,
        1,
        22,
        12,
        4,
        0,
        0,
        16,
        21,
        2,
        0,
        1,
        22,
        12,
        4,
        0,
        0,
        16,
        24,
        2,
        0,
        1,
        17,
        7,
        4,
        0,
        1,
        28,
        7,
        7,
        0,
        0,
        15,
        12,
        4,
        0,
        1,
        18,
        12,
        4,
        0,
        0,
        15,
        24,
        2,
        0,
        -12,
        24,
        12,
        4,
        0,
        13,
        9,
        6,
        4,
        0,
        0,
        9,
        6,
        0,
        0,
        1,
        17,
        4,
        4,
        0,
        1,
        28,
        6,
        4,
        0,
        1,
        18,
        12,
        4,
        0,
        0,
        15,
        7,
        2,
        0,
        1,
        17,
        12,
        4,
        0,
        1,
        28,
        6,
        4,
        0,
        1,
        18,
        12,
        4,
        0,
        0,
        15,
        15,
        2,
        0,
        1,
        17,
        7,
        4,
        0,
        1,
        28,
        6,
        4,
        0,
        1,
        18,
        12,
        4,
        0,
        0,
        15,
        10,
        2,
        0,
        1,
        17,
        5,
        4,
        0,
        1,
        18,
        12,
        4,
        0,
        0,
        15,
        8,
        2,
        0,
        1,
        22,
        6,
        4,
        0,
        0,
        9,
        7,
        7,
        0,
        0,
        9,
        5,
        0,
        0,
        1,
        17,
        6,
        4,
        0,
        1,
        28,
        6,
        4,
        0,
        1,
        18,
        12,
        4,
        0,
        0,
        15,
        9,
        2,
        0,
        -15,
        18,
        6,
        4,
        0,
        16,
        9,
        4,
        4,
        0,
        2,
        9,
        12,
        4,
        0,
        0,
        15,
        12,
        0,
        0,
        0,
        0,
        12,
        4,
        0,
        2,
        9,
        7,
        4,
        0,
        0,
        10,
        7,
        0,
        0,
        0,
        0,
        7,
        4,
        0,
        2,
        9,
        5,
        4,
        0,
        0,
        8,
        5,
        0,
        0,
        0,
        0,
        5,
        4,
        0,
        2,
        9,
        6,
        4,
        0,
        0,
        9,
        6,
        0,
        0,
        0,
        0,
        6,
        4,
        0,
        3,
        8,
        3,
        3,
        0,
        2,
        9,
        1,
        4,
        0,
        0,
        4,
        5,
        0,
        0,
        0,
        0,
        5,
        0,
        0,
        1,
        9,
        1,
        4,
        0,
        0,
        4,
        12,
        1,
        0,
        0,
        0,
        12,
        1,
        0,
        3,
        8,
        13,
        3,
        0,
        2,
        9,
        1,
        4,
        0,
        0,
        4,
        3,
        3,
        0,
        0,
        0,
        3,
        3,
        0,
        1,
        9,
        1,
        4,
        0,
        0,
        4,
        12,
        1,
        0,
        0,
        0,
        12,
        1,
        0,
        1,
        9,
        1,
        4,
        0,
        0,
        4,
        7,
        7,
        0,
        0,
        9,
        7,
        7,
        0,
        0,
        -9,
        7,
        7,
        0,
        0,
        9,
        7,
        7,
        0,
        4,
        8,
        6,
        3,
        0,
        2,
        9,
        1,
        4,
        0,
        0,
        4,
        5,
        0,
        0,
        0,
        0,
        5,
        0,
        0,
        1,
        9,
        1,
        4,
        0,
        0,
        4,
        12,
        1,
        0,
        0,
        0,
        12,
        1,
        0,
        4,
        7,
        7,
        7,
        0,
        0,
        9,
        7,
        7,
        0,
        0,
        13,
        4,
        4,
        0,
        0,
        8,
        7,
        7,
        0,
        0,
        9,
        6,
        3,
        0,
        1,
        9,
        12,
        4,
        0,
        0,
        15,
        7,
        2,
        0,
        2,
        7,
        6,
        3,
        0,
        0,
        12,
        4,
        4,
        0,
        0,
        8,
        6,
        3,
        0,
        2,
        4,
        7,
        7,
        0,
    ]

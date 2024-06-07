from __future__ import annotations

import textwrap
from collections.abc import Callable, Sequence
from copy import copy
from pathlib import Path

import pytest

from rflx.expr import Equal, Number
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    BUILTIN_TYPES,
    FINAL,
    INITIAL,
    OPAQUE,
    UNCHECKED_OPAQUE,
    Cache,
    Enumeration,
    Field,
    Integer,
    Link,
    Message,
    Model,
    Session,
    State,
    Transition,
    TypeDecl,
    UncheckedMessage,
    UncheckedModel,
    type_decl,
)
from rflx.model.cache import Digest
from rflx.model.top_level_declaration import TopLevelDeclaration, UncheckedTopLevelDeclaration
from rflx.rapidflux import Location, RecordFluxError
from tests.data import models


def test_illegal_redefinition_of_builtin_type() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:1:2: error: illegal redefinition of built-in type "Boolean"\n'
            r'<stdin>:3:4: error: illegal redefinition of built-in type "Opaque"'
            r"$"
        ),
    ):
        Model(
            [
                BOOLEAN,
                OPAQUE,
                Integer(
                    ID("P::Boolean"),
                    Number(0),
                    Number(255),
                    Number(8),
                    location=Location((1, 2)),
                ),
                Integer(
                    ID("P::Opaque"),
                    Number(0),
                    Number(255),
                    Number(8),
                    location=Location((3, 4)),
                ),
            ],
        )


def test_name_conflict_types() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:11:30: error: name conflict for type "P::T"\n'
            r'<stdin>:10:20: info: previous occurrence of "P::T"'
            r"$"
        ),
    ):
        Model(
            [
                Integer(ID("P::T"), Number(0), Number(255), Number(8), location=Location((10, 20))),
                Integer(ID("P::T"), Number(1), Number(100), Number(8), location=Location((11, 30))),
            ],
        )


def test_conflicting_refinements() -> None:
    r1 = copy(models.refinement())
    r1.location = Location((10, 20))
    r2 = copy(models.refinement())
    r2.location = Location((10, 30))

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:10:30: error: conflicting refinement of "P::M" with "P::M"\n'
            r"<stdin>:10:20: info: previous occurrence of refinement"
            r"$"
        ),
    ):
        Model([models.message(), r1, r2])


def test_name_conflict_sessions() -> None:
    s1 = copy(models.session())
    s1.location = Location((10, 20))
    s2 = copy(models.session())
    s2.location = Location((10, 30))

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:10:30: error: name conflict for session "P::S"\n'
            r'<stdin>:10:20: info: previous occurrence of "P::S"'
            r"$"
        ),
    ):
        Model([s1, s2])


def test_conflicting_literal_builtin_type() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:3:31: error: literal "Boolean" conflicts with type declaration\n'
            r'__BUILTINS__:1:1: info: conflicting type "__BUILTINS__::Boolean"'
            r"$"
        ),
    ):
        Model(
            [
                *BUILTIN_TYPES.values(),
                Enumeration(
                    "P::T",
                    [
                        (ID("E1", Location((3, 27))), Number(1)),
                        (ID("Boolean", Location((3, 31))), Number(2)),
                    ],
                    Number(8),
                    always_valid=False,
                ),
            ],
        )


def test_name_conflict_between_literal_and_type() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:3:27: error: literal "FOO" conflicts with type declaration\n'
            r'<stdin>:4:16: info: conflicting type "P::Foo"\n'
            r'<stdin>:3:32: error: literal "BAR" conflicts with type declaration\n'
            r'<stdin>:5:16: info: conflicting type "P::Bar"'
            r"$"
        ),
    ):
        Model(
            [
                Enumeration(
                    "P::T",
                    [
                        (ID("FOO", Location((3, 27))), Number(1)),
                        (ID("BAR", Location((3, 32))), Number(2)),
                    ],
                    Number(8),
                    always_valid=False,
                ),
                Integer("P::Foo", Number(0), Number(255), Number(8), Location((4, 16))),
                Integer("P::Bar", Number(0), Number(255), Number(8), Location((5, 16))),
            ],
        )


def test_invalid_enumeration_type_builtin_literals() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r"<stdin>:3:16: error: conflicting literals: False, True\n"
            r'__BUILTINS__:1:1: info: previous occurrence of "False"\n'
            r'__BUILTINS__:1:1: info: previous occurrence of "True"'
            r"$"
        ),
    ):
        Model(
            [
                *BUILTIN_TYPES.values(),
                Enumeration(
                    "P::T",
                    [("True", Number(1)), ("False", Number(2))],
                    Number(8),
                    always_valid=False,
                    location=Location((3, 16)),
                ),
            ],
        )


@pytest.mark.parametrize(
    ("types", "model"),
    [
        ([models.tlv_message], models.tlv_model),
        ([models.tlv_with_checksum_message], models.tlv_with_checksum_model),
        ([models.ethernet_frame], models.ethernet_model),
        ([models.enumeration_message], models.enumeration_model),
        ([models.universal_refinement], models.universal_model),
        (
            [
                models.sequence_message,
                models.sequence_messages_message,
                models.sequence_sequence_size_defined_by_message_size,
            ],
            models.sequence_model,
        ),
    ],
)
def test_init_introduce_type_dependencies(
    types: Sequence[Callable[[], TypeDecl]],
    model: Callable[[], Model],
) -> None:
    assert Model([t() for t in types]).types == model().types


def test_invalid_enumeration_type_identical_literals() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r"<stdin>:4:16: error: conflicting literals: Bar\n"
            r'<stdin>:3:33: info: previous occurrence of "Bar"'
            r"$"
        ),
    ):
        Model(
            [
                Enumeration(
                    "P::T1",
                    [("Foo", Number(1)), (ID("Bar", Location((3, 33))), Number(2))],
                    Number(8),
                    always_valid=False,
                ),
                Enumeration(
                    "P::T2",
                    [("Bar", Number(1)), ("Baz", Number(2))],
                    Number(8),
                    always_valid=False,
                    location=Location((4, 16)),
                ),
            ],
        )


def test_write_specification_files(tmp_path: Path) -> None:
    t = Integer("P::T", Number(0), Number(255), Number(8))
    v = type_decl.Sequence("P::V", element_type=t)
    s = Session("P::S", [State("A", [Transition("null")])], [], [], [])
    m = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field("Foo"), location=Location((1, 1))),
            Link(Field("Foo"), FINAL, location=Location((2, 2))),
        ],
        {Field(ID("Foo", location=Location((1, 1)))): t},
    )
    Model([t, v, s, m]).write_specification_files(tmp_path)
    expected_path = tmp_path / Path("p.rflx")
    assert list(tmp_path.glob("*.rflx")) == [expected_path]
    assert expected_path.read_text() == textwrap.dedent(
        """\
        package P is

           type T is range 0 .. 255 with Size => 8;

           type V is sequence of P::T;

           generic
           session S is
           begin
              state A is
              begin
              transition
                 goto null
              end A;
           end S;

           type M is
              message
                 Foo : P::T;
              end message;

        end P;""",
    )


def test_write_specification_files_missing_deps(tmp_path: Path) -> None:
    s = Integer("P::S", Number(0), Number(65535), Number(16))
    t = Integer("P::T", Number(0), Number(255), Number(8))
    v = type_decl.Sequence("P::V", element_type=t)
    m = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field("Foo"), location=Location((1, 1))),
            Link(Field("Foo"), FINAL, location=Location((2, 2))),
        ],
        {Field(ID("Foo", location=Location((1, 1)))): t},
    )
    Model([s, v, m]).write_specification_files(tmp_path)
    expected_path = tmp_path / Path("p.rflx")
    assert list(tmp_path.glob("*.rflx")) == [expected_path]
    assert expected_path.read_text() == textwrap.dedent(
        """\
        package P is

           type S is range 0 .. 65535 with Size => 16;

           type T is range 0 .. 255 with Size => 8;

           type V is sequence of P::T;

           type M is
              message
                 Foo : P::T;
              end message;

        end P;""",
    )


def test_write_specification_file_multiple_packages(tmp_path: Path) -> None:
    t = Integer("P::T", Number(0), Number(255), Number(8))
    u = type_decl.Sequence("Q::U", element_type=t)
    u1 = type_decl.Sequence("Q::U1", element_type=t)
    v = Integer("R::V", Number(0), Number(65535), Number(16))
    links = [
        Link(INITIAL, Field("Victor"), location=Location((1, 1))),
        Link(Field("Victor"), Field("Uniform"), location=Location((2, 2))),
        Link(Field("Uniform"), FINAL, location=Location((3, 3))),
    ]
    fields = {
        Field(ID("Victor", location=Location((1, 1)))): v,
        Field(ID("Uniform", location=Location((2, 2)))): u,
    }
    m = Message(ID("R::M", Location((1, 1))), links, fields)
    Model([t, v, u1, m, u]).write_specification_files(tmp_path)
    p_path, q_path, r_path = (tmp_path / Path(pkg + ".rflx") for pkg in ("p", "q", "r"))
    assert set(tmp_path.glob("*.rflx")) == {p_path, q_path, r_path}
    assert p_path.read_text() == textwrap.dedent(
        """\
        package P is

           type T is range 0 .. 255 with Size => 8;

        end P;""",
    )
    assert q_path.read_text() == textwrap.dedent(
        """\
        with P;

        package Q is

           type U1 is sequence of P::T;

           type U is sequence of P::T;

        end Q;""",
    )
    assert r_path.read_text() == textwrap.dedent(
        """\
        with Q;

        package R is

           type V is range 0 .. 65535 with Size => 16;

           type M is
              message
                 Victor : R::V
                    then Uniform
                       with Size => Message'Last - Victor'Last;
                 Uniform : Q::U;
              end message;

        end R;""",
    )


def test_write_specification_file_multiple_packages_missing_deps(tmp_path: Path) -> None:
    t = Integer("P::T", Number(0), Number(255), Number(8))
    u = type_decl.Sequence("R::U", element_type=t)
    u1 = type_decl.Sequence("Q::U1", element_type=t)
    v = Integer("R::V", Number(0), Number(65535), Number(16))
    links = [
        Link(INITIAL, Field("Victor"), location=Location((1, 1))),
        Link(Field("Victor"), Field("Uniform"), location=Location((2, 2))),
        Link(Field("Uniform"), FINAL, location=Location((3, 3))),
    ]
    fields = {
        Field(ID("Victor", location=Location((1, 1)))): v,
        Field(ID("Uniform", location=Location((2, 2)))): u,
    }
    m = Message(ID("R::M", Location((1, 1))), links, fields)
    Model([u1, m, u, v]).write_specification_files(tmp_path)
    p_path, q_path, r_path = (tmp_path / Path(pkg + ".rflx") for pkg in ("p", "q", "r"))
    assert set(tmp_path.glob("*.rflx")) == {p_path, q_path, r_path}
    assert p_path.read_text() == textwrap.dedent(
        """\
        package P is

           type T is range 0 .. 255 with Size => 8;

        end P;""",
    )
    assert q_path.read_text() == textwrap.dedent(
        """\
        with P;

        package Q is

           type U1 is sequence of P::T;

        end Q;""",
    )
    assert r_path.read_text() == textwrap.dedent(
        """\
        with P;

        package R is

           type V is range 0 .. 65535 with Size => 16;

           type U is sequence of P::T;

           type M is
              message
                 Victor : R::V
                    then Uniform
                       with Size => Message'Last - Victor'Last;
                 Uniform : R::U;
              end message;

        end R;""",
    )


def test_write_specification_files_line_too_long(tmp_path: Path) -> None:
    t = Integer("P::" + "T" * 120, Number(0), Number(255), Number(8))
    Model([t]).write_specification_files(tmp_path)
    expected_path = tmp_path / Path("p.rflx")
    assert list(tmp_path.glob("*.rflx")) == [expected_path]
    assert expected_path.read_text().startswith("-- style: disable = line-length\n\npackage P is")


@pytest.mark.parametrize(
    ("unchecked", "expected"),
    [
        ([], []),
        (
            [
                type_decl.UncheckedInteger(
                    ID("P::T"),
                    Number(0),
                    Number(128),
                    Number(8),
                    Location((1, 2)),
                ),
            ],
            [
                lambda: type_decl.Integer(
                    ID("P::T"),
                    Number(0),
                    Number(128),
                    Number(8),
                    Location((1, 2)),
                ),
            ],
        ),
        (
            [
                type_decl.UncheckedInteger(
                    ID("P::I"),
                    Number(0),
                    Number(128),
                    Number(8),
                    Location((1, 2)),
                ),
                type_decl.UncheckedEnumeration(
                    ID("P::E"),
                    [(ID("A"), Number(0)), (ID("B"), Number(1))],
                    Number(8),
                    always_valid=False,
                    location=Location((1, 2)),
                ),
            ],
            [
                lambda: type_decl.Integer(
                    ID("P::I"),
                    Number(0),
                    Number(128),
                    Number(8),
                    Location((1, 2)),
                ),
                lambda: type_decl.Enumeration(
                    ID("P::E"),
                    [(ID("A"), Number(0)), (ID("B"), Number(1))],
                    Number(8),
                    always_valid=False,
                    location=Location((1, 2)),
                ),
            ],
        ),
        (
            [
                UNCHECKED_OPAQUE,
                UncheckedMessage(
                    ID("P::M", Location((1, 1))),
                    [
                        Link(
                            INITIAL,
                            Field("F"),
                            size=Number(16, location=Location((1, 1))),
                            location=Location((1, 1)),
                        ),
                        Link(Field("F"), FINAL, location=Location((2, 2))),
                    ],
                    [],
                    [
                        (Field(ID("F", location=Location((1, 1)))), OPAQUE.identifier, []),
                    ],
                    None,
                    None,
                    None,
                ),
            ],
            [
                lambda: OPAQUE,
                lambda: Message(
                    ID("P::M", Location((1, 1))),
                    [
                        Link(
                            INITIAL,
                            Field("F"),
                            size=Number(16, location=Location((1, 1))),
                            location=Location((1, 1)),
                        ),
                        Link(Field("F"), FINAL, location=Location((2, 2))),
                    ],
                    {
                        Field(ID("F", location=Location((1, 1)))): OPAQUE,
                    },
                ),
            ],
        ),
    ],
)
def test_unchecked_model_checked(
    unchecked: list[UncheckedTopLevelDeclaration],
    expected: list[Callable[[], TopLevelDeclaration]],
    tmp_path: Path,
) -> None:
    cache = Cache(tmp_path / "test.json")

    declarations = [d() for d in expected]

    assert UncheckedModel(unchecked, RecordFluxError()).checked(cache=cache) == Model(declarations)

    messages = [d for d in declarations if isinstance(d, Message)]
    if messages:
        for d in messages:
            cache.is_verified(Digest(d))
    else:
        assert not cache._verified  # noqa: SLF001


@pytest.mark.parametrize(
    ("unchecked", "expected", "expect_cached"),
    [
        (
            [
                type_decl.UncheckedInteger(
                    ID("P::T"),
                    Number(0),
                    Number(128),
                    Number(2),
                    Location((1, 2)),
                ),
            ],
            r'^<stdin>:1:2: error: size of "T" too small$',
            [],
        ),
        (
            [
                UNCHECKED_OPAQUE,
                type_decl.UncheckedInteger(
                    ID("P::I"),
                    Number(0),
                    Number(128),
                    Number(2),
                    Location((1, 2)),
                ),
                type_decl.UncheckedEnumeration(
                    ID("E", Location((3, 4))),
                    [(ID("A"), Number(0)), (ID("B"), Number(1))],
                    Number(8),
                    always_valid=False,
                    location=Location((2, 3)),
                ),
                UncheckedMessage(
                    ID("P::M", Location((1, 1))),
                    [
                        Link(
                            INITIAL,
                            Field("F1"),
                            size=Number(16, location=Location((1, 1))),
                            location=Location((1, 1)),
                        ),
                        Link(
                            Field(ID("F1", Location((5, 6)))),
                            FINAL,
                            condition=Equal(Number(1), Number(1)),
                            location=Location((5, 5)),
                        ),
                    ],
                    [],
                    [
                        (
                            Field(ID("F1", location=Location((1, 1)))),
                            UNCHECKED_OPAQUE.identifier,
                            [],
                        ),
                    ],
                    None,
                    None,
                    None,
                ),
            ],
            r"^"
            r'<stdin>:1:2: error: size of "I" too small\n'
            r'<stdin>:3:4: error: invalid format for identifier "E"\n'
            r'<stdin>:5:6: error: condition "1 = 1" on transition "F1" -> "Final"'
            r" is always true"
            r"$",
            [],
        ),
        (
            [
                type_decl.UncheckedInteger(
                    ID("P::T", Location((1, 1))),
                    Number(0),
                    Number(255),
                    Number(8),
                    Location((1, 2)),
                ),
                UncheckedMessage(
                    ID("P::T", Location((2, 1))),
                    [
                        Link(
                            INITIAL,
                            Field("F"),
                            location=Location((1, 1)),
                        ),
                        Link(
                            Field(ID("F", Location((3, 1)))),
                            FINAL,
                            location=Location((1, 1)),
                        ),
                    ],
                    [],
                    [
                        (
                            Field(ID("F", location=Location((1, 1)))),
                            ID("P::T", location=Location((2, 2))),
                            [],
                        ),
                    ],
                    None,
                    None,
                    location=((2, 10)),
                ),
                UncheckedMessage(
                    ID("P::M", Location((4, 1))),
                    [
                        Link(
                            INITIAL,
                            Field("F"),
                            location=Location((1, 1)),
                        ),
                        Link(
                            Field(ID("F", Location((5, 1)))),
                            FINAL,
                            location=Location((2, 2)),
                        ),
                    ],
                    [],
                    [
                        (
                            Field(ID("F", location=Location((1, 1)))),
                            ID("P::T", location=Location((2, 2))),
                            [],
                        ),
                    ],
                    None,
                    None,
                    None,
                ),
            ],
            r"^"
            r'<stdin>:2:1: error: duplicate declaration of "P::T"\n'
            r'<stdin>:1:2: info: previous occurrence of "P::T"'
            r"$",
            ["P::M"],
        ),
    ],
)
def test_unchecked_model_checked_error(
    unchecked: list[UncheckedTopLevelDeclaration],
    expected: str,
    expect_cached: list[str],
    tmp_path: Path,
) -> None:
    cache = Cache(tmp_path / "test.json")

    with pytest.raises(RecordFluxError, match=expected):
        UncheckedModel(unchecked, RecordFluxError()).checked(cache=cache)

    assert list(cache._verified) == expect_cached  # noqa: SLF001

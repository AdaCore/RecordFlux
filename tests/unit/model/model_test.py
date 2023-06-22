from __future__ import annotations

import textwrap
from collections.abc import Sequence
from copy import copy
from pathlib import Path

import pytest

import rflx.model.type_ as mty
from rflx.error import Location, RecordFluxError
from rflx.expression import Equal, Number
from rflx.identifier import ID
from rflx.model import (
    BUILTIN_TYPES,
    FINAL,
    INITIAL,
    OPAQUE,
    UNCHECKED_OPAQUE,
    AbstractMessage,
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
    Type,
    UncheckedMessage,
    UncheckedModel,
)
from rflx.model.top_level_declaration import TopLevelDeclaration, UncheckedTopLevelDeclaration
from tests.data import models


def test_name_conflict_types() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:11:30: model: error: name conflict for type "P::T"\n'
            r'<stdin>:10:20: model: info: previous occurrence of "P::T"'
            r"$"
        ),
    ):
        Model(
            [
                Integer(ID("P::T"), Number(0), Number(255), Number(8), location=Location((10, 20))),
                Integer(ID("P::T"), Number(1), Number(100), Number(8), location=Location((11, 30))),
            ]
        )


def test_conflicting_refinements() -> None:
    r1 = copy(models.REFINEMENT)
    r1.location = Location((10, 20))
    r2 = copy(models.REFINEMENT)
    r2.location = Location((10, 30))

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:10:30: model: error: conflicting refinement of "P::M" with "P::M"\n'
            r"<stdin>:10:20: model: info: previous occurrence of refinement"
            r"$"
        ),
    ):
        Model([models.MESSAGE, r1, r2]),


def test_name_conflict_sessions() -> None:
    s1 = copy(models.SESSION)
    s1.location = Location((10, 20))
    s2 = copy(models.SESSION)
    s2.location = Location((10, 30))

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:10:30: model: error: name conflict for session "P::S"\n'
            r'<stdin>:10:20: model: info: previous occurrence of "P::S"'
            r"$"
        ),
    ):
        Model([s1, s2])


def test_conflicting_literal_builtin_type() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:3:31: model: error: literal "Boolean" conflicts with type declaration\n'
            r'__BUILTINS__:0:0: model: info: conflicting type "__BUILTINS__::Boolean"'
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
            ]
        )


def test_name_conflict_between_literal_and_type() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:3:27: model: error: literal "FOO" conflicts with type declaration\n'
            r'<stdin>:4:16: model: info: conflicting type "P::Foo"\n'
            r'<stdin>:3:32: model: error: literal "BAR" conflicts with type declaration\n'
            r'<stdin>:5:16: model: info: conflicting type "P::Bar"'
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
            ]
        )


def test_invalid_enumeration_type_builtin_literals() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r"<stdin>:3:16: model: error: conflicting literals: False, True\n"
            r'__BUILTINS__:0:0: model: info: previous occurrence of "False"\n'
            r'__BUILTINS__:0:0: model: info: previous occurrence of "True"'
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
            ]
        )


@pytest.mark.parametrize(
    "types,model",
    [
        ([models.TLV_MESSAGE], models.TLV_MODEL),
        ([models.TLV_WITH_CHECKSUM_MESSAGE], models.TLV_WITH_CHECKSUM_MODEL),
        ([models.ETHERNET_FRAME], models.ETHERNET_MODEL),
        ([models.ENUMERATION_MESSAGE], models.ENUMERATION_MODEL),
        ([models.UNIVERSAL_REFINEMENT], models.UNIVERSAL_MODEL),
        (
            [
                models.SEQUENCE_MESSAGE,
                models.SEQUENCE_MESSAGES_MESSAGE,
                models.SEQUENCE_SEQUENCE_SIZE_DEFINED_BY_MESSAGE_SIZE,
            ],
            models.SEQUENCE_MODEL,
        ),
    ],
)
def test_init_introduce_type_dependencies(types: Sequence[Type], model: Model) -> None:
    assert Model(types).types == model.types


def test_invalid_enumeration_type_identical_literals() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r"<stdin>:4:16: model: error: conflicting literals: Bar\n"
            r'<stdin>:3:33: model: info: previous occurrence of "Bar"'
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
            ]
        )


def test_write_specification_files(tmp_path: Path) -> None:
    t = Integer("P::T", Number(0), Number(255), Number(8))
    v = mty.Sequence("P::V", element_type=t)
    s = Session("P::S", [State("A", [Transition("null")])], [], [], [])
    m = Message("P::M", [Link(INITIAL, Field("Foo")), Link(Field("Foo"), FINAL)], {Field("Foo"): t})
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

        end P;"""
    )


def test_write_specification_files_missing_deps(tmp_path: Path) -> None:
    s = Integer("P::S", Number(0), Number(65535), Number(16))
    t = Integer("P::T", Number(0), Number(255), Number(8))
    v = mty.Sequence("P::V", element_type=t)
    m = Message("P::M", [Link(INITIAL, Field("Foo")), Link(Field("Foo"), FINAL)], {Field("Foo"): t})
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

        end P;"""
    )


def test_write_specification_file_multiple_packages(tmp_path: Path) -> None:
    t = Integer("P::T", Number(0), Number(255), Number(8))
    u = mty.Sequence("Q::U", element_type=t)
    u1 = mty.Sequence("Q::U1", element_type=t)
    v = Integer("R::V", Number(0), Number(65535), Number(16))
    links = [
        Link(INITIAL, Field("Victor")),
        Link(Field("Victor"), Field("Uniform")),
        Link(Field("Uniform"), FINAL),
    ]
    fields = {Field("Victor"): v, Field("Uniform"): u}
    m = Message("R::M", links, fields)
    Model([t, v, u1, m, u]).write_specification_files(tmp_path)
    p_path, q_path, r_path = (tmp_path / Path(pkg + ".rflx") for pkg in ("p", "q", "r"))
    assert set(tmp_path.glob("*.rflx")) == {p_path, q_path, r_path}
    assert p_path.read_text() == textwrap.dedent(
        """\
        package P is

           type T is range 0 .. 255 with Size => 8;

        end P;"""
    )
    assert q_path.read_text() == textwrap.dedent(
        """\
        with P;

        package Q is

           type U1 is sequence of P::T;

           type U is sequence of P::T;

        end Q;"""
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

        end R;"""
    )


def test_write_specification_file_multiple_packages_missing_deps(tmp_path: Path) -> None:
    t = Integer("P::T", Number(0), Number(255), Number(8))
    u = mty.Sequence("R::U", element_type=t)
    u1 = mty.Sequence("Q::U1", element_type=t)
    v = Integer("R::V", Number(0), Number(65535), Number(16))
    links = [
        Link(INITIAL, Field("Victor")),
        Link(Field("Victor"), Field("Uniform")),
        Link(Field("Uniform"), FINAL),
    ]
    fields = {Field("Victor"): v, Field("Uniform"): u}
    m = Message("R::M", links, fields)
    Model([u1, m, u, v]).write_specification_files(tmp_path)
    p_path, q_path, r_path = (tmp_path / Path(pkg + ".rflx") for pkg in ("p", "q", "r"))
    assert set(tmp_path.glob("*.rflx")) == {p_path, q_path, r_path}
    assert p_path.read_text() == textwrap.dedent(
        """\
        package P is

           type T is range 0 .. 255 with Size => 8;

        end P;"""
    )
    assert q_path.read_text() == textwrap.dedent(
        """\
        with P;

        package Q is

           type U1 is sequence of P::T;

        end Q;"""
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

        end R;"""
    )


def test_write_specification_files_line_too_long(tmp_path: Path) -> None:
    t = Integer("P::" + "T" * 120, Number(0), Number(255), Number(8))
    Model([t]).write_specification_files(tmp_path)
    expected_path = tmp_path / Path("p.rflx")
    assert list(tmp_path.glob("*.rflx")) == [expected_path]
    assert expected_path.read_text().startswith("-- style: disable = line-length\n\npackage P is")


@pytest.mark.parametrize(
    ["unchecked", "expected"],
    [
        ([], []),
        (
            [mty.UncheckedInteger(ID("P::T"), Number(0), Number(128), Number(8), Location((1, 2)))],
            [mty.Integer(ID("P::T"), Number(0), Number(128), Number(8), Location((1, 2)))],
        ),
        (
            [
                mty.UncheckedInteger(
                    ID("P::I"),
                    Number(0),
                    Number(128),
                    Number(8),
                    Location((1, 2)),
                ),
                mty.UncheckedEnumeration(
                    ID("P::E"),
                    [(ID("A"), Number(0)), (ID("B"), Number(1))],
                    Number(8),
                    always_valid=False,
                    location=Location((1, 2)),
                ),
            ],
            [
                mty.Integer(
                    ID("P::I"),
                    Number(0),
                    Number(128),
                    Number(8),
                    Location((1, 2)),
                ),
                mty.Enumeration(
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
                    ID("P::M"),
                    [
                        Link(INITIAL, Field("F"), size=Number(16)),
                        Link(Field("F"), FINAL),
                    ],
                    [],
                    [
                        (Field("F"), OPAQUE.identifier, []),
                    ],
                    None,
                    None,
                    None,
                ),
            ],
            [
                OPAQUE,
                Message(
                    "P::M",
                    [
                        Link(INITIAL, Field("F"), size=Number(16)),
                        Link(Field("F"), FINAL),
                    ],
                    {
                        Field("F"): OPAQUE,
                    },
                ),
            ],
        ),
    ],
)
def test_unchecked_model_checked(
    unchecked: list[UncheckedTopLevelDeclaration],
    expected: list[TopLevelDeclaration],
    tmp_path: Path,
) -> None:
    cache = Cache(tmp_path / "test.json")

    assert UncheckedModel(unchecked, RecordFluxError()).checked(cache=cache) == Model(expected)

    messages = [d for d in expected if isinstance(d, AbstractMessage)]
    if messages:
        for d in messages:
            cache.is_verified(d)
    else:
        assert not cache._verified


@pytest.mark.parametrize(
    ["unchecked", "expected"],
    [
        (
            [mty.UncheckedInteger(ID("P::T"), Number(0), Number(128), Number(2), Location((1, 2)))],
            r'^<stdin>:1:2: model: error: size of "T" too small$',
        ),
        (
            [
                UNCHECKED_OPAQUE,
                mty.UncheckedInteger(
                    ID("P::I"), Number(0), Number(128), Number(2), Location((1, 2))
                ),
                mty.UncheckedEnumeration(
                    ID("E", Location((3, 4))),
                    [(ID("A"), Number(0)), (ID("B"), Number(1))],
                    Number(8),
                    always_valid=False,
                    location=Location((2, 3)),
                ),
                UncheckedMessage(
                    ID("P::M"),
                    [
                        Link(
                            INITIAL,
                            Field("F1"),
                            size=Number(16),
                        ),
                        Link(
                            Field(ID("F1", Location((5, 6)))),
                            FINAL,
                            condition=Equal(Number(1), Number(1)),
                        ),
                    ],
                    [],
                    [
                        (Field("F1"), UNCHECKED_OPAQUE.identifier, []),
                    ],
                    None,
                    None,
                    None,
                ),
            ],
            r"^"
            r'<stdin>:1:2: model: error: size of "I" too small\n'
            r'<stdin>:3:4: model: error: invalid format for identifier "E"\n'
            r'<stdin>:5:6: model: error: condition "1 = 1" on transition "F1" -> "Final"'
            r" is always true"
            r"$",
        ),
    ],
)
def test_unchecked_model_checked_error(
    unchecked: list[UncheckedTopLevelDeclaration], expected: str, tmp_path: Path
) -> None:
    cache = Cache(tmp_path / "test.json")

    with pytest.raises(RecordFluxError, match=expected):
        UncheckedModel(unchecked, RecordFluxError()).checked(cache=cache)

    assert not cache._verified

import itertools
from typing import Mapping, Sequence

from rflx.ada import (
    Assignment,
    CallStatement,
    Declaration,
    ExpressionFunctionDeclaration,
    FunctionSpecification,
    ObjectDeclaration,
    PragmaStatement,
    Statement,
)
from rflx.expression import (
    TRUE,
    UNDEFINED,
    Add,
    Aggregate,
    And,
    AndThen,
    Call,
    Div,
    Equal,
    Expr,
    First,
    ForAllIn,
    GreaterEqual,
    If,
    Indexed,
    Last,
    Length,
    LessEqual,
    Mod,
    Name,
    NamedAggregate,
    NotEqual,
    Number,
    Or,
    Selected,
    Size,
    Sub,
    ValueRange,
    Variable,
)
from rflx.model import (
    FINAL,
    INITIAL,
    Enumeration,
    Field,
    Link,
    Message,
    ModularInteger,
    RangeInteger,
    Scalar,
    Type,
)

from .types import Types

NULL = Name("null")
VALID_CONTEXT = Call("Valid_Context", [Name("Ctx")])  # WORKAROUND: Componolit/Workarounds#1


class GeneratorCommon:
    def __init__(self, prefix: str = "") -> None:
        self.types = Types(prefix)

    def substitution(self, message: Message, prefix: bool = True) -> Mapping[Name, Expr]:
        def prefixed(name: str) -> Expr:
            return Selected(Name("Ctx"), name) if prefix else Name(name)

        first = prefixed("First")
        last = prefixed("Last")
        cursors = prefixed("Cursors")

        return {
            **{First("Message"): first},
            **{Last("Message"): last},
            **{
                First(f.name): Selected(Indexed(cursors, Name(f.affixed_name)), "First")
                for f in message.fields
            },
            **{
                Last(f.name): Selected(Indexed(cursors, Name(f.affixed_name)), "Last")
                for f in message.fields
            },
            **{
                Length(f.name): Add(
                    Sub(
                        Selected(Indexed(cursors, Name(f.affixed_name)), "Last"),
                        Selected(Indexed(cursors, Name(f.affixed_name)), "First"),
                    ),
                    Number(1),
                )
                for f in message.fields
            },
            **{
                Variable(f.name): Call(
                    self.types.bit_length,
                    [Selected(Indexed(cursors, Name(f.affixed_name)), f"Value.{f.name}_Value")],
                )
                for f, t in message.types.items()
                if not isinstance(t, Enumeration)
            },
            **{
                Variable(f.name): Call(
                    self.types.bit_length,
                    [Selected(Indexed(cursors, Name(f.affixed_name)), f"Value.{f.name}_Value")],
                )
                for f, t in message.types.items()
                if isinstance(t, Enumeration)
            },
            **{
                Variable(l): Call(self.types.bit_length, [Call("Convert", [Name(l)])])
                for l in itertools.chain.from_iterable(
                    t.literals.keys() for t in message.types.values() if isinstance(t, Enumeration)
                )
            },
        }

    def public_substitution(self, message: Message) -> Mapping[Name, Expr]:
        return {
            **{First("Message"): Selected(Name("Ctx"), "First")},
            **{Last("Message"): Selected(Name("Ctx"), "Last")},
            **{
                First(f.name): Call("Field_First", [Name("Ctx"), Name(f.affixed_name)])
                for f in message.fields
            },
            **{
                Last(f.name): Call("Field_Last", [Name("Ctx"), Name(f.affixed_name)])
                for f in message.fields
            },
            **{
                Length(f.name): Call("Field_Length", [Name("Ctx"), Name(f.affixed_name)])
                for f in message.fields
            },
            **{
                Variable(f.name): Call(
                    self.types.bit_length, [Call(f"Get_{f.name}", [Name("Ctx")])]
                )
                for f, t in message.types.items()
                if not isinstance(t, Enumeration)
            },
            **{
                Variable(f.name): Call(
                    self.types.bit_length, [Call("Convert", [Call(f"Get_{f.name}", [Name("Ctx")])])]
                )
                for f, t in message.types.items()
                if isinstance(t, Enumeration)
            },
            **{
                Variable(l): Call(self.types.bit_length, [Call("Convert", [Name(l)])])
                for l in itertools.chain.from_iterable(
                    t.literals.keys() for t in message.types.values() if isinstance(t, Enumeration)
                )
            },
        }

    def message_structure_invariant(
        self, message: Message, link: Link = None, prefix: bool = True
    ) -> Expr:
        def prefixed(name: str) -> Expr:
            return Selected(Name("Ctx"), name) if prefix else Name(name)

        if not link:
            return self.message_structure_invariant(message, message.outgoing(INITIAL)[0], prefix)

        source = link.source
        target = link.target

        if target is FINAL:
            return TRUE

        field_type = message.types[target]
        condition = link.condition.simplified(self.substitution(message, prefix))
        length = (
            Size(base_type_name(field_type))
            if isinstance(field_type, Scalar)
            else link.length.simplified(self.substitution(message, prefix))
        )
        first = (
            Name(prefixed("First"))
            if source == INITIAL
            else link.first.simplified(
                {
                    **self.substitution(message, prefix),
                    **{
                        UNDEFINED: Add(
                            Selected(
                                Indexed(prefixed("Cursors"), Name(source.affixed_name)), "Last"
                            ),
                            Number(1),
                        )
                    },
                }
            )
        )

        return If(
            [
                (
                    AndThen(
                        Call(
                            "Structural_Valid",
                            [Indexed(prefixed("Cursors"), Name(target.affixed_name))],
                        ),
                        condition,
                    ),
                    AndThen(
                        Equal(
                            Add(
                                Sub(
                                    Selected(
                                        Indexed(prefixed("Cursors"), Name(target.affixed_name)),
                                        "Last",
                                    ),
                                    Selected(
                                        Indexed(prefixed("Cursors"), Name(target.affixed_name)),
                                        "First",
                                    ),
                                ),
                                Number(1),
                            ),
                            length,
                        ),
                        Equal(
                            Selected(
                                Indexed(prefixed("Cursors"), Name(target.affixed_name)),
                                "Predecessor",
                            ),
                            Name(source.affixed_name),
                        ),
                        Equal(
                            Selected(
                                Indexed(prefixed("Cursors"), Name(target.affixed_name)), "First"
                            ),
                            first,
                        ),
                        *[
                            self.message_structure_invariant(message, l, prefix)
                            for l in message.outgoing(target)
                        ],
                    ),
                )
            ]
        ).simplified()

    def context_predicate(self, message: Message, composite_fields: Sequence[Field]) -> Expr:
        def valid_predecessors_invariant() -> Expr:
            return AndThen(
                *[
                    If(
                        [
                            (
                                Call(
                                    "Structural_Valid", [Indexed("Cursors", Name(f.affixed_name))]
                                ),
                                Or(
                                    *[
                                        AndThen(
                                            Call(
                                                "Structural_Valid"
                                                if l.source in composite_fields
                                                else "Valid",
                                                [Indexed("Cursors", Name(l.source.affixed_name))],
                                            ),
                                            Equal(
                                                Selected(
                                                    Indexed("Cursors", Name(f.affixed_name)),
                                                    "Predecessor",
                                                ),
                                                Name(l.source.affixed_name),
                                            ),
                                            l.condition,
                                        ).simplified(self.substitution(message, False))
                                        for l in message.incoming(f)
                                    ]
                                ),
                            )
                        ]
                    )
                    for f in message.fields
                    if f not in message.direct_successors(INITIAL)
                ]
            )

        def invalid_successors_invariant() -> Expr:
            return AndThen(
                *[
                    If(
                        [
                            (
                                AndThen(
                                    *[
                                        Call("Invalid", [Indexed("Cursors", Name(p.affixed_name))])
                                        for p in message.direct_predecessors(f)
                                    ]
                                ),
                                Call("Invalid", [Indexed("Cursors", Name(f.affixed_name))]),
                            )
                        ]
                    )
                    for f in message.fields
                    if f not in message.direct_successors(INITIAL)
                ]
            )

        return AndThen(
            If(
                [
                    (
                        NotEqual(Name(Name("Buffer")), NULL),
                        And(
                            Equal(First(Name("Buffer")), Name(Name("Buffer_First"))),
                            Equal(Last(Name("Buffer")), Name(Name("Buffer_Last"))),
                        ),
                    )
                ]
            ),
            GreaterEqual(
                Call(self.types.byte_index, [Name(Name("First"))]), Name(Name("Buffer_First"))
            ),
            LessEqual(Call(self.types.byte_index, [Name(Name("Last"))]), Name(Name("Buffer_Last"))),
            LessEqual(Name(Name("First")), Name(Name("Last"))),
            LessEqual(Name(Name("Last")), Div(Last(self.types.bit_index), Number(2))),
            ForAllIn(
                "F",
                ValueRange(First("Field"), Last("Field")),
                If(
                    [
                        (
                            Call("Structural_Valid", [Indexed(Name("Cursors"), Name("F"))]),
                            And(
                                GreaterEqual(
                                    Selected(Indexed(Name("Cursors"), Name("F")), "First"),
                                    Name(Name("First")),
                                ),
                                LessEqual(
                                    Selected(Indexed(Name("Cursors"), Name("F")), "Last"),
                                    Name(Name("Last")),
                                ),
                                LessEqual(
                                    Selected(Indexed(Name("Cursors"), Name("F")), "First"),
                                    Add(
                                        Selected(Indexed(Name("Cursors"), Name("F")), "Last"),
                                        Number(1),
                                    ),
                                ),
                                Equal(
                                    Selected(
                                        Selected(Indexed(Name("Cursors"), Name("F")), "Value"),
                                        "Fld",
                                    ),
                                    Name("F"),
                                ),
                            ),
                        )
                    ]
                ),
            ),
            valid_predecessors_invariant(),
            invalid_successors_invariant(),
            self.message_structure_invariant(message, prefix=False),
        )

    def valid_path_to_next_field_condition(
        self, message: Message, field: Field, field_type: Type
    ) -> Sequence[Expr]:
        return [
            If(
                [
                    (
                        l.condition,
                        And(
                            Equal(
                                Call("Predecessor", [Name("Ctx"), Name(l.target.affixed_name)],),
                                Name(field.affixed_name),
                            ),
                            Call("Valid_Next", [Name("Ctx"), Name(l.target.affixed_name)])
                            if l.target != FINAL
                            else TRUE,
                        ),
                    )
                ]
            ).simplified(
                {
                    **{
                        Variable(field.name): Call("Convert", [Name("Value")])
                        if isinstance(field_type, Enumeration) and field_type.always_valid
                        else Name("Value")
                    },
                    **self.public_substitution(message),
                }
            )
            for l in message.outgoing(field)
            if l.target != FINAL
        ]

    @staticmethod
    def sufficient_space_for_field_condition(field_name: Name) -> Expr:
        return GreaterEqual(
            Call("Available_Space", [Name("Ctx"), field_name]),
            Call("Field_Length", [Name("Ctx"), field_name]),
        )

    def initialize_field_statements(self, message: Message, field: Field) -> Sequence[Statement]:
        return [
            CallStatement("Reset_Dependent_Fields", [Name("Ctx"), Name(field.affixed_name)],),
            Assignment(
                "Ctx",
                Aggregate(
                    Selected("Ctx", "Buffer_First"),
                    Selected("Ctx", "Buffer_Last"),
                    Selected("Ctx", "First"),
                    Name("Last"),
                    Selected("Ctx", "Buffer"),
                    Selected("Ctx", "Cursors"),
                ),
            ),
            # WORKAROUND:
            # Limitation of GNAT Community 2019 / SPARK Pro 20.0
            # Provability of predicate is increased by adding part of
            # predicate as assert
            PragmaStatement(
                "Assert", [str(self.message_structure_invariant(message, prefix=True))],
            ),
            Assignment(
                Indexed(Selected("Ctx", "Cursors"), Name(field.affixed_name)),
                NamedAggregate(
                    ("State", Name("S_Structural_Valid")),
                    ("First", Name("First")),
                    ("Last", Name("Last")),
                    ("Value", NamedAggregate(("Fld", Name(field.affixed_name))),),
                    (
                        "Predecessor",
                        Selected(
                            Indexed(Selected("Ctx", "Cursors"), Name(field.affixed_name),),
                            "Predecessor",
                        ),
                    ),
                ),
            ),
            Assignment(
                Indexed(
                    Selected("Ctx", "Cursors"),
                    Call("Successor", [Name("Ctx"), Name(field.affixed_name)]),
                ),
                NamedAggregate(
                    ("State", Name("S_Invalid")), ("Predecessor", Name(field.affixed_name)),
                ),
            ),
        ]

    def field_bit_location_declarations(self, field_name: Name) -> Sequence[Declaration]:
        return [
            ObjectDeclaration(
                ["First"],
                self.types.bit_index,
                Call("Field_First", [Name("Ctx"), field_name]),
                True,
            ),
            ObjectDeclaration(
                ["Last"], self.types.bit_index, Call("Field_Last", [Name("Ctx"), field_name]), True,
            ),
        ]

    def field_byte_location_declarations(self) -> Sequence[Declaration]:
        return [
            ExpressionFunctionDeclaration(
                FunctionSpecification("Buffer_First", self.types.index),
                Call(self.types.byte_index, [Name("First")]),
            ),
            ExpressionFunctionDeclaration(
                FunctionSpecification("Buffer_Last", self.types.index),
                Call(self.types.byte_index, [Name("Last")]),
            ),
            ExpressionFunctionDeclaration(
                FunctionSpecification("Offset", self.types.offset),
                Call(
                    self.types.offset,
                    [Mod(Sub(Number(8), Mod(Name("Last"), Number(8))), Number(8))],
                ),
            ),
        ]


def base_type_name(scalar_type: Scalar) -> str:
    if isinstance(scalar_type, (RangeInteger, Enumeration)):
        return scalar_type.full_base_name
    if isinstance(scalar_type, ModularInteger):
        return scalar_type.full_name
    assert False, 'unexpected scalar type "{type(scalar_type)}"'
    return None


def sequence_name(message: Message, field: Field) -> str:
    return f"{message.types[field].name}_Sequence"


def length_dependent_condition(message: Message) -> bool:
    return any(
        [
            isinstance(v.name, str) and v.name.endswith("'Length")
            for l in message.structure
            for v in l.condition.variables(True)
        ]
    )

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
from rflx.identifier import ID
from rflx.model import (
    BUILTINS_PACKAGE,
    FINAL,
    INITIAL,
    Enumeration,
    Field,
    Link,
    Message,
    ModularInteger,
    Scalar,
)

from .types import Types

NULL = Variable("null")
VALID_CONTEXT = Call("Valid_Context", [Variable("Ctx")])  # WORKAROUND: Componolit/Workarounds#1


class GeneratorCommon:
    def __init__(self, prefix: str = "") -> None:
        self.prefix = prefix
        self.types = Types(prefix)

    def substitution(
        self, message: Message, prefix: bool = True, public: bool = False
    ) -> Mapping[Name, Expr]:
        def prefixed(name: str) -> Expr:
            return Variable(f"Ctx.{name}") if prefix else Variable(name)

        first = prefixed("First")
        last = prefixed("Last")
        cursors = prefixed("Cursors")

        def field_first(field: Field) -> Expr:
            if public:
                return Call("Field_First", [Variable("Ctx"), Variable(field.affixed_name)])
            return Selected(Indexed(cursors, Variable(field.affixed_name)), "First")

        def field_last(field: Field) -> Expr:
            if public:
                return Call("Field_Last", [Variable("Ctx"), Variable(field.affixed_name)])
            return Selected(Indexed(cursors, Variable(field.affixed_name)), "Last")

        def field_length(field: Field) -> Expr:
            if public:
                return Call("Field_Length", [Variable("Ctx"), Variable(field.affixed_name)])
            return Add(
                Sub(
                    Selected(Indexed(cursors, Variable(field.affixed_name)), "Last"),
                    Selected(Indexed(cursors, Variable(field.affixed_name)), "First"),
                ),
                Number(1),
            )

        def field_value(field: Field) -> Expr:
            if public:
                return Call(self.types.bit_length, [Call(f"Get_{field.name}", [Variable("Ctx")])])
            return Call(
                self.types.bit_length,
                [
                    Selected(
                        Indexed(cursors, Variable(field.affixed_name)), f"Value.{field.name}_Value"
                    )
                ],
            )

        def enum_field_value(field: Field) -> Expr:
            if public:
                return Call(
                    self.types.bit_length,
                    [Call("To_Base", [Call(f"Get_{field.name}", [Variable("Ctx")])])],
                )
            return Call(
                self.types.bit_length,
                [
                    Selected(
                        Indexed(cursors, Variable(field.affixed_name)), f"Value.{field.name}_Value"
                    )
                ],
            )

        return {
            **{First(Variable("Message")): first},
            **{Last(Variable("Message")): last},
            **{Length(Variable("Message")): Add(last, -first, Number(1))},
            **{First(Variable(f.name)): field_first(f) for f in message.fields},
            **{Last(Variable(f.name)): field_last(f) for f in message.fields},
            **{Length(Variable(f.name)): field_length(f) for f in message.fields},
            **{
                Variable(f.name): field_value(f)
                for f, t in message.types.items()
                if not isinstance(t, Enumeration)
            },
            **{
                Variable(f.name): enum_field_value(f)
                for f, t in message.types.items()
                if isinstance(t, Enumeration)
            },
            **{
                Variable(l): Call(self.types.bit_length, [Call("To_Base", [Variable(l)])])
                for t in message.types.values()
                if isinstance(t, Enumeration)
                for l in t.literals.keys()
            },
            **{
                Variable(t.package * l): Call(
                    self.types.bit_length, [Call("To_Base", [Variable(t.package * l)])]
                )
                for t in message.types.values()
                if isinstance(t, Enumeration)
                for l in t.literals.keys()
            },
        }

    def message_structure_invariant(
        self, message: Message, link: Link = None, prefix: bool = True
    ) -> Expr:
        def prefixed(name: str) -> Expr:
            return Selected(Variable("Ctx"), name) if prefix else Variable(name)

        if not link:
            return self.message_structure_invariant(message, message.outgoing(INITIAL)[0], prefix)

        source = link.source
        target = link.target

        if target is FINAL:
            return TRUE

        field_type = message.types[target]
        condition = link.condition.simplified(self.substitution(message, prefix))
        length = (
            Size(Variable(self.prefix * full_base_type_name(field_type)))
            if isinstance(field_type, Scalar)
            else link.length.simplified(self.substitution(message, prefix))
        )
        first = (
            prefixed("First")
            if source == INITIAL
            else link.first.simplified(
                {
                    **self.substitution(message, prefix),
                    **{
                        UNDEFINED: Add(
                            Selected(
                                Indexed(prefixed("Cursors"), Variable(source.affixed_name)), "Last"
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
                            [Indexed(prefixed("Cursors"), Variable(target.affixed_name))],
                        ),
                        condition,
                    ),
                    AndThen(
                        Equal(
                            Add(
                                Sub(
                                    Selected(
                                        Indexed(prefixed("Cursors"), Variable(target.affixed_name)),
                                        "Last",
                                    ),
                                    Selected(
                                        Indexed(prefixed("Cursors"), Variable(target.affixed_name)),
                                        "First",
                                    ),
                                ),
                                Number(1),
                            ),
                            length,
                        ),
                        Equal(
                            Selected(
                                Indexed(prefixed("Cursors"), Variable(target.affixed_name)),
                                "Predecessor",
                            ),
                            Variable(source.affixed_name),
                        ),
                        Equal(
                            Selected(
                                Indexed(prefixed("Cursors"), Variable(target.affixed_name)), "First"
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
                                    "Structural_Valid",
                                    [Indexed(Variable("Cursors"), Variable(f.affixed_name))],
                                ),
                                Or(
                                    *[
                                        AndThen(
                                            Call(
                                                "Structural_Valid"
                                                if l.source in composite_fields
                                                else "Valid",
                                                [
                                                    Indexed(
                                                        Variable("Cursors"),
                                                        Variable(l.source.affixed_name),
                                                    )
                                                ],
                                            ),
                                            Equal(
                                                Selected(
                                                    Indexed(
                                                        Variable("Cursors"),
                                                        Variable(f.affixed_name),
                                                    ),
                                                    "Predecessor",
                                                ),
                                                Variable(l.source.affixed_name),
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
                                        Call(
                                            "Invalid",
                                            [
                                                Indexed(
                                                    Variable("Cursors"), Variable(p.affixed_name)
                                                )
                                            ],
                                        )
                                        for p in message.direct_predecessors(f)
                                    ]
                                ),
                                Call(
                                    "Invalid",
                                    [Indexed(Variable("Cursors"), Variable(f.affixed_name))],
                                ),
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
                        NotEqual(Variable("Buffer"), NULL),
                        And(
                            Equal(First(Variable("Buffer")), Variable("Buffer_First")),
                            Equal(Last(Variable("Buffer")), Variable("Buffer_Last")),
                        ),
                    )
                ]
            ),
            GreaterEqual(
                Call(self.types.byte_index, [Variable("First")]), Variable("Buffer_First")
            ),
            LessEqual(Call(self.types.byte_index, [Variable("Last")]), Variable("Buffer_Last")),
            LessEqual(Variable("First"), Variable("Last")),
            LessEqual(Variable("Last"), Div(Last(Variable(self.types.bit_index)), Number(2))),
            ForAllIn(
                "F",
                ValueRange(First(Variable("Field")), Last(Variable("Field"))),
                If(
                    [
                        (
                            Call("Structural_Valid", [Indexed(Variable("Cursors"), Variable("F"))]),
                            And(
                                GreaterEqual(
                                    Selected(Indexed(Variable("Cursors"), Variable("F")), "First"),
                                    Variable("First"),
                                ),
                                LessEqual(
                                    Selected(Indexed(Variable("Cursors"), Variable("F")), "Last"),
                                    Variable("Last"),
                                ),
                                LessEqual(
                                    Selected(Indexed(Variable("Cursors"), Variable("F")), "First"),
                                    Add(
                                        Selected(
                                            Indexed(Variable("Cursors"), Variable("F")), "Last"
                                        ),
                                        Number(1),
                                    ),
                                ),
                                Equal(
                                    Selected(
                                        Selected(
                                            Indexed(Variable("Cursors"), Variable("F")), "Value"
                                        ),
                                        "Fld",
                                    ),
                                    Variable("F"),
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

    def valid_path_to_next_field_condition(self, message: Message, field: Field) -> Sequence[Expr]:
        return [
            If(
                [
                    (
                        l.condition,
                        And(
                            Equal(
                                Call(
                                    "Predecessor",
                                    [Variable("Ctx"), Variable(l.target.affixed_name)],
                                ),
                                Variable(field.affixed_name),
                            ),
                            Call("Valid_Next", [Variable("Ctx"), Variable(l.target.affixed_name)])
                            if l.target != FINAL
                            else TRUE,
                        ),
                    )
                ]
            ).simplified(self.substitution(message, public=True))
            for l in message.outgoing(field)
            if l.target != FINAL
        ]

    @staticmethod
    def sufficient_space_for_field_condition(field_name: Name) -> Expr:
        return GreaterEqual(
            Call("Available_Space", [Variable("Ctx"), field_name]),
            Call("Field_Length", [Variable("Ctx"), field_name]),
        )

    def initialize_field_statements(self, message: Message, field: Field) -> Sequence[Statement]:
        return [
            CallStatement(
                "Reset_Dependent_Fields", [Variable("Ctx"), Variable(field.affixed_name)],
            ),
            Assignment(
                "Ctx",
                Aggregate(
                    Variable("Ctx.Buffer_First"),
                    Variable("Ctx.Buffer_Last"),
                    Variable("Ctx.First"),
                    Variable("Last"),
                    Variable("Ctx.Buffer"),
                    Variable("Ctx.Cursors"),
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
                Indexed(Variable("Ctx.Cursors"), Variable(field.affixed_name)),
                NamedAggregate(
                    ("State", Variable("S_Structural_Valid")),
                    ("First", Variable("First")),
                    ("Last", Variable("Last")),
                    ("Value", NamedAggregate(("Fld", Variable(field.affixed_name))),),
                    (
                        "Predecessor",
                        Selected(
                            Indexed(Variable("Ctx.Cursors"), Variable(field.affixed_name),),
                            "Predecessor",
                        ),
                    ),
                ),
            ),
            Assignment(
                Indexed(
                    Variable("Ctx.Cursors"),
                    Call("Successor", [Variable("Ctx"), Variable(field.affixed_name)]),
                ),
                NamedAggregate(
                    ("State", Variable("S_Invalid")), ("Predecessor", Variable(field.affixed_name)),
                ),
            ),
        ]

    def field_bit_location_declarations(self, field_name: Name) -> Sequence[Declaration]:
        return [
            ObjectDeclaration(
                ["First"],
                self.types.bit_index,
                Call("Field_First", [Variable("Ctx"), field_name]),
                True,
            ),
            ObjectDeclaration(
                ["Last"],
                self.types.bit_index,
                Call("Field_Last", [Variable("Ctx"), field_name]),
                True,
            ),
        ]

    def field_byte_location_declarations(self) -> Sequence[Declaration]:
        return [
            ExpressionFunctionDeclaration(
                FunctionSpecification("Buffer_First", self.types.index),
                Call(self.types.byte_index, [Variable("First")]),
            ),
            ExpressionFunctionDeclaration(
                FunctionSpecification("Buffer_Last", self.types.index),
                Call(self.types.byte_index, [Variable("Last")]),
            ),
            ExpressionFunctionDeclaration(
                FunctionSpecification("Offset", self.types.offset),
                Call(
                    self.types.offset,
                    [Mod(Sub(Number(8), Mod(Variable("Last"), Number(8))), Number(8))],
                ),
            ),
        ]


def base_type_name(scalar_type: Scalar) -> ID:
    if isinstance(scalar_type, ModularInteger):
        return ID(scalar_type.name)

    return ID(f"{scalar_type.name}_Base")


def full_base_type_name(scalar_type: Scalar) -> ID:
    if scalar_type.package == BUILTINS_PACKAGE:
        return ID(f"Builtin_Types.{scalar_type.name}_Base")

    if isinstance(scalar_type, ModularInteger):
        return scalar_type.identifier

    return ID(f"{scalar_type.full_name}_Base")


def enum_name(enum_type: Enumeration) -> ID:
    return ID(f"{enum_type.name}_Enum")


def full_enum_name(enum_type: Enumeration) -> ID:
    return ID(f"{enum_type.full_name}_Enum")


def sequence_name(message: Message, field: Field) -> ID:
    return ID(f"{message.types[field].name}_Sequence")


def length_dependent_condition(message: Message) -> bool:
    return any(
        [
            isinstance(v.name, str) and v.name.endswith("'Length")
            for l in message.structure
            for v in l.condition.variables(True)
        ]
    )

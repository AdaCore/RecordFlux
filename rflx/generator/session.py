# pylint: disable = too-many-lines

from __future__ import annotations

from collections.abc import Callable, Iterable, Mapping, Sequence
from dataclasses import dataclass, field as dataclass_field
from typing import NoReturn, Optional, Union

import attr

from rflx import expression as expr, model, typing_ as rty
from rflx.ada import (
    FALSE,
    TRUE,
    Add,
    And,
    AndThen,
    Annotate,
    Assignment,
    Call,
    CallStatement,
    Case,
    CaseStatement,
    ChoiceList,
    ClassPrecondition,
    CommentStatement,
    Component,
    Constrained,
    ContextItem,
    Declaration,
    Declare,
    EnumerationType,
    Equal,
    ExitStatement,
    Expr,
    ExpressionFunctionDeclaration,
    First,
    FunctionSpecification,
    GenericProcedureInstantiation,
    Ghost,
    GotoStatement,
    Greater,
    GreaterEqual,
    IfStatement,
    In,
    Indexed,
    InOutParameter,
    Label,
    Last,
    Length,
    Less,
    LessEqual,
    LoopEntry,
    Min,
    Mod,
    Mul,
    NamedAggregate,
    Not,
    NotEqual,
    NullStatement,
    Number,
    ObjectDeclaration,
    Or,
    OutParameter,
    Parameter,
    Postcondition,
    Pragma,
    PragmaStatement,
    Precondition,
    PrivateType,
    ProcedureSpecification,
    RecordType,
    Selected,
    Size,
    SizeAspect,
    Slice,
    Statement,
    String,
    Sub,
    SubprogramBody,
    SubprogramDeclaration,
    UnitPart,
    UseTypeClause,
    ValueRange,
    Variable,
    While,
    WithClause,
)
from rflx.common import unique
from rflx.const import BUILTINS_PACKAGE, INTERNAL_PACKAGE
from rflx.error import Location, Subsystem, fail, fatal_fail
from rflx.identifier import ID
from rflx.model import FINAL_STATE, declaration as decl, statement as stmt

from . import common, const
from .allocator import AllocatorGenerator


@dataclass
class SessionContext:
    referenced_types: list[ID] = dataclass_field(default_factory=list)
    referenced_types_body: list[ID] = dataclass_field(default_factory=list)
    referenced_packages_body: list[ID] = dataclass_field(default_factory=list)
    used_types: list[ID] = dataclass_field(default_factory=list)
    used_types_body: list[ID] = dataclass_field(default_factory=list)
    state_exception: set[ID] = dataclass_field(default_factory=set)


@dataclass
class EvaluatedDeclaration:
    global_declarations: list[Declaration] = dataclass_field(default_factory=list)
    initialization_declarations: list[Declaration] = dataclass_field(default_factory=list)
    initialization: list[Statement] = dataclass_field(default_factory=list)
    finalization: list[Statement] = dataclass_field(default_factory=list)

    def __iadd__(self, other: object) -> EvaluatedDeclaration:
        if isinstance(other, EvaluatedDeclaration):
            return EvaluatedDeclaration(
                [*self.global_declarations, *other.global_declarations],
                [*self.initialization_declarations, *other.initialization_declarations],
                [*self.initialization, *other.initialization],
                [*self.finalization, *other.finalization],
            )

        return NotImplemented


@attr.s()
class ExceptionHandler:
    session_context_state_exception: set[ID] = attr.ib()
    state: model.State = attr.ib()
    finalization: Sequence[Statement] = attr.ib()

    def execute(self) -> list[Statement]:
        assert (
            self.state.exception_transition
        ), f'missing exception transition for state "{self.state.identifier}"'
        return [
            Assignment(
                "Ctx.P.Next_State",
                Variable(state_id(self.state.exception_transition.target)),
            ),
            *self.finalization,
            GotoStatement(f"Finalize_{self.state.identifier}"),
        ]

    def copy(self, finalization: Sequence[Statement]) -> ExceptionHandler:
        return ExceptionHandler(
            self.session_context_state_exception, self.state, [*finalization, *self.finalization]
        )


@dataclass
class ChannelAccess:
    state: ID
    message: ID
    message_type: ID


class SessionGenerator:  # pylint: disable = too-many-instance-attributes
    def __init__(
        self,
        session: model.Session,
        allocator: AllocatorGenerator,
        prefix: str = "",
        debug: common.Debug = common.Debug.NONE,
    ) -> None:
        self._session = session
        self._prefix = prefix
        self._debug = debug
        self._allocator = allocator

        self._session_context = SessionContext()
        self._declaration_context: list[ContextItem] = []
        self._body_context: list[ContextItem] = []
        self._unit_part = UnitPart()

        self._create()

    @property
    def unit_identifier(self) -> ID:
        return self._session.identifier

    @property
    def declaration_context(self) -> list[ContextItem]:
        return self._declaration_context

    @property
    def body_context(self) -> list[ContextItem]:
        return self._body_context

    @property
    def unit_part(self) -> UnitPart:
        return self._unit_part

    def _ada_type(self, identifier: ID) -> ID:
        if model.is_builtin_type(identifier):
            return identifier.name

        return model.internal_type_identifier(identifier, self._session.package)

    def _model_type(self, identifier: ID) -> model.Type:
        return self._session.types[
            model.internal_type_identifier(identifier, self._session.package)
        ]

    def _create(self) -> None:
        state_machine = self._create_state_machine()
        self._declaration_context, self._body_context = self._create_context()
        self._unit_part = UnitPart(body=self._create_use_clauses_body()) + state_machine

    def _create_context(self) -> tuple[list[ContextItem], list[ContextItem]]:
        declaration_context: list[ContextItem] = []

        if self._allocator.required:
            declaration_context.append(WithClause(self._prefix * self._allocator.unit_identifier))

        if any(
            t.parent not in [INTERNAL_PACKAGE, BUILTINS_PACKAGE]
            for t in [*self._session_context.used_types, *self._session_context.used_types_body]
        ):
            declaration_context.append(WithClause(self._prefix * const.TYPES_PACKAGE))

        body_context: list[ContextItem] = [
            *(
                [
                    WithClause(self._prefix * ID("RFLX_Debug"))
                    if self._debug == common.Debug.EXTERNAL
                    else WithClause("Ada.Text_IO")
                ]
                if self._debug != common.Debug.NONE
                else []
            ),
        ]

        for referenced_types, context in [
            (self._session_context.referenced_types, declaration_context),
            (self._session_context.referenced_types_body, body_context),
        ]:
            for type_identifier in referenced_types:
                if type_identifier.parent in [INTERNAL_PACKAGE, BUILTINS_PACKAGE]:
                    continue
                type_ = self._model_type(type_identifier)
                context.extend(
                    [
                        *(
                            [WithClause(self._prefix * type_.package)]
                            if type_.package != self._session.identifier.parent
                            else []
                        ),
                        *(
                            [
                                WithClause(self._prefix * type_.identifier),
                            ]
                            if isinstance(type_, (model.Message, model.Sequence))
                            else []
                        ),
                    ]
                )

        body_context.extend(
            [WithClause(self._prefix * p) for p in self._session_context.referenced_packages_body],
        )

        for type_identifier in self._session_context.used_types_body:
            if type_identifier.parent in [INTERNAL_PACKAGE, BUILTINS_PACKAGE]:
                continue
            if type_identifier in self._session_context.used_types:
                continue
            if type_identifier in [
                const.TYPES_LENGTH,
                const.TYPES_INDEX,
                const.TYPES_BIT_LENGTH,
            ]:
                body_context.append(
                    WithClause(self._prefix * const.TYPES_PACKAGE),
                )

        body_context = [
            i for i in body_context if isinstance(i, WithClause) and i not in declaration_context
        ]

        return (declaration_context, body_context)

    def _create_use_clauses_body(self) -> list[Declaration]:
        return [
            UseTypeClause(self._prefix * type_identifier)
            for type_identifier in self._session_context.used_types_body
            if type_identifier.parent
            not in [INTERNAL_PACKAGE, BUILTINS_PACKAGE, self._session.identifier.parent]
            and type_identifier not in self._session_context.used_types
        ]

    def _create_state_machine(self) -> UnitPart:
        evaluated_declarations = self._evaluate_declarations(
            self._session.declarations.values(),
            session_global=True,
        )
        assert all(
            isinstance(d, ObjectDeclaration)
            and len(d.identifiers) == 1
            and isinstance(d.type_identifier, Variable)
            for d in evaluated_declarations.global_declarations
        )
        global_variables = {
            d.identifiers[0]: (d.type_identifier.identifier, d.expression)
            for d in evaluated_declarations.global_declarations
            if isinstance(d, ObjectDeclaration)
            and len(d.identifiers) == 1
            and isinstance(d.type_identifier, Variable)
        }

        def is_global(identifier: ID) -> bool:
            return identifier in global_variables

        composite_globals = [
            d
            for d in self._session.declarations.values()
            if isinstance(d, decl.VariableDeclaration)
            and isinstance(d.type_, (rty.Message, rty.Sequence))
        ]

        unit = UnitPart()
        unit += self._create_abstract_functions(self._session.parameters.values())
        unit += self._create_uninitialized_function(composite_globals, is_global)
        unit += self._create_global_initialized_function(composite_globals, is_global)
        unit += self._create_initialized_function(composite_globals)
        unit += self._create_states(self._session, composite_globals, is_global)
        unit += self._create_active_function(self._session)
        unit += self._create_initialize_procedure(
            self._session,
            evaluated_declarations.initialization_declarations,
            evaluated_declarations.initialization,
        )
        unit += self._create_finalize_procedure(
            evaluated_declarations.initialization_declarations,
            evaluated_declarations.finalization,
        )

        channel_reads = self._channel_io(self._session, read=True)
        channel_writes = self._channel_io(self._session, write=True)
        has_reads = bool([read for reads in channel_reads.values() for read in reads])
        has_writes = bool([write for writes in channel_writes.values() for write in writes])

        if has_reads:
            unit += self._create_reset_messages_before_write_procedure(self._session, is_global)

        unit += self._create_tick_procedure(self._session, has_reads)
        unit += self._create_in_io_state_function(self._session)
        unit += self._create_run_procedure()
        unit += self._create_state_function()

        if has_writes:
            unit += self._create_has_data_function(channel_writes, is_global)
            unit += self._create_read_buffer_size_function(channel_writes, is_global)
            unit += self._create_read_procedure(channel_writes, is_global)

        if has_reads:
            unit += self._create_needs_data_function(channel_reads)
            unit += self._create_write_buffer_size_function(channel_reads, is_global)
            unit += self._create_write_procedure(channel_reads, is_global)

        return (
            self._create_use_clauses(self._session_context.used_types)
            + self._create_channel_and_state_types(self._session)
            + self._create_context_type(self._session.initial_state.identifier, global_variables)
            + unit
        )

    @staticmethod
    def _channel_io(
        session: model.Session, read: bool = False, write: bool = False
    ) -> dict[ID, list[ChannelAccess]]:
        assert (read and not write) or (not read and write)

        channels: dict[ID, list[ChannelAccess]] = {
            parameter.identifier: []
            for parameter in session.parameters.values()
            if isinstance(parameter, decl.ChannelDeclaration)
        }
        for state in session.states:
            for action in state.actions:
                if (
                    isinstance(action, stmt.ChannelAttributeStatement)
                    and isinstance(action, stmt.Read if read else stmt.Write)
                    and isinstance(action.parameter, expr.Variable)
                    and isinstance(action.parameter.type_, rty.Message)
                ):
                    channels[action.identifier].append(
                        ChannelAccess(
                            state.identifier,
                            action.parameter.identifier,
                            action.parameter.type_.identifier,
                        )
                    )

        return channels

    def _create_use_clauses(self, used_types: Sequence[ID]) -> UnitPart:
        return UnitPart(
            [
                UseTypeClause(self._prefix * t)
                for t in used_types
                if not model.is_builtin_type(t) and not model.is_internal_type(t)
            ],
        )

    @staticmethod
    def _create_channel_and_state_types(session: model.Session) -> UnitPart:
        channel_params = [
            x for x in session.parameters.values() if isinstance(x, decl.ChannelDeclaration)
        ]
        return UnitPart(
            [
                *(
                    [
                        EnumerationType(
                            "Channel",
                            {ID(f"C_{parameter.identifier}"): None for parameter in channel_params},
                        )
                    ]
                    if channel_params
                    else []
                ),
                EnumerationType(
                    "State",
                    {state_id(s.identifier): None for s in [*session.states, model.State("Final")]},
                ),
            ],
        )

    def _create_context_type(
        self, initial_state: ID, global_variables: Mapping[ID, tuple[ID, Optional[Expr]]]
    ) -> UnitPart:
        return UnitPart(
            [
                PrivateType("Private_Context"),
                RecordType(
                    "Context",
                    [
                        Component("P", "Private_Context"),
                    ],
                    abstract=True,
                    tagged=True,
                    limited=True,
                ),
            ],
            private=[
                RecordType(
                    "Private_Context",
                    [
                        Component("Next_State", "State", Variable(state_id(initial_state))),
                        *[
                            Component(
                                identifier,
                                type_identifier,
                                expression
                                if expression is not None or type_identifier.name == ID("Context")
                                else First(type_identifier),
                            )
                            for identifier, (
                                type_identifier,
                                expression,
                            ) in global_variables.items()
                        ],
                        *(
                            [
                                Component("Slots", self._allocator.unit_identifier * "Slots"),
                                Component("Memory", self._allocator.unit_identifier * "Memory"),
                            ]
                            if self._allocator.required
                            else []
                        ),
                    ],
                ),
            ],
        )

    def _create_abstract_functions(
        self,
        parameters: Iterable[decl.FormalDeclaration],
    ) -> UnitPart:
        result: list[Declaration] = []

        for parameter in parameters:
            if isinstance(parameter, decl.ChannelDeclaration):
                pass
            elif isinstance(parameter, decl.FunctionDeclaration):
                result.extend(self._create_abstract_function(parameter))
            else:
                fatal_fail(
                    f'unexpected formal parameter "{parameter.identifier}"',
                    Subsystem.GENERATOR,
                    location=parameter.location,
                )

        return UnitPart(result)

    def _create_abstract_function(
        self, function: decl.FunctionDeclaration
    ) -> Sequence[SubprogramDeclaration]:
        procedure_parameters: list[Parameter] = [InOutParameter(["Ctx"], "Context")]

        if function.type_ == rty.Undefined():
            fatal_fail(
                f'return type of function "{function.identifier}" is undefined',
                Subsystem.GENERATOR,
                location=function.location,
            )
        if function.type_ == rty.OPAQUE:
            fatal_fail(
                f'Opaque as return type of function "{function.identifier}" not allowed',
                Subsystem.GENERATOR,
                location=function.location,
            )
        if isinstance(function.type_, rty.Sequence):
            fail(
                f'sequence as return type of function "{function.identifier}" not yet supported',
                Subsystem.GENERATOR,
                location=function.location,
            )
        if isinstance(function.type_, rty.Message):
            if not function.type_.is_definite:
                fatal_fail(
                    "non-definite message"
                    f' in return type of function "{function.identifier}" not allowed',
                    Subsystem.GENERATOR,
                    location=function.location,
                )
            if any(
                isinstance(field_type, rty.Sequence) and not field_type == rty.OPAQUE
                for field_type in function.type_.types.values()
            ):
                fail(
                    "message containing sequence fields"
                    f' in return type of function "{function.identifier}" not yet supported',
                    Subsystem.GENERATOR,
                    location=function.location,
                )

        self._session_context.referenced_types.append(function.return_type)

        for a in function.arguments:
            if isinstance(a.type_, rty.Sequence) and not a.type_ == rty.OPAQUE:
                fail(
                    f'sequence as parameter of function "{function.identifier}" not yet supported',
                    Subsystem.GENERATOR,
                    location=function.location,
                )
            procedure_parameters.append(
                Parameter(
                    [a.identifier],
                    const.TYPES_BYTES
                    if a.type_ == rty.OPAQUE
                    else ID("Boolean")
                    if a.type_ == rty.BOOLEAN
                    else self._prefix * a.type_identifier * "Structure"
                    if isinstance(a.type_, rty.Message)
                    else self._prefix * a.type_identifier,
                )
            )

            assert isinstance(a.type_, (rty.Integer, rty.Enumeration, rty.Message, rty.Sequence))

            self._session_context.referenced_types.append(a.type_.identifier)

        procedure_parameters.append(
            OutParameter(
                [ID("RFLX_Result")],
                self._prefix * function.return_type * "Structure"
                if isinstance(function.type_, rty.Message)
                else ID("Boolean")
                if function.type_ == rty.BOOLEAN
                else self._prefix * function.return_type,
            )
        )

        return [
            SubprogramDeclaration(
                ProcedureSpecification(
                    function.identifier,
                    procedure_parameters,
                ),
                [ClassPrecondition(Not(Constrained("RFLX_Result")))]
                if isinstance(function.type_, rty.Enumeration) and function.type_.always_valid
                else [],
                abstract=True,
            )
        ]

    def _create_uninitialized_function(
        self,
        composite_globals: Sequence[decl.VariableDeclaration],
        is_global: Callable[[ID], bool],
    ) -> UnitPart:
        specification = FunctionSpecification(
            "Uninitialized",
            "Boolean",
            [Parameter(["Ctx" if composite_globals else "Unused_Ctx"], "Context'Class")],
        )
        return UnitPart(
            [
                SubprogramDeclaration(specification),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    And(
                        *[
                            Not(
                                Call(
                                    declaration.type_.identifier * "Has_Buffer",
                                    [Variable(context_id(declaration.identifier, is_global))],
                                )
                            )
                            for declaration in composite_globals
                            if isinstance(declaration.type_, (rty.Message, rty.Sequence))
                        ],
                        *(
                            [
                                Call(
                                    self._allocator.unit_identifier * "Uninitialized",
                                    [Variable("Ctx.P.Slots")],
                                ),
                            ]
                            if composite_globals
                            else []
                        ),
                    ),
                ),
            ],
        )

    def _create_global_initialized_function(
        self,
        composite_globals: Sequence[decl.VariableDeclaration],
        is_global: Callable[[ID], bool],
    ) -> UnitPart:
        if not composite_globals:
            return UnitPart()

        self._session_context.used_types.append(const.TYPES_INDEX)

        specification = FunctionSpecification(
            "Global_Initialized",
            "Boolean",
            [Parameter(["Ctx"], "Context'Class")],
        )

        return UnitPart(
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        *[
                            e
                            for d in composite_globals
                            for e in [
                                Call(
                                    d.type_identifier * "Has_Buffer",
                                    [Variable(context_id(d.identifier, is_global))],
                                ),
                                Equal(
                                    Variable(context_id(d.identifier, is_global) * "Buffer_First"),
                                    First(const.TYPES_INDEX),
                                ),
                                Equal(
                                    Variable(context_id(d.identifier, is_global) * "Buffer_Last"),
                                    Add(
                                        First(const.TYPES_INDEX),
                                        Number(self._allocator.get_size(d.identifier) - 1),
                                    ),
                                ),
                            ]
                        ],
                    ),
                ),
            ],
        )

    def _create_initialized_function(
        self, composite_globals: Sequence[decl.VariableDeclaration]
    ) -> UnitPart:
        specification = FunctionSpecification(
            "Initialized",
            "Boolean",
            [
                Parameter(
                    ["Ctx" if composite_globals or self._allocator.required else "Unused_Ctx"],
                    "Context'Class",
                )
            ],
        )
        return UnitPart(
            [
                SubprogramDeclaration(specification),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        *[
                            *(
                                [
                                    Call(
                                        ID("Global_Initialized"),
                                        [Variable("Ctx")],
                                    ),
                                ]
                                if composite_globals
                                else []
                            ),
                            *(
                                [
                                    Call(
                                        self._allocator.unit_identifier * "Global_Allocated",
                                        [Variable("Ctx.P.Slots")],
                                    ),
                                ]
                                if self._allocator.required
                                else []
                            ),
                        ]
                    ),
                ),
            ],
        )

    def _create_states(
        self,
        session: model.Session,
        composite_globals: Sequence[decl.VariableDeclaration],
        is_global: Callable[[ID], bool],
    ) -> UnitPart:
        if self._allocator.get_global_slot_ptrs() or self._allocator.get_local_slot_ptrs():
            self._session_context.used_types_body.append(const.TYPES_BYTES_PTR)

        unit_body: list[Declaration] = []

        for state in session.states:
            if state == FINAL_STATE:
                continue

            invariant = []
            slots = []

            for d in state.declarations.values():
                assert isinstance(d, decl.VariableDeclaration)
                if isinstance(d.type_, (rty.Message, rty.Sequence)):
                    identifier = context_id(d.identifier, is_global)
                    type_identifier = self._ada_type(d.type_.identifier)
                    invariant.extend(
                        [
                            *(
                                [Call("Global_Initialized", [Variable("Ctx")])]
                                if composite_globals
                                else []
                            ),
                            Call(type_identifier * "Has_Buffer", [Variable(identifier)]),
                            Equal(
                                Variable(identifier * "Buffer_First"),
                                First(self._prefix * const.TYPES_INDEX),
                            ),
                            # Due to the reuse of allocation slots, `Buffer_Last` can be greater
                            # then the actual required size.
                            GreaterEqual(
                                Variable(identifier * "Buffer_Last"),
                                Add(
                                    First(self._prefix * const.TYPES_INDEX),
                                    Number(
                                        self._allocator.get_size(d.identifier, state.identifier) - 1
                                    ),
                                ),
                            ),
                            Equal(
                                Variable("Ctx.P.Slots" * self._allocator.get_slot_ptr(d.location)),
                                Variable("null"),
                            ),
                        ]
                    )
                    slots.append(self._allocator.get_slot_ptr(d.location))

            invariant.extend(
                [
                    *[
                        Equal(
                            Variable("Ctx.P.Slots" * s),
                            Variable("null"),
                        )
                        for s in self._allocator.get_global_slot_ptrs()
                    ],
                    *[
                        NotEqual(
                            Variable("Ctx.P.Slots" * s),
                            Variable("null"),
                        )
                        for s in self._allocator.get_local_slot_ptrs()
                        if s not in slots
                    ],
                ]
            )

            evaluated_declarations = self._evaluate_declarations(
                state.declarations.values(), is_global
            )
            statements = [
                *[
                    s
                    for a in state.actions
                    for s in self._state_action(
                        state.identifier,
                        a,
                        ExceptionHandler(
                            self._session_context.state_exception,
                            state,
                            [PragmaStatement("Assert", [Call(f"{state.identifier}_Invariant")])],
                        ),
                        is_global,
                    )
                ],
                *self._determine_next_state(state.transitions, is_global),
            ]

            unit_body += [
                SubprogramBody(
                    ProcedureSpecification(
                        state.identifier,
                        [
                            InOutParameter(["Ctx"], "Context'Class"),
                        ],
                    ),
                    [
                        *evaluated_declarations.global_declarations,
                        *evaluated_declarations.initialization_declarations,
                        ExpressionFunctionDeclaration(
                            FunctionSpecification(f"{state.identifier}_Invariant", "Boolean"),
                            And(*invariant),
                            [Annotate("GNATprove", "Inline_For_Proof"), Ghost()],
                        ),
                        *(
                            [ObjectDeclaration(["RFLX_Exception"], "Boolean", FALSE)]
                            if state.identifier in self._session_context.state_exception
                            else []
                        ),
                    ],
                    [
                        *evaluated_declarations.initialization,
                        PragmaStatement("Assert", [Call(f"{state.identifier}_Invariant")]),
                        *statements,
                        PragmaStatement("Assert", [Call(f"{state.identifier}_Invariant")]),
                        *([Label(f"Finalize_{state.identifier}")] if state.has_exceptions else []),
                        *evaluated_declarations.finalization,
                        *(
                            [
                                PragmaStatement(
                                    "Assert", [Call("Global_Initialized", [Variable("Ctx")])]
                                )
                            ]
                            if composite_globals and evaluated_declarations.finalization
                            else []
                        ),
                    ],
                    aspects=[
                        Precondition(Call("Initialized", [Variable("Ctx")])),
                        Postcondition(Call("Initialized", [Variable("Ctx")])),
                    ],
                ),
            ]

        return UnitPart(body=unit_body)

    def _determine_next_state(
        self, transitions: Sequence[model.Transition], is_global: Callable[[ID], bool]
    ) -> Sequence[Statement]:
        return (
            [
                IfStatement(
                    [
                        (
                            t.condition.substituted(self._substitution(is_global)).ada_expr(),
                            [
                                Assignment(
                                    "Ctx.P.Next_State",
                                    Variable(state_id(t.target)),
                                )
                            ],
                        )
                        for t in transitions[:-1]
                    ],
                    [
                        Assignment(
                            "Ctx.P.Next_State",
                            Variable(state_id(transitions[-1].target)),
                        )
                    ],
                )
            ]
            if transitions
            else []
        )

    @staticmethod
    def _create_active_function(session: model.Session) -> UnitPart:
        specification = FunctionSpecification(
            "Active",
            "Boolean",
            [Parameter(["Ctx" if len(session.states) > 1 else "Unused_Ctx"], "Context'Class")],
        )
        return UnitPart(
            [
                SubprogramDeclaration(specification),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    NotEqual(
                        Variable("Ctx.P.Next_State"), Variable(state_id(FINAL_STATE.identifier))
                    )
                    if len(session.states) > 1
                    else Variable("False"),
                ),
            ],
        )

    @staticmethod
    def _create_initialize_procedure(
        session: model.Session,
        declarations: Sequence[Declaration],
        initialization: Sequence[Statement],
    ) -> UnitPart:
        specification = ProcedureSpecification(
            "Initialize", [InOutParameter(["Ctx"], "Context'Class")]
        )
        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(Call("Uninitialized", [Variable("Ctx")])),
                        Postcondition(
                            And(
                                Call("Initialized", [Variable("Ctx")]),
                                Call("Active", [Variable("Ctx")]),
                            )
                        ),
                    ],
                ),
            ],
            [
                SubprogramBody(
                    specification,
                    declarations,
                    [
                        *initialization,
                        Assignment(
                            "Ctx.P.Next_State", Variable(state_id(session.initial_state.identifier))
                        ),
                    ],
                ),
            ],
        )

    @staticmethod
    def _create_finalize_procedure(
        declarations: Sequence[Declaration],
        finalization: Sequence[Statement],
    ) -> UnitPart:
        specification = ProcedureSpecification(
            "Finalize", [InOutParameter(["Ctx"], "Context'Class")]
        )
        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(Call("Initialized", [Variable("Ctx")])),
                        Postcondition(
                            And(
                                Call("Uninitialized", [Variable("Ctx")]),
                                Not(Call("Active", [Variable("Ctx")])),
                            )
                        ),
                    ],
                ),
            ],
            [
                SubprogramBody(
                    specification,
                    declarations,
                    [
                        *finalization,
                        Assignment("Ctx.P.Next_State", Variable(state_id(FINAL_STATE.identifier))),
                    ],
                ),
            ],
        )

    def _create_reset_messages_before_write_procedure(
        self,
        session: model.Session,
        is_global: Callable[[ID], bool],
    ) -> UnitPart:
        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        specification = ProcedureSpecification(
            "Reset_Messages_Before_Write", [InOutParameter(["Ctx"], "Context'Class")]
        )
        states = [
            (
                state,
                [
                    (
                        action.parameter.identifier,
                        action.parameter.type_,
                    )
                    for action in state.actions
                    if (
                        isinstance(action, stmt.Read)
                        and isinstance(action.parameter, expr.Variable)
                        and isinstance(action.parameter.type_, rty.Message)
                    )
                ],
            )
            for state in session.states
        ]

        return UnitPart(
            body=[
                SubprogramBody(
                    specification,
                    [],
                    [
                        CaseStatement(
                            Variable("Ctx.P.Next_State"),
                            [
                                (
                                    Variable(state_id(state.identifier)),
                                    [
                                        CallStatement(
                                            message_type.identifier * "Reset",
                                            [
                                                Variable(context_id(message, is_global)),
                                                Variable(context_id(message, is_global) * "First"),
                                                Sub(
                                                    Variable(
                                                        context_id(message, is_global) * "First"
                                                    ),
                                                    Number(1),
                                                ),
                                                *[
                                                    Variable(context_id(message, is_global) * p)
                                                    for p in message_type.parameter_types
                                                ],
                                            ],
                                        )
                                        for message, message_type in reads
                                    ]
                                    if reads
                                    else [NullStatement()],
                                )
                                for state, reads in states
                            ],
                        ),
                    ],
                    aspects=[
                        Precondition(Call("Initialized", [Variable("Ctx")])),
                        Postcondition(Call("Initialized", [Variable("Ctx")])),
                    ],
                )
            ],
        )

    def _create_tick_procedure(self, session: model.Session, has_writes: bool) -> UnitPart:
        specification = ProcedureSpecification("Tick", [InOutParameter(["Ctx"], "Context'Class")])
        return UnitPart(
            [
                Pragma("Warnings", [Variable("Off"), String('subprogram "Tick" has no effect')]),
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(Call("Initialized", [Variable("Ctx")])),
                        Postcondition(Call("Initialized", [Variable("Ctx")])),
                    ],
                ),
                Pragma("Warnings", [Variable("On"), String('subprogram "Tick" has no effect')]),
            ],
            [
                SubprogramBody(
                    specification,
                    [],
                    [
                        CaseStatement(
                            Variable("Ctx.P.Next_State"),
                            [
                                (
                                    Variable(state_id(s.identifier)),
                                    [
                                        *self._debug_output(f"State: {s.identifier}"),
                                        CallStatement(s.identifier, [Variable("Ctx")]),
                                    ]
                                    if s != FINAL_STATE
                                    else [NullStatement()],
                                )
                                for s in session.states
                            ],
                        ),
                        *(
                            [CallStatement("Reset_Messages_Before_Write", [Variable("Ctx")])]
                            if has_writes
                            else []
                        ),
                    ],
                )
            ],
        )

    @staticmethod
    def _create_in_io_state_function(session: model.Session) -> UnitPart:
        io_states = [
            state
            for state in session.states
            if any(
                True
                for action in state.actions
                if (
                    isinstance(action, (stmt.Read, stmt.Write))
                    and isinstance(action.parameter, expr.Variable)
                    and isinstance(action.parameter.type_, rty.Message)
                )
            )
        ]
        in_io_state_specification = FunctionSpecification(
            "In_IO_State",
            "Boolean",
            [Parameter(["Ctx" if io_states else "Unused_Ctx"], "Context'Class")],
        )
        return UnitPart(
            [
                SubprogramDeclaration(in_io_state_specification),
            ],
            [
                ExpressionFunctionDeclaration(
                    in_io_state_specification,
                    In(
                        Variable("Ctx.P.Next_State"),
                        ChoiceList(*[Variable(state_id(state.identifier)) for state in io_states]),
                    )
                    if io_states
                    else FALSE,
                ),
            ],
        )

    @staticmethod
    def _create_run_procedure() -> UnitPart:
        specification = ProcedureSpecification("Run", [InOutParameter(["Ctx"], "Context'Class")])
        return UnitPart(
            [
                Pragma("Warnings", [Variable("Off"), String('subprogram "Run" has no effect')]),
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(Call("Initialized", [Variable("Ctx")])),
                        Postcondition(Call("Initialized", [Variable("Ctx")])),
                    ],
                ),
                Pragma("Warnings", [Variable("On"), String('subprogram "Run" has no effect')]),
            ],
            [
                SubprogramBody(
                    specification,
                    [],
                    [
                        CallStatement("Tick", [Variable("Ctx")]),
                        While(
                            And(
                                Call("Active", [Variable("Ctx")]),
                                Not(Call("In_IO_State", [Variable("Ctx")])),
                            ),
                            [
                                PragmaStatement(
                                    "Loop_Invariant", [Call("Initialized", [Variable("Ctx")])]
                                ),
                                CallStatement("Tick", [Variable("Ctx")]),
                            ],
                        ),
                    ],
                ),
            ],
        )

    @staticmethod
    def _create_state_function() -> UnitPart:
        specification = FunctionSpecification(
            "Next_State", "State", [Parameter(["Ctx"], "Context'Class")]
        )
        return UnitPart(
            [
                SubprogramDeclaration(specification),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Variable("Ctx.P.Next_State"),
                )
            ],
        )

    @staticmethod
    def _create_has_data_function(
        channel_writes: dict[ID, list[ChannelAccess]],
        is_global: Callable[[ID], bool],
    ) -> UnitPart:
        specification = FunctionSpecification(
            "Has_Data",
            "Boolean",
            [Parameter(["Ctx"], "Context'Class"), Parameter(["Chan"], "Channel")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification, [Precondition(Call("Initialized", [Variable("Ctx")]))]
                ),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Chan"),
                        [
                            (
                                Variable(f"C_{channel}"),
                                Case(
                                    Variable("Ctx.P.Next_State"),
                                    [
                                        *[
                                            (
                                                Variable(state_id(write.state)),
                                                And(
                                                    Call(
                                                        write.message_type * "Well_Formed_Message",
                                                        [
                                                            Variable(
                                                                context_id(write.message, is_global)
                                                            )
                                                        ],
                                                    ),
                                                    Greater(
                                                        Call(
                                                            write.message_type * "Byte_Size",
                                                            [
                                                                Variable(
                                                                    context_id(
                                                                        write.message, is_global
                                                                    )
                                                                )
                                                            ],
                                                        ),
                                                        Number(0),
                                                    ),
                                                ),
                                            )
                                            for write in writes
                                        ],
                                        (Variable("others"), FALSE),
                                    ],
                                ),
                            )
                            for channel, writes in channel_writes.items()
                        ],
                    ),
                )
            ],
        )

    @staticmethod
    def _create_needs_data_function(channel_reads: dict[ID, list[ChannelAccess]]) -> UnitPart:
        specification = FunctionSpecification(
            "Needs_Data",
            "Boolean",
            [Parameter(["Ctx"], "Context'Class"), Parameter(["Chan"], "Channel")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification, [Precondition(Call("Initialized", [Variable("Ctx")]))]
                ),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Chan"),
                        [
                            (
                                Variable(f"C_{channel}"),
                                Case(
                                    Variable("Ctx.P.Next_State"),
                                    [
                                        *[(Variable(state_id(read.state)), TRUE) for read in reads],
                                        (Variable("others"), FALSE),
                                    ],
                                ),
                            )
                            for channel, reads in channel_reads.items()
                        ],
                    ),
                )
            ],
        )

    @staticmethod
    def _create_read_buffer_size_function(
        channel_writes: dict[ID, list[ChannelAccess]],
        is_global: Callable[[ID], bool],
    ) -> UnitPart:
        specification = FunctionSpecification(
            "Read_Buffer_Size",
            const.TYPES_LENGTH,
            [Parameter(["Ctx"], "Context'Class"), Parameter(["Chan"], "Channel")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Call("Initialized", [Variable("Ctx")]),
                                Call("Has_Data", [Variable("Ctx"), Variable("Chan")]),
                            )
                        ),
                    ],
                ),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Chan"),
                        [
                            (
                                Variable(f"C_{channel}"),
                                Case(
                                    Variable("Ctx.P.Next_State"),
                                    [
                                        *[
                                            (
                                                Variable(state_id(write.state)),
                                                Call(
                                                    write.message_type * "Byte_Size",
                                                    [
                                                        Variable(
                                                            context_id(write.message, is_global)
                                                        )
                                                    ],
                                                ),
                                            )
                                            for write in writes
                                        ],
                                        (Variable("others"), const.UNREACHABLE),
                                    ],
                                ),
                            )
                            for channel, writes in channel_writes.items()
                        ],
                    ),
                )
            ],
        )

    @staticmethod
    def _create_write_buffer_size_function(
        channel_reads: dict[ID, list[ChannelAccess]],
        is_global: Callable[[ID], bool],
    ) -> UnitPart:
        specification = FunctionSpecification(
            "Write_Buffer_Size",
            const.TYPES_LENGTH,
            [
                Parameter(["Ctx"], "Context'Class"),
                Parameter(["Chan"], "Channel"),
            ],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Call("Initialized", [Variable("Ctx")]),
                                Call("Needs_Data", [Variable("Ctx"), Variable("Chan")]),
                            )
                        ),
                    ],
                ),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Chan"),
                        [
                            (
                                Variable(f"C_{channel}"),
                                Case(
                                    Variable("Ctx.P.Next_State"),
                                    [
                                        *[
                                            (
                                                Variable(state_id(read.state)),
                                                Call(
                                                    read.message_type * "Buffer_Length",
                                                    [Variable(context_id(read.message, is_global))],
                                                ),
                                            )
                                            for read in reads
                                        ],
                                        (Variable("others"), const.UNREACHABLE),
                                    ],
                                )
                                if reads
                                else Number(0),
                            )
                            for channel, reads in channel_reads.items()
                        ],
                    ),
                )
            ],
        )

    def _create_read_procedure(
        self,
        channel_writes: dict[ID, list[ChannelAccess]],
        is_global: Callable[[ID], bool],
    ) -> UnitPart:
        self._session_context.used_types.append(const.TYPES_INDEX)
        self._session_context.used_types.append(const.TYPES_LENGTH)

        specification = ProcedureSpecification(
            "Read",
            [
                Parameter(["Ctx"], "Context'Class"),
                Parameter(["Chan"], "Channel"),
                OutParameter(["Buffer"], const.TYPES_BYTES),
                Parameter(["Offset"], const.TYPES_LENGTH, Number(0)),
            ],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Call("Initialized", [Variable("Ctx")]),
                                Call("Has_Data", [Variable("Ctx"), Variable("Chan")]),
                                Greater(Length("Buffer"), Number(0)),
                                LessEqual(
                                    Variable("Offset"),
                                    Sub(Last(const.TYPES_LENGTH), Length("Buffer")),
                                ),
                                LessEqual(
                                    Add(Length("Buffer"), Variable("Offset")),
                                    Call("Read_Buffer_Size", [Variable("Ctx"), Variable("Chan")]),
                                ),
                            )
                        ),
                        Postcondition(
                            Call("Initialized", [Variable("Ctx")]),
                        ),
                    ],
                ),
            ],
            [
                SubprogramBody(
                    specification,
                    [
                        ExpressionFunctionDeclaration(
                            FunctionSpecification(
                                "Read_Pre",
                                "Boolean",
                                [Parameter(["Message_Buffer"], const.TYPES_BYTES)],
                            ),
                            AndThen(
                                Greater(Length("Buffer"), Number(0)),
                                Less(Variable("Offset"), Length("Message_Buffer")),
                            ),
                        ),
                        SubprogramBody(
                            ProcedureSpecification(
                                "Read", [Parameter(["Message_Buffer"], const.TYPES_BYTES)]
                            ),
                            [
                                ObjectDeclaration(
                                    ["Length"],
                                    const.TYPES_INDEX,
                                    Call(
                                        const.TYPES_INDEX,
                                        [
                                            Min(
                                                const.TYPES_LENGTH,
                                                Length("Buffer"),
                                                Sub(
                                                    Length("Message_Buffer"),
                                                    Variable("Offset"),
                                                ),
                                            )
                                        ],
                                    ),
                                    constant=True,
                                ),
                                ObjectDeclaration(
                                    ["Buffer_Last"],
                                    const.TYPES_INDEX,
                                    Add(First("Buffer"), -Number(1), Variable("Length")),
                                    constant=True,
                                ),
                            ],
                            [
                                Assignment(
                                    Slice(
                                        Variable("Buffer"),
                                        First("Buffer"),
                                        Call(const.TYPES_INDEX, [Variable("Buffer_Last")]),
                                    ),
                                    Slice(
                                        Variable("Message_Buffer"),
                                        Call(
                                            const.TYPES_INDEX,
                                            [
                                                Add(
                                                    Call(
                                                        const.TYPES_LENGTH,
                                                        [First("Message_Buffer")],
                                                    ),
                                                    Variable("Offset"),
                                                )
                                            ],
                                        ),
                                        Add(
                                            First("Message_Buffer"),
                                            -Number(2),
                                            Call(
                                                const.TYPES_INDEX,
                                                [Add(Variable("Offset"), Number(1))],
                                            ),
                                            Variable("Length"),
                                        ),
                                    ),
                                )
                            ],
                            aspects=[Precondition(Call("Read_Pre", [Variable("Message_Buffer")]))],
                        ),
                        *[
                            GenericProcedureInstantiation(
                                (type_ * "Read").flat,
                                ProcedureSpecification(type_ * "Generic_Read"),
                                ["Read", "Read_Pre"],
                            )
                            for type_ in sorted(
                                {
                                    write.message_type
                                    for writes in channel_writes.values()
                                    for write in writes
                                }
                            )
                        ],
                    ],
                    [
                        Assignment(
                            Variable("Buffer"),
                            NamedAggregate(("others", Number(0))),
                        ),
                        CaseStatement(
                            Variable("Chan"),
                            [
                                (
                                    Variable(f"C_{channel}"),
                                    [
                                        CaseStatement(
                                            Variable("Ctx.P.Next_State"),
                                            [
                                                *[
                                                    (
                                                        Variable(state_id(write.state)),
                                                        [
                                                            CallStatement(
                                                                (write.message_type * "Read").flat,
                                                                [
                                                                    Variable(
                                                                        context_id(
                                                                            write.message, is_global
                                                                        )
                                                                    )
                                                                ],
                                                            ),
                                                        ],
                                                    )
                                                    for write in writes
                                                ],
                                                (
                                                    Variable("others"),
                                                    [
                                                        PragmaStatement(
                                                            "Warnings",
                                                            [
                                                                Variable("Off"),
                                                                String("unreachable code"),
                                                            ],
                                                        ),
                                                        NullStatement(),
                                                        PragmaStatement(
                                                            "Warnings",
                                                            [
                                                                Variable("On"),
                                                                String("unreachable code"),
                                                            ],
                                                        ),
                                                    ],
                                                ),
                                            ],
                                        )
                                    ],
                                )
                                for channel, writes in channel_writes.items()
                            ],
                        ),
                    ],
                )
            ],
        )

    def _create_write_procedure(
        self,
        channel_reads: dict[ID, list[ChannelAccess]],
        is_global: Callable[[ID], bool],
    ) -> UnitPart:
        self._session_context.used_types.append(const.TYPES_INDEX)
        self._session_context.used_types.append(const.TYPES_LENGTH)

        specification = ProcedureSpecification(
            "Write",
            [
                InOutParameter(["Ctx"], "Context'Class"),
                Parameter(["Chan"], "Channel"),
                Parameter(["Buffer"], const.TYPES_BYTES),
                Parameter(["Offset"], const.TYPES_LENGTH, Number(0)),
            ],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Call("Initialized", [Variable("Ctx")]),
                                Call("Needs_Data", [Variable("Ctx"), Variable("Chan")]),
                                Greater(Length("Buffer"), Number(0)),
                                LessEqual(
                                    Variable("Offset"),
                                    Sub(Last(const.TYPES_LENGTH), Length("Buffer")),
                                ),
                                LessEqual(
                                    Add(Length("Buffer"), Variable("Offset")),
                                    Call("Write_Buffer_Size", [Variable("Ctx"), Variable("Chan")]),
                                ),
                            )
                        ),
                        Postcondition(
                            Call("Initialized", [Variable("Ctx")]),
                        ),
                    ],
                ),
            ],
            [
                SubprogramBody(
                    specification,
                    [
                        ObjectDeclaration(
                            ["Write_Buffer_Length"],
                            const.TYPES_LENGTH,
                            Call("Write_Buffer_Size", [Variable("Ctx"), Variable("Chan")]),
                            constant=True,
                        ),
                        ExpressionFunctionDeclaration(
                            FunctionSpecification(
                                "Write_Pre",
                                "Boolean",
                                [
                                    Parameter(["Context_Buffer_Length"], const.TYPES_LENGTH),
                                    Parameter(["Offset"], const.TYPES_LENGTH),
                                ],
                            ),
                            AndThen(
                                Greater(Length("Buffer"), Number(0)),
                                Equal(
                                    Variable("Context_Buffer_Length"),
                                    Variable("Write_Buffer_Length"),
                                ),
                                LessEqual(
                                    Variable("Offset"),
                                    Sub(Last(const.TYPES_LENGTH), Length("Buffer")),
                                ),
                                LessEqual(
                                    Add(Length("Buffer"), Variable("Offset")),
                                    Variable("Write_Buffer_Length"),
                                ),
                            ),
                        ),
                        SubprogramBody(
                            ProcedureSpecification(
                                "Write",
                                [
                                    OutParameter(["Message_Buffer"], const.TYPES_BYTES),
                                    OutParameter(["Length"], const.TYPES_LENGTH),
                                    Parameter(["Context_Buffer_Length"], const.TYPES_LENGTH),
                                    Parameter(["Offset"], const.TYPES_LENGTH),
                                ],
                            ),
                            [],
                            [
                                Assignment(Variable("Length"), Length("Buffer")),
                                Assignment(
                                    Variable("Message_Buffer"),
                                    NamedAggregate(("others", Number(0))),
                                ),
                                Assignment(
                                    Slice(
                                        Variable("Message_Buffer"),
                                        First("Message_Buffer"),
                                        Call(
                                            const.TYPES_INDEX,
                                            [
                                                Add(
                                                    Call(
                                                        const.TYPES_LENGTH,
                                                        [First("Message_Buffer")],
                                                    ),
                                                    -Number(1),
                                                    Variable("Length"),
                                                )
                                            ],
                                        ),
                                    ),
                                    Variable("Buffer"),
                                ),
                            ],
                            aspects=[
                                Precondition(
                                    AndThen(
                                        Call(
                                            "Write_Pre",
                                            [
                                                Variable("Context_Buffer_Length"),
                                                Variable("Offset"),
                                            ],
                                        ),
                                        LessEqual(
                                            Variable("Offset"),
                                            Sub(
                                                Last(const.TYPES_LENGTH),
                                                Length("Message_Buffer"),
                                            ),
                                        ),
                                        Equal(
                                            Add(Length("Message_Buffer"), Variable("Offset")),
                                            Variable("Write_Buffer_Length"),
                                        ),
                                    )
                                ),
                                Postcondition(
                                    LessEqual(Variable("Length"), Length("Message_Buffer")),
                                ),
                            ],
                        ),
                        *[
                            GenericProcedureInstantiation(
                                (type_ * "Write").flat,
                                ProcedureSpecification(type_ * "Generic_Write"),
                                ["Write", "Write_Pre"],
                            )
                            for type_ in sorted(
                                {
                                    read.message_type
                                    for reads in channel_reads.values()
                                    for read in reads
                                }
                            )
                        ],
                    ],
                    [
                        CaseStatement(
                            Variable("Chan"),
                            [
                                (
                                    Variable(f"C_{channel}"),
                                    [
                                        CaseStatement(
                                            Variable("Ctx.P.Next_State"),
                                            [
                                                *[
                                                    (
                                                        Variable(state_id(write.state)),
                                                        [
                                                            CallStatement(
                                                                (write.message_type * "Write").flat,
                                                                [
                                                                    Variable(
                                                                        context_id(
                                                                            write.message, is_global
                                                                        )
                                                                    ),
                                                                    Variable("Offset"),
                                                                ],
                                                            ),
                                                        ],
                                                    )
                                                    for write in reads
                                                ],
                                                (
                                                    Variable("others"),
                                                    [
                                                        PragmaStatement(
                                                            "Warnings",
                                                            [
                                                                Variable("Off"),
                                                                String("unreachable code"),
                                                            ],
                                                        ),
                                                        NullStatement(),
                                                        PragmaStatement(
                                                            "Warnings",
                                                            [
                                                                Variable("On"),
                                                                String("unreachable code"),
                                                            ],
                                                        ),
                                                    ],
                                                ),
                                            ],
                                        )
                                    ],
                                )
                                for channel, reads in channel_reads.items()
                            ],
                        ),
                    ],
                )
            ],
        )

    def _evaluate_declarations(
        self,
        declarations: Iterable[decl.BasicDeclaration],
        is_global: Callable[[ID], bool] = None,
        session_global: bool = False,
    ) -> EvaluatedDeclaration:
        if session_global:

            def always_true(_: ID) -> bool:
                return True

            is_global = always_true

        assert is_global

        result = EvaluatedDeclaration()
        has_composite_declarations = False

        for declaration in declarations:
            if isinstance(declaration, decl.VariableDeclaration):
                result += self._declare(
                    declaration.identifier,
                    declaration.type_,
                    is_global,
                    declaration.location,
                    declaration.expression,
                    session_global=session_global,
                )
                if isinstance(declaration.type_, (rty.Message, rty.Sequence)):
                    has_composite_declarations |= True
            elif isinstance(declaration, decl.RenamingDeclaration):
                fail(
                    f'renaming declaration "{declaration.identifier}" not yet supported',
                    Subsystem.GENERATOR,
                    location=declaration.location,
                )
            else:
                fatal_fail(
                    f'unexpected declaration "{declaration.identifier}"',
                    Subsystem.GENERATOR,
                    location=declaration.location,
                )

        if session_global and self._allocator.required:
            result.initialization.insert(
                0,
                CallStatement(
                    self._allocator.unit_identifier * "Initialize",
                    [Variable("Ctx.P.Slots"), Variable("Ctx.P.Memory")],
                ),
            )
            result.finalization.append(
                CallStatement(
                    self._allocator.unit_identifier * "Finalize",
                    [Variable("Ctx.P.Slots")],
                )
            )

        return result

    def _state_action(
        self,
        state: ID,
        action: stmt.Statement,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        if isinstance(action, stmt.VariableAssignment):
            result = self._assign(
                action.identifier,
                action.type_,
                action.expression,
                exception_handler,
                is_global,
                state,
                action.location,
            )

        elif isinstance(action, stmt.MessageFieldAssignment):
            result = self._assign_message_field(
                action.message,
                action.field,
                action.type_,
                action.expression,
                exception_handler,
                is_global,
            )

        elif isinstance(action, stmt.Append):
            result = self._append(action, exception_handler, is_global, state)

        elif isinstance(action, stmt.Extend):
            fail(
                "Extend statement not yet supported",
                Subsystem.GENERATOR,
                location=action.location,
            )

        elif isinstance(action, stmt.Reset):
            result = self._reset(action, is_global)

        elif isinstance(action, stmt.Read):
            result = self._read(action, is_global)

        elif isinstance(action, stmt.Write):
            result = self._write(action)

        else:
            fatal_fail(
                f'unexpected statement "{type(action).__name__}"',
                Subsystem.GENERATOR,
                location=action.location,
            )

        return [CommentStatement(str(action.location)), *result]

    def _declare(  # pylint: disable = too-many-arguments, too-many-branches
        self,
        identifier: ID,
        type_: rty.Type,
        is_global: Callable[[ID], bool],
        alloc_id: Optional[Location],
        expression: expr.Expr = None,
        constant: bool = False,
        session_global: bool = False,
    ) -> EvaluatedDeclaration:
        result = EvaluatedDeclaration()

        if expression:
            for e in expression.findall(lambda x: isinstance(x, expr.Call)):
                fail(
                    "initialization using function call not yet supported",
                    Subsystem.GENERATOR,
                    location=e.location,
                )

        if type_ == rty.OPAQUE:
            initialization = expression
            object_type: Expr = Variable(const.TYPES_BYTES)
            aspects = []

            if isinstance(expression, expr.Aggregate):
                if len(expression.elements) == 0:
                    object_type = Slice(
                        Variable(const.TYPES_BYTES),
                        Last(const.TYPES_INDEX),
                        First(const.TYPES_INDEX),
                    )
                    initialization = None
                if len(expression.elements) > 0:
                    aspects.append(
                        SizeAspect(Mul(Number(len(expression.elements)), Size(const.TYPES_BYTE)))
                    )

            result.global_declarations.append(
                ObjectDeclaration(
                    [identifier],
                    object_type,
                    initialization.substituted(self._substitution(is_global)).ada_expr()
                    if initialization
                    else None,
                    constant=constant if initialization else False,
                    aliased=False,
                    aspects=aspects,
                )
            )

        elif isinstance(type_, (rty.Integer, rty.Enumeration)):
            result.global_declarations.append(
                ObjectDeclaration(
                    [identifier],
                    self._ada_type(type_.identifier),
                    expression.ada_expr() if expression else None,
                )
            )
            if session_global and expression:
                result.initialization.append(
                    Assignment(
                        variable_id(identifier, is_global),
                        self._convert_type(expression, type_).ada_expr(),
                    ),
                )

        elif isinstance(type_, (rty.Message, rty.Sequence)):
            if expression is not None:
                fail(
                    f"initialization for {type_} not yet supported",
                    Subsystem.GENERATOR,
                    location=expression.location,
                )

            type_identifier = self._ada_type(type_.identifier)

            result.global_declarations.append(
                self._declare_context(
                    identifier,
                    type_identifier,
                    (lambda x: False) if session_global else is_global,
                )
            )
            result.initialization_declarations.append(self._declare_buffer(identifier))
            result.initialization.extend(
                [
                    *self._allocate_buffer(identifier, alloc_id),
                    self._initialize_context(
                        identifier,
                        type_identifier,
                        is_global,
                        parameters=(
                            {
                                n: First(self._ada_type(t.identifier))
                                for n, t in type_.parameter_types.items()
                                if isinstance(t, (rty.Integer, rty.Enumeration))
                            }
                            if isinstance(type_, rty.Message)
                            else None
                        ),
                    ),
                ]
            )
            result.finalization.extend(
                self._free_context_buffer(identifier, type_identifier, is_global, alloc_id)
            )
        elif isinstance(type_, rty.Structure):
            # Messages with initialization clauses are not optimized
            assert expression is None

            type_identifier = self._ada_type(type_.identifier)
            result.initialization_declarations.extend(
                [
                    ObjectDeclaration(
                        [identifier],
                        type_identifier * "Structure",
                    ),
                ],
            )

        else:
            fatal_fail(
                f"unexpected variable declaration for {type_}",
                Subsystem.GENERATOR,
                location=identifier.location,
            )

        assert isinstance(
            type_, (rty.Integer, rty.Enumeration, rty.Message, rty.Sequence, rty.Structure)
        )

        if session_global:
            self._session_context.referenced_types.append(type_.identifier)
        else:
            self._session_context.referenced_types_body.append(type_.identifier)

        return result

    def _declare_and_assign(  # pylint: disable = too-many-arguments
        self,
        variables: Sequence[tuple[ID, rty.Type, expr.Expr]],
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
        constant: bool = False,
    ) -> Sequence[Statement]:
        if not variables:
            return statements

        k, type_, v = variables[0]
        initialized_in_declaration = isinstance(v, expr.Aggregate) and type_ == rty.OPAQUE
        evaluated_declaration = self._declare(
            k,
            type_,
            is_global,
            alloc_id,
            v if initialized_in_declaration else None,
            constant=constant,
        )

        return [
            Declare(
                [
                    *evaluated_declaration.global_declarations,
                    *evaluated_declaration.initialization_declarations,
                ],
                [
                    *[
                        *evaluated_declaration.initialization,
                        *(
                            self._assign(k, type_, v, exception_handler, is_global, state, alloc_id)
                            if not initialized_in_declaration
                            else []
                        ),
                    ],
                    *self._declare_and_assign(
                        variables[1:],
                        statements,
                        exception_handler,
                        is_global,
                        state,
                        alloc_id,
                        constant,
                    ),
                    *evaluated_declaration.finalization,
                ],
            ),
        ]

    def _assign(  # pylint: disable = too-many-arguments
        self,
        target: ID,
        target_type: rty.Type,
        expression: expr.Expr,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        if isinstance(expression, expr.MessageAggregate):
            return self._assign_to_message_aggregate(
                target, expression, exception_handler, is_global, state
            )

        if isinstance(expression, expr.DeltaMessageAggregate):
            return self._assign_to_delta_message_aggregate(
                target, expression, exception_handler, is_global, state
            )

        if isinstance(target_type, rty.Message):
            for v in expression.findall(
                lambda x: isinstance(x, expr.Variable) and x.identifier == target
            ):
                fail(
                    f'referencing assignment target "{target}" of type message in expression'
                    " not yet supported",
                    Subsystem.GENERATOR,
                    location=v.location,
                )

        if isinstance(expression, expr.Binding):
            return self._assign_to_binding(
                target, expression, exception_handler, is_global, state, alloc_id
            )

        if isinstance(expression, expr.Selected):
            return self._assign_to_selected(target, expression, exception_handler, is_global)

        if isinstance(expression, expr.Head):
            return self._assign_to_head(
                target, expression, exception_handler, is_global, state, alloc_id
            )

        if isinstance(expression, expr.Comprehension):
            assert isinstance(target_type, rty.Sequence)
            return self._assign_to_comprehension(
                target, target_type, expression, exception_handler, is_global, state, alloc_id
            )

        if isinstance(expression, expr.Call):
            return self._assign_to_call(target, expression, exception_handler, is_global, state)

        if isinstance(expression, expr.Conversion):
            return self._assign_to_conversion(target, expression, exception_handler, is_global)

        if isinstance(
            expression,
            (
                expr.Variable,
                expr.Literal,
                expr.Number,
                expr.MathBinExpr,
                expr.MathAssExpr,
                expr.Relation,
                expr.Attribute,
                expr.Aggregate,
                expr.BoolAssExpr,
                expr.CaseExpr,
            ),
        ) and (
            isinstance(expression.type_, (rty.AnyInteger, rty.Enumeration))
            or expression.type_ == rty.OPAQUE
        ):
            value = expression.substituted(self._substitution(is_global))
            assert isinstance(
                target_type, (rty.Integer, rty.Enumeration, rty.Message, rty.Sequence)
            )
            return self._if_valid_fields(
                expression,
                [
                    Assignment(
                        variable_id(target, is_global),
                        self._convert_type(value, target_type).ada_expr(),
                    ),
                ],
                exception_handler,
                is_global,
            )

        if isinstance(expression, expr.Variable) and isinstance(
            expression.type_, (rty.Message, rty.Sequence)
        ):
            _unsupported_expression(expression, "in assignment")

        _unexpected_expression(expression, "in assignment")

    def _assign_to_binding(  # pylint: disable = too-many-branches, too-many-arguments
        self,
        target: ID,
        binding: expr.Binding,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        variables = []

        for k, v in binding.data.items():
            if isinstance(binding.expr, expr.MessageAggregate):
                assert isinstance(binding.expr.type_, rty.Message)
                for f, f_v in binding.expr.field_values.items():
                    if expr.Variable(k) == f_v:
                        type_ = binding.expr.type_.types[f]
                        break
                    if expr.Opaque(k) == f_v:
                        type_ = v.type_
                        break
                else:
                    fatal_fail(
                        f'"{k}" must be value of message aggregate',
                        Subsystem.GENERATOR,
                        location=binding.location,
                    )
            elif isinstance(binding.expr, expr.Call):
                for a, t in zip(binding.expr.args, binding.expr.argument_types):
                    if expr.Variable(k) == a:
                        type_ = t
                        break
                else:
                    fatal_fail(
                        f'"{k}" must be argument of call',
                        Subsystem.GENERATOR,
                        location=binding.location,
                    )
            elif isinstance(binding.expr, expr.Conversion):
                assert isinstance(binding.expr.argument, expr.Selected)
                assert isinstance(binding.expr.argument.prefix, expr.Variable)
                assert binding.expr.argument.prefix.identifier == k
                type_ = binding.expr.argument.prefix.type_
            elif isinstance(binding.expr, expr.Selected):
                assert isinstance(binding.expr.prefix, expr.Variable)
                assert binding.expr.prefix.identifier == k
                type_ = binding.expr.prefix.type_
            else:
                fail(
                    f'binding for expression "{type(binding.expr).__name__}" not yet supported',
                    Subsystem.GENERATOR,
                    location=binding.expr.location,
                )
            variables.append((k, type_, v))

        return self._declare_and_assign(
            variables,
            self._assign(
                target,
                type_,
                binding.expr,
                exception_handler,
                is_global,
                state,
                alloc_id=alloc_id,
            ),
            exception_handler,
            is_global,
            state,
            alloc_id=alloc_id,
            constant=True,
        )

    def _assign_to_selected(
        self,
        target: ID,
        selected: expr.Selected,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        if not isinstance(selected.prefix, expr.Variable):
            fail(
                f'accessing field of expression "{type(selected.prefix).__name__}"'
                " not yet supported",
                Subsystem.GENERATOR,
                location=selected.prefix.location,
            )

        if isinstance(selected.prefix.type_, rty.Structure):
            return [
                Assignment(
                    Variable(variable_id(target, is_global)),
                    Variable(selected.prefix.identifier * selected.selector),
                )
            ]

        assert isinstance(selected.prefix.type_, rty.Message)

        message_type = selected.prefix.type_.identifier
        message_context = context_id(selected.prefix.identifier, is_global)
        selector = selected.selector

        if (
            isinstance(selected.type_, (rty.AnyInteger, rty.Enumeration))
            or selected.type_ == rty.OPAQUE
        ):
            if selector in selected.prefix.type_.parameter_types:
                return [
                    Assignment(
                        Variable(variable_id(target, is_global)),
                        Variable(message_context * selector),
                    )
                ]

            return [
                self._if(
                    Call(
                        message_type * "Valid",
                        [
                            Variable(message_context),
                            Variable(message_type * f"F_{selector}"),
                        ],
                    ),
                    [
                        Assignment(
                            Variable(variable_id(target, is_global)),
                            Call(
                                message_type * f"Get_{selector}",
                                [Variable(message_context)],
                            ),
                        )
                    ],
                    f'access to invalid field "{selector}" of' f' "{message_context}"',
                    exception_handler,
                )
            ]

        if isinstance(selected.type_, rty.Sequence):
            # https://github.com/Componolit/RecordFlux/issues/577
            # The relevant buffer part has to be copied from the message context into a
            # sequence context. With the current implementation the sequence needs to
            # be parsed after copying. It must be ensured that the sequence is not
            # accidentally parsed beyond the original end of the sequence in the message
            # (i.e. misinterpreting trailing bytes in the new buffer as sequence elements).
            fail(
                "copying of sequence not yet supported",
                Subsystem.GENERATOR,
                location=target.location,
            )

        fatal_fail(
            f'unexpected type ({selected.type_}) for "{selected}" in assignment of "{target}"',
            Subsystem.GENERATOR,
            location=target.location,
        )

    def _assign_to_message_aggregate(
        self,
        target: ID,
        message_aggregate: expr.MessageAggregate,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
    ) -> Sequence[Statement]:
        assert isinstance(message_aggregate.type_, rty.Message)

        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        target_type = message_aggregate.type_.identifier
        target_context = context_id(target, is_global)
        parameter_values = [
            (f, v, t)
            for f, v in message_aggregate.field_values.items()
            if f in message_aggregate.type_.parameter_types
            for t in [message_aggregate.type_.parameter_types[f]]
            if isinstance(t, (rty.Integer, rty.Enumeration))
        ]

        assign_to_message_aggregate = [
            CallStatement(
                target_type * "Reset",
                [
                    Variable(target_context),
                ],
                {
                    p: self._convert_type(v, t)
                    .substituted(self._substitution(is_global))
                    .ada_expr()
                    for p, v, t in parameter_values
                },
            ),
            *self._set_message_fields(
                target_context, message_aggregate, exception_handler, is_global, state
            ),
        ]

        return assign_to_message_aggregate

    def _assign_to_delta_message_aggregate(
        self,
        target: ID,
        delta_message_aggregate: expr.DeltaMessageAggregate,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
    ) -> Sequence[Statement]:
        assert isinstance(delta_message_aggregate.type_, rty.Message)

        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        target_type_id = delta_message_aggregate.type_.identifier
        target_context = context_id(target, is_global)

        fields = list(delta_message_aggregate.field_values)
        first_field = fields[0]
        last_field = fields[-1]

        required_space, required_space_precondition = self._required_space(
            self._message_subpath_size(delta_message_aggregate), is_global, state
        )

        return [
            self._raise_exception_if(
                Not(
                    Call(
                        target_type_id * "Valid_Next",
                        [
                            Variable(target_context),
                            Variable(target_type_id * model.Field(first_field).affixed_name),
                        ],
                    )
                ),
                f'trying to set message fields "{first_field}" to "{last_field}" although'
                f' "{first_field}" is not valid next field',
                exception_handler,
            ),
            *(
                [
                    self._raise_exception_if(
                        Not(required_space_precondition),
                        "violated precondition for calculating required space for setting message"
                        f' fields "{first_field}" to "{last_field}" (one of the message arguments'
                        " is invalid or has a too small buffer)",
                        exception_handler,
                    )
                ]
                if required_space_precondition
                else []
            ),
            self._raise_exception_if(
                Less(
                    Call(
                        target_type_id * "Available_Space",
                        [
                            Variable(target_context),
                            Variable(target_type_id * model.Field(first_field).affixed_name),
                        ],
                    ),
                    required_space,
                ),
                f'insufficient space for setting message fields "{first_field}" to "{last_field}"',
                exception_handler,
            ),
            *[
                s
                for f, v in delta_message_aggregate.field_values.items()
                for s in self._set_message_field(
                    target_context,
                    f,
                    delta_message_aggregate.type_,
                    v,
                    exception_handler,
                    is_global,
                )
            ],
        ]

    def _assign_to_head(  # pylint: disable = too-many-arguments
        self,
        target: ID,
        head: expr.Head,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        assert isinstance(head.prefix.type_, (rty.Sequence, rty.Aggregate))

        if not isinstance(head.prefix.type_.element, (rty.Integer, rty.Enumeration, rty.Message)):
            fatal_fail(
                f"unexpected sequence element type ({head.prefix.type_.element})"
                f' for "{head}" in assignment of "{target}"',
                Subsystem.GENERATOR,
                location=head.location,
            )

        if isinstance(head.prefix, expr.Comprehension):
            return self._assign_to_head_comprehension(
                target, head, exception_handler, is_global, state, alloc_id
            )
        return self._assign_to_head_sequence(
            target, head, exception_handler, is_global, state, alloc_id
        )

    def _assign_to_head_comprehension(  # pylint: disable = too-many-arguments, too-many-locals
        self,
        target: ID,
        head: expr.Head,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        comprehension = head.prefix
        assert isinstance(comprehension, expr.Comprehension)
        assert isinstance(comprehension.sequence.type_, rty.Sequence)
        sequence_type_id = comprehension.sequence.type_.identifier
        sequence_element_type = comprehension.sequence.type_.element

        if isinstance(sequence_element_type, rty.Message):
            if isinstance(comprehension.sequence, expr.Variable):
                sequence_id = ID(f"{comprehension.sequence}")
                comprehension_sequence_id = copy_id(sequence_id)
            elif isinstance(comprehension.sequence, expr.Selected):
                selected = comprehension.sequence

                if not isinstance(selected.prefix, expr.Variable):
                    _unsupported_expression(
                        selected.prefix, "as prefix of Selected in list comprehension"
                    )

                sequence_id = ID(
                    f"RFLX_{selected.prefix}_{selected.selector}",
                    location=selected.location,
                )
                comprehension_sequence_id = sequence_id

            else:
                assert False

            def comprehension_statements(
                local_exception_handler: ExceptionHandler,
            ) -> list[Statement]:
                assert isinstance(comprehension, expr.Comprehension)
                assert isinstance(head.type_, (rty.Integer, rty.Enumeration, rty.Message))
                assert isinstance(
                    sequence_element_type, (rty.Message, rty.Integer, rty.Enumeration)
                )
                default_assignment = []
                if isinstance(head.type_, (rty.Integer, rty.Enumeration)):
                    default_assignment = [Assignment(target, First(head.type_.identifier))]
                return [
                    Declare(
                        [ObjectDeclaration([found_id(target)], "Boolean", FALSE)],
                        [
                            *default_assignment,
                            self._comprehension(
                                comprehension_sequence_id,
                                sequence_type_id,
                                target,
                                head.type_,
                                comprehension.iterator,
                                sequence_element_type.identifier,
                                comprehension.selector,
                                comprehension.condition,
                                local_exception_handler,
                                is_global,
                                alloc_id,
                            ),
                            self._raise_exception_if(
                                Not(Variable(found_id(target))),
                                f'failed to find valid element in "{sequence_id}"',
                                local_exception_handler,
                            ),
                        ],
                    )
                ]

            if isinstance(comprehension.sequence, expr.Variable):
                return [
                    self._declare_sequence_copy(
                        sequence_id,
                        sequence_type_id,
                        comprehension_statements,
                        exception_handler,
                        is_global,
                        alloc_id,
                    )
                ]
            if isinstance(comprehension.sequence, expr.Selected):
                assert isinstance(selected.prefix, expr.Variable)
                assert isinstance(selected.prefix.type_, rty.Message)
                message_id = ID(selected.prefix.name)
                message_type = selected.prefix.type_.identifier
                message_field = selected.selector
                source_buffer_size = self._allocator.get_size(message_id, state)
                target_buffer_size = self._allocator.get_size(target, state)

                return [
                    self._if_well_formed_message(
                        message_type,
                        context_id(message_id, is_global),
                        [
                            self._declare_message_field_sequence_copy(
                                message_id,
                                message_type,
                                message_field,
                                sequence_id,
                                sequence_type_id,
                                comprehension_statements,
                                target_buffer_size < source_buffer_size,
                                exception_handler,
                                is_global,
                                alloc_id,
                            ),
                        ],
                        exception_handler,
                    )
                ]
            assert False
        fail(
            f"iterating over sequence of {sequence_element_type}"
            " in list comprehension not yet supported",
            Subsystem.GENERATOR,
            location=comprehension.sequence.location,
        )

    def _assign_to_head_sequence(  # pylint: disable = too-many-arguments, too-many-locals
        self,
        target: ID,
        head: expr.Head,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        assert isinstance(head.prefix.type_, rty.Sequence)
        assert isinstance(head.type_, (rty.Integer, rty.Enumeration, rty.Message))

        if not isinstance(head.prefix, expr.Variable):
            _unsupported_expression(head.prefix, "in Head attribute")

        target_type = head.type_.identifier
        sequence_type = head.prefix.type_.identifier
        sequence_id = head.prefix.identifier
        sequence_context = context_id(sequence_id, is_global)
        sequence_identifier = ID(f"{head.prefix}")

        if isinstance(head.prefix.type_.element, (rty.Integer, rty.Enumeration)):
            return [
                self._if(
                    AndThen(
                        Call(sequence_type * "Valid", [Variable(sequence_context)]),
                        Call(
                            sequence_type * "Has_Element",
                            [Variable(sequence_context)],
                        ),
                        GreaterEqual(
                            Call(
                                sequence_type * "Size",
                                [Variable(sequence_context)],
                            ),
                            Size(head.prefix.type_.element.identifier),
                        ),
                    ),
                    [
                        Assignment(
                            Variable(variable_id(target, is_global)),
                            Call(
                                sequence_type * "Head",
                                [Variable(sequence_context)],
                            ),
                        )
                    ],
                    f"access to first element of invalid or empty sequence"
                    f' "{sequence_context}"',
                    exception_handler,
                )
            ]

        assert isinstance(head.prefix.type_.element, rty.Message)

        self._session_context.used_types_body.append(const.TYPES_LENGTH)
        self._session_context.referenced_types_body.append(target_type)

        target_context = context_id(target, is_global)
        target_buffer = buffer_id("RFLX_Target_" + target)
        element_context = ID("RFLX_Head_Ctx")
        copied_sequence_context = context_id(copy_id(sequence_id), is_global)
        source_buffer_size = self._allocator.get_size(sequence_id, state)
        target_buffer_size = self._allocator.get_size(target, state)

        def statements(exception_handler: ExceptionHandler) -> list[Statement]:
            update_context = self._update_context(
                copied_sequence_context,
                element_context,
                sequence_type,
            )
            local_exception_handler = exception_handler.copy(update_context)

            return [
                IfStatement(
                    [
                        (
                            Call(
                                sequence_type * "Has_Element",
                                [Variable(copied_sequence_context)],
                            ),
                            [
                                Declare(
                                    [
                                        ObjectDeclaration(
                                            [element_context],
                                            target_type * "Context",
                                        ),
                                        ObjectDeclaration(
                                            [target_buffer],
                                            self._prefix * const.TYPES_BYTES_PTR,
                                        ),
                                    ],
                                    [
                                        CallStatement(
                                            sequence_type * "Switch",
                                            [
                                                Variable(copied_sequence_context),
                                                Variable(element_context),
                                            ],
                                        ),
                                        CallStatement(
                                            target_type * "Verify_Message",
                                            [Variable(element_context)],
                                        ),
                                        self._if_well_formed_message(
                                            target_type,
                                            element_context,
                                            [
                                                *self._take_buffer(
                                                    target,
                                                    target_type,
                                                    is_global,
                                                    target_buffer,
                                                ),
                                                self._copy_to_buffer(
                                                    target_type,
                                                    element_context,
                                                    target_buffer,
                                                    target_buffer_size < source_buffer_size,
                                                    local_exception_handler.copy(
                                                        [
                                                            CallStatement(
                                                                target_type * "Initialize",
                                                                [
                                                                    Variable(target_context),
                                                                    Variable(target_buffer),
                                                                ],
                                                            )
                                                        ]
                                                    ),
                                                ),
                                                CallStatement(
                                                    target_type * "Initialize",
                                                    [
                                                        Variable(target_context),
                                                        Variable(target_buffer),
                                                        Call(
                                                            target_type * "Size",
                                                            [Variable(element_context)],
                                                        ),
                                                    ],
                                                ),
                                                CallStatement(
                                                    target_type * "Verify_Message",
                                                    [
                                                        Variable(target_context),
                                                    ],
                                                ),
                                            ],
                                            local_exception_handler,
                                        ),
                                        *self._update_context(
                                            copied_sequence_context,
                                            element_context,
                                            sequence_type,
                                        ),
                                    ],
                                ),
                            ],
                        )
                    ],
                    exception_handler.execute(),
                )
            ]

        return [
            self._declare_sequence_copy(
                sequence_identifier,
                sequence_type,
                statements,
                exception_handler,
                is_global,
                alloc_id,
            ),
        ]

    def _assign_to_comprehension(  # pylint: disable = too-many-arguments
        self,
        target: ID,
        target_type: rty.Sequence,
        comprehension: expr.Comprehension,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        # pylint: disable = too-many-locals

        assert isinstance(comprehension.type_, (rty.Sequence, rty.Aggregate))
        assert isinstance(comprehension.sequence.type_, rty.Sequence)

        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        target_id = target
        target_context = context_id(target_id, is_global)
        sequence_type_id = comprehension.sequence.type_.identifier
        iterator_id = comprehension.iterator

        sequence_element_type = comprehension.sequence.type_.element

        reset_target = CallStatement(target_type.identifier * "Reset", [Variable(target_context)])

        if isinstance(sequence_element_type, rty.Message):
            iterator_type_id = sequence_element_type.identifier

            if isinstance(comprehension.sequence, expr.Variable):
                sequence_id = ID(f"{comprehension.sequence}")

                def statements(local_exception_handler: ExceptionHandler) -> list[Statement]:
                    return [
                        reset_target,
                        self._comprehension(
                            copy_id(sequence_id),
                            sequence_type_id,
                            target_id,
                            target_type,
                            iterator_id,
                            iterator_type_id,
                            comprehension.selector,
                            comprehension.condition,
                            local_exception_handler,
                            is_global,
                            alloc_id,
                        ),
                    ]

                return [
                    self._declare_sequence_copy(
                        sequence_id,
                        sequence_type_id,
                        statements,
                        exception_handler,
                        is_global,
                        alloc_id,
                    ),
                ]

            if isinstance(comprehension.sequence, expr.Selected):
                selected = comprehension.sequence

                if not isinstance(selected.prefix, expr.Variable):
                    _unsupported_expression(
                        selected.prefix, "as prefix of Selected in list comprehension"
                    )

                assert isinstance(selected.prefix.type_, rty.Message)

                message_id = ID(selected.prefix.name)
                message_type = selected.prefix.type_.identifier
                sequence_id = ID(
                    f"RFLX_{selected.prefix}_{selected.selector}",
                    location=selected.location,
                )
                message_field = selected.selector
                source_buffer_size = self._allocator.get_size(message_id, state)
                target_buffer_size = self._allocator.get_size(target, state)

                return [
                    reset_target,
                    self._if_well_formed_message(
                        message_type,
                        context_id(message_id, is_global),
                        [
                            self._declare_message_field_sequence_copy(
                                message_id,
                                message_type,
                                message_field,
                                sequence_id,
                                sequence_type_id,
                                lambda local_exception_handler: [
                                    self._comprehension(
                                        sequence_id,
                                        sequence_type_id,
                                        target_id,
                                        target_type,
                                        iterator_id,
                                        iterator_type_id,
                                        comprehension.selector,
                                        comprehension.condition,
                                        local_exception_handler,
                                        is_global,
                                        alloc_id,
                                    )
                                ],
                                target_buffer_size < source_buffer_size,
                                exception_handler,
                                is_global,
                                alloc_id,
                            ),
                        ],
                        exception_handler,
                    ),
                ]

            _unsupported_expression(comprehension.sequence, "as sequence in list comprehension")

        fail(
            f"iterating over sequence of {sequence_element_type}"
            " in list comprehension not yet supported",
            Subsystem.GENERATOR,
            location=comprehension.sequence.location,
        )

    def _assign_to_call(  # pylint: disable = too-many-locals
        self,
        target: ID,
        call_expr: expr.Call,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
    ) -> Sequence[Statement]:
        pre_call: list[Statement] = []
        post_call = []
        local_declarations = []
        target_id = variable_id(target, is_global)
        message_id = context_id(target, is_global)

        if isinstance(call_expr.type_, rty.Message):
            type_identifier = self._ada_type(call_expr.type_.identifier)
            local_declarations.append(
                ObjectDeclaration(
                    [target_id],
                    type_identifier * "Structure",
                )
            )
            post_call.append(
                self._if(
                    Call(type_identifier * "Valid_Structure", [Variable(target_id)]),
                    [
                        self._if(
                            Call(
                                type_identifier * "Sufficient_Buffer_Length",
                                [Variable(message_id), Variable(target_id)],
                            ),
                            [
                                CallStatement(
                                    type_identifier * "To_Context",
                                    [
                                        Variable(target_id),
                                        Variable(message_id),
                                    ],
                                ),
                            ],
                            f'insufficient space for converting message "{target}"',
                            exception_handler,
                        )
                    ],
                    f'"{call_expr.identifier}" returned an invalid message',
                    exception_handler,
                )
            )

        elif isinstance(call_expr.type_, rty.Structure):
            type_identifier = self._ada_type(call_expr.type_.identifier)
            post_call.append(
                self._raise_exception_if(
                    Not(Call(type_identifier * "Valid_Structure", [Variable(target_id)])),
                    f'"{call_expr.identifier}" returned an invalid message',
                    exception_handler,
                )
            )

        arguments: list[expr.Expr] = []

        assert len(call_expr.args) == len(call_expr.argument_types)

        for i, (a, t) in enumerate(zip(call_expr.args, call_expr.argument_types)):
            if not isinstance(
                a,
                (
                    expr.Number,
                    expr.Variable,
                    expr.Literal,
                    expr.Selected,
                    expr.Size,
                    expr.String,
                    expr.Aggregate,
                ),
            ) and not (
                isinstance(a, expr.Opaque)
                and isinstance(a.prefix, expr.Variable)
                and a.type_ == rty.OPAQUE
            ):
                _unsupported_expression(a, "as function argument")

            if isinstance(a, expr.Variable) and isinstance(a.type_, rty.Message):
                type_identifier = self._ada_type(a.type_.identifier)
                local_declarations.append(
                    ObjectDeclaration(
                        [a.identifier],
                        type_identifier * "Structure",
                    )
                )
                pre_call.append(
                    CallStatement(
                        type_identifier * "To_Structure",
                        [
                            Variable(context_id(a.identifier, is_global)),
                            Variable(a.identifier),
                        ],
                    ),
                )
                arguments.append(a)
            elif isinstance(a, expr.Selected) and a.type_ == rty.OPAQUE:
                assert isinstance(a.prefix, expr.Variable)
                assert isinstance(a.prefix.type_, rty.Message)
                assert isinstance(a.type_, rty.Sequence)
                argument_name = f"RFLX_{call_expr.identifier}_Arg_{i}_{a.prefix}"
                argument_length = f"{argument_name}_Length"
                argument = expr.Slice(
                    expr.Variable(argument_name),
                    expr.First(const.TYPES_INDEX),
                    expr.Add(
                        expr.First(const.TYPES_INDEX),
                        expr.Call(
                            const.TYPES_INDEX,
                            [
                                expr.Variable(argument_length),
                            ],
                        ),
                        -expr.Number(2),
                    ),
                )
                type_identifier = self._ada_type(a.prefix.type_.identifier)
                local_declarations.extend(
                    [
                        # https://github.com/Componolit/RecordFlux/issues/917
                        # The use of intermediate buffers should be removed.
                        ObjectDeclaration(
                            [argument_name],
                            Slice(
                                Variable(const.TYPES_BYTES),
                                First(const.TYPES_INDEX),
                                Add(
                                    First(const.TYPES_INDEX),
                                    Number(
                                        self._allocator.get_size(a.prefix.identifier, state) - 1
                                    ),
                                ),
                            ),
                            NamedAggregate(("others", Number(0))),
                        ),
                        ObjectDeclaration(
                            [argument_length],
                            const.TYPES_LENGTH,
                            Add(
                                Call(
                                    const.TYPES_TO_LENGTH,
                                    [
                                        Call(
                                            type_identifier * "Field_Size",
                                            [
                                                Variable(
                                                    context_id(a.prefix.identifier, is_global)
                                                ),
                                                Variable(type_identifier * f"F_{a.selector}"),
                                            ],
                                        ),
                                    ],
                                ),
                                Number(1),
                            ),
                            constant=True,
                        ),
                    ]
                )
                pre_call.append(
                    CallStatement(
                        type_identifier * f"Get_{a.selector}",
                        [
                            Variable(context_id(a.prefix.identifier, is_global)),
                            argument.ada_expr(),
                        ],
                    ),
                )
                arguments.append(argument)
            elif (
                isinstance(a, expr.Opaque)
                and isinstance(a.prefix, expr.Variable)
                and isinstance(a.prefix.type_, (rty.Message, rty.Sequence))
            ):
                self._session_context.used_types_body.append(const.TYPES_LENGTH)
                argument_name = f"RFLX_{call_expr.identifier}_Arg_{i}_{a.prefix}"
                argument_length = f"{argument_name}_Length"
                argument = expr.Slice(
                    expr.Variable(argument_name),
                    expr.First(const.TYPES_INDEX),
                    expr.Add(
                        expr.First(const.TYPES_INDEX),
                        expr.Call(
                            const.TYPES_INDEX,
                            [expr.Add(expr.Variable(argument_length), expr.Number(1))],
                        ),
                        -expr.Number(2),
                    ),
                )
                type_identifier = self._ada_type(a.prefix.type_.identifier)
                message_context = context_id(a.prefix.identifier, is_global)
                local_declarations.extend(
                    [
                        # https://github.com/Componolit/RecordFlux/issues/917
                        # The use of intermediate buffers should be removed.
                        ObjectDeclaration(
                            [argument_name],
                            Slice(
                                Variable(const.TYPES_BYTES),
                                First(const.TYPES_INDEX),
                                Add(
                                    First(const.TYPES_INDEX),
                                    Number(
                                        self._allocator.get_size(a.prefix.identifier, state) - 1
                                    ),
                                ),
                            ),
                            NamedAggregate(("others", Number(0))),
                        ),
                        ObjectDeclaration(
                            [argument_length],
                            const.TYPES_LENGTH,
                            Call(
                                type_identifier * "Byte_Size",
                                [
                                    Variable(message_context),
                                ],
                            ),
                            constant=True,
                        ),
                    ]
                )
                pre_call.extend(
                    [
                        self._raise_exception_if(
                            Not(
                                Call(
                                    type_identifier
                                    * (
                                        "Well_Formed_Message"
                                        if isinstance(a.prefix.type_, rty.Message)
                                        else "Valid"
                                    ),
                                    [Variable(message_context)],
                                )
                            ),
                            f'invalid message "{message_context}"',
                            exception_handler,
                        ),
                        CallStatement(
                            type_identifier * "Data",
                            [
                                Variable(message_context),
                                argument.ada_expr(),
                            ],
                        ),
                    ]
                )
                arguments.append(argument)
            elif (
                isinstance(a, expr.Literal)
                and isinstance(a.type_, rty.Enumeration)
                and a.type_.always_valid
                and a.identifier in self._session.literals
            ):
                arguments.append(expr.NamedAggregate(("Known", expr.TRUE), ("Enum", a)))
            else:
                arguments.append(
                    expr.Call(t.identifier, [a])
                    if isinstance(t, rty.Integer) and not a.type_.is_compatible_strong(t)
                    else a
                )

        call = [
            CallStatement(
                call_expr.identifier,
                [
                    Variable("Ctx"),
                    *[a.substituted(self._substitution(is_global)).ada_expr() for a in arguments],
                    Variable(target_id),
                ],
            )
        ]

        if local_declarations:
            return self._if_valid_fields(
                expr.Or(*call_expr.args),
                [
                    Declare(
                        local_declarations,
                        [
                            *pre_call,
                            *call,
                            *post_call,
                        ],
                    )
                ],
                exception_handler,
                is_global,
            )

        if isinstance(call_expr.type_, rty.Structure):
            return [*call, *post_call]

        return call

    def _assign_to_conversion(
        self,
        target: ID,
        conversion: expr.Conversion,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        assert isinstance(conversion.type_, rty.Message)
        assert isinstance(conversion.argument, expr.Selected)
        assert conversion.argument.type_ == rty.OPAQUE
        assert isinstance(conversion.argument.prefix, expr.Variable)
        assert isinstance(conversion.argument.prefix.type_, rty.Message)

        pdu = conversion.argument.prefix.type_
        sdu = conversion.type_
        field = conversion.argument.selector
        refinements = [
            r for r in pdu.refinements if r.sdu.identifier == sdu.identifier and r.field == field
        ]
        if not refinements:
            fatal_fail(
                f'no refinement for field "{field}" of message "{pdu.identifier}"'
                f' leads to "{sdu.identifier}"',
                Subsystem.GENERATOR,
                location=conversion.location,
            )
        assert len(refinements) == 1
        refinement = refinements[0]
        contains_package = refinement.package * "Contains"

        self._session_context.referenced_packages_body.append(contains_package)

        return [
            self._if(
                Call(
                    contains_package
                    * common.contains_function_name(
                        refinement.package, pdu.identifier, sdu.identifier, field
                    ),
                    [Variable(context_id(conversion.argument.prefix.identifier, is_global))],
                ),
                [
                    CallStatement(
                        contains_package * f"Copy_{field}",
                        [
                            Variable(context_id(conversion.argument.prefix.identifier, is_global)),
                            Variable(context_id(target, is_global)),
                        ],
                    ),
                    CallStatement(
                        sdu.identifier * "Verify_Message",
                        [
                            Variable(context_id(target, is_global)),
                        ],
                    ),
                ],
                f'invalid conversion "{conversion}"',
                exception_handler,
            )
        ]

    def _assign_message_field(  # pylint: disable = too-many-arguments
        self,
        target: ID,
        target_field: ID,
        message_type: rty.Type,
        value: expr.Expr,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        assert isinstance(message_type, rty.Message)

        target_context = context_id(target, is_global)

        return [
            self._raise_exception_if(
                Not(
                    Call(
                        message_type.identifier * "Valid_Next",
                        [
                            Variable(target_context),
                            Variable(message_type.identifier * f"F_{target_field}"),
                        ],
                    )
                ),
                f'trying to set message field "{target_field}" to "{value}" although'
                f' "{target_field}" is not valid next field',
                exception_handler,
            ),
            self._raise_exception_if(
                Not(
                    Call(
                        message_type.identifier * "Sufficient_Space",
                        [
                            Variable(target_context),
                            Variable(message_type.identifier * f"F_{target_field}"),
                        ],
                    )
                ),
                f'insufficient space in message "{target_context}" to set field "{target_field}"'
                f' to "{value}"',
                exception_handler,
            ),
            *self._set_message_field(
                target_context,
                target_field,
                message_type,
                value,
                exception_handler,
                is_global,
            ),
        ]

    def _append(
        self,
        append: stmt.Append,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
    ) -> Sequence[Statement]:
        assert isinstance(append.type_, rty.Sequence)

        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        def check(
            sequence_type: ID, required_space: Expr, precondition: Expr = None
        ) -> list[Statement]:
            return [
                *(
                    [
                        IfStatement(
                            [
                                (
                                    Not(precondition),
                                    [
                                        *self._debug_output("Error: unexpected size"),
                                        *exception_handler.execute(),
                                    ],
                                )
                            ]
                        )
                    ]
                    if precondition
                    else []
                ),
                IfStatement(
                    [
                        (
                            Or(
                                Not(
                                    Call(
                                        sequence_type * "Has_Element",
                                        [Variable(sequence_context)],
                                    )
                                ),
                                Less(
                                    Call(
                                        sequence_type * "Available_Space",
                                        [Variable(sequence_context)],
                                    ),
                                    required_space,
                                ),
                            ),
                            [
                                *self._debug_output(
                                    "Error: insufficient space for appending to sequence"
                                    f' "{sequence_context}"'
                                ),
                                *exception_handler.execute(),
                            ],
                        )
                    ]
                ),
            ]

        if isinstance(append.type_.element, (rty.Integer, rty.Enumeration)):
            if isinstance(append.parameter, (expr.Variable, expr.Literal, expr.Number)):
                sequence_type = append.type_.identifier
                sequence_context = context_id(append.identifier, is_global)
                element_type = append.type_.element.identifier

                return [
                    *check(sequence_type, Size(element_type)),
                    CallStatement(
                        sequence_type * "Append_Element",
                        [Variable(sequence_context), append.parameter.ada_expr()],
                    ),
                ]

            _unsupported_expression(append.parameter, "in Append statement")

        if isinstance(append.type_.element, rty.Message):
            sequence_type = append.type_.identifier
            sequence_context = context_id(append.identifier, is_global)
            element_type = append.type_.element.identifier
            element_context = context_id("RFLX_Element_" + append.identifier, is_global)

            self._session_context.referenced_types_body.append(element_type)

            if isinstance(append.parameter, expr.MessageAggregate):
                required_space, required_space_precondition = self._required_space(
                    self._message_size(append.parameter), is_global, state
                )
            else:
                _unsupported_expression(append.parameter, "in Append statement")

            update_context = self._update_context(sequence_context, element_context, sequence_type)
            local_exception_handler = exception_handler.copy(update_context)

            return [
                *check(sequence_type, required_space, required_space_precondition),
                Declare(
                    [ObjectDeclaration([element_context], element_type * "Context")],
                    [
                        CallStatement(
                            sequence_type * "Switch",
                            [Variable(sequence_context), Variable(element_context)],
                        ),
                        *(
                            self._set_message_fields(
                                element_context,
                                append.parameters[0],
                                local_exception_handler,
                                is_global,
                                state,
                                size_check=False,
                            )
                            if isinstance(append.parameters[0], expr.MessageAggregate)
                            else []
                        ),
                        *update_context,
                    ],
                ),
            ]

        fatal_fail(
            f"unexpected element type {append.type_.element} in Append statement",
            Subsystem.GENERATOR,
            location=append.parameter.location,
        )

    @staticmethod
    def _read(read: stmt.Read, is_global: Callable[[ID], bool]) -> Sequence[Statement]:
        assert isinstance(read.parameter.type_, rty.Message)

        if not isinstance(read.parameter, expr.Variable):
            _unsupported_expression(read.parameter, "in Read statement")

        target_type = read.parameter.type_.identifier
        target_context = context_id(read.parameter.identifier, is_global)
        return [
            CallStatement(target_type * "Verify_Message", [Variable(target_context)]),
        ]

    @staticmethod
    def _write(
        write: stmt.Write,
    ) -> Sequence[Statement]:
        assert isinstance(write.parameter.type_, rty.Message)

        if not isinstance(write.parameter, expr.Variable):
            _unsupported_expression(write.parameter, "in Write statement")

        return []

    def _reset(
        self,
        reset: stmt.Reset,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        assert isinstance(reset.type_, (rty.Message, rty.Sequence))

        target_type = reset.type_.identifier
        target_context = context_id(reset.identifier, is_global)
        return [
            CallStatement(
                target_type * "Reset",
                [Variable(target_context)],
                {
                    n: e.substituted(self._substitution(is_global)).ada_expr()
                    for n, e in reset.associations.items()
                },
            ),
        ]

    def _message_size(self, message_aggregate: expr.MessageAggregate) -> expr.Expr:
        message = self._model_type(message_aggregate.identifier)
        assert isinstance(message, model.Message)
        return message.size({model.Field(f): v for f, v in message_aggregate.field_values.items()})

    def _message_subpath_size(
        self, delta_message_aggregate: expr.DeltaMessageAggregate
    ) -> expr.Expr:
        assert isinstance(delta_message_aggregate.type_, rty.Message)
        message = self._model_type(delta_message_aggregate.type_.identifier)
        assert isinstance(message, model.Message)
        return message.size(
            {model.Field(f): v for f, v in delta_message_aggregate.field_values.items()},
            delta_message_aggregate.identifier,
            subpath=True,
        )

    def _required_space(
        self, size: expr.Expr, is_global: Callable[[ID], bool], state: ID
    ) -> tuple[Expr, Optional[Expr]]:
        required_space = (
            size.substituted(
                lambda x: expr.Call(const.TYPES_BIT_LENGTH, [x])
                if (isinstance(x, expr.Variable) and isinstance(x.type_, rty.AnyInteger))
                else x
            )
            .substituted(self._substitution(is_global))
            .ada_expr()
        )
        precondition = [
            *unique(
                e
                for v in size.variables()
                if isinstance(v.type_, (rty.Message, rty.Sequence))
                for s in [expr.Size(v).substituted(self._substitution(is_global)).ada_expr()]
                for e in [
                    LessEqual(
                        s,
                        Number(self._allocator.get_size(v.identifier, state) * 8),
                    ),
                    Equal(Mod(s, Size(const.TYPES_BYTE)), Number(0)),
                ]
            ),
            *unique(
                Call(
                    s.prefix.type_.identifier * "Well_Formed",
                    [
                        Variable(context_id(s.prefix.identifier, is_global)),
                        Variable(s.prefix.type_.identifier * model.Field(s.selector).affixed_name),
                    ],
                )
                for s in size.findall(lambda x: isinstance(x, expr.Selected))
                if isinstance(s, expr.Selected)
                and isinstance(s.prefix, expr.Variable)
                and isinstance(s.prefix.type_, rty.Message)
            ),
        ]
        return (required_space, AndThen(*precondition) if precondition else None)

    def _substitution(self, is_global: Callable[[ID], bool]) -> Callable[[expr.Expr], expr.Expr]:
        # pylint: disable = too-many-statements

        def func(expression: expr.Expr) -> expr.Expr:
            # pylint: disable = too-many-branches, too-many-return-statements
            if isinstance(expression, (expr.Relation, expr.MathBinExpr)):
                for e in [expression.left, expression.right]:
                    if isinstance(e.type_, rty.Integer) or (
                        isinstance(e.type_, rty.Enumeration) and not e.type_.always_valid
                    ):
                        self._session_context.used_types_body.append(e.type_.identifier)

            if isinstance(expression, expr.MathAssExpr):
                for e in expression.terms:
                    if isinstance(e.type_, (rty.Integer, rty.Enumeration)):
                        self._session_context.used_types_body.append(e.type_.identifier)

            if isinstance(expression, expr.And):
                return expr.AndThen(*expression.terms)

            if isinstance(expression, expr.Or):
                return expr.OrElse(*expression.terms)

            if isinstance(expression, expr.Selected):
                if isinstance(expression.prefix, expr.Variable):
                    assert isinstance(expression.prefix.type_, rty.Message)
                    if expression.selector in expression.prefix.type_.parameter_types:
                        return expr.Selected(
                            expr.Variable(context_id(expression.prefix.identifier, is_global)),
                            expression.selector,
                        )
                    return expr.Call(
                        expression.prefix.type_.identifier * f"Get_{expression.selector}",
                        [expr.Variable(context_id(expression.prefix.identifier, is_global))],
                    )

                assert False

            if isinstance(expression, expr.Valid):
                if isinstance(expression.prefix, expr.Variable):
                    if isinstance(expression.prefix.type_, rty.Message):
                        return expr.Call(
                            expression.prefix.type_.identifier * "Well_Formed_Message",
                            [expr.Variable(context_id(expression.prefix.identifier, is_global))],
                        )

                    if isinstance(expression.prefix.type_, rty.Sequence):
                        return expr.Call(
                            expression.prefix.type_.identifier * "Valid",
                            [expr.Variable(context_id(expression.prefix.identifier, is_global))],
                        )

                    assert False

                if isinstance(expression.prefix, expr.Selected):
                    assert isinstance(expression.prefix.prefix, expr.Variable)
                    assert isinstance(expression.prefix.prefix.type_, rty.Message)
                    type_name = expression.prefix.prefix.type_.identifier
                    return expr.Call(
                        type_name
                        * (
                            "Valid"
                            if isinstance(expression.prefix.type_, (rty.Integer, rty.Enumeration))
                            else "Well_Formed"
                        ),
                        [
                            expr.Variable(
                                context_id(expression.prefix.prefix.identifier, is_global)
                            ),
                            expr.Variable(type_name * f"F_{expression.prefix.selector}"),
                        ],
                    )

                assert False

            if isinstance(expression, expr.Present):
                if isinstance(expression.prefix, expr.Selected):
                    assert isinstance(expression.prefix.prefix, expr.Variable)
                    assert isinstance(expression.prefix.prefix.type_, rty.Message)
                    type_name = expression.prefix.prefix.type_.identifier
                    return expr.Call(
                        type_name * "Present",
                        [
                            expr.Variable(
                                context_id(expression.prefix.prefix.identifier, is_global)
                            ),
                            expr.Variable(type_name * f"F_{expression.prefix.selector}"),
                        ],
                    )

                assert False

            if isinstance(expression, expr.Aggregate):
                assert len(expression.elements) > 0
                if len(expression.elements) == 1:
                    return expr.NamedAggregate(
                        (
                            str(expr.First(const.TYPES_INDEX)),
                            expr.Val(const.TYPES_BYTE, expression.elements[0]),
                        )
                    )
                return expr.Aggregate(*[expr.Val(const.TYPES_BYTE, e) for e in expression.elements])

            if isinstance(expression, expr.Equal):
                if expression.left == expr.TRUE and isinstance(expression.right, expr.Variable):
                    return expression.right.copy(
                        identifier=variable_id(expression.right.identifier, is_global)
                    )
                if isinstance(expression.left, expr.Variable) and expression.right == expr.TRUE:
                    return expression.left.copy(
                        identifier=variable_id(expression.left.identifier, is_global)
                    )
                if expression.left == expr.FALSE and isinstance(expression.right, expr.Variable):
                    return expr.Not(
                        expression.right.copy(
                            identifier=variable_id(expression.right.identifier, is_global)
                        )
                    )
                if isinstance(expression.left, expr.Variable) and expression.right == expr.FALSE:
                    return expr.Not(
                        expression.left.copy(
                            identifier=variable_id(expression.left.identifier, is_global)
                        )
                    )

            if isinstance(expression, expr.NotEqual):
                if expression.left == expr.TRUE and isinstance(expression.right, expr.Variable):
                    return expr.Not(
                        expression.right.copy(
                            identifier=variable_id(expression.right.identifier, is_global)
                        )
                    )
                if isinstance(expression.left, expr.Variable) and expression.right == expr.TRUE:
                    return expr.Not(
                        expression.left.copy(
                            identifier=variable_id(expression.left.identifier, is_global)
                        )
                    )
                if expression.left == expr.FALSE and isinstance(expression.right, expr.Variable):
                    return expression.right.copy(
                        identifier=variable_id(expression.right.identifier, is_global)
                    )
                if isinstance(expression.left, expr.Variable) and expression.right == expr.FALSE:
                    return expression.left.copy(
                        identifier=variable_id(expression.left.identifier, is_global)
                    )

            if isinstance(expression, (expr.Equal, expr.NotEqual)) and isinstance(
                expression.left.type_, rty.Enumeration
            ):
                relation = expression.__class__
                if expression.left.type_.always_valid:
                    selected = None
                    literal = None

                    if isinstance(expression.left, expr.Selected):
                        selected = expression.left.substituted(self._substitution(is_global))
                        literal = expression.right
                    elif isinstance(expression.right, expr.Selected):
                        selected = expression.right.substituted(self._substitution(is_global))
                        literal = expression.left

                    if selected and literal:
                        assert isinstance(literal.type_, rty.Enumeration)
                        self._session_context.used_types_body.append(
                            literal.type_.identifier + "_Enum"
                        )
                        return expr.AndThen(
                            expr.Selected(selected, "Known"),
                            relation(expr.Selected(selected, "Enum"), literal),
                        )

            if isinstance(expression, expr.Size):
                if isinstance(expression.prefix, (expr.Variable, expr.Literal)):
                    if (
                        isinstance(expression.prefix.type_, rty.AnyInteger)
                        or (
                            isinstance(expression.prefix.type_, rty.Aggregate)
                            and isinstance(expression.prefix.type_.element, rty.AnyInteger)
                        )
                        or (
                            isinstance(expression.prefix.type_, (rty.Integer, rty.Enumeration))
                            and expression.prefix.identifier == expression.prefix.type_.identifier
                        )
                    ):
                        return expression

                    if (
                        isinstance(expression.prefix.type_, (rty.Message, rty.Sequence))
                        and expression.prefix.type_ != rty.OPAQUE
                    ):
                        type_ = expression.prefix.type_.identifier
                        context = context_id(expression.prefix.identifier, is_global)
                        return expr.Call(type_ * "Size", [expr.Variable(context)])

                    _unexpected_expression(expression.prefix, "in Size attribute")

                if isinstance(expression.prefix, expr.Selected):
                    assert isinstance(expression.prefix.prefix, expr.Variable)
                    assert isinstance(expression.prefix.prefix.type_, (rty.Message, rty.Structure))
                    type_ = expression.prefix.prefix.type_.identifier
                    if isinstance(expression.prefix.prefix.type_, rty.Message):
                        context = context_id(expression.prefix.prefix.identifier, is_global)
                        return expr.Call(
                            type_ * "Field_Size",
                            [
                                expr.Variable(context),
                                expr.Variable(type_ * "F_" + expression.prefix.selector),
                            ],
                        )

                    return expr.Call(
                        type_ * f"Field_Size_{expression.prefix.selector}",
                        [expression.prefix.prefix],
                    )

                _unsupported_expression(expression.prefix, "in Size attribute")

            if isinstance(expression, expr.HasData):
                assert isinstance(expression.prefix, expr.Variable)
                assert isinstance(expression.prefix.type_, rty.Message)
                type_ = expression.prefix.type_.identifier
                context = context_id(expression.prefix.identifier, is_global)
                return expr.Greater(
                    expr.Call(type_ * "Byte_Size", [expr.Variable(context)]), expr.Number(0)
                )

            if isinstance(expression, expr.Opaque):
                assert expression.type_ == rty.OPAQUE
                if isinstance(expression.prefix, expr.Variable):
                    if isinstance(expression.prefix.type_, (rty.Message, rty.Sequence)):
                        type_ = expression.prefix.type_.identifier
                        context = context_id(expression.prefix.identifier, is_global)
                        return expr.Call(type_ * "Data", [expr.Variable(context)])

                _unsupported_expression(expression.prefix, "in Opaque attribute")

            if isinstance(expression, expr.Head):
                _unsupported_expression(expression, "in expression")

            if isinstance(expression, expr.Variable):
                return expression.copy(identifier=variable_id(expression.identifier, is_global))

            return expression

        return func

    def _if(
        self,
        condition: Expr,
        statements: Sequence[Statement],
        error_message: str,
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        return IfStatement(
            [
                (condition, statements),
            ],
            [
                *self._debug_output(f"Error: {error_message}"),
                *exception_handler.execute(),
            ],
        )

    def _if_valid_sequence(
        self,
        sequence_type: ID,
        sequence_context: ID,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        return self._if(
            Call(sequence_type * "Valid", [Variable(sequence_context)]),
            statements,
            f'invalid sequence "{sequence_context}"',
            exception_handler,
        )

    def _if_well_formed_message(
        self,
        message_type: ID,
        message_context: ID,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        return self._if(
            Call(
                message_type * "Well_Formed_Message",
                [Variable(message_context)],
            ),
            statements,
            f'invalid message "{message_context}"',
            exception_handler,
        )

    def _if_well_formed_message_field(
        self,
        message_type: ID,
        message_context: ID,
        message_field: ID,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        return self._if(
            Call(
                message_type * "Well_Formed",
                [
                    Variable(message_context),
                    Variable(message_type * model.Field(message_field).affixed_name),
                ],
            ),
            statements,
            f'invalid message field "{message_type * message_field}"',
            exception_handler,
        )

    def _if_valid_fields(
        self,
        expression: expr.Expr,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        """Ensure that all referenced fields in the expression are valid."""

        selected = [
            s
            for s in expression.findall(lambda x: isinstance(x, expr.Selected))
            if isinstance(s, expr.Selected)
            and isinstance(s.prefix.type_, rty.Message)
            and s.selector in s.prefix.type_.fields
        ]

        if selected:
            expressions = " or ".join(f'"{s}"' for s in selected)
            return [
                self._if(
                    expr.AndThen(*[expr.Valid(e) for e in selected])
                    .substituted(self._substitution(is_global))
                    .ada_expr(),
                    statements,
                    f"reference to invalid message field in {expressions}",
                    exception_handler,
                )
            ]

        return statements

    def _if_sufficient_space_in_sequence(
        self,
        required_space: Expr,
        sequence_type: ID,
        sequence_context: ID,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        return self._if(
            AndThen(
                Call(
                    sequence_type * "Has_Element",
                    [Variable(sequence_context)],
                ),
                GreaterEqual(
                    Call(
                        sequence_type * "Available_Space",
                        [Variable(sequence_context)],
                    ),
                    required_space,
                ),
            ),
            statements,
            f'insufficient space in sequence "{sequence_context}"',
            exception_handler,
        )

    def _ensure(
        self,
        statements: list[Statement],
        property_expression: Expr,
        error_message: str,
        exception_handler: ExceptionHandler,
    ) -> list[Statement]:
        nested: list[Statement] = []
        statements.append(
            IfStatement(
                [
                    (
                        property_expression,
                        nested,
                    )
                ],
                [
                    *self._debug_output(f"Error: {error_message}"),
                    *exception_handler.execute(),
                ],
            )
        )
        return nested

    def _raise_exception_if(
        self,
        condition: Expr,
        error_message: str,
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        return IfStatement(
            [
                (
                    condition,
                    [
                        *self._debug_output(f"Error: {error_message}"),
                        *exception_handler.execute(),
                    ],
                ),
            ]
        )

    def _set_message_fields(  # pylint: disable = too-many-arguments
        self,
        target_context: ID,
        message_aggregate: expr.MessageAggregate,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        size_check: bool = True,
    ) -> Sequence[Statement]:
        assert isinstance(message_aggregate.type_, rty.Message)

        message_type = message_aggregate.type_
        size = self._message_size(message_aggregate)
        required_space, required_space_precondition = self._required_space(size, is_global, state)

        statements: list[Statement] = (
            [
                *(
                    [
                        self._raise_exception_if(
                            Not(required_space_precondition),
                            "violated precondition for calculating required space of message for"
                            f' "{target_context}" (one of the message arguments is invalid or has a'
                            " too small buffer)",
                            exception_handler,
                        )
                    ]
                    if required_space_precondition
                    else []
                ),
                self._raise_exception_if(
                    Less(
                        Call(
                            message_type.identifier * "Available_Space",
                            [
                                Variable(target_context),
                                Variable(
                                    message_type.identifier
                                    * model.Field(next(iter(message_type.field_types))).affixed_name
                                ),
                            ],
                        ),
                        required_space,
                    ),
                    f'insufficient space in "{target_context}" for creating message',
                    exception_handler,
                ),
            ]
            if size_check
            else []
        )

        for f, v in message_aggregate.field_values.items():
            if f not in message_type.field_types:
                continue

            statements.extend(
                self._set_message_field(
                    target_context, f, message_type, v, exception_handler, is_global
                )
            )

        return statements

    def _set_message_field(
        self,
        message_context: ID,
        field: ID,
        message_type: rty.Message,
        value: expr.Expr,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        # pylint: disable = too-many-arguments, too-many-statements, too-many-branches, too-many-locals

        message_type_id = message_type.identifier
        field_type = message_type.field_types[field]
        statements: list[Statement] = []
        result = statements

        if isinstance(field_type, rty.Sequence):
            size: Expr
            if isinstance(value, expr.Variable) and isinstance(
                value.type_, (rty.Message, rty.Sequence)
            ):
                type_ = value.type_.identifier
                context = context_id(value.identifier, is_global)
                statements = self._ensure(
                    statements,
                    Call(
                        message_type_id * "Valid_Length",
                        [
                            Variable(message_context),
                            Variable(message_type_id * f"F_{field}"),
                            Call(type_ * "Byte_Size", [Variable(context)]),
                        ],
                    ),
                    f'invalid message field size for "{value}"',
                    exception_handler,
                )
            else:
                if isinstance(value, expr.Aggregate):
                    size = Mul(Number(len(value.elements)), Size(const.TYPES_BYTE))
                elif isinstance(value, expr.Selected):
                    assert isinstance(value.prefix, expr.Variable) and isinstance(
                        value.prefix.type_, rty.Compound
                    )
                    value_message_type_id = value.prefix.type_.identifier
                    if isinstance(value.prefix.type_, rty.Message):
                        value_message_context = context_id(value.prefix.identifier, is_global)
                        statements = self._ensure(
                            statements,
                            Call(
                                value_message_type_id * "Valid_Next",
                                [
                                    Variable(value_message_context),
                                    Variable(value_message_type_id * f"F_{value.selector}"),
                                ],
                            ),
                            f'access to invalid next message field for "{value}"',
                            exception_handler,
                        )
                        size = Call(
                            value_message_type_id * "Field_Size",
                            [
                                Variable(value_message_context),
                                Variable(value_message_type_id * f"F_{value.selector}"),
                            ],
                        )
                    else:
                        assert isinstance(value.prefix.type_, rty.Structure)
                        size = Call(
                            value_message_type_id * f"Field_Size_{value.selector}",
                            [Variable(value.prefix.identifier)],
                        )

                elif isinstance(value, expr.Opaque):
                    size = (
                        expr.Size(value.prefix)
                        .substituted(self._substitution(is_global))
                        .ada_expr()
                    )
                else:
                    size = Size(value.substituted(self._substitution(is_global)).ada_expr())

                statements = self._ensure(
                    statements,
                    Call(
                        message_type_id * "Valid_Length",
                        [
                            Variable(message_context),
                            Variable(message_type_id * f"F_{field}"),
                            Call(const.TYPES_TO_LENGTH, [size]),
                        ],
                    ),
                    f'invalid message field size for "{value}"',
                    exception_handler,
                )

        assert_sufficient_space = PragmaStatement(
            "Assert",
            [
                Call(
                    message_type_id * "Sufficient_Space",
                    [
                        Variable(message_context),
                        Variable(message_type_id * model.Field(field).affixed_name),
                    ],
                )
            ],
        )

        if isinstance(value, (expr.Number, expr.Aggregate, expr.CaseExpr)) or (
            isinstance(
                value,
                (expr.Variable, expr.Literal, expr.MathBinExpr, expr.MathAssExpr, expr.Size),
            )
            and isinstance(value.type_, (rty.AnyInteger, rty.Enumeration, rty.Aggregate))
        ):
            if isinstance(value, expr.Aggregate) and len(value.elements) == 0:
                statements.append(
                    CallStatement(
                        message_type_id * f"Set_{field}_Empty", [Variable(message_context)]
                    )
                )
            else:
                value = self._convert_type(value, field_type).substituted(
                    self._substitution(is_global)
                )
                statements.extend(
                    [
                        assert_sufficient_space,
                        CallStatement(
                            message_type_id * f"Set_{field}",
                            [
                                Variable(message_context),
                                value.ada_expr(),
                            ],
                        ),
                    ]
                )
        elif isinstance(value, expr.Variable) and isinstance(value.type_, rty.Sequence):
            sequence_context = context_id(value.identifier, is_global)
            statements.extend(
                [
                    assert_sufficient_space,
                    CallStatement(
                        message_type_id * f"Set_{field}",
                        [Variable(message_context), Variable(sequence_context)],
                    ),
                ]
            )
        elif isinstance(value, expr.Variable) and isinstance(value.type_, rty.Message):
            _unsupported_expression(value, "in message aggregate")
        elif (
            isinstance(value, expr.Selected)
            and isinstance(value.prefix, expr.Variable)
            and isinstance(value.prefix.type_, rty.Message)
        ):
            value_message_type_id = value.prefix.type_.identifier
            value_message_context = context_id(value.prefix.identifier, is_global)
            statements = self._ensure(
                statements,
                Call(
                    value_message_type_id
                    * ("Well_Formed" if isinstance(value.type_, rty.Sequence) else "Valid"),
                    [
                        Variable(value_message_context),
                        Variable(value_message_type_id * f"F_{value.selector}"),
                    ],
                ),
                f'access to invalid message field in "{value}"',
                exception_handler,
            )
            if isinstance(field_type, (rty.Integer, rty.Enumeration)):
                get_field_value = self._convert_type(
                    expr.Call(
                        value_message_type_id * f"Get_{value.selector}",
                        [expr.Variable(value_message_context)],
                    ),
                    field_type,
                    value.type_,
                ).ada_expr()
                statements.extend(
                    [
                        CallStatement(
                            message_type_id * f"Set_{field}",
                            [
                                Variable(message_context),
                                get_field_value,
                            ],
                        ),
                    ]
                )
            else:
                assert field_type == rty.OPAQUE
                self._session_context.used_types_body.append(const.TYPES_LENGTH)
                statements.extend(
                    [
                        self._set_opaque_field_to_message_field(
                            message_type_id,
                            message_context,
                            field,
                            value_message_type_id,
                            value_message_context,
                            value.selector,
                        ),
                    ]
                )
        elif (
            isinstance(value, expr.Selected)
            and isinstance(value.prefix, expr.Variable)
            and isinstance(value.prefix.type_, rty.Structure)
        ):
            if isinstance(field_type, (rty.Integer, rty.Enumeration)):
                statements.extend(
                    [
                        CallStatement(
                            message_type_id * f"Set_{field}",
                            [
                                Variable(message_context),
                                self._convert_type(value, field_type, value.type_)
                                .simplified()
                                .ada_expr(),
                            ],
                        )
                    ]
                )
            else:
                assert field_type == rty.OPAQUE
                self._session_context.used_types_body.append(const.TYPES_LENGTH)
                statements.extend(
                    [
                        self._set_opaque_field_to_message_field_from_structure(
                            message_type_id,
                            message_context,
                            field,
                            value.prefix.type_.identifier,
                            value.prefix.identifier,
                            value.selector,
                        ),
                    ]
                )
        elif isinstance(value, expr.Opaque) and isinstance(value.prefix, expr.Variable):
            assert value.type_ == rty.OPAQUE
            assert isinstance(value.prefix.type_, rty.Message)
            value_message_type_id = value.prefix.type_.identifier
            value_message_context = context_id(value.prefix.identifier, is_global)
            statements.extend(
                [
                    self._set_opaque_field_to_message(
                        message_type_id,
                        message_context,
                        field,
                        value_message_type_id,
                        value_message_context,
                    ),
                ]
            )
        else:
            _unsupported_expression(value, "as value of message field")

        return result

    @staticmethod
    def _set_opaque_field(  # pylint: disable = too-many-arguments
        target_type: ID,
        target_context: ID,
        field: ID,
        get_preconditions: Expr,
        get_statements: Sequence[Statement],
        length: Expr,
        pre_declarations: Sequence[Declaration] = None,
        post_statements: Sequence[Statement] = None,
    ) -> Declare:
        pre_declarations = pre_declarations if pre_declarations else []
        post_statements = post_statements if post_statements else []
        return Declare(
            [
                *pre_declarations,
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "RFLX_Process_Data_Pre",
                        "Boolean",
                        [Parameter(["Length"], const.TYPES_LENGTH)],
                    ),
                    get_preconditions,
                ),
                SubprogramBody(
                    ProcedureSpecification(
                        "RFLX_Process_Data",
                        [OutParameter(["Data"], const.TYPES_BYTES)],
                    ),
                    [],
                    get_statements,
                    aspects=[Precondition(Call("RFLX_Process_Data_Pre", [Length("Data")]))],
                ),
                GenericProcedureInstantiation(
                    "RFLX_" + (target_type * f"Set_{field}").flat,
                    ProcedureSpecification(target_type * f"Generic_Set_{field}"),
                    ["RFLX_Process_Data", "RFLX_Process_Data_Pre"],
                ),
            ],
            [
                CallStatement(
                    "RFLX_" + (target_type * f"Set_{field}").flat,
                    [
                        Variable(target_context),
                        length,
                    ],
                ),
                *post_statements,
            ],
        )

    def _set_opaque_field_to_message_field(  # pylint: disable = too-many-arguments
        self,
        target_type: ID,
        target_context: ID,
        field: ID,
        message_type: ID,
        message_context: ID,
        message_field: ID,
    ) -> Declare:
        # Prevent aliasing in generic setter function by moving context into temporary variable
        temporary_message_context = f"RFLX_{message_context.flat}_Tmp"
        return self._set_opaque_field(
            target_type,
            target_context,
            field,
            pre_declarations=[
                Pragma(
                    "Warnings",
                    [
                        Variable("Off"),
                        String("is not modified, could be declared constant"),
                    ],
                ),
                ObjectDeclaration(
                    [temporary_message_context],
                    message_type * "Context",
                    Variable(message_context),
                ),
                Pragma(
                    "Warnings",
                    [
                        Variable("On"),
                        String("is not modified, could be declared constant"),
                    ],
                ),
            ],
            get_preconditions=AndThen(
                Call(
                    message_type * "Has_Buffer",
                    [Variable(temporary_message_context)],
                ),
                Call(
                    message_type * "Well_Formed",
                    [
                        Variable(temporary_message_context),
                        Variable(message_type * f"F_{message_field}"),
                    ],
                ),
                Equal(
                    Variable("Length"),
                    Call(
                        const.TYPES_TO_LENGTH,
                        [
                            Call(
                                message_type * "Field_Size",
                                [
                                    Variable(temporary_message_context),
                                    Variable(message_type * f"F_{message_field}"),
                                ],
                            )
                        ],
                    ),
                ),
            ),
            get_statements=[
                CallStatement(
                    message_type * f"Get_{message_field}",
                    [
                        Variable(temporary_message_context),
                        Variable("Data"),
                    ],
                ),
            ],
            length=Call(
                const.TYPES_TO_LENGTH,
                [
                    Call(
                        message_type * "Field_Size",
                        [
                            Variable(temporary_message_context),
                            Variable(message_type * f"F_{message_field}"),
                        ],
                    )
                ],
            ),
            post_statements=[Assignment(message_context, Variable(temporary_message_context))],
        )

    def _set_opaque_field_to_message_field_from_structure(  # pylint: disable = too-many-arguments
        self,
        target_type: ID,
        target_context: ID,
        field: ID,
        structure_type_id: ID,
        structure: ID,
        structure_field: ID,
    ) -> Declare:
        return self._set_opaque_field(
            target_type,
            target_context,
            field,
            pre_declarations=[],
            get_preconditions=AndThen(
                Call(
                    structure_type_id * "Valid_Structure",
                    [
                        Variable(structure),
                    ],
                ),
                Equal(
                    Variable("Length"),
                    Call(
                        const.TYPES_TO_LENGTH,
                        [
                            Call(
                                structure_type_id * f"Field_Size_{structure_field}",
                                [
                                    Variable(structure),
                                ],
                            ),
                        ],
                    ),
                ),
            ),
            get_statements=[
                Assignment(
                    Variable("Data"),
                    Indexed(
                        Variable(f"{structure}.{structure_field}"),
                        ValueRange(
                            First(f"{structure}.{structure_field}"),
                            Add(
                                First(f"{structure}.{structure_field}"), Length("Data"), -Number(1)
                            ),
                        ),
                    ),
                )
            ],
            length=Call(
                const.TYPES_TO_LENGTH,
                [
                    Call(
                        structure_type_id * f"Field_Size_{structure_field}",
                        [
                            Variable(structure),
                        ],
                    ),
                ],
            ),
            post_statements=[],
        )

    def _set_opaque_field_to_message(
        self,
        target_type: ID,
        target_context: ID,
        field: ID,
        message_type: ID,
        message_context: ID,
    ) -> Declare:
        return self._set_opaque_field(
            target_type,
            target_context,
            field,
            get_preconditions=AndThen(
                Call(
                    message_type * "Has_Buffer",
                    [Variable(message_context)],
                ),
                Call(
                    message_type * "Well_Formed_Message",
                    [
                        Variable(message_context),
                    ],
                ),
                Equal(
                    Variable("Length"),
                    Call(
                        message_type * "Byte_Size",
                        [
                            Variable(message_context),
                        ],
                    ),
                ),
            ),
            get_statements=[
                CallStatement(
                    message_type * "Data",
                    [
                        Variable(message_context),
                        Variable("Data"),
                    ],
                ),
            ],
            length=Call(
                message_type * "Byte_Size",
                [
                    Variable(message_context),
                ],
            ),
        )

    def _declare_context_buffer(
        self,
        identifier: ID,
        type_: ID,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Declaration]:
        return [
            self._declare_context(identifier, type_, is_global),
            self._declare_buffer(identifier),
        ]

    def _declare_context(
        self,
        identifier: ID,
        type_: ID,
        is_global: Callable[[ID], bool],
    ) -> Declaration:
        self._session_context.referenced_types_body.append(type_)
        return ObjectDeclaration([context_id(identifier, is_global)], type_ * "Context")

    @staticmethod
    def _declare_buffer(identifier: ID) -> Declaration:
        return ObjectDeclaration([buffer_id(identifier)], const.TYPES_BYTES_PTR)

    def _declare_sequence_copy(  # pylint: disable = too-many-arguments
        self,
        sequence_identifier: ID,
        sequence_type: ID,
        statements: Callable[[ExceptionHandler], Sequence[Statement]],
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        alloc_id: Optional[Location],
    ) -> IfStatement:
        # https://github.com/Componolit/RecordFlux/issues/577
        sequence_context = context_id(sequence_identifier, is_global)
        take_buffer = self._take_buffer(copy_id(sequence_identifier), sequence_type, is_global)
        free_buffer = self._free_buffer(copy_id(sequence_identifier), alloc_id)

        return self._if_valid_sequence(
            sequence_type,
            sequence_context,
            [
                Declare(
                    self._declare_context_buffer(
                        copy_id(sequence_identifier), sequence_type, is_global
                    ),
                    [
                        *self._allocate_buffer(copy_id(sequence_identifier), alloc_id),
                        self._copy_to_buffer(
                            sequence_type,
                            sequence_context,
                            copy_id(buffer_id(sequence_identifier)),
                            target_buffer_is_smaller=False,
                            exception_handler=exception_handler.copy(free_buffer),
                        ),
                        self._initialize_context(
                            copy_id(sequence_identifier),
                            sequence_type,
                            is_global,
                            last=Call(
                                sequence_type * "Sequence_Last",
                                [Variable(sequence_context)],
                            ),
                        ),
                        *statements(exception_handler.copy([*take_buffer, *free_buffer])),
                        *take_buffer,
                        *free_buffer,
                    ],
                )
            ],
            exception_handler,
        )

    def _declare_message_field_sequence_copy(  # pylint: disable = too-many-arguments
        self,
        message_identifier: ID,
        message_type: ID,
        message_field: ID,
        sequence_identifier: ID,
        sequence_type: ID,
        statements: Callable[[ExceptionHandler], Sequence[Statement]],
        target_buffer_is_smaller: bool,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        alloc_id: Optional[Location],
    ) -> Declare:
        # https://github.com/Componolit/RecordFlux/issues/577
        take_buffer = self._take_buffer(sequence_identifier, sequence_type, is_global)
        free_buffer = self._free_buffer(sequence_identifier, alloc_id)
        local_exception_handler = exception_handler.copy(free_buffer)
        return Declare(
            self._declare_context_buffer(sequence_identifier, sequence_type, is_global),
            [
                *self._allocate_buffer(sequence_identifier, alloc_id),
                self._copy_to_buffer(
                    message_type,
                    context_id(message_identifier, is_global),
                    buffer_id(sequence_identifier),
                    target_buffer_is_smaller,
                    local_exception_handler,
                ),
                self._if_well_formed_message_field(
                    message_type,
                    context_id(message_identifier, is_global),
                    message_field,
                    [
                        self._initialize_context(
                            sequence_identifier,
                            sequence_type,
                            is_global,
                            first=Call(
                                message_type * "Field_First",
                                [
                                    Variable(context_id(message_identifier, is_global)),
                                    Variable(
                                        message_type * model.Field(message_field).affixed_name
                                    ),
                                ],
                            ),
                            last=Call(
                                message_type * "Field_Last",
                                [
                                    Variable(context_id(message_identifier, is_global)),
                                    Variable(
                                        message_type * model.Field(message_field).affixed_name
                                    ),
                                ],
                            ),
                        ),
                        *statements(exception_handler.copy([*take_buffer, *free_buffer])),
                        *take_buffer,
                    ],
                    local_exception_handler,
                ),
                *free_buffer,
            ],
        )

    def _comprehension(  # pylint: disable = too-many-arguments, too-many-locals
        self,
        sequence_identifier: ID,
        sequence_type: ID,
        target_identifier: ID,
        target_type: Union[rty.Sequence, rty.Integer, rty.Enumeration, rty.Message],
        iterator_identifier: ID,
        iterator_type: ID,
        selector: expr.Expr,
        condition: expr.Expr,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        alloc_id: Optional[Location],
    ) -> While:
        assert not isinstance(selector, expr.MessageAggregate)

        assert (
            isinstance(target_type.element, (rty.Integer, rty.Enumeration, rty.Message))
            if isinstance(target_type, rty.Sequence)
            else isinstance(target_type, (rty.Integer, rty.Enumeration, rty.Message))
        )

        target_type_id = target_type.identifier

        update_context = self._update_context(
            context_id(sequence_identifier, is_global),
            context_id(iterator_identifier, is_global),
            sequence_type,
        )
        local_exception_handler = exception_handler.copy(update_context)

        target_invariants = [
            PragmaStatement(
                "Loop_Invariant",
                [
                    Equal(
                        Selected(
                            Variable(context_id(sequence_identifier, is_global)), "Buffer_First"
                        ),
                        LoopEntry(
                            Selected(
                                Variable(context_id(sequence_identifier, is_global)),
                                "Buffer_First",
                            )
                        ),
                    ),
                ],
            ),
            PragmaStatement(
                "Loop_Invariant",
                [
                    Equal(
                        Selected(
                            Variable(context_id(sequence_identifier, is_global)), "Buffer_Last"
                        ),
                        LoopEntry(
                            Selected(
                                Variable(context_id(sequence_identifier, is_global)),
                                "Buffer_Last",
                            )
                        ),
                    ),
                ],
            ),
        ]
        if isinstance(target_type, (rty.Message, rty.Sequence)):
            target_invariants += [
                PragmaStatement(
                    "Loop_Invariant",
                    [
                        Equal(
                            Selected(
                                Variable(context_id(target_identifier, is_global)), "Buffer_First"
                            ),
                            LoopEntry(
                                Selected(
                                    Variable(context_id(target_identifier, is_global)),
                                    "Buffer_First",
                                )
                            ),
                        ),
                    ],
                ),
                PragmaStatement(
                    "Loop_Invariant",
                    [
                        Equal(
                            Selected(
                                Variable(context_id(target_identifier, is_global)), "Buffer_Last"
                            ),
                            LoopEntry(
                                Selected(
                                    Variable(context_id(target_identifier, is_global)),
                                    "Buffer_Last",
                                )
                            ),
                        ),
                    ],
                ),
                PragmaStatement(
                    "Loop_Invariant",
                    [
                        Call(
                            target_type_id * "Has_Buffer",
                            [Variable(context_id(target_identifier, is_global))],
                        )
                    ],
                ),
            ]

        if isinstance(target_type, rty.Sequence):
            target_invariants += [
                PragmaStatement(
                    "Loop_Invariant",
                    [
                        Call(
                            target_type_id * "Valid",
                            [Variable(context_id(target_identifier, is_global))],
                        )
                    ],
                ),
            ]

        return While(
            Call(
                sequence_type * "Has_Element",
                [Variable(context_id(sequence_identifier, is_global))],
            ),
            [
                PragmaStatement(
                    "Loop_Invariant",
                    [
                        Call(
                            sequence_type * "Has_Buffer",
                            [Variable(context_id(sequence_identifier, is_global))],
                        )
                    ],
                ),
                *target_invariants,
                PragmaStatement(
                    "Loop_Invariant",
                    [
                        Equal(Variable(buffer_id(sequence_identifier)), Variable("null")),
                    ],
                ),
                PragmaStatement(
                    "Loop_Invariant",
                    [
                        Equal(
                            Variable("Ctx.P.Slots" * self._allocator.get_slot_ptr(alloc_id)),
                            Variable("null"),
                        ),
                    ],
                ),
                Declare(
                    [self._declare_context(iterator_identifier, iterator_type, is_global)],
                    [
                        CallStatement(
                            sequence_type * "Switch",
                            [
                                Variable(context_id(sequence_identifier, is_global)),
                                Variable(context_id(iterator_identifier, is_global)),
                            ],
                        ),
                        CallStatement(
                            iterator_type * "Verify_Message",
                            [Variable(context_id(iterator_identifier, is_global))],
                        ),
                        *self._if_valid_fields(
                            condition,
                            [
                                IfStatement(
                                    [
                                        (
                                            condition.substituted(
                                                self._substitution(is_global)
                                            ).ada_expr(),
                                            self._comprehension_append_element(
                                                target_identifier,
                                                target_type,
                                                selector,
                                                update_context,
                                                local_exception_handler,
                                                is_global,
                                            )
                                            if isinstance(target_type, rty.Sequence)
                                            else self._comprehension_assign_element(
                                                target_identifier,
                                                target_type,
                                                selector,
                                                update_context,
                                                local_exception_handler,
                                                is_global,
                                            ),
                                        )
                                    ]
                                )
                            ],
                            local_exception_handler,
                            is_global,
                        ),
                        *update_context,
                    ],
                ),
            ],
        )

    def _comprehension_assign_element(  # pylint: disable = too-many-arguments
        self,
        target_identifier: ID,
        target_type: Union[rty.Integer, rty.Enumeration, rty.Message],
        selector: expr.Expr,
        update_context: Sequence[Statement],
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        target_type_id = target_type.identifier
        assign_element: Sequence[Statement]

        if isinstance(target_type, rty.Message):
            if not isinstance(selector, expr.Variable):
                fail(
                    "expressions other than variables not yet supported"
                    " as selector for message types",
                    Subsystem.GENERATOR,
                    location=selector.location,
                )
            element_id = selector.identifier + "_Ctx"
            target_context = context_id(target_identifier, is_global)
            target_buffer = "RFLX_Target_" + target_identifier

            assign_element = [
                self._if_well_formed_message(
                    target_type.identifier,
                    element_id,
                    [
                        Declare(
                            [self._declare_buffer(target_buffer)],
                            [
                                *self._take_buffer(
                                    target_identifier,
                                    target_type_id,
                                    is_global,
                                    buffer_id(target_buffer),
                                ),
                                self._copy_to_buffer(
                                    target_type_id,
                                    element_id,
                                    buffer_id(target_buffer),
                                    target_buffer_is_smaller=True,
                                    exception_handler=exception_handler,
                                ),
                                CallStatement(
                                    target_type_id * "Initialize",
                                    [
                                        Variable(variable_id(target_context, is_global)),
                                        Variable(buffer_id(target_buffer)),
                                        Call(target_type_id * "Size", [Variable(element_id)]),
                                    ],
                                ),
                                CallStatement(
                                    target_type_id * "Verify_Message",
                                    [Variable(variable_id(target_context, is_global))],
                                ),
                            ],
                        ),
                    ],
                    exception_handler,
                )
            ]

        elif isinstance(target_type, (rty.Integer, rty.Enumeration)):
            assign_element = self._if_valid_fields(
                selector,
                [
                    Assignment(
                        Variable(variable_id(target_identifier, is_global)),
                        selector.substituted(self._substitution(is_global)).ada_expr(),
                    ),
                ],
                exception_handler,
                is_global,
            )
        else:
            assert False

        return [
            *assign_element,
            Assignment(Variable(found_id(target_identifier)), TRUE),
            *update_context,
            ExitStatement(),
        ]

    def _comprehension_append_element(
        self,
        target_identifier: ID,
        target_type: rty.Sequence,
        selector: expr.Expr,
        _: Sequence[Statement],
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        assert isinstance(target_type, rty.Sequence)

        target_type_id = target_type.identifier
        required_space: Expr
        append_element: Statement

        if isinstance(target_type.element, rty.Message):
            if not isinstance(selector, expr.Variable):
                fail(
                    "expressions other than variables not yet supported"
                    " as selector for message types",
                    Subsystem.GENERATOR,
                    location=selector.location,
                )

            element_id = selector.identifier + "_Ctx"
            required_space = Call(
                target_type.element.identifier * "Size",
                [Variable(element_id)],
            )
            append_element = self._if_well_formed_message(
                target_type.element.identifier,
                element_id,
                [
                    self._if(
                        Greater(
                            Call(
                                target_type.element.identifier * "Size",
                                [
                                    Variable(element_id),
                                ],
                            ),
                            Number(0),
                        ),
                        [
                            CallStatement(
                                target_type_id * "Append_Element",
                                [
                                    Variable(context_id(target_identifier, is_global)),
                                    Variable(element_id),
                                ],
                            )
                        ],
                        "empty messages cannot be appended to sequence",
                        exception_handler,
                    )
                ],
                exception_handler,
            )

        elif isinstance(target_type.element, (rty.Integer, rty.Enumeration)):
            required_space = Size(
                target_type.element.identifier + "_Enum"
                if isinstance(
                    target_type.element,
                    rty.Enumeration,
                )
                and target_type.element.always_valid
                else target_type.element.identifier
            )
            append_element = CallStatement(
                target_type_id * "Append_Element",
                [
                    Variable(context_id(target_identifier, is_global)),
                    selector.substituted(self._substitution(is_global)).ada_expr(),
                ],
            )

        else:
            assert False

        return [
            self._if_sufficient_space_in_sequence(
                required_space,
                target_type_id,
                context_id(target_identifier, is_global),
                [append_element],
                exception_handler,
            ),
        ]

    def _free_context_buffer(
        self,
        identifier: ID,
        type_: ID,
        is_global: Callable[[ID], bool],
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        return [
            *self._take_buffer(identifier, type_, is_global),
            *self._free_buffer(identifier, alloc_id),
        ]

    def _free_buffer(self, identifier: ID, alloc_id: Optional[Location]) -> Sequence[Statement]:
        slot = Variable("Ctx.P.Slots" * self._allocator.get_slot_ptr(alloc_id))
        return [
            PragmaStatement("Assert", [Equal(slot, Variable("null"))]),
            PragmaStatement(
                "Assert", [NotEqual(Variable(buffer_id(identifier)), Variable("null"))]
            ),
            Assignment(
                slot,
                Variable(buffer_id(identifier)),
            ),
            PragmaStatement("Assert", [NotEqual(slot, Variable("null"))]),
        ]

    @staticmethod
    def _take_buffer(
        identifier: ID, type_: ID, is_global: Callable[[ID], bool], buf: ID = None
    ) -> Sequence[Statement]:
        context = context_id(identifier, is_global)
        buf = buf or buffer_id(identifier)
        return [
            # https://github.com/Componolit/Workarounds/issues/32
            PragmaStatement(
                "Warnings",
                [
                    Variable("Off"),
                    String(
                        f'"{context.ada_str}" is set by "Take_Buffer" but not used after the call'
                    ),
                ],
            ),
            CallStatement(
                type_ * "Take_Buffer",
                [
                    Variable(context),
                    Variable(buf),
                ],
            ),
            PragmaStatement(
                "Warnings",
                [
                    Variable("On"),
                    String(
                        f'"{context.ada_str}" is set by "Take_Buffer" but not used after the call'
                    ),
                ],
            ),
        ]

    @staticmethod
    def _update_context(
        sequence_context: ID, element_context: ID, sequence_type: ID
    ) -> Sequence[Statement]:
        return [
            # https://github.com/Componolit/Workarounds/issues/32
            PragmaStatement(
                "Warnings",
                [
                    Variable("Off"),
                    String(
                        f'"{element_context.ada_str}" is set by "Update" '
                        f"but not used after the call"
                    ),
                ],
            ),
            CallStatement(
                sequence_type * "Update",
                [
                    Variable(sequence_context),
                    Variable(element_context),
                ],
            ),
            PragmaStatement(
                "Warnings",
                [
                    Variable("On"),
                    String(
                        f'"{element_context.ada_str}" is set by "Update"'
                        f" but not used after the call"
                    ),
                ],
            ),
        ]

    def _allocate_buffer(self, identifier: ID, alloc_id: Optional[Location]) -> Sequence[Statement]:
        self._session_context.used_types_body.append(const.TYPES_INDEX)
        slot_id = Variable("Ctx.P.Slots" * self._allocator.get_slot_ptr(alloc_id))
        return [
            Assignment(buffer_id(identifier), slot_id),
            PragmaStatement("Warnings", [Variable("Off"), String("unused assignment")]),
            Assignment(slot_id, Variable("null")),
            PragmaStatement("Warnings", [Variable("On"), String("unused assignment")]),
        ]

    @staticmethod
    def _initialize_context(  # pylint: disable = too-many-arguments
        identifier: ID,
        type_: ID,
        is_global: Callable[[ID], bool],
        first: Expr = None,
        last: Expr = None,
        parameters: Mapping[ID, Expr] = None,
        written_last: Expr = None,
    ) -> CallStatement:
        return CallStatement(
            type_ * "Initialize",
            [
                Variable(context_id(identifier, is_global)),
                Variable(buffer_id(identifier)),
                *(
                    [
                        first
                        or Call(const.TYPES_TO_FIRST_BIT_INDEX, [First(buffer_id(identifier))]),
                        last or Call(const.TYPES_TO_FIRST_BIT_INDEX, [Last(buffer_id(identifier))]),
                    ]
                    if first or last
                    else []
                ),
                *([written_last] if written_last else []),
            ],
            parameters,
        )

    def _copy_to_buffer(
        self,
        type_: ID,
        source_context: ID,
        target_buffer: ID,
        target_buffer_is_smaller: bool,
        exception_handler: ExceptionHandler,
    ) -> Statement:
        self._session_context.used_types_body.append(const.TYPES_LENGTH)
        copy = CallStatement(
            type_ * "Copy",
            [
                Variable(source_context),
                Indexed(
                    Variable(target_buffer * "all"),
                    ValueRange(
                        First(target_buffer),
                        Add(
                            First(target_buffer),
                            Call(
                                const.TYPES_INDEX,
                                [
                                    Add(
                                        Call(
                                            type_ * "Byte_Size",
                                            [Variable(source_context)],
                                        ),
                                        Number(1),
                                    )
                                ],
                            ),
                            -Number(2),
                        ),
                    ),
                ),
            ],
        )

        if target_buffer_is_smaller:
            return IfStatement(
                [
                    (
                        LessEqual(
                            Call(
                                type_ * "Byte_Size",
                                [Variable(source_context)],
                            ),
                            Length(target_buffer),
                        ),
                        [
                            copy,
                        ],
                    )
                ],
                exception_handler.execute(),
            )

        return copy

    def _convert_type(
        self, expression: expr.Expr, target_type: rty.Type, expression_type: rty.Type = None
    ) -> expr.Expr:
        if not expression_type:
            expression_type = expression.type_

        if expression_type.is_compatible_strong(target_type):
            return expression

        assert isinstance(target_type, (rty.Integer, rty.Enumeration)), target_type

        self._session_context.referenced_types_body.append(target_type.identifier)
        return expr.Conversion(target_type.identifier, expression)

    def _debug_output(self, string: str) -> Sequence[CallStatement]:
        return (
            [
                CallStatement(
                    self._prefix * ID("RFLX_Debug.Print")
                    if self._debug == common.Debug.EXTERNAL
                    else "Ada.Text_IO.Put_Line",
                    [String(string)],
                )
            ]
            if self._debug != common.Debug.NONE
            else []
        )


def copy_id(identifier: ID) -> ID:
    return ID("RFLX_Copy_" + identifier.flat)


def variable_id(identifier: ID, is_global: Callable[[ID], bool]) -> ID:
    if is_global(identifier):
        return "Ctx.P" * identifier

    return identifier


def context_id(identifier: ID, is_global: Callable[[ID], bool]) -> ID:
    identifier = identifier + "_Ctx"

    if is_global(identifier):
        return "Ctx.P" * identifier

    return identifier


def buffer_id(identifier: ID) -> ID:
    return identifier + "_Buffer"


def found_id(identifier: ID) -> ID:
    return ID("RFLX_" + identifier.flat + "_Found")


def state_id(identifier: ID) -> ID:
    assert identifier != ID("null")
    return "S_" + identifier


def _unexpected_expression(expression: expr.Expr, context: str) -> NoReturn:
    fatal_fail(
        f"unexpected {type(expression).__name__} with {expression.type_} {context}",
        Subsystem.GENERATOR,
        location=expression.location,
    )


def _unsupported_expression(expression: expr.Expr, context: str) -> NoReturn:
    fail(
        f"{type(expression).__name__} with {expression.type_} {context} not yet supported",
        Subsystem.GENERATOR,
        location=expression.location,
    )

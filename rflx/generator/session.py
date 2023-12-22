from __future__ import annotations

import typing as ty
from collections.abc import Callable, Iterable, Mapping, Sequence
from dataclasses import dataclass, field as dataclass_field
from functools import partial, singledispatchmethod
from typing import NoReturn, Optional, Union

from rflx import ada, ir, model, typing_ as rty
from rflx.ada import (
    FALSE,
    TRUE,
    Add,
    Aggregate,
    And,
    AndThen,
    Annotate,
    Aspect,
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
    Conversion,
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
    Literal,
    LoopEntry,
    Min,
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
    Val,
    ValueRange,
    Variable,
    While,
    WithClause,
)
from rflx.const import BUILTINS_PACKAGE, INTERNAL_PACKAGE
from rflx.error import Location, Subsystem, fail, fatal_fail
from rflx.identifier import ID

from . import common, const
from .allocator import AllocatorGenerator


@dataclass
class SessionContext:
    referenced_types: list[ID] = dataclass_field(default_factory=list)
    referenced_types_body: list[ID] = dataclass_field(default_factory=list)
    referenced_packages_body: list[ID] = dataclass_field(default_factory=list)
    used_types: list[ID] = dataclass_field(default_factory=list)
    used_types_body: list[ID] = dataclass_field(default_factory=list)
    states_with_exceptions: set[ID] = dataclass_field(default_factory=set)


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


@dataclass
class ExceptionHandler:
    state: ir.State
    finalization: Sequence[Statement]
    record_state_with_exceptions: Callable[[], None]

    def execute(self) -> list[Statement]:
        assert (
            self.state.exception_transition
        ), f'missing exception transition for state "{self.state.identifier}"'
        self.record_state_with_exceptions()
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
            self.state,
            [*finalization, *self.finalization],
            self.record_state_with_exceptions,
        )


@dataclass
class ChannelAccess:
    state: ID
    message: ID
    message_type: ID


class SessionGenerator:
    def __init__(
        self,
        session: ir.Session,
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

        if len(identifier.parts) > 1 and identifier.parent == BUILTINS_PACKAGE:
            return self._prefix * const.TYPES_PACKAGE * identifier.name

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

        if any(t.parent == const.TYPES for t in self._session_context.used_types):
            declaration_context.append(WithClause(self._prefix * const.TYPES_PACKAGE))

        body_context: list[ContextItem] = [
            *(
                [
                    WithClause(self._prefix * ID("RFLX_Debug"))
                    if self._debug == common.Debug.EXTERNAL
                    else WithClause("Ada.Text_IO"),
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
                    ],
                )

        body_context.extend(
            [WithClause(self._prefix * p) for p in self._session_context.referenced_packages_body],
        )

        if any(
            t.parent == const.TYPES
            for t in (
                set(self._session_context.used_types_body) - set(self._session_context.used_types)
            )
        ):
            body_context.append(WithClause(self._prefix * const.TYPES_PACKAGE))

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
            *[
                UseTypeClause(self._prefix * type_identifier)
                for type_identifier in self._session_context.used_types_body
                if type_identifier.parent
                not in [INTERNAL_PACKAGE, BUILTINS_PACKAGE, self._session.identifier.parent]
                and type_identifier not in self._session_context.used_types
            ],
            *(
                [
                    # TODO(eng/recordflux/RecordFlux#1370): Remove the need to suppress warnings
                    Pragma(
                        "Warnings",
                        [
                            Variable("Off"),
                            String('"*" is already use-visible through previous use_type_clause'),
                        ],
                    ),
                    Pragma(
                        "Warnings",
                        [
                            Variable("Off"),
                            String('use clause for type "*" defined at * has no effect'),
                        ],
                    ),
                    UseTypeClause(self._prefix * const.TYPES_BASE_INT),
                    Pragma(
                        "Warnings",
                        [
                            Variable("On"),
                            String('use clause for type "*" defined at * has no effect'),
                        ],
                    ),
                    Pragma(
                        "Warnings",
                        [
                            Variable("On"),
                            String('"*" is already use-visible through previous use_type_clause'),
                        ],
                    ),
                ]
                if any(
                    type_identifier == rty.BASE_INTEGER.identifier
                    for type_identifier in self._session_context.used_types_body
                )
                else []
            ),
        ]

    def _create_state_machine(self) -> UnitPart:
        evaluated_declarations = self._evaluate_declarations(
            self._session.declarations,
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
            for d in self._session.declarations
            if isinstance(d, ir.VarDecl) and isinstance(d.type_, (rty.Message, rty.Sequence))
        ]

        unit = UnitPart()
        unit += self._create_abstract_functions(self._session.parameters)
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
        session: ir.Session,
        read: bool = False,
        write: bool = False,
    ) -> dict[ID, list[ChannelAccess]]:
        assert (read and not write) or (not read and write)

        channels: dict[ID, list[ChannelAccess]] = {
            parameter.identifier: []
            for parameter in session.parameters
            if isinstance(parameter, ir.ChannelDecl)
        }
        for state in session.states:
            for action in state.actions:
                if (
                    isinstance(action, ir.ChannelStmt)
                    and isinstance(action, ir.Read if read else ir.Write)
                    and isinstance(action.expression, ir.Var)
                    and isinstance(action.expression.type_, rty.Message)
                ):
                    channels[action.channel].append(
                        ChannelAccess(
                            state.identifier,
                            action.expression.identifier,
                            action.expression.type_.identifier,
                        ),
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
    def _create_channel_and_state_types(session: ir.Session) -> UnitPart:
        channel_params = [x for x in session.parameters if isinstance(x, ir.ChannelDecl)]
        return UnitPart(
            [
                *(
                    [
                        EnumerationType(
                            "Channel",
                            {ID(f"C_{parameter.identifier}"): None for parameter in channel_params},
                        ),
                    ]
                    if channel_params
                    else []
                ),
                EnumerationType(
                    "State",
                    {state_id(s.identifier): None for s in [*session.states, ir.FINAL_STATE]},
                ),
            ],
        )

    def _create_context_type(
        self,
        initial_state: ID,
        global_variables: Mapping[ID, tuple[ID, Optional[Expr]]],
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
        parameters: Iterable[ir.FormalDecl],
    ) -> UnitPart:
        result: list[Declaration] = []

        for parameter in parameters:
            if isinstance(parameter, ir.ChannelDecl):
                pass
            elif isinstance(parameter, ir.FuncDecl):
                result.extend(self._create_abstract_function(parameter))
            else:
                fatal_fail(
                    f'unexpected formal parameter "{parameter.identifier}"',
                    Subsystem.GENERATOR,
                    location=parameter.location,
                )

        return UnitPart(result)

    def _create_abstract_function(self, function: ir.FuncDecl) -> Sequence[SubprogramDeclaration]:
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
                isinstance(field_type, rty.Sequence) and field_type != rty.OPAQUE
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
            if isinstance(a.type_, rty.Sequence) and a.type_ != rty.OPAQUE:
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
                ),
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
            ),
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
            ),
        ]

    def _create_uninitialized_function(
        self,
        composite_globals: Sequence[ir.VarDecl],
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
                                ),
                            )
                            for declaration in composite_globals
                            if isinstance(declaration.type_, (rty.Message, rty.Sequence))
                            and declaration.type_ != rty.OPAQUE
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
        composite_globals: Sequence[ir.VarDecl],
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
                                    d.type_.identifier * "Has_Buffer",
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

    def _create_initialized_function(self, composite_globals: Sequence[ir.VarDecl]) -> UnitPart:
        specification = FunctionSpecification(
            "Initialized",
            "Boolean",
            [
                Parameter(
                    ["Ctx" if composite_globals or self._allocator.required else "Unused_Ctx"],
                    "Context'Class",
                ),
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
                        ],
                    ),
                ),
            ],
        )

    def _create_states(
        self,
        session: ir.Session,
        composite_globals: Sequence[ir.VarDecl],
        is_global: Callable[[ID], bool],
    ) -> UnitPart:
        if self._allocator.get_global_slot_ptrs() or self._allocator.get_local_slot_ptrs():
            self._session_context.used_types_body.append(const.TYPES_BYTES_PTR)

        unit_body: list[Declaration] = []

        for state in session.states:
            if state == ir.FINAL_STATE:
                continue

            invariant = []
            slots = []

            declarations = [
                *[a for a in state.actions if isinstance(a, ir.VarDecl)],
                *[
                    s
                    for a in state.actions
                    if isinstance(a, ir.Assign)
                    and isinstance(a.expression, (ir.Comprehension, ir.Find))
                    for s in [*a.expression.selector.stmts, *a.expression.condition.stmts]
                    if isinstance(s, ir.VarDecl)
                ],
                *[
                    s
                    for t in state.transitions
                    for s in t.condition.stmts
                    if isinstance(s, ir.VarDecl)
                ],
            ]

            for d in declarations:
                if isinstance(d.type_, (rty.Message, rty.Sequence)) and d.type_ != rty.OPAQUE:
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
                                        self._allocator.get_size(d.identifier, state.identifier)
                                        - 1,
                                    ),
                                ),
                            ),
                            Equal(
                                Variable("Ctx.P.Slots" * self._allocator.get_slot_ptr(d.location)),
                                Variable("null"),
                            ),
                        ],
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
                ],
            )

            evaluated_declarations = self._evaluate_declarations(declarations, is_global)
            exception_handler = ExceptionHandler(
                state,
                [PragmaStatement("Assert", [Call(f"{state.identifier}_Invariant")])],
                partial(
                    lambda state: self._session_context.states_with_exceptions.add(
                        state.identifier,
                    ),
                    state,
                ),
            )
            statements = [
                *[
                    s
                    for a in state.actions
                    for s in self._state_action(
                        state.identifier,
                        a,
                        exception_handler,
                        is_global,
                    )
                ],
                *self._determine_next_state(
                    state.transitions,
                    exception_handler,
                    is_global,
                    state.identifier,
                ),
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
                    ],
                    [
                        *evaluated_declarations.initialization,
                        PragmaStatement("Assert", [Call(f"{state.identifier}_Invariant")]),
                        *statements,
                        PragmaStatement("Assert", [Call(f"{state.identifier}_Invariant")]),
                        *(
                            [Label(f"Finalize_{state.identifier}")]
                            if state.identifier in self._session_context.states_with_exceptions
                            else []
                        ),
                        *evaluated_declarations.finalization,
                        *(
                            [
                                PragmaStatement(
                                    "Assert",
                                    [Call("Global_Initialized", [Variable("Ctx")])],
                                ),
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
        self,
        transitions: Sequence[ir.Transition],
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
    ) -> Sequence[Statement]:
        return (
            [
                *[
                    a
                    for t in transitions
                    for s in t.condition.stmts
                    for a in self._state_action(state, s, exception_handler, is_global)
                ],
                IfStatement(
                    [
                        (
                            self._to_ada_expr(t.condition.expr, is_global),
                            [
                                Assignment(
                                    "Ctx.P.Next_State",
                                    Variable(state_id(t.target)),
                                ),
                            ],
                        )
                        for t in transitions[:-1]
                    ],
                    [
                        Assignment(
                            "Ctx.P.Next_State",
                            Variable(state_id(transitions[-1].target)),
                        ),
                    ],
                ),
            ]
            if transitions
            else []
        )

    @staticmethod
    def _create_active_function(session: ir.Session) -> UnitPart:
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
                        Variable("Ctx.P.Next_State"),
                        Variable(state_id(ir.FINAL_STATE.identifier)),
                    )
                    if len(session.states) > 1
                    else Variable("False"),
                ),
            ],
        )

    @staticmethod
    def _create_initialize_procedure(
        session: ir.Session,
        declarations: Sequence[Declaration],
        initialization: Sequence[Statement],
    ) -> UnitPart:
        specification = ProcedureSpecification(
            "Initialize",
            [InOutParameter(["Ctx"], "Context'Class")],
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
                            ),
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
                            "Ctx.P.Next_State",
                            Variable(state_id(session.initial_state.identifier)),
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
            "Finalize",
            [InOutParameter(["Ctx"], "Context'Class")],
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
                            ),
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
                        Assignment(
                            "Ctx.P.Next_State",
                            Variable(state_id(ir.FINAL_STATE.identifier)),
                        ),
                    ],
                ),
            ],
        )

    def _create_reset_messages_before_write_procedure(
        self,
        session: ir.Session,
        is_global: Callable[[ID], bool],
    ) -> UnitPart:
        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        specification = ProcedureSpecification(
            "Reset_Messages_Before_Write",
            [InOutParameter(["Ctx"], "Context'Class")],
        )
        states = [
            (
                state,
                [
                    (
                        action.expression.identifier,
                        action.expression.type_,
                    )
                    for action in state.actions
                    if (
                        isinstance(action, ir.Read)
                        and isinstance(action.expression, ir.Var)
                        and isinstance(action.expression.type_, rty.Message)
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
                                                        context_id(message, is_global) * "First",
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
                ),
            ],
        )

    def _create_tick_procedure(self, session: ir.Session, has_writes: bool) -> UnitPart:
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
                                    if s != ir.FINAL_STATE
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
                ),
            ],
        )

    @staticmethod
    def _create_in_io_state_function(session: ir.Session) -> UnitPart:
        io_states = [
            state
            for state in session.states
            if any(
                True
                for action in state.actions
                if (
                    isinstance(action, (ir.Read, ir.Write))
                    and isinstance(action.expression, ir.Var)
                    and isinstance(action.expression.type_, rty.Message)
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
                                    "Loop_Invariant",
                                    [Call("Initialized", [Variable("Ctx")])],
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
            "Next_State",
            "State",
            [Parameter(["Ctx"], "Context'Class")],
        )
        return UnitPart(
            [
                SubprogramDeclaration(specification),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Variable("Ctx.P.Next_State"),
                ),
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
                    specification,
                    [Precondition(Call("Initialized", [Variable("Ctx")]))],
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
                                                                context_id(
                                                                    write.message,
                                                                    is_global,
                                                                ),
                                                            ),
                                                        ],
                                                    ),
                                                    Greater(
                                                        Call(
                                                            write.message_type * "Byte_Size",
                                                            [
                                                                Variable(
                                                                    context_id(
                                                                        write.message,
                                                                        is_global,
                                                                    ),
                                                                ),
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
                ),
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
                    specification,
                    [Precondition(Call("Initialized", [Variable("Ctx")]))],
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
                ),
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
                            ),
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
                                                            context_id(write.message, is_global),
                                                        ),
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
                ),
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
                            ),
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
                ),
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
                            ),
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
                                "Read",
                                [Parameter(["Message_Buffer"], const.TYPES_BYTES)],
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
                                            ),
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
                                                ),
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
                                ),
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
                                },
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
                                                                            write.message,
                                                                            is_global,
                                                                        ),
                                                                    ),
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
                                        ),
                                    ],
                                )
                                for channel, writes in channel_writes.items()
                            ],
                        ),
                    ],
                ),
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
                            ),
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
                                                ),
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
                                    ),
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
                                },
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
                                                                            write.message,
                                                                            is_global,
                                                                        ),
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
                                        ),
                                    ],
                                )
                                for channel, reads in channel_reads.items()
                            ],
                        ),
                    ],
                ),
            ],
        )

    def _evaluate_declarations(
        self,
        declarations: Iterable[ir.VarDecl],
        is_global: Optional[Callable[[ID], bool]] = None,
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
                ),
            )

        return result

    def _state_action(
        self,
        state: ID,
        action: ir.Stmt,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        if isinstance(action, ir.VarDecl):
            return []

        if isinstance(action, ir.Assign):
            result = self._assign(
                action.target,
                action.type_,
                action.expression,
                exception_handler,
                is_global,
                state,
                action.location,
            )

        elif isinstance(action, ir.FieldAssign):
            result = self._message_field_assign(
                action.message,
                action.field,
                action.type_,
                action.expression,
                exception_handler,
                is_global,
            )

        elif isinstance(action, ir.Append):
            result = self._append(action, exception_handler, is_global)

        elif isinstance(action, ir.Extend):
            fail(
                "Extend statement not yet supported",
                Subsystem.GENERATOR,
                location=action.location,
            )

        elif isinstance(action, ir.Reset):
            result = self._reset(action, is_global)

        elif isinstance(action, ir.Read):
            result = self._read(action, is_global)

        elif isinstance(action, ir.Write):
            result = self._write(action)

        elif isinstance(action, ir.Check):
            result = self._check(action.expression, action.origin, exception_handler, is_global)

        else:
            fatal_fail(
                f'unexpected statement "{type(action).__name__}"',
                Subsystem.GENERATOR,
                location=action.location,
            )

        assert action.location is not None
        return [CommentStatement(str(action.location)), *result]

    def _declare(  # noqa: PLR0912, PLR0913
        self,
        identifier: ID,
        type_: rty.Type,
        is_global: Callable[[ID], bool],
        alloc_id: Optional[Location],
        expression: Optional[ir.ComplexExpr] = None,
        constant: bool = False,
        session_global: bool = False,
    ) -> EvaluatedDeclaration:
        result = EvaluatedDeclaration()

        if expression and isinstance(expression.expr, ir.Call):
            fail(
                "initialization using function call not yet supported",
                Subsystem.GENERATOR,
                location=expression.expr.location,
            )

        if type_ == rty.OPAQUE:
            initialization = None
            object_type: Expr = Variable(const.TYPES_BYTES)
            aspects: list[Aspect] = []

            if expression:
                if expression.is_expr() and isinstance(expression.expr, ir.Agg):
                    e = expression.expr
                    if len(e.elements) == 0:
                        object_type = Slice(
                            Variable(const.TYPES_BYTES),
                            Last(const.TYPES_INDEX),
                            First(const.TYPES_INDEX),
                        )
                        initialization = None
                    if len(e.elements) > 0:
                        aspects.append(
                            SizeAspect(Mul(Number(len(e.elements)), Size(const.TYPES_BYTE))),
                        )
                        initialization = expression.expr
                else:
                    fail(
                        "initialization not yet supported",
                        Subsystem.GENERATOR,
                        location=expression.expr.location,
                    )

            result.global_declarations.append(
                ObjectDeclaration(
                    [identifier],
                    object_type,
                    self._to_ada_expr(initialization, is_global) if initialization else None,
                    constant=constant if initialization else False,
                    aliased=False,
                    aspects=aspects,
                ),
            )

        elif isinstance(type_, (rty.UniversalInteger, rty.Integer, rty.Enumeration)):
            result.global_declarations.append(
                ObjectDeclaration(
                    [identifier],
                    self._ada_type(type_.identifier)
                    if isinstance(type_, rty.NamedType)
                    else const.TYPES_BASE_INT,
                    self._to_ada_expr(expression.expr, is_global)
                    if expression and expression.is_basic_expr()
                    else None,
                ),
            )
            if expression:
                if session_global and expression.is_basic_expr():
                    result.initialization.append(
                        Assignment(
                            variable_id(identifier, is_global),
                            self._to_ada_expr(
                                self._convert_type(expression.expr, type_),
                                is_global,
                            ),
                        ),
                    )
                if not expression.is_basic_expr():
                    fail(
                        "initialization with complex expression not yet supported",
                        Subsystem.GENERATOR,
                        location=expression.expr.location,
                    )

        elif isinstance(type_, (rty.Message, rty.Sequence)):
            if expression is not None:
                fail(
                    f"initialization for {type_} not yet supported",
                    Subsystem.GENERATOR,
                    location=expression.expr.location,
                )

            type_identifier = self._ada_type(type_.identifier)

            result.global_declarations.append(
                self._declare_context(
                    identifier,
                    type_identifier,
                    (lambda _: False) if session_global else is_global,
                ),
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
                ],
            )
            result.finalization.extend(
                self._free_context_buffer(identifier, type_identifier, is_global, alloc_id),
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

        assert isinstance(type_, (rty.NamedType, rty.UniversalInteger)), type_

        type_identifier = (
            type_.identifier if isinstance(type_, rty.NamedType) else const.TYPES_BASE_INT
        )
        if session_global:
            self._session_context.referenced_types.append(type_identifier)
        else:
            self._session_context.referenced_types_body.append(type_identifier)

        return result

    def _assign(  # noqa: PLR0913
        self,
        target: ID,
        target_type: rty.Type,
        expression: ir.Expr,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        if isinstance(expression, ir.DeltaMsgAgg):
            return self._assign_to_delta_message_aggregate(
                target,
                expression,
                exception_handler,
                is_global,
            )

        if isinstance(expression, ir.MsgAgg):
            return self._assign_to_message_aggregate(
                target,
                expression,
                exception_handler,
                is_global,
            )

        if (
            isinstance(target_type, rty.Message)
            and isinstance(expression, ir.Var)
            and expression.identifier == target
        ):
            fail(
                f'referencing assignment target "{target}" of type message in expression'
                " not yet supported",
                Subsystem.GENERATOR,
                location=expression.location,
            )

        if isinstance(expression, ir.FieldAccess):
            return self._assign_to_field_access(target, expression, exception_handler, is_global)

        if isinstance(expression, ir.Head):
            return self._assign_to_head(
                target,
                expression,
                exception_handler,
                is_global,
                state,
                alloc_id,
            )

        if isinstance(expression, ir.Comprehension):
            assert isinstance(target_type, rty.Sequence)
            return self._assign_to_comprehension(
                target,
                target_type,
                expression,
                exception_handler,
                is_global,
                state,
                alloc_id,
            )

        if isinstance(expression, ir.Find):
            return self._assign_to_find(
                target,
                expression,
                exception_handler,
                is_global,
                state,
                alloc_id,
            )

        if isinstance(expression, ir.Call):
            return self._assign_to_call(target, expression, exception_handler, is_global, state)

        if isinstance(expression, ir.Conversion):
            return self._assign_to_conversion(target, expression, exception_handler, is_global)

        if isinstance(
            expression,
            (
                ir.Var,
                ir.EnumLit,
                ir.IntVal,
                ir.BoolVal,
                ir.BinaryIntExpr,
                ir.Relation,
                ir.Attr,
                ir.FieldAccessAttr,
                ir.Agg,
                ir.BinaryBoolExpr,
                ir.CaseExpr,
            ),
        ) and (
            isinstance(expression.type_, (rty.AnyInteger, rty.Enumeration, rty.Aggregate))
            or expression.type_ == rty.OPAQUE
        ):
            assert isinstance(
                target_type,
                (rty.Integer, rty.Enumeration, rty.Message, rty.Sequence),
            ), target_type
            return [
                Assignment(
                    variable_id(target, is_global),
                    self._to_ada_expr(self._convert_type(expression, target_type), is_global),
                ),
            ]

        if isinstance(expression, ir.Var) and isinstance(
            expression.type_,
            (rty.Message, rty.Sequence),
        ):
            _unsupported_expression(expression, "in assignment")

        _unexpected_expression(expression, "in assignment")

    def _assign_to_field_access(
        self,
        target: ID,
        field_access: ir.FieldAccess,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        if isinstance(field_access.message_type, rty.Structure):
            return [
                Assignment(
                    Variable(variable_id(target, is_global)),
                    Variable(field_access.message * field_access.field),
                ),
            ]

        assert isinstance(field_access.message_type, rty.Message)

        message_type = field_access.message_type.identifier
        message_context = context_id(field_access.message, is_global)
        field = field_access.field

        if (
            isinstance(field_access.type_, (rty.AnyInteger, rty.Enumeration))
            or field_access.type_ == rty.OPAQUE
        ):
            if field in field_access.message_type.parameter_types:
                return [
                    Assignment(
                        Variable(variable_id(target, is_global)),
                        Variable(message_context * field),
                    ),
                ]

            return [
                # TODO(eng/recordflux/RecordFlux#861): Move check into IR
                self._if(
                    Call(
                        message_type * "Valid",
                        [
                            Variable(message_context),
                            Variable(message_type * f"F_{field}"),
                        ],
                    ),
                    [
                        Assignment(
                            Variable(variable_id(target, is_global)),
                            Call(
                                message_type * f"Get_{field}",
                                [Variable(message_context)],
                            ),
                        ),
                    ],
                    f'access to invalid field "{field}" of "{message_context}"',
                    exception_handler,
                ),
            ]

        if isinstance(field_access.type_, rty.Sequence):
            # Eng/RecordFlux/RecordFlux#577
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
            f'unexpected type ({field_access.type_}) for "{field_access}"'
            f' in assignment of "{target}"',
            Subsystem.GENERATOR,
            location=target.location,
        )

    def _assign_to_message_aggregate(
        self,
        target: ID,
        message_aggregate: ir.MsgAgg,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
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

        return [
            CallStatement(
                target_type * "Reset",
                [
                    Variable(target_context),
                ],
                {
                    p: self._to_ada_expr(self._convert_type(v, t), is_global)
                    for p, v, t in parameter_values
                },
            ),
            *self._set_message_fields(
                target_context,
                message_aggregate,
                exception_handler,
                is_global,
            ),
        ]

    def _assign_to_delta_message_aggregate(
        self,
        target: ID,
        delta_message_aggregate: ir.DeltaMsgAgg,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        assert isinstance(delta_message_aggregate.type_, rty.Message)

        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        target_type_id = delta_message_aggregate.type_.identifier
        target_context = context_id(target, is_global)

        fields = list(delta_message_aggregate.field_values)
        first_field = fields[0]
        last_field = fields[-1]

        return [
            # TODO(eng/recordflux/RecordFlux#861): Move check into IR
            self._raise_exception_if(
                Not(
                    Call(
                        target_type_id * "Valid_Next",
                        [
                            Variable(target_context),
                            Variable(target_type_id * model.Field(first_field).affixed_name),
                        ],
                    ),
                ),
                f'trying to set message fields "{first_field}" to "{last_field}" although'
                f' "{first_field}" is not valid next field',
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

    def _assign_to_head(  # noqa: PLR0913
        self,
        target: ID,
        head: ir.Head,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        if not isinstance(head.type_, (rty.Integer, rty.Enumeration, rty.Message)):
            fatal_fail(
                f"unexpected sequence element type {head.type_}"
                f' for "{head}" in assignment of "{target}"',
                Subsystem.GENERATOR,
                location=head.location,
            )

        return self._assign_to_head_sequence(
            target,
            head,
            exception_handler,
            is_global,
            state,
            alloc_id,
        )

    def _assign_to_find(  # noqa: PLR0913
        self,
        target: ID,
        find: ir.Find,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        assert isinstance(find.sequence.type_, rty.Sequence)
        sequence_type_id = find.sequence.type_.identifier
        sequence_element_type = find.sequence.type_.element

        if isinstance(sequence_element_type, rty.Message):
            if isinstance(find.sequence, ir.Var):
                sequence_id = ID(f"{find.sequence}")
                comprehension_sequence_id = copy_id(sequence_id)
            elif isinstance(find.sequence, ir.FieldAccess):
                selected = find.sequence
                sequence_id = ID(
                    f"RFLX_{selected.message}_{selected.field}",
                    location=selected.location,
                )
                comprehension_sequence_id = sequence_id

            else:
                assert False

            def comprehension_statements(
                local_exception_handler: ExceptionHandler,
            ) -> list[Statement]:
                assert isinstance(find.type_, (rty.Integer, rty.Enumeration, rty.Message))
                assert isinstance(
                    sequence_element_type,
                    (rty.Message, rty.Integer, rty.Enumeration),
                )
                default_assignment = []
                if isinstance(find.type_, (rty.Integer, rty.Enumeration)):
                    default_assignment = [Assignment(target, First(find.type_.identifier))]
                return [
                    Declare(
                        [ObjectDeclaration([found_id(target)], "Boolean", FALSE)],
                        [
                            *default_assignment,
                            self._comprehension(
                                comprehension_sequence_id,
                                sequence_type_id,
                                target,
                                find.type_,
                                find.iterator,
                                sequence_element_type.identifier,
                                find.selector.stmts,
                                find.selector.expr,
                                find.condition.stmts,
                                find.condition.expr,
                                local_exception_handler,
                                is_global,
                                state,
                                alloc_id,
                            ),
                            # TODO(eng/recordflux/RecordFlux#861): Move check into IR
                            self._raise_exception_if(
                                Not(Variable(found_id(target))),
                                f'failed to find valid element in "{sequence_id}"',
                                local_exception_handler,
                            ),
                        ],
                    ),
                ]

            if isinstance(find.sequence, ir.Var):
                return [
                    self._declare_sequence_copy(
                        sequence_id,
                        sequence_type_id,
                        comprehension_statements,
                        exception_handler,
                        is_global,
                        alloc_id,
                    ),
                ]
            if isinstance(find.sequence, ir.FieldAccess):
                assert isinstance(selected.message_type, rty.Message)
                message_id = selected.message
                message_type = selected.message_type.identifier
                message_field = selected.field
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
                    ),
                ]
            assert False
        fail(
            f"iterating over sequence of {sequence_element_type}"
            " in list comprehension not yet supported",
            Subsystem.GENERATOR,
            location=find.sequence.location,
        )

    def _assign_to_head_sequence(  # noqa: PLR0913
        self,
        target: ID,
        head: ir.Head,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        assert isinstance(head.prefix_type, rty.Sequence)
        assert isinstance(head.type_, (rty.Integer, rty.Enumeration, rty.Message))

        target_type = head.type_.identifier
        sequence_type = head.prefix_type.identifier
        sequence_id = head.prefix
        sequence_context = context_id(sequence_id, is_global)
        sequence_identifier = ID(f"{head.prefix}")

        if isinstance(head.type_, (rty.Integer, rty.Enumeration)):
            return [
                # TODO(eng/recordflux/RecordFlux#861): Move check into IR
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
                            Size(target_type),
                        ),
                    ),
                    [
                        Assignment(
                            Variable(variable_id(target, is_global)),
                            Call(
                                sequence_type * "Head",
                                [Variable(sequence_context)],
                            ),
                        ),
                    ],
                    f"access to first element of invalid or empty sequence"
                    f' "{sequence_context}"',
                    exception_handler,
                ),
            ]

        assert isinstance(head.type_, rty.Message)

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
                                                            ),
                                                        ],
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
                        ),
                    ],
                    exception_handler.execute(),
                ),
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

    def _assign_to_comprehension(  # noqa: PLR0913
        self,
        target: ID,
        target_type: rty.Sequence,
        comprehension: ir.Comprehension,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
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

            if isinstance(comprehension.sequence, ir.Var):
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
                            comprehension.selector.stmts,
                            comprehension.selector.expr,
                            comprehension.condition.stmts,
                            comprehension.condition.expr,
                            local_exception_handler,
                            is_global,
                            state,
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

            if isinstance(comprehension.sequence, ir.FieldAccess):
                field_access = comprehension.sequence

                assert isinstance(field_access.message_type, rty.Message)

                message_id = ID(field_access.message)
                message_type = field_access.message_type.identifier
                sequence_id = ID(
                    f"RFLX_{field_access.message}_{field_access.field}",
                    location=field_access.location,
                )
                message_field = field_access.field
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
                                        comprehension.selector.stmts,
                                        comprehension.selector.expr,
                                        comprehension.condition.stmts,
                                        comprehension.condition.expr,
                                        local_exception_handler,
                                        is_global,
                                        state,
                                        alloc_id,
                                    ),
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

            assert False

        fail(
            f"iterating over sequence of {sequence_element_type}"
            " in list comprehension not yet supported",
            Subsystem.GENERATOR,
            location=comprehension.sequence.location,
        )

    def _assign_to_call(
        self,
        target: ID,
        call_expr: ir.Call,
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
                ),
            )
            post_call.append(
                # TODO(eng/recordflux/RecordFlux#861): Move check into IR
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
                        ),
                    ],
                    f'"{call_expr.identifier}" returned an invalid message',
                    exception_handler,
                ),
            )

        elif isinstance(call_expr.type_, rty.Structure):
            type_identifier = self._ada_type(call_expr.type_.identifier)
            post_call.append(
                # TODO(eng/recordflux/RecordFlux#861): Move check into IR
                self._raise_exception_if(
                    Not(Call(type_identifier * "Valid_Structure", [Variable(target_id)])),
                    f'"{call_expr.identifier}" returned an invalid message',
                    exception_handler,
                ),
            )

        arguments: list[Expr] = []

        assert len(call_expr.arguments) == len(call_expr.argument_types)

        for i, (a, t) in enumerate(zip(call_expr.arguments, call_expr.argument_types)):
            if not isinstance(
                a,
                (
                    ir.BoolVal,
                    ir.IntVal,
                    ir.Var,
                    ir.EnumLit,
                    ir.FieldAccess,
                    ir.Size,
                    ir.Str,
                    ir.Agg,
                    ir.Opaque,
                ),
            ):
                _unsupported_expression(a, "as function argument")

            if isinstance(a, ir.Var) and isinstance(a.type_, rty.Message):
                type_identifier = self._ada_type(a.type_.identifier)
                local_declarations.append(
                    ObjectDeclaration(
                        [a.identifier],
                        type_identifier * "Structure",
                    ),
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
                arguments.append(self._to_ada_expr(a, is_global))
            elif isinstance(a, ir.FieldAccess) and a.type_ == rty.OPAQUE:
                assert isinstance(a.type_, rty.Sequence)
                argument_name = f"RFLX_{call_expr.identifier}_Arg_{i}_{a.message}"
                argument_length = f"{argument_name}_Length"
                argument = Slice(
                    Variable(argument_name),
                    First(const.TYPES_INDEX),
                    Add(
                        First(const.TYPES_INDEX),
                        Call(
                            const.TYPES_INDEX,
                            [
                                Variable(argument_length),
                            ],
                        ),
                        -Number(2),
                    ),
                )
                type_identifier = self._ada_type(a.message_type.identifier)
                local_declarations.extend(
                    [
                        # Eng/RecordFlux/RecordFlux#917
                        # The use of intermediate buffers should be removed.
                        ObjectDeclaration(
                            [argument_name],
                            Slice(
                                Variable(const.TYPES_BYTES),
                                First(const.TYPES_INDEX),
                                Add(
                                    First(const.TYPES_INDEX),
                                    Number(self._allocator.get_size(a.message, state) - 1),
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
                                                Variable(context_id(a.message, is_global)),
                                                Variable(type_identifier * f"F_{a.field}"),
                                            ],
                                        ),
                                    ],
                                ),
                                Number(1),
                            ),
                            constant=True,
                        ),
                    ],
                )
                pre_call.append(
                    CallStatement(
                        type_identifier * f"Get_{a.field}",
                        [Variable(context_id(a.message, is_global)), argument],
                    ),
                )
                arguments.append(argument)
            elif isinstance(a, ir.Opaque) and isinstance(
                a.prefix_type,
                (rty.Message, rty.Sequence),
            ):
                self._session_context.used_types_body.append(const.TYPES_LENGTH)
                argument_name = f"RFLX_{call_expr.identifier}_Arg_{i}_{a.prefix}"
                argument_length = f"{argument_name}_Length"
                argument = Slice(
                    Variable(argument_name),
                    First(const.TYPES_INDEX),
                    Add(
                        First(const.TYPES_INDEX),
                        Call(
                            const.TYPES_INDEX,
                            [Add(Variable(argument_length), Number(1))],
                        ),
                        -Number(2),
                    ),
                )
                type_identifier = self._ada_type(a.prefix_type.identifier)
                context = context_id(a.prefix, is_global)
                local_declarations.extend(
                    [
                        # Eng/RecordFlux/RecordFlux#917
                        # The use of intermediate buffers should be removed.
                        ObjectDeclaration(
                            [argument_name],
                            Slice(
                                Variable(const.TYPES_BYTES),
                                First(const.TYPES_INDEX),
                                Add(
                                    First(const.TYPES_INDEX),
                                    Number(self._allocator.get_size(a.prefix, state) - 1),
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
                                    Variable(context),
                                ],
                            ),
                            constant=True,
                        ),
                    ],
                )
                pre_call.extend(
                    [
                        # TODO(eng/recordflux/RecordFlux#861): Move check into IR
                        self._raise_exception_if(
                            Not(
                                Call(
                                    type_identifier
                                    * (
                                        "Well_Formed_Message"
                                        if isinstance(a.prefix_type, rty.Message)
                                        else "Valid"
                                    ),
                                    [Variable(context)],
                                ),
                            ),
                            f'invalid "{context}"',
                            exception_handler,
                        ),
                        CallStatement(type_identifier * "Data", [Variable(context), argument]),
                    ],
                )
                arguments.append(argument)
            else:
                arguments.append(
                    self._to_ada_expr(self._convert_type(a, t), is_global),
                )

        call = [
            CallStatement(
                call_expr.identifier,
                [
                    Variable("Ctx"),
                    *arguments,
                    Variable(target_id),
                ],
            ),
        ]

        if local_declarations:
            return [
                Declare(
                    local_declarations,
                    [
                        *pre_call,
                        *call,
                        *post_call,
                    ],
                ),
            ]

        if isinstance(call_expr.type_, rty.Structure):
            return [*call, *post_call]

        return call

    def _assign_to_conversion(
        self,
        target: ID,
        conversion: ir.Conversion,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        assert isinstance(conversion.type_, rty.Message)
        assert isinstance(conversion.argument, ir.FieldAccess), conversion.argument
        assert conversion.argument.type_ == rty.OPAQUE
        assert isinstance(conversion.argument.message_type, rty.Message)

        pdu = conversion.argument.message_type
        sdu = conversion.type_
        field = conversion.argument.field
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
            # TODO(eng/recordflux/RecordFlux#861): Move check into IR
            self._if(
                Call(
                    contains_package
                    * common.contains_function_name(
                        refinement.package,
                        pdu.identifier,
                        sdu.identifier,
                        field,
                    ),
                    [Variable(context_id(conversion.argument.message, is_global))],
                ),
                [
                    CallStatement(
                        contains_package * f"Copy_{field}",
                        [
                            Variable(context_id(conversion.argument.message, is_global)),
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
            ),
        ]

    def _message_field_assign(  # noqa: PLR0913
        self,
        target: ID,
        target_field: ID,
        message_type: rty.Type,
        value: ir.Expr,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        assert isinstance(message_type, rty.Message)

        target_context = context_id(target, is_global)

        return [
            # TODO(eng/recordflux/RecordFlux#861): Move check into IR
            self._raise_exception_if(
                Not(
                    Call(
                        message_type.identifier * "Valid_Next",
                        [
                            Variable(target_context),
                            Variable(message_type.identifier * f"F_{target_field}"),
                        ],
                    ),
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
                    ),
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
        append: ir.Append,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        assert isinstance(append.type_, rty.Sequence)

        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        def check(
            sequence_type: ID,
            required_space: Expr,
            precondition: Optional[Expr] = None,
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
                                ),
                            ],
                        ),
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
                                    ),
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
                                    f' "{sequence_context}"',
                                ),
                                *exception_handler.execute(),
                            ],
                        ),
                    ],
                ),
            ]

        if isinstance(append.type_.element, (rty.Integer, rty.Enumeration)):
            if isinstance(append.expression, (ir.Var, ir.EnumLit, ir.IntVal)):
                sequence_type = append.type_.identifier
                sequence_context = context_id(append.sequence, is_global)
                element_type = append.type_.element.identifier

                return [
                    *check(sequence_type, Size(element_type)),
                    CallStatement(
                        sequence_type * "Append_Element",
                        [
                            Variable(sequence_context),
                            self._to_ada_expr(append.expression, is_global),
                        ],
                    ),
                ]

            _unsupported_expression(append.expression, "in Append statement")

        if isinstance(append.type_.element, rty.Message):
            sequence_type = append.type_.identifier
            sequence_context = context_id(append.sequence, is_global)
            element_type = append.type_.element.identifier
            element_context = context_id("RFLX_Element_" + append.sequence, is_global)

            self._session_context.referenced_types_body.append(element_type)

            if not isinstance(append.expression, ir.MsgAgg):
                _unsupported_expression(append.expression, "in Append statement")

            update_context = self._update_context(sequence_context, element_context, sequence_type)
            local_exception_handler = exception_handler.copy(update_context)

            return [
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
                                append.expression,
                                local_exception_handler,
                                is_global,
                            )
                            if isinstance(append.expression, ir.MsgAgg)
                            else []
                        ),
                        *update_context,
                    ],
                ),
            ]

        fatal_fail(
            f"unexpected element type {append.type_.element} in Append statement",
            Subsystem.GENERATOR,
            location=append.expression.location,
        )

    @staticmethod
    def _read(read: ir.Read, is_global: Callable[[ID], bool]) -> Sequence[Statement]:
        if not isinstance(read.expression, ir.Var) or not isinstance(
            read.expression.type_,
            rty.Message,
        ):
            _unsupported_expression(read.expression, "in Read statement")

        target_type = read.expression.type_.identifier
        target_context = context_id(read.expression.identifier, is_global)
        return [
            CallStatement(target_type * "Verify_Message", [Variable(target_context)]),
        ]

    @staticmethod
    def _write(
        write: ir.Write,
    ) -> Sequence[Statement]:
        if not isinstance(write.expression, ir.Var) or not isinstance(
            write.expression.type_,
            rty.Message,
        ):
            _unsupported_expression(write.expression, "in Write statement")

        return []

    def _check(
        self,
        expression: ir.BoolExpr,
        origin: Optional[ir.Origin],
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        location = f"for {origin.location}" if origin and origin.location else ""
        return [
            self._raise_exception_if(
                Not(self._to_ada_expr(expression, is_global)),
                f"precondition failed{location}",
                exception_handler,
            ),
        ]

    def _reset(
        self,
        reset: ir.Reset,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        assert isinstance(reset.type_, (rty.Message, rty.Sequence))

        target_type = reset.type_.identifier
        target_context = context_id(reset.identifier, is_global)
        return [
            CallStatement(
                target_type * "Reset",
                [Variable(target_context)],
                {n: self._to_ada_expr(e, is_global) for n, e in reset.parameter_values.items()},
            ),
        ]

    @singledispatchmethod
    def _to_ada_expr(self, expression: ir.Expr, is_global: Callable[[ID], bool]) -> Expr:
        raise NotImplementedError(f"{type(expression).__name__} is not yet supported")

    @_to_ada_expr.register
    def _(self, expression: ir.Var, is_global: ty.Callable[[ID], bool]) -> Expr:
        # TODO(eng/recordflux/RecordFlux#1359): Replace typing.Callable by collections.abc.Callable
        return Variable(variable_id(expression.identifier, is_global))

    @_to_ada_expr.register
    def _(self, expression: ir.IntVar, is_global: ty.Callable[[ID], bool]) -> Expr:
        return Variable(variable_id(expression.identifier, is_global), expression.negative)

    @_to_ada_expr.register
    def _(self, expression: ir.EnumLit, _is_global: ty.Callable[[ID], bool]) -> Expr:
        literal = Literal(expression.identifier)

        if expression.type_.always_valid:
            return NamedAggregate(("Known", TRUE), ("Enum", literal))

        return literal

    @_to_ada_expr.register
    def _(self, expression: ir.IntVal, _is_global: ty.Callable[[ID], bool]) -> Expr:
        return Number(expression.value)

    @_to_ada_expr.register
    def _(self, expression: ir.BoolVal, _is_global: ty.Callable[[ID], bool]) -> Expr:
        return Literal(str(expression.value))

    @_to_ada_expr.register
    def _(self, expression: ir.Valid, is_global: ty.Callable[[ID], bool]) -> Expr:
        if isinstance(expression.prefix_type, rty.Message):
            return Call(
                expression.prefix_type.identifier * "Well_Formed_Message",
                [Variable(context_id(expression.prefix, is_global))],
            )

        if isinstance(expression.prefix_type, rty.Structure):
            return Call(
                expression.prefix_type.identifier * "Valid_Structure",
                [Variable(expression.prefix)],
            )

        if isinstance(expression.prefix_type, rty.Sequence):
            return Call(
                expression.prefix_type.identifier * "Valid",
                [Variable(context_id(expression.prefix, is_global))],
            )

        assert False, expression.prefix_type

    def _convert_types_of_int_relation(self, expression: ir.Relation) -> ir.Relation:
        if (
            isinstance(expression.left, ir.IntExpr)
            and isinstance(expression.right, ir.IntExpr)
            and isinstance(expression.left.type_, rty.Integer)
            and isinstance(expression.right.type_, rty.Integer)
            and (
                expression.left.type_ != rty.BASE_INTEGER
                or expression.right.type_ != rty.BASE_INTEGER
            )
        ):
            self._session_context.used_types_body.append(rty.BASE_INTEGER.identifier)
            self._session_context.referenced_types_body.append(rty.BASE_INTEGER.identifier)

            result = expression.__class__(
                ir.IntConversion(
                    self._ada_type(rty.BASE_INTEGER.identifier),
                    expression.left,
                    rty.BASE_INTEGER,
                )
                if expression.left.type_ != rty.BASE_INTEGER
                else expression.left,
                ir.IntConversion(
                    self._ada_type(rty.BASE_INTEGER.identifier),
                    expression.right,
                    rty.BASE_INTEGER,
                )
                if expression.right.type_ != rty.BASE_INTEGER
                else expression.right,
            )
            assert isinstance(result, ir.Relation)
            return result

        self._record_used_types(expression)

        return expression

    def _relation_to_ada_expr(
        self,
        expression: ir.Relation,
        is_global: ty.Callable[[ID], bool],
    ) -> Expr:
        assert isinstance(expression, (ir.Equal, ir.NotEqual))
        if (
            isinstance(expression.left.type_, rty.Enumeration)
            and expression.left.type_.always_valid
        ):
            relation = Equal if isinstance(expression, ir.Equal) else NotEqual

            self._session_context.used_types_body.append(expression.left.type_.identifier)
            return relation(
                self._to_ada_expr(expression.left, is_global),
                self._to_ada_expr(expression.right, is_global),
            )

        result = getattr(ada, expression.__class__.__name__)(
            self._to_ada_expr(expression.left, is_global),
            self._to_ada_expr(expression.right, is_global),
        )
        assert isinstance(result, Expr)
        return result

    @_to_ada_expr.register
    def _(self, expression: ir.Size, is_global: ty.Callable[[ID], bool]) -> Expr:
        if (
            isinstance(expression.prefix_type, rty.AnyInteger)
            or (
                isinstance(expression.prefix_type, rty.Aggregate)
                and isinstance(expression.prefix_type.element, rty.AnyInteger)
            )
            or (
                isinstance(expression.prefix_type, (rty.Integer, rty.Enumeration))
                and expression.prefix == expression.prefix_type.identifier
            )
        ):
            return Size(expression.prefix)

        if (
            isinstance(expression.prefix_type, (rty.Message, rty.Sequence))
            and expression.prefix_type != rty.OPAQUE
        ):
            type_ = expression.prefix_type.identifier
            context = context_id(expression.prefix, is_global)
            return Call(type_ * "Size", [Variable(context)])

        assert False

    @_to_ada_expr.register
    def _(self, expression: ir.HasData, is_global: ty.Callable[[ID], bool]) -> Expr:
        assert isinstance(expression.prefix_type, rty.Message)
        type_ = expression.prefix_type.identifier
        context = context_id(expression.prefix, is_global)
        return Greater(Call(type_ * "Byte_Size", [Variable(context)]), Number(0))

    @_to_ada_expr.register
    def _(self, expression: ir.Opaque, is_global: ty.Callable[[ID], bool]) -> Expr:
        raise NotImplementedError

    @_to_ada_expr.register
    def _(self, expression: ir.Head, _is_global: ty.Callable[[ID], bool]) -> Expr:
        _unsupported_expression(expression, "in expression")

    @_to_ada_expr.register
    def _(self, expression: ir.FieldValid, is_global: ty.Callable[[ID], bool]) -> Expr:
        assert isinstance(expression.message_type, rty.Message)
        type_name = expression.message_type.identifier
        return Call(
            type_name
            * (
                "Valid"
                if isinstance(expression.field_type, (rty.Integer, rty.Enumeration))
                else "Well_Formed"
            ),
            [
                Variable(context_id(expression.message, is_global)),
                Variable(type_name * f"F_{expression.field}"),
            ],
        )

    @_to_ada_expr.register
    def _(self, expression: ir.FieldPresent, is_global: ty.Callable[[ID], bool]) -> Expr:
        assert isinstance(expression.message_type, rty.Message)
        type_name = expression.message_type.identifier
        return Call(
            type_name
            * (
                "Valid"
                if isinstance(expression.field_type, (rty.Integer, rty.Enumeration))
                else "Well_Formed"
            ),
            [
                Variable(context_id(expression.message, is_global)),
                Variable(type_name * f"F_{expression.field}"),
            ],
        )

    @_to_ada_expr.register
    def _(self, expression: ir.FieldSize, is_global: ty.Callable[[ID], bool]) -> Expr:
        type_ = expression.message_type.identifier
        if isinstance(expression.message_type, rty.Message):
            context = context_id(expression.message, is_global)
            return Call(
                type_ * "Field_Size",
                [Variable(context), Variable(type_ * "F_" + expression.field)],
            )

        assert isinstance(expression.message_type, rty.Structure)
        return Call(type_ * f"Field_Size_{expression.field}", [Variable(expression.message)])

    @_to_ada_expr.register
    def _(self, expression: ir.UnaryExpr, is_global: ty.Callable[[ID], bool]) -> Expr:
        result = getattr(ada, expression.__class__.__name__)(
            self._to_ada_expr(expression.expression, is_global),
        )
        assert isinstance(result, Expr)
        return result

    @_to_ada_expr.register
    def _(self, expression: ir.BinaryExpr, is_global: ty.Callable[[ID], bool]) -> Expr:
        self._record_used_types(expression)
        name = expression.__class__.__name__
        if name == "And":
            name = "AndThen"
        elif name == "Or":
            name = "OrElse"
        result = getattr(ada, name)(
            self._to_ada_expr(expression.left, is_global),
            self._to_ada_expr(expression.right, is_global),
        )
        assert isinstance(result, Expr)
        return result

    @_to_ada_expr.register
    def _(self, expression: ir.Relation, is_global: ty.Callable[[ID], bool]) -> Expr:
        relation = self._convert_types_of_int_relation(expression)
        result = getattr(ada, relation.__class__.__name__)(
            self._to_ada_expr(relation.left, is_global),
            self._to_ada_expr(relation.right, is_global),
        )
        assert isinstance(result, Expr)
        return result

    @_to_ada_expr.register
    def _(self, expression: ir.Equal, is_global: ty.Callable[[ID], bool]) -> Expr:
        if expression.left == ir.BoolVal(value=True) and isinstance(expression.right, ir.Var):
            return Variable(variable_id(expression.right.identifier, is_global))
        if isinstance(expression.left, ir.Var) and expression.right == ir.BoolVal(value=True):
            return Variable(variable_id(expression.left.identifier, is_global))
        if expression.left == ir.BoolVal(value=False) and isinstance(expression.right, ir.Var):
            return Not(Variable(variable_id(expression.right.identifier, is_global)))
        if isinstance(expression.left, ir.Var) and expression.right == ir.BoolVal(value=False):
            return Not(Variable(variable_id(expression.left.identifier, is_global)))
        return self._relation_to_ada_expr(
            self._convert_types_of_int_relation(expression),
            is_global,
        )

    @_to_ada_expr.register
    def _(self, expression: ir.NotEqual, is_global: ty.Callable[[ID], bool]) -> Expr:
        if expression.left == ir.BoolVal(value=True) and isinstance(expression.right, ir.Var):
            return Not(Variable(variable_id(expression.right.identifier, is_global)))
        if isinstance(expression.left, ir.Var) and expression.right == ir.BoolVal(value=True):
            return Not(Variable(variable_id(expression.left.identifier, is_global)))
        if expression.left == ir.BoolVal(value=False) and isinstance(expression.right, ir.Var):
            return Variable(variable_id(expression.right.identifier, is_global))
        if isinstance(expression.left, ir.Var) and expression.right == ir.BoolVal(value=False):
            return Variable(variable_id(expression.left.identifier, is_global))
        return self._relation_to_ada_expr(
            self._convert_types_of_int_relation(expression),
            is_global,
        )

    @_to_ada_expr.register
    def _(self, expression: ir.Call, is_global: ty.Callable[[ID], bool]) -> Expr:
        raise NotImplementedError

    @_to_ada_expr.register
    def _(self, expression: ir.FieldAccess, is_global: ty.Callable[[ID], bool]) -> Expr:
        if expression.field in expression.message_type.parameter_types:
            return Selected(Variable(context_id(expression.message, is_global)), expression.field)
        if isinstance(expression.message_type, rty.Structure):
            raise NotImplementedError
        return Call(
            expression.message_type.identifier * f"Get_{expression.field}",
            [Variable(context_id(expression.message, is_global))],
        )

    @_to_ada_expr.register
    def _(self, expression: ir.IntFieldAccess, is_global: ty.Callable[[ID], bool]) -> Expr:
        if expression.field in expression.message_type.parameter_types:
            return Selected(
                Variable(context_id(expression.message, is_global)),
                expression.field,
                expression.negative,
            )
        if isinstance(expression.message_type, rty.Structure):
            return Selected(Variable(expression.message), expression.field, expression.negative)
        return Call(
            expression.message_type.identifier * f"Get_{expression.field}",
            [Variable(context_id(expression.message, is_global))],
            negative=expression.negative,
        )

    @_to_ada_expr.register
    def _(self, expression: ir.IfExpr, is_global: ty.Callable[[ID], bool]) -> Expr:
        assert expression.then_expr.is_expr()
        assert expression.else_expr.is_expr()
        return ada.If(
            [
                (
                    self._to_ada_expr(expression.condition, is_global),
                    self._to_ada_expr(expression.then_expr.expr, is_global),
                ),
            ],
            self._to_ada_expr(expression.else_expr.expr, is_global),
        )

    @_to_ada_expr.register
    def _(self, expression: ir.Conversion, is_global: ty.Callable[[ID], bool]) -> Expr:
        return Conversion(expression.identifier, self._to_ada_expr(expression.argument, is_global))

    @_to_ada_expr.register
    def _(self, expression: ir.Agg, is_global: ty.Callable[[ID], bool]) -> Expr:
        assert len(expression.elements) > 0
        if len(expression.elements) == 1:
            return NamedAggregate(
                (
                    First(const.TYPES_INDEX),
                    Val(const.TYPES_BYTE, self._to_ada_expr(expression.elements[0], is_global)),
                ),
            )
        return Aggregate(
            *[Val(const.TYPES_BYTE, self._to_ada_expr(e, is_global)) for e in expression.elements],
        )

    @_to_ada_expr.register
    def _(self, expression: ir.NamedAgg, is_global: ty.Callable[[ID], bool]) -> Expr:
        elements: list[tuple[Union[ID, ada.Expr], ada.Expr]] = [
            (
                n if isinstance(n, ID) else self._to_ada_expr(n, is_global),
                self._to_ada_expr(e, is_global),
            )
            for n, e in expression.elements
        ]
        return NamedAggregate(*elements)

    @_to_ada_expr.register
    def _(self, expression: ir.Str, is_global: ty.Callable[[ID], bool]) -> Expr:
        raise NotImplementedError

    @_to_ada_expr.register
    def _(self, expression: ir.CaseExpr, is_global: ty.Callable[[ID], bool]) -> Expr:
        choices = [
            (self._to_ada_expr(choice, is_global), self._to_ada_expr(expr, is_global))
            for choices, expr in expression.choices
            for choice in choices
        ]
        return ada.CaseExpr(self._to_ada_expr(expression.expression, is_global), choices)

    def _record_used_types(self, expression: ir.BinaryExpr) -> None:
        for e in [expression.left, expression.right]:
            if isinstance(e.type_, rty.Integer) or (
                isinstance(e.type_, rty.Enumeration) and not e.type_.always_valid
            ):
                self._session_context.used_types_body.append(e.type_.identifier)
                self._session_context.referenced_types_body.append(e.type_.identifier)

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
        # TODO(eng/recordflux/RecordFlux#861): Move check into IR
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
        # TODO(eng/recordflux/RecordFlux#861): Move check into IR
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
        # TODO(eng/recordflux/RecordFlux#861): Move check into IR
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

    def _if_sufficient_space_in_sequence(
        self,
        required_space: Expr,
        sequence_type: ID,
        sequence_context: ID,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        # TODO(eng/recordflux/RecordFlux#861): Move check into IR
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
                    ),
                ],
                [
                    *self._debug_output(f"Error: {error_message}"),
                    *exception_handler.execute(),
                ],
            ),
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
            ],
        )

    def _set_message_fields(
        self,
        target_context: ID,
        message_aggregate: ir.MsgAgg,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        assert isinstance(message_aggregate.type_, rty.Message)

        message_type = message_aggregate.type_

        statements: list[Statement] = []

        for f, v in message_aggregate.field_values.items():
            if f not in message_type.field_types:
                continue

            statements.extend(
                self._set_message_field(
                    target_context,
                    f,
                    message_type,
                    v,
                    exception_handler,
                    is_global,
                ),
            )

        return statements

    def _set_message_field(  # noqa: PLR0913, PLR0912, PLR0915
        self,
        message_context: ID,
        field: ID,
        message_type: rty.Message,
        value: ir.Expr,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
    ) -> Sequence[Statement]:
        if isinstance(value, ir.FieldAccess) and value.type_ == rty.OPAQUE:
            target_context = message_context
            target_message_type = message_type
            target_field = field
            source_context = context_id(value.message, is_global)
            source_field = value.field
            source_message_type = value.message_type
            self._session_context.used_types_body.append(const.TYPES_LENGTH)

            if isinstance(source_message_type, rty.Message):
                return [
                    self._set_opaque_field_to_message_field(
                        target_message_type.identifier,
                        target_context,
                        target_field,
                        source_message_type.identifier,
                        source_context,
                        source_field,
                    ),
                ]

            assert isinstance(source_message_type, rty.Structure)
            return [
                self._set_opaque_field_to_message_field_from_structure(
                    target_message_type.identifier,
                    target_context,
                    target_field,
                    source_message_type.identifier,
                    value.message,
                    source_field,
                ),
            ]

        if isinstance(value, ir.Opaque):
            target_context = message_context
            target_message_type = message_type
            target_field = field
            source_context = context_id(value.prefix, is_global)
            assert isinstance(value.prefix_type, rty.Message)
            source_message_type = value.prefix_type
            return [
                self._set_opaque_field_to_message(
                    target_message_type.identifier,
                    target_context,
                    target_field,
                    source_message_type.identifier,
                    source_context,
                ),
            ]

        message_type_id = message_type.identifier
        field_type = message_type.field_types[field]
        statements: list[Statement] = []
        result = statements

        if isinstance(field_type, rty.Sequence):
            size: Expr
            if isinstance(value, ir.Var) and isinstance(value.type_, (rty.Message, rty.Sequence)):
                type_ = value.type_.identifier
                context = context_id(value.identifier, is_global)
                # TODO(eng/recordflux/RecordFlux#861): Move check into IR
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
                if isinstance(value, ir.Agg):
                    size = Mul(Number(len(value.elements)), Size(const.TYPES_BYTE))
                else:
                    size = Size(self._to_ada_expr(value, is_global))

                # TODO(eng/recordflux/RecordFlux#861): Move check into IR
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
                ),
            ],
        )

        if isinstance(value, (ir.IntVal, ir.BoolVal, ir.FieldAccess, ir.Agg, ir.CaseExpr)) or (
            isinstance(
                value,
                (ir.Var, ir.EnumLit, ir.BinaryIntExpr, ir.Size),
            )
            and isinstance(value.type_, (rty.AnyInteger, rty.Enumeration, rty.Aggregate))
        ):
            if isinstance(value, ir.Agg) and len(value.elements) == 0:
                statements.append(
                    CallStatement(
                        message_type_id * f"Set_{field}_Empty",
                        [Variable(message_context)],
                    ),
                )
            else:
                ada_value: ada.Expr
                if (
                    isinstance(value, ir.Var)
                    and isinstance(value.type_, rty.Enumeration)
                    and value.type_.always_valid
                ):
                    ada_value = Selected(self._to_ada_expr(value, is_global), "Enum")
                elif isinstance(value, ir.EnumLit):
                    ada_value = Literal(value.identifier)
                else:
                    ada_value = self._to_ada_expr(self._convert_type(value, field_type), is_global)
                statements.extend(
                    [
                        assert_sufficient_space,
                        CallStatement(
                            message_type_id * f"Set_{field}",
                            [
                                Variable(message_context),
                                ada_value,
                            ],
                        ),
                    ],
                )
        elif isinstance(value, ir.Var) and isinstance(value.type_, rty.Sequence):
            sequence_context = context_id(value.identifier, is_global)
            statements.extend(
                [
                    assert_sufficient_space,
                    CallStatement(
                        message_type_id * f"Set_{field}",
                        [Variable(message_context), Variable(sequence_context)],
                    ),
                ],
            )
        elif isinstance(value, ir.Var) and isinstance(value.type_, rty.Message):
            _unsupported_expression(value, "in message aggregate")
        else:
            _unsupported_expression(value, "as value of message field")

        return result

    @staticmethod
    def _set_opaque_field(  # noqa: PLR0913
        target_type: ID,
        target_context: ID,
        field: ID,
        get_preconditions: Expr,
        get_statements: Sequence[Statement],
        length: Expr,
        pre_declarations: Optional[Sequence[Declaration]] = None,
        post_statements: Optional[Sequence[Statement]] = None,
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

    def _set_opaque_field_to_message_field(  # noqa: PLR0913
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
                            ),
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
                    ),
                ],
            ),
            post_statements=[Assignment(message_context, Variable(temporary_message_context))],
        )

    def _set_opaque_field_to_message_field_from_structure(  # noqa: PLR0913
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
                                First(f"{structure}.{structure_field}"),
                                Length("Data"),
                                -Number(1),
                            ),
                        ),
                    ),
                ),
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

    def _declare_sequence_copy(  # noqa: PLR0913
        self,
        sequence_identifier: ID,
        sequence_type: ID,
        statements: Callable[[ExceptionHandler], Sequence[Statement]],
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        alloc_id: Optional[Location],
    ) -> IfStatement:
        # Eng/RecordFlux/RecordFlux#577
        sequence_context = context_id(sequence_identifier, is_global)
        take_buffer = self._take_buffer(copy_id(sequence_identifier), sequence_type, is_global)
        free_buffer = self._free_buffer(copy_id(sequence_identifier), alloc_id)

        return self._if_valid_sequence(
            sequence_type,
            sequence_context,
            [
                Declare(
                    self._declare_context_buffer(
                        copy_id(sequence_identifier),
                        sequence_type,
                        is_global,
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
                ),
            ],
            exception_handler,
        )

    def _declare_message_field_sequence_copy(  # noqa: PLR0913
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
        # Eng/RecordFlux/RecordFlux#577
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
                                        message_type * model.Field(message_field).affixed_name,
                                    ),
                                ],
                            ),
                            last=Call(
                                message_type * "Field_Last",
                                [
                                    Variable(context_id(message_identifier, is_global)),
                                    Variable(
                                        message_type * model.Field(message_field).affixed_name,
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

    def _comprehension(  # noqa: PLR0913
        self,
        sequence_identifier: ID,
        sequence_type: ID,
        target_identifier: ID,
        target_type: Union[rty.Sequence, rty.Integer, rty.Enumeration, rty.Message],
        iterator_identifier: ID,
        iterator_type: ID,
        selector_stmts: list[ir.Stmt],
        selector: ir.Expr,
        condition_stmts: list[ir.Stmt],
        condition: ir.Expr,
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
        alloc_id: Optional[Location],
    ) -> While:
        assert not isinstance(selector, ir.MsgAgg)

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
                            Variable(context_id(sequence_identifier, is_global)),
                            "Buffer_First",
                        ),
                        LoopEntry(
                            Selected(
                                Variable(context_id(sequence_identifier, is_global)),
                                "Buffer_First",
                            ),
                        ),
                    ),
                ],
            ),
            PragmaStatement(
                "Loop_Invariant",
                [
                    Equal(
                        Selected(
                            Variable(context_id(sequence_identifier, is_global)),
                            "Buffer_Last",
                        ),
                        LoopEntry(
                            Selected(
                                Variable(context_id(sequence_identifier, is_global)),
                                "Buffer_Last",
                            ),
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
                                Variable(context_id(target_identifier, is_global)),
                                "Buffer_First",
                            ),
                            LoopEntry(
                                Selected(
                                    Variable(context_id(target_identifier, is_global)),
                                    "Buffer_First",
                                ),
                            ),
                        ),
                    ],
                ),
                PragmaStatement(
                    "Loop_Invariant",
                    [
                        Equal(
                            Selected(
                                Variable(context_id(target_identifier, is_global)),
                                "Buffer_Last",
                            ),
                            LoopEntry(
                                Selected(
                                    Variable(context_id(target_identifier, is_global)),
                                    "Buffer_Last",
                                ),
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
                        ),
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
                        ),
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
                        ),
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
                        *[
                            a
                            for s in condition_stmts
                            for a in self._state_action(
                                state,
                                s,
                                local_exception_handler,
                                is_global,
                            )
                        ],
                        IfStatement(
                            [
                                (
                                    self._to_ada_expr(condition, is_global),
                                    self._comprehension_append_element(
                                        target_identifier,
                                        target_type,
                                        selector_stmts,
                                        selector,
                                        update_context,
                                        local_exception_handler,
                                        is_global,
                                        state,
                                    )
                                    if isinstance(target_type, rty.Sequence)
                                    else self._comprehension_assign_element(
                                        target_identifier,
                                        target_type,
                                        selector_stmts,
                                        selector,
                                        update_context,
                                        local_exception_handler,
                                        is_global,
                                        state,
                                    ),
                                ),
                            ],
                        ),
                        *update_context,
                    ],
                ),
            ],
        )

    def _comprehension_assign_element(  # noqa: PLR0913
        self,
        target_identifier: ID,
        target_type: Union[rty.Integer, rty.Enumeration, rty.Message],
        selector_stmts: list[ir.Stmt],
        selector: ir.Expr,
        update_context: Sequence[Statement],
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
    ) -> Sequence[Statement]:
        target_type_id = target_type.identifier
        assign_element: Sequence[Statement]

        if isinstance(target_type, rty.Message):
            if not isinstance(selector, ir.Var):
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
                ),
            ]

        elif isinstance(target_type, (rty.Integer, rty.Enumeration)):
            assign_element = [
                Assignment(
                    Variable(variable_id(target_identifier, is_global)),
                    self._to_ada_expr(selector, is_global),
                ),
            ]
        else:
            assert False

        return [
            *[
                a
                for s in selector_stmts
                for a in self._state_action(state, s, exception_handler, is_global)
            ],
            *assign_element,
            Assignment(Variable(found_id(target_identifier)), TRUE),
            *update_context,
            ExitStatement(),
        ]

    def _comprehension_append_element(  # noqa: PLR0913
        self,
        target_identifier: ID,
        target_type: rty.Sequence,
        selector_stmts: list[ir.Stmt],
        selector: ir.Expr,
        _: Sequence[Statement],
        exception_handler: ExceptionHandler,
        is_global: Callable[[ID], bool],
        state: ID,
    ) -> Sequence[Statement]:
        assert isinstance(target_type, rty.Sequence)

        target_type_id = target_type.identifier
        required_space: Expr
        append_element: Statement

        if isinstance(target_type.element, rty.Message):
            if not isinstance(selector, ir.Var):
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
                    # TODO(eng/recordflux/RecordFlux#861): Move check into IR
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
                            ),
                        ],
                        "empty messages cannot be appended to sequence",
                        exception_handler,
                    ),
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
                else target_type.element.identifier,
            )
            append_element = CallStatement(
                target_type_id * "Append_Element",
                [
                    Variable(context_id(target_identifier, is_global)),
                    self._to_ada_expr(selector, is_global),
                ],
            )

        else:
            assert False

        return [
            *[
                a
                for s in selector_stmts
                for a in self._state_action(state, s, exception_handler, is_global)
            ],
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
                "Assert",
                [NotEqual(Variable(buffer_id(identifier)), Variable("null"))],
            ),
            Assignment(
                slot,
                Variable(buffer_id(identifier)),
            ),
            PragmaStatement("Assert", [NotEqual(slot, Variable("null"))]),
        ]

    @staticmethod
    def _take_buffer(
        identifier: ID,
        type_: ID,
        is_global: Callable[[ID], bool],
        buf: Optional[ID] = None,
    ) -> Sequence[Statement]:
        context = context_id(identifier, is_global)
        buf = buf or buffer_id(identifier)
        return [
            # Eng/RecordFlux/Workarounds#32
            PragmaStatement(
                "Warnings",
                [
                    Variable("Off"),
                    String(
                        f'"{context.ada_str}" is set by "Take_Buffer" but not used after the call',
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
                        f'"{context.ada_str}" is set by "Take_Buffer" but not used after the call',
                    ),
                ],
            ),
        ]

    @staticmethod
    def _update_context(
        sequence_context: ID,
        element_context: ID,
        sequence_type: ID,
    ) -> Sequence[Statement]:
        return [
            # Eng/RecordFlux/Workarounds#32
            PragmaStatement(
                "Warnings",
                [
                    Variable("Off"),
                    String(
                        f'"{element_context.ada_str}" is set by "Update" '
                        f"but not used after the call",
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
                        f" but not used after the call",
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
    def _initialize_context(  # noqa: PLR0913
        identifier: ID,
        type_: ID,
        is_global: Callable[[ID], bool],
        first: Optional[Expr] = None,
        last: Optional[Expr] = None,
        parameters: Optional[Mapping[ID, Expr]] = None,
        written_last: Optional[Expr] = None,
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
                                    ),
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
                    ),
                ],
                exception_handler.execute(),
            )

        return copy

    def _convert_type(
        self,
        expression: ir.Expr,
        target_type: rty.Type,
    ) -> ir.Expr:
        if target_type.is_compatible_strong(expression.type_):
            return expression

        assert isinstance(target_type, (rty.Integer, rty.Enumeration)), target_type

        self._session_context.referenced_types_body.append(target_type.identifier)

        if isinstance(expression, ir.BinaryIntExpr):
            assert isinstance(target_type, rty.Integer)
            return expression.__class__(
                ir.IntConversion(
                    self._ada_type(target_type.identifier),
                    expression.left,
                    target_type,
                ),
                ir.IntConversion(
                    self._ada_type(target_type.identifier),
                    expression.right,
                    target_type,
                ),
            )

        return ir.Conversion(self._ada_type(target_type.identifier), expression, target_type)

    def _debug_output(self, string: str) -> Sequence[CallStatement]:
        return (
            [
                CallStatement(
                    self._prefix * ID("RFLX_Debug.Print")
                    if self._debug == common.Debug.EXTERNAL
                    else "Ada.Text_IO.Put_Line",
                    [String(string)],
                ),
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


def _unexpected_expression(expression: ir.Expr, context: str) -> NoReturn:
    fatal_fail(
        f'unexpected expression "{type(expression).__name__}" with {expression.type_} {context}',
        Subsystem.GENERATOR,
        location=expression.location,
    )


def _unsupported_expression(expression: ir.Expr, context: str) -> NoReturn:
    fail(
        f"{type(expression).__name__} with {expression.type_} {context} not yet supported",
        Subsystem.GENERATOR,
        location=expression.location,
    )

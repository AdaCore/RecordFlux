# pylint: disable = too-many-lines

from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass, field as dataclass_field
from typing import (
    Callable,
    Generator,
    Iterable,
    List,
    Mapping,
    NoReturn,
    Optional,
    Sequence,
    Set,
    Tuple,
)

import attr

from rflx import expression as expr, identifier as rid, model, typing_ as rty
from rflx.ada import (
    FALSE,
    ID,
    TRUE,
    Add,
    And,
    AndThen,
    Assignment,
    Call,
    CallStatement,
    Case,
    CaseStatement,
    ChoiceList,
    ContextItem,
    Declaration,
    Declare,
    EnumerationType,
    Equal,
    ExitStatement,
    Expr,
    ExpressionFunctionDeclaration,
    First,
    FormalDeclaration,
    FormalSubprogramDeclaration,
    FunctionSpecification,
    GenericProcedureInstantiation,
    Greater,
    GreaterEqual,
    IfStatement,
    In,
    Indexed,
    Initialized,
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
    ProcedureSpecification,
    Raise,
    RaiseStatement,
    RelaxedInitialization,
    ReturnStatement,
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
from rflx.const import BUILTINS_PACKAGE, INTERNAL_PACKAGE
from rflx.error import Location, Subsystem, fail, fatal_fail
from rflx.model import declaration as decl, statement as stmt

from . import const
from .allocator import AllocatorGenerator


@dataclass
class SessionContext:
    referenced_types: List[rid.ID] = dataclass_field(default_factory=list)
    referenced_types_body: List[rid.ID] = dataclass_field(default_factory=list)
    referenced_packages_body: List[rid.ID] = dataclass_field(default_factory=list)
    used_types: List[rid.ID] = dataclass_field(default_factory=list)
    used_types_body: List[rid.ID] = dataclass_field(default_factory=list)
    state_exception: Set[rid.ID] = dataclass_field(default_factory=set)


@dataclass
class EvaluatedDeclaration:
    global_declarations: List[Declaration] = dataclass_field(default_factory=list)
    initialization_declarations: List[Declaration] = dataclass_field(default_factory=list)
    initialization: List[Statement] = dataclass_field(default_factory=list)
    finalization: List[Statement] = dataclass_field(default_factory=list)

    def __iadd__(self, other: object) -> "EvaluatedDeclaration":
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
    session_context_state_exception: Set[rid.ID] = attr.ib()
    state: model.State = attr.ib()
    finalization: Sequence[Statement] = attr.ib()
    inside_declare: bool = attr.ib(False)
    raised_deferred_exception: bool = attr.ib(False)

    def execute(self) -> Sequence[Statement]:
        if self.inside_declare:
            # ISSUE: Componolit/Workarounds#569
            self.session_context_state_exception.add(self.state.identifier)
            self.raised_deferred_exception = True
            return [Assignment("RFLX_Exception", TRUE)]

        assert (
            self.state.exception_transition
        ), f'missing exception transition for state "{self.state.identifier}"'
        return [
            Assignment(
                "P_Next_State",
                Variable(f"S_{self.state.exception_transition.target}"),
            ),
            *self.finalization,
            ReturnStatement(),
        ]

    def execute_deferred(self) -> Sequence[Statement]:
        # ISSUE: Componolit/Workarounds#569
        if self.raised_deferred_exception and not self.inside_declare:
            self.raised_deferred_exception = False
            return [
                IfStatement(
                    [
                        (
                            Variable("RFLX_Exception"),
                            self.execute(),
                        )
                    ]
                )
            ]

        return []

    @contextmanager
    def local(self) -> Generator["ExceptionHandler", None, None]:
        """
        Return an exception handler which can be used inside declare blocks.

        This is needed because SPARK does not yet support return statements in the scope of a local
        owning declaration.
        """
        # ISSUE: Componolit/Workarounds#569

        local_exception_handler = ExceptionHandler(
            self.session_context_state_exception, self.state, self.finalization, inside_declare=True
        )
        yield local_exception_handler
        self.raised_deferred_exception |= local_exception_handler.raised_deferred_exception


@dataclass
class ChannelAccess:
    state: rid.ID
    message: ID
    message_type: ID


class SessionGenerator:  # pylint: disable = too-many-instance-attributes
    def __init__(
        self,
        session: model.Session,
        allocator: AllocatorGenerator,
        prefix: str = "",
        debug: bool = False,
    ) -> None:
        self._session = session
        self._prefix = prefix
        self._debug = debug
        self._allocator = allocator

        self._session_context = SessionContext()
        self._declaration_context: List[ContextItem] = []
        self._body_context: List[ContextItem] = []
        self._formal_parameters: List[FormalDeclaration] = []
        self._unit_part = UnitPart()

        self._create()

    @property
    def unit_identifier(self) -> ID:
        return ID(self._session.identifier)

    @property
    def declaration_context(self) -> List[ContextItem]:
        return self._declaration_context

    @property
    def body_context(self) -> List[ContextItem]:
        return self._body_context

    @property
    def formal_parameters(self) -> List[FormalDeclaration]:
        return self._formal_parameters

    @property
    def unit_part(self) -> UnitPart:
        return self._unit_part

    def _ada_type(self, identifier: rid.ID) -> ID:
        if model.is_builtin_type(identifier):
            return ID(identifier.name)

        return ID(model.qualified_type_identifier(identifier, self._session.package))

    def _model_type(self, identifier: rid.ID) -> model.Type:
        return self._session.types[
            model.qualified_type_identifier(identifier, self._session.package)
        ]

    def _create(self) -> None:
        self._unit_part = self._create_state_machine()
        self._formal_parameters = self._create_formal_parameters(self._session.parameters.values())
        self._declaration_context, self._body_context = self._create_context()

    def _create_formal_parameters(
        self,
        parameters: Iterable[decl.FormalDeclaration],
    ) -> List[FormalDeclaration]:
        result: List[FormalDeclaration] = []

        for parameter in parameters:
            if isinstance(parameter, decl.ChannelDeclaration):
                pass
            elif isinstance(parameter, decl.FunctionDeclaration):
                result.extend(self._create_function_parameters(parameter))
            elif isinstance(parameter, decl.TypeDeclaration):
                fail(
                    f'type declaration "{parameter.identifier}" not yet supported',
                    Subsystem.GENERATOR,
                    location=parameter.location,
                )
            else:
                fatal_fail(
                    f'unexpected formal parameter "{parameter.identifier}"',
                    Subsystem.GENERATOR,
                    location=parameter.location,
                )

        return result

    def _create_function_parameters(
        self, function: decl.FunctionDeclaration
    ) -> List[FormalDeclaration]:
        procedure_parameters: List[Parameter] = []

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

        procedure_parameters.append(
            OutParameter(
                [ID(function.identifier)],
                ID(self._prefix * function.return_type * "Structure")
                if isinstance(function.type_, rty.Message)
                else ID(self._prefix * function.return_type),
            )
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
                    [ID(a.identifier)],
                    ID(const.TYPES_BYTES)
                    if a.type_ == rty.OPAQUE
                    else ID(self._prefix * a.type_identifier * "Structure")
                    if isinstance(a.type_, rty.Message)
                    else ID(self._prefix * a.type_identifier),
                )
            )

            assert isinstance(a.type_, (rty.Integer, rty.Enumeration, rty.Message, rty.Sequence))

            self._session_context.referenced_types.append(a.type_.identifier)

        return [
            FormalSubprogramDeclaration(
                ProcedureSpecification(
                    ID(function.identifier),
                    procedure_parameters,
                )
            )
        ]

    def _create_context(self) -> Tuple[List[ContextItem], List[ContextItem]]:
        declaration_context: List[ContextItem] = []

        declaration_context.append(WithClause(self._prefix * self._allocator.unit_identifier))
        if self._session_context.used_types or self._session_context.used_types_body:
            declaration_context.append(WithClause(self._prefix * const.TYPES_PACKAGE))

        body_context: List[ContextItem] = [
            *([WithClause("Ada.Text_IO")] if self._debug else []),
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
                        WithClause(self._prefix * ID(type_.package)),
                        *(
                            [
                                WithClause(self._prefix * ID(type_.identifier)),
                            ]
                            if isinstance(type_, (model.Message, model.Sequence))
                            else []
                        ),
                    ]
                )

        body_context.extend(
            [
                WithClause(self._prefix * ID(p))
                for p in self._session_context.referenced_packages_body
            ],
        )

        for type_identifier in self._session_context.used_types_body:
            if type_identifier.parent in [INTERNAL_PACKAGE, BUILTINS_PACKAGE]:
                continue
            if type_identifier in self._session_context.used_types:
                continue
            if type_identifier in [const.TYPES_LENGTH, const.TYPES_INDEX, const.TYPES_BIT_LENGTH]:
                body_context.append(
                    WithClause(self._prefix * const.TYPES_PACKAGE),
                )
            body_context.append(
                UseTypeClause(self._prefix * ID(type_identifier)),
            )

        use_type_clauses = {i for i in body_context if isinstance(i, UseTypeClause)}
        body_context = [
            i
            for i in body_context
            if isinstance(i, UseTypeClause)
            or (
                isinstance(i, WithClause)
                and (
                    i not in declaration_context
                    or any(utc.identifier.parent == i.identifier for utc in use_type_clauses)
                )
            )
        ]

        return (declaration_context, body_context)

    def _create_state_machine(self) -> UnitPart:
        evaluated_declarations = self._evaluate_declarations(
            self._session.declarations.values(),
            session_global=True,
        )

        evaluated_declarations.initialization.insert(
            0, CallStatement(ID(self._allocator.unit_identifier * "Initialize"))
        )

        unit = UnitPart()
        unit += self._create_uninitialized_function(self._session.declarations.values())
        unit += self._create_states(self._session)
        unit += self._create_initialized_function(self._session)
        unit += self._create_active_function(self._session)
        unit += self._create_initialize_procedure(
            self._session,
            evaluated_declarations.initialization_declarations,
            evaluated_declarations.initialization,
        )
        unit += self._create_finalize_procedure(
            self._session,
            evaluated_declarations.initialization_declarations,
            evaluated_declarations.finalization,
        )

        channel_reads = self._channel_io(self._session, read=True)
        channel_writes = self._channel_io(self._session, write=True)
        has_reads = bool([read for reads in channel_reads.values() for read in reads])
        has_writes = bool([write for writes in channel_writes.values() for write in writes])

        if has_reads:
            unit += self._create_reset_messages_before_write_procedure(self._session)

        unit += self._create_tick_procedure(self._session, has_reads)
        unit += self._create_run_procedure(self._session)
        unit += self._create_state_function()

        if has_writes:
            unit += self._create_has_data_function(channel_writes)
            unit += self._create_read_buffer_size_function(channel_writes)
            unit += self._create_read_procedure(channel_writes)

        if has_reads:
            unit += self._create_needs_data_function(channel_reads)
            unit += self._create_write_buffer_size_function(channel_reads)
            unit += self._create_write_procedure(channel_reads)

        return (
            self._create_declarations(self._session, evaluated_declarations.global_declarations)
            + unit
        )

    @staticmethod
    def _channel_io(
        session: model.Session, read: bool = False, write: bool = False
    ) -> dict[rid.ID, list[ChannelAccess]]:
        assert (read and not write) or (not read and write)

        channels: dict[rid.ID, list[ChannelAccess]] = {
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
                            ID(action.parameter.identifier),
                            ID(action.parameter.type_.identifier),
                        )
                    )

        return channels

    def _create_declarations(
        self, session: model.Session, declarations: Sequence[Declaration]
    ) -> UnitPart:
        return UnitPart(
            [
                *[
                    UseTypeClause(self._prefix * ID(t))
                    for t in self._session_context.used_types
                    if not model.is_builtin_type(t) and not model.is_internal_type(t)
                ],
                EnumerationType(
                    "Channel",
                    {
                        ID(f"C_{parameter.identifier}"): None
                        for parameter in session.parameters.values()
                        if isinstance(parameter, decl.ChannelDeclaration)
                    },
                ),
                EnumerationType("State", {ID(f"S_{s.identifier}"): None for s in session.states}),
            ],
            private=[
                ObjectDeclaration(["P_Next_State"], "State", Variable(f"S_{session.initial}")),
                *declarations,
            ],
        )

    @staticmethod
    def _create_uninitialized_function(declarations: Iterable[decl.BasicDeclaration]) -> UnitPart:
        specification = FunctionSpecification("Uninitialized", "Boolean")
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
                                    ID(declaration.type_.identifier * "Has_Buffer"),
                                    [Variable(context_id(declaration.identifier))],
                                )
                            )
                            for declaration in declarations
                            if isinstance(declaration, decl.VariableDeclaration)
                            and isinstance(declaration.type_, (rty.Message, rty.Sequence))
                        ]
                    ),
                ),
            ],
        )

    def _create_states(self, session: model.Session) -> UnitPart:
        unit_body: List[Declaration] = []

        for state in session.states:
            if not state.is_null:
                evaluated_declarations = self._evaluate_declarations(state.declarations.values())
                actions = [
                    s
                    for a in state.actions
                    for s in self._state_action(
                        state.identifier,
                        a,
                        ExceptionHandler(
                            self._session_context.state_exception,
                            state,
                            evaluated_declarations.finalization,
                        ),
                    )
                ]
                if state.identifier in self._session_context.state_exception:
                    evaluated_declarations.global_declarations.append(
                        ObjectDeclaration(["RFLX_Exception"], "Boolean", FALSE)
                    )

                unit_body += [
                    SubprogramBody(
                        ProcedureSpecification(
                            ID(state.identifier), [OutParameter(["P_Next_State"], "State")]
                        ),
                        [
                            *evaluated_declarations.global_declarations,
                            *evaluated_declarations.initialization_declarations,
                        ],
                        [
                            *evaluated_declarations.initialization,
                            *actions,
                            *(
                                [
                                    IfStatement(
                                        [
                                            (
                                                t.condition.substituted(
                                                    self._substitution()
                                                ).ada_expr(),
                                                [
                                                    Assignment(
                                                        "P_Next_State", Variable(f"S_{t.target}")
                                                    )
                                                ],
                                            )
                                            for t in state.transitions[:-1]
                                        ],
                                        [
                                            Assignment(
                                                "P_Next_State",
                                                Variable(f"S_{state.transitions[-1].target}"),
                                            )
                                        ],
                                    )
                                ]
                                if state.transitions
                                else []
                            ),
                            *evaluated_declarations.finalization,
                        ],
                        aspects=[
                            Precondition(Call("Initialized")),
                            Postcondition(Call("Initialized")),
                        ],
                    )
                ]

        return UnitPart(body=unit_body)

    def _create_initialized_function(self, session: model.Session) -> UnitPart:
        specification = FunctionSpecification("Initialized", "Boolean")
        context_declarations = [
            d
            for d in session.declarations.values()
            if isinstance(d, decl.VariableDeclaration)
            and isinstance(d.type_, (rty.Message, rty.Sequence))
        ]
        if context_declarations:
            self._session_context.used_types.append(const.TYPES_INDEX)
        return UnitPart(
            [
                SubprogramDeclaration(specification),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        *[
                            e
                            for d in context_declarations
                            for e in [
                                Call(
                                    ID(d.type_identifier) * "Has_Buffer",
                                    [Variable(context_id(d.identifier))],
                                ),
                                Equal(
                                    Variable(context_id(d.identifier) * "Buffer_First"),
                                    First(const.TYPES_INDEX),
                                ),
                                Equal(
                                    Variable(context_id(d.identifier) * "Buffer_Last"),
                                    Add(
                                        First(const.TYPES_INDEX),
                                        Number(self._allocator.get_size(d.identifier) - 1),
                                    ),
                                ),
                            ]
                        ],
                        Call(ID(self._allocator.unit_identifier * "Global_Allocated")),
                    ),
                ),
            ],
        )

    @staticmethod
    def _create_active_function(session: model.Session) -> UnitPart:
        specification = FunctionSpecification("Active", "Boolean")
        return UnitPart(
            [
                SubprogramDeclaration(specification),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    NotEqual(Variable("P_Next_State"), Variable(f"S_{session.final}")),
                ),
            ],
        )

    @staticmethod
    def _create_initialize_procedure(
        session: model.Session,
        declarations: Sequence[Declaration],
        initialization: Sequence[Statement],
    ) -> UnitPart:
        specification = ProcedureSpecification("Initialize")
        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(Call("Uninitialized")),
                        Postcondition(And(Call("Initialized"), Call("Active"))),
                    ],
                ),
            ],
            [
                SubprogramBody(
                    specification,
                    declarations,
                    [
                        *initialization,
                        Assignment("P_Next_State", Variable(f"S_{session.initial}")),
                    ],
                ),
            ],
        )

    @staticmethod
    def _create_finalize_procedure(
        session: model.Session,
        declarations: Sequence[Declaration],
        finalization: Sequence[Statement],
    ) -> UnitPart:
        specification = ProcedureSpecification("Finalize")
        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(Call("Initialized")),
                        Postcondition(And(Call("Uninitialized"), Not(Call("Active")))),
                    ],
                ),
            ],
            [
                SubprogramBody(
                    specification,
                    declarations,
                    [
                        *finalization,
                        Assignment("P_Next_State", Variable(f"S_{session.final}")),
                    ],
                ),
            ],
        )

    def _create_reset_messages_before_write_procedure(
        self,
        session: model.Session,
    ) -> UnitPart:
        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        specification = ProcedureSpecification("Reset_Messages_Before_Write")
        states = [
            (
                state,
                [
                    (
                        ID(action.parameter.identifier),
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
                            Variable("P_Next_State"),
                            [
                                (
                                    Variable(f"S_{state.identifier}"),
                                    [
                                        CallStatement(
                                            ID(message_type.identifier) * "Reset",
                                            [
                                                Variable(context_id(message)),
                                                Variable(context_id(message) * "First"),
                                                Sub(
                                                    Variable(context_id(message) * "First"),
                                                    Number(1),
                                                ),
                                                *[
                                                    Variable(context_id(message) * p)
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
                        Precondition(Call("Initialized")),
                        Postcondition(Call("Initialized")),
                    ],
                )
            ],
        )

    def _create_tick_procedure(self, session: model.Session, has_writes: bool) -> UnitPart:
        specification = ProcedureSpecification("Tick")
        return UnitPart(
            [
                Pragma("Warnings", [Variable("Off"), String('subprogram "Tick" has no effect')]),
                SubprogramDeclaration(
                    specification,
                    [Precondition(Variable("Initialized")), Postcondition(Variable("Initialized"))],
                ),
                Pragma("Warnings", [Variable("On"), String('subprogram "Tick" has no effect')]),
            ],
            [
                SubprogramBody(
                    specification,
                    [],
                    [
                        CaseStatement(
                            Variable("P_Next_State"),
                            [
                                (
                                    Variable(f"S_{s.identifier}"),
                                    [
                                        *self._debug_output(f"State: {s.identifier}"),
                                        CallStatement(ID(s.identifier), [Variable("P_Next_State")])
                                        if s.identifier != session.final
                                        else NullStatement(),
                                    ],
                                )
                                for s in session.states
                            ],
                        ),
                        *([CallStatement("Reset_Messages_Before_Write")] if has_writes else []),
                    ],
                )
            ],
        )

    @staticmethod
    def _create_run_procedure(session: model.Session) -> UnitPart:
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
        specification = ProcedureSpecification("Run")
        return UnitPart(
            [
                Pragma("Warnings", [Variable("Off"), String('subprogram "Run" has no effect')]),
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(Call("Initialized")),
                        Postcondition(Call("Initialized")),
                    ],
                ),
                Pragma("Warnings", [Variable("On"), String('subprogram "Run" has no effect')]),
            ],
            [
                ExpressionFunctionDeclaration(
                    FunctionSpecification("In_IO_State", "Boolean"),
                    In(
                        Variable("P_Next_State"),
                        ChoiceList(*[Variable(f"S_{state.identifier}") for state in io_states]),
                    ),
                ),
                SubprogramBody(
                    specification,
                    [],
                    [
                        CallStatement("Tick"),
                        While(
                            And(Call("Active"), Not(Call("In_IO_State"))),
                            [
                                PragmaStatement("Loop_Invariant", [Variable("Initialized")]),
                                CallStatement("Tick"),
                            ],
                        ),
                    ],
                ),
            ],
        )

    @staticmethod
    def _create_state_function() -> UnitPart:
        specification = FunctionSpecification("Next_State", "State")
        return UnitPart(
            [
                SubprogramDeclaration(specification),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Variable("P_Next_State"),
                )
            ],
        )

    @staticmethod
    def _create_has_data_function(channel_writes: dict[rid.ID, list[ChannelAccess]]) -> UnitPart:
        specification = FunctionSpecification(
            "Has_Data", "Boolean", [Parameter(["Chan"], "Channel")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(specification, [Precondition(Call("Initialized"))]),
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Chan"),
                        [
                            (
                                Variable(f"C_{channel}"),
                                Case(
                                    Variable("P_Next_State"),
                                    [
                                        *[
                                            (
                                                Variable(f"S_{write.state}"),
                                                And(
                                                    Call(
                                                        write.message_type
                                                        * "Structural_Valid_Message",
                                                        [Variable(context_id(write.message))],
                                                    ),
                                                    Greater(
                                                        Call(
                                                            write.message_type * "Byte_Size",
                                                            [Variable(context_id(write.message))],
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
    def _create_needs_data_function(channel_reads: dict[rid.ID, list[ChannelAccess]]) -> UnitPart:
        specification = FunctionSpecification(
            "Needs_Data", "Boolean", [Parameter(["Chan"], "Channel")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(specification, [Precondition(Call("Initialized"))]),
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Chan"),
                        [
                            (
                                Variable(f"C_{channel}"),
                                Case(
                                    Variable("P_Next_State"),
                                    [
                                        *[(Variable(f"S_{read.state}"), TRUE) for read in reads],
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
        channel_writes: dict[rid.ID, list[ChannelAccess]]
    ) -> UnitPart:
        specification = FunctionSpecification(
            "Read_Buffer_Size", const.TYPES_LENGTH, [Parameter(["Chan"], "Channel")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(Call("Initialized"), Call("Has_Data", [Variable("Chan")]))
                        ),
                    ],
                ),
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Chan"),
                        [
                            (
                                Variable(f"C_{channel}"),
                                Case(
                                    Variable("P_Next_State"),
                                    [
                                        *[
                                            (
                                                Variable(f"S_{write.state}"),
                                                Call(
                                                    write.message_type * "Byte_Size",
                                                    [Variable(context_id(write.message))],
                                                ),
                                            )
                                            for write in writes
                                        ],
                                        (Variable("others"), Raise("Program_Error")),
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
        channel_reads: dict[rid.ID, list[ChannelAccess]]
    ) -> UnitPart:
        specification = FunctionSpecification(
            "Write_Buffer_Size", const.TYPES_LENGTH, [Parameter(["Chan"], "Channel")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(specification),
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Chan"),
                        [
                            (Variable(f"C_{channel}"), Number(4096) if reads else Number(0))
                            for channel, reads in channel_reads.items()
                        ],
                    ),
                )
            ],
        )

    def _create_read_procedure(self, channel_writes: dict[rid.ID, list[ChannelAccess]]) -> UnitPart:
        self._session_context.used_types.append(const.TYPES_INDEX)
        self._session_context.used_types.append(const.TYPES_LENGTH)

        specification = ProcedureSpecification(
            "Read",
            [
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
                                Call("Initialized"),
                                Call("Has_Data", [Variable("Chan")]),
                                Greater(Length("Buffer"), Number(0)),
                                LessEqual(
                                    Variable("Offset"),
                                    Sub(Last(const.TYPES_LENGTH), Length("Buffer")),
                                ),
                                LessEqual(
                                    Add(Length("Buffer"), Variable("Offset")),
                                    Call("Read_Buffer_Size", [Variable("Chan")]),
                                ),
                            )
                        ),
                        Postcondition(
                            Call("Initialized"),
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
                                            Variable("P_Next_State"),
                                            [
                                                *[
                                                    (
                                                        Variable(f"S_{write.state}"),
                                                        [
                                                            CallStatement(
                                                                (write.message_type * "Read").flat,
                                                                [
                                                                    Variable(
                                                                        context_id(write.message)
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
                                                        RaiseStatement("Program_Error"),
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

    def _create_write_procedure(self, channel_reads: dict[rid.ID, list[ChannelAccess]]) -> UnitPart:
        self._session_context.used_types.append(const.TYPES_INDEX)
        self._session_context.used_types.append(const.TYPES_LENGTH)

        specification = ProcedureSpecification(
            "Write",
            [
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
                                Call("Initialized"),
                                Call("Needs_Data", [Variable("Chan")]),
                                Greater(Length("Buffer"), Number(0)),
                                LessEqual(
                                    Variable("Offset"),
                                    Sub(Last(const.TYPES_LENGTH), Length("Buffer")),
                                ),
                                LessEqual(
                                    Add(Length("Buffer"), Variable("Offset")),
                                    Call("Write_Buffer_Size", [Variable("Chan")]),
                                ),
                            )
                        ),
                        Postcondition(
                            Call("Initialized"),
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
                                    Call("Write_Buffer_Size", [Variable("Chan")]),
                                ),
                                LessEqual(
                                    Variable("Offset"),
                                    Sub(Last(const.TYPES_LENGTH), Length("Buffer")),
                                ),
                                LessEqual(
                                    Add(Length("Buffer"), Variable("Offset")),
                                    Call("Write_Buffer_Size", [Variable("Chan")]),
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
                                Assignment(
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
                                                    Variable("Length"),
                                                )
                                            ],
                                        ),
                                        Last("Message_Buffer"),
                                    ),
                                    NamedAggregate(("others", Number(0))),
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
                                            Call("Write_Buffer_Size", [Variable("Chan")]),
                                        ),
                                    )
                                ),
                                Postcondition(
                                    And(
                                        LessEqual(Variable("Length"), Length("Message_Buffer")),
                                        Initialized("Message_Buffer"),
                                    )
                                ),
                                RelaxedInitialization(Variable("Message_Buffer")),
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
                                            Variable("P_Next_State"),
                                            [
                                                *[
                                                    (
                                                        Variable(f"S_{write.state}"),
                                                        [
                                                            CallStatement(
                                                                (write.message_type * "Write").flat,
                                                                [
                                                                    Variable(
                                                                        context_id(write.message)
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
                                                        RaiseStatement("Program_Error"),
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
        session_global: bool = False,
    ) -> EvaluatedDeclaration:
        result = EvaluatedDeclaration()

        for declaration in declarations:
            if isinstance(declaration, decl.VariableDeclaration):
                result += self._declare(
                    declaration.identifier,
                    declaration.type_,
                    declaration.location,
                    declaration.expression,
                    session_global=session_global,
                )
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

        return result

    def _state_action(
        self, state: rid.ID, action: stmt.Statement, exception_handler: ExceptionHandler
    ) -> Sequence[Statement]:
        if isinstance(action, stmt.Assignment):
            result = self._assign(
                action.identifier,
                action.type_,
                action.expression,
                exception_handler,
                state,
                action.location,
            )

        elif isinstance(action, stmt.Append):
            result = self._append(action, exception_handler)

        elif isinstance(action, stmt.Extend):
            fail(
                "Extend statement not yet supported",
                Subsystem.GENERATOR,
                location=action.location,
            )

        elif isinstance(action, stmt.Reset):
            result = self._reset(action)

        elif isinstance(action, stmt.Read):
            result = self._read(action)

        elif isinstance(action, stmt.Write):
            result = self._write(action)

        else:
            fatal_fail(
                f'unexpected statement "{type(action).__name__}"',
                Subsystem.GENERATOR,
                location=action.location,
            )

        return [*result, *exception_handler.execute_deferred()]

    def _declare(  # pylint: disable = too-many-arguments
        self,
        identifier: rid.ID,
        type_: rty.Type,
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
                    [ID(identifier)],
                    object_type,
                    initialization.substituted(self._substitution()).ada_expr()
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
                    [ID(identifier)],
                    self._ada_type(type_.identifier),
                    expression.ada_expr() if expression else None,
                )
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
                self._declare_context(ID(identifier), type_identifier)
            )
            result.initialization_declarations.append(self._declare_buffer(ID(identifier)))
            result.initialization.extend(
                [
                    *self._allocate_buffer(identifier, alloc_id),
                    self._initialize_context(
                        identifier,
                        type_identifier,
                        parameters=(
                            {
                                ID(n): First(self._ada_type(t.identifier))
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
                self._free_context_buffer(ID(identifier), type_identifier, alloc_id)
            )

        else:
            fatal_fail(
                f"unexpected variable declaration for {type_}",
                Subsystem.GENERATOR,
                location=identifier.location,
            )

        assert isinstance(type_, (rty.Integer, rty.Enumeration, rty.Message, rty.Sequence))

        if session_global:
            self._session_context.referenced_types.append(type_.identifier)
        else:
            self._session_context.referenced_types_body.append(type_.identifier)

        return result

    def _declare_and_assign(  # pylint: disable = too-many-arguments
        self,
        variables: Sequence[Tuple[rid.ID, rty.Type, expr.Expr]],
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
        state: rid.ID,
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
            alloc_id,
            v if initialized_in_declaration else None,
            constant=constant,
        )

        with exception_handler.local() as local_exception_handler:
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
                                self._assign(k, type_, v, local_exception_handler, state, alloc_id)
                                if not initialized_in_declaration
                                else []
                            ),
                        ],
                        *self._declare_and_assign(
                            variables[1:],
                            statements,
                            exception_handler,
                            state,
                            alloc_id,
                            constant,
                        ),
                        *evaluated_declaration.finalization,
                    ],
                ),
                *exception_handler.execute_deferred(),
            ]

    def _assign(  # pylint: disable = too-many-arguments
        self,
        target: rid.ID,
        target_type: rty.Type,
        expression: expr.Expr,
        exception_handler: ExceptionHandler,
        state: rid.ID,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        if isinstance(expression, expr.MessageAggregate):
            return self._assign_to_message_aggregate(target, expression, exception_handler, state)

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
            return self._assign_to_binding(target, expression, exception_handler, state, alloc_id)

        if isinstance(expression, expr.Selected):
            return self._assign_to_selected(target, expression, exception_handler)

        if isinstance(expression, expr.Head):
            return self._assign_to_head(target, expression, exception_handler, alloc_id)

        if isinstance(expression, expr.Comprehension):
            assert isinstance(target_type, rty.Sequence)
            return self._assign_to_comprehension(
                target, target_type, expression, exception_handler, alloc_id
            )

        if isinstance(expression, expr.Call):
            return self._assign_to_call(target, expression, exception_handler)

        if isinstance(expression, expr.Conversion):
            return self._assign_to_conversion(target, expression, exception_handler)

        if isinstance(
            expression,
            (
                expr.Variable,
                expr.Number,
                expr.MathBinExpr,
                expr.MathAssExpr,
                expr.Relation,
                expr.Attribute,
                expr.Aggregate,
            ),
        ) and (
            isinstance(expression.type_, (rty.AnyInteger, rty.Enumeration))
            or expression.type_ == rty.OPAQUE
        ):
            value = expression.substituted(self._substitution())
            assert isinstance(
                target_type, (rty.Integer, rty.Enumeration, rty.Message, rty.Sequence)
            )
            return self._if_valid_fields(
                expression,
                [
                    Assignment(ID(target), self._convert_type(value, target_type).ada_expr()),
                ],
                exception_handler,
            )

        if isinstance(expression, expr.Variable) and isinstance(
            expression.type_, (rty.Message, rty.Sequence)
        ):
            _unsupported_expression(expression, "in assignment")

        _unexpected_expression(expression, "in assignment")

    def _assign_to_binding(  # pylint: disable = too-many-branches
        self,
        target: rid.ID,
        binding: expr.Binding,
        exception_handler: ExceptionHandler,
        state: rid.ID,
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

        with exception_handler.local() as local_exception_handler:
            return self._declare_and_assign(
                variables,
                self._assign(
                    target,
                    type_,
                    binding.expr,
                    local_exception_handler,
                    state,
                    alloc_id=alloc_id,
                ),
                exception_handler,
                state,
                alloc_id=alloc_id,
                constant=True,
            )

    def _assign_to_selected(
        self, target: rid.ID, selected: expr.Selected, exception_handler: ExceptionHandler
    ) -> Sequence[Statement]:
        if not isinstance(selected.prefix, expr.Variable):
            fail(
                f'accessing field of expression "{type(selected.prefix).__name__}"'
                " not yet supported",
                Subsystem.GENERATOR,
                location=selected.prefix.location,
            )

        assert isinstance(selected.prefix.type_, rty.Message)

        message_type = ID(selected.prefix.type_.identifier)
        message_context = context_id(selected.prefix.identifier)
        selector = selected.selector

        if (
            isinstance(selected.type_, (rty.AnyInteger, rty.Enumeration))
            or selected.type_ == rty.OPAQUE
        ):
            return [
                IfStatement(
                    [
                        (
                            Call(
                                message_type * "Valid",
                                [
                                    Variable(message_context),
                                    Variable(message_type * f"F_{selector}"),
                                ],
                            ),
                            [
                                Assignment(
                                    Variable(ID(target)),
                                    Call(
                                        message_type * f"Get_{selector}",
                                        [Variable(message_context)],
                                    ),
                                )
                            ],
                        )
                    ],
                    # ISSUE: Componolit/RecordFlux#569
                    [
                        *self._debug_output(
                            f'Error: access to invalid field "{selector}" of'
                            f' "{message_context}"'
                        ),
                        *exception_handler.execute(),
                    ],
                )
            ]

        if isinstance(selected.type_, rty.Sequence):
            # ISSUE: Componolit/RecordFlux#577
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
        target: rid.ID,
        message_aggregate: expr.MessageAggregate,
        exception_handler: ExceptionHandler,
        state: rid.ID,
    ) -> Sequence[Statement]:
        assert isinstance(message_aggregate.type_, rty.Message)

        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        size = self._message_size(message_aggregate)
        size_variables = [
            v for v in size.variables() if isinstance(v.type_, (rty.Message, rty.Sequence))
        ]
        required_space = (
            size.substituted(
                lambda x: expr.Call(const.TYPES_BIT_LENGTH, [x])
                if isinstance(x, expr.Selected)
                else x
            )
            .substituted(self._substitution())
            .ada_expr()
        )
        target_type = ID(message_aggregate.type_.identifier)
        target_context = context_id(target)
        parameter_values = [
            (f, v, t)
            for f, v in message_aggregate.field_values.items()
            if f in message_aggregate.type_.parameter_types
            for t in [message_aggregate.type_.parameter_types[f]]
            if isinstance(t, (rty.Integer, rty.Enumeration))
        ]

        assign_to_message_aggregate = [
            self._if_sufficient_space(
                Call(
                    const.TYPES_BIT_LENGTH,
                    [
                        required_space,
                    ],
                ),
                target_context,
                [
                    CallStatement(
                        target_type * "Reset",
                        [
                            Variable(target_context),
                            Call(
                                const.TYPES_TO_FIRST_BIT_INDEX,
                                [
                                    Variable(f"{target_context}.Buffer_First"),
                                ],
                            ),
                            Add(
                                Call(
                                    const.TYPES_TO_FIRST_BIT_INDEX,
                                    [
                                        Variable(f"{target_context}.Buffer_First"),
                                    ],
                                ),
                                Call(
                                    const.TYPES_BIT_LENGTH,
                                    [
                                        required_space,
                                    ],
                                ),
                                -Number(1),
                            ),
                        ],
                        {
                            ID(p): self._convert_type(v, t)
                            .substituted(self._substitution())
                            .ada_expr()
                            for p, v, t in parameter_values
                        },
                    ),
                    *self._set_message_fields(
                        target_type, target_context, message_aggregate, exception_handler
                    ),
                ],
                exception_handler,
            ),
            *exception_handler.execute_deferred(),
        ]

        if size_variables:
            expressions = " or ".join(f'"{v}"' for v in size_variables)
            return [
                IfStatement(
                    [
                        (
                            AndThen(
                                *[
                                    e
                                    for v in size_variables
                                    if isinstance(v.type_, (rty.Message, rty.Sequence))
                                    for s in [
                                        expr.Size(v).substituted(self._substitution()).ada_expr()
                                    ]
                                    for e in [
                                        LessEqual(
                                            s,
                                            Number(
                                                self._allocator.get_size(v.identifier, state) * 8
                                            ),
                                        ),
                                        Equal(Mod(s, Size(const.TYPES_BYTE)), Number(0)),
                                    ]
                                ]
                            ),
                            assign_to_message_aggregate,
                        )
                    ],
                    # ISSUE: Componolit/RecordFlux#569
                    [
                        *self._debug_output(f"Error: unexpected size of {expressions}"),
                        *exception_handler.execute(),
                    ],
                )
            ]

        return assign_to_message_aggregate

    def _assign_to_head(
        self,
        target: rid.ID,
        head: expr.Head,
        exception_handler: ExceptionHandler,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        assert isinstance(head.prefix.type_, rty.Sequence)

        if not isinstance(head.prefix.type_.element, (rty.Integer, rty.Enumeration, rty.Message)):
            fatal_fail(
                f"unexpected sequence element type ({head.prefix.type_.element})"
                f' for "{head}" in assignment of "{target}"',
                Subsystem.GENERATOR,
                location=head.location,
            )

        assert isinstance(head.type_, (rty.Integer, rty.Enumeration, rty.Message))

        if not isinstance(head.prefix, expr.Variable):
            _unsupported_expression(head.prefix, "in Head attribute")

        target_type = ID(head.type_.identifier)
        sequence_type = ID(head.prefix.type_.identifier)
        sequence_context = context_id(head.prefix.identifier)
        sequence_identifier = ID(rid.ID(f"{head.prefix}"))

        if isinstance(head.prefix.type_.element, (rty.Integer, rty.Enumeration)):
            return [
                IfStatement(
                    [
                        (
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
                                    Size(ID(head.prefix.type_.element.identifier)),
                                ),
                            ),
                            [
                                Assignment(
                                    Variable(ID(target)),
                                    Call(
                                        sequence_type * "Head",
                                        [Variable(sequence_context)],
                                    ),
                                )
                            ],
                        )
                    ],
                    # ISSUE: Componolit/RecordFlux#569
                    [
                        *self._debug_output(
                            f"Error: access to first element of invalid or empty sequence"
                            f' "{sequence_context}"'
                        ),
                        *exception_handler.execute(),
                    ],
                )
            ]

        assert isinstance(head.prefix.type_.element, rty.Message)

        self._session_context.used_types_body.append(const.TYPES_LENGTH)
        self._session_context.referenced_types_body.append(target_type)

        target_context = context_id(target)
        target_buffer = buffer_id("RFLX_Target_" + target)
        element_context = ID("RFLX_Head_Ctx")
        with exception_handler.local() as local_exception_handler:
            return [
                self._declare_sequence_copy(
                    sequence_identifier,
                    sequence_type,
                    [
                        IfStatement(
                            [
                                (
                                    Call(
                                        sequence_type * "Has_Element",
                                        [Variable(copy_id(sequence_context))],
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
                                                        Variable(copy_id(sequence_context)),
                                                        Variable(element_context),
                                                    ],
                                                ),
                                                CallStatement(
                                                    target_type * "Verify_Message",
                                                    [Variable(element_context)],
                                                ),
                                                self._if_structural_valid_message(
                                                    target_type,
                                                    element_context,
                                                    [
                                                        *self._take_buffer(
                                                            ID(target),
                                                            target_type,
                                                            target_buffer,
                                                        ),
                                                        self._copy_to_buffer(
                                                            target_type,
                                                            element_context,
                                                            target_buffer,
                                                            local_exception_handler,
                                                        ),
                                                        CallStatement(
                                                            target_type * "Initialize",
                                                            [
                                                                Variable(target_context),
                                                                Variable(target_buffer),
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
                                                    copy_id(sequence_context),
                                                    element_context,
                                                    sequence_type,
                                                ),
                                            ],
                                        ),
                                    ],
                                )
                            ],
                            local_exception_handler.execute(),
                        )
                    ],
                    exception_handler,
                    alloc_id,
                ),
                *exception_handler.execute_deferred(),
            ]

    def _assign_to_comprehension(
        self,
        target: rid.ID,
        target_type: rty.Sequence,
        comprehension: expr.Comprehension,
        exception_handler: ExceptionHandler,
        alloc_id: Optional[Location],
    ) -> Sequence[Statement]:
        # pylint: disable = too-many-locals

        assert isinstance(comprehension.type_, (rty.Sequence, rty.Aggregate))
        if not isinstance(comprehension.type_.element, (rty.Integer, rty.Enumeration)):
            fail(
                f"creating sequence with element {comprehension.type_.element}"
                " using list comprehension not yet supported",
                Subsystem.GENERATOR,
                location=comprehension.location,
            )
        assert isinstance(comprehension.sequence.type_, rty.Sequence)

        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        target_id = ID(target)
        sequence_type_id = ID(comprehension.sequence.type_.identifier)
        iterator_id = ID(comprehension.iterator)

        sequence_element_type = comprehension.sequence.type_.element

        if isinstance(sequence_element_type, rty.Message):
            iterator_type_id = ID(sequence_element_type.identifier)

            if isinstance(comprehension.sequence, expr.Variable):
                sequence_id = ID(rid.ID(f"{comprehension.sequence}"))
                with exception_handler.local() as local_exception_handler:
                    return [
                        self._declare_sequence_copy(
                            sequence_id,
                            sequence_type_id,
                            [
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
                                )
                            ],
                            exception_handler,
                            alloc_id,
                        ),
                        *exception_handler.execute_deferred(),
                    ]

            if isinstance(comprehension.sequence, expr.Selected):
                selected = comprehension.sequence

                if not isinstance(selected.prefix, expr.Variable):
                    _unsupported_expression(
                        selected.prefix, "as prefix of Selected in list comprehension"
                    )

                assert isinstance(selected.prefix.type_, rty.Message)

                message_id = ID(selected.prefix.name)
                message_type = ID(selected.prefix.type_.identifier)
                sequence_id = ID(
                    rid.ID(
                        f"RFLX_{selected.prefix}_{selected.selector}",
                        location=selected.location,
                    )
                )
                message_field = ID(selected.selector)

                with exception_handler.local() as local_exception_handler:
                    return [
                        self._if_structural_valid_message(
                            message_type,
                            context_id(message_id),
                            [
                                self._declare_message_field_sequence_copy(
                                    message_id,
                                    message_type,
                                    message_field,
                                    sequence_id,
                                    sequence_type_id,
                                    [
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
                                        )
                                    ],
                                    exception_handler,
                                    alloc_id,
                                ),
                            ],
                            exception_handler,
                        ),
                        *exception_handler.execute_deferred(),
                    ]

            _unsupported_expression(comprehension.sequence, "as sequence in list comprehension")

        fail(
            f"iterating over sequence of {sequence_element_type}"
            " in list comprehension not yet supported",
            Subsystem.GENERATOR,
            location=comprehension.sequence.location,
        )

    def _assign_to_call(
        self,
        target: rid.ID,
        call_expr: expr.Call,
        exception_handler: ExceptionHandler,
    ) -> Sequence[Statement]:
        pre_call = []
        post_call = []
        local_declarations = []

        if isinstance(call_expr.type_, rty.Message):
            type_identifier = self._ada_type(call_expr.type_.identifier)
            local_declarations.append(
                ObjectDeclaration(
                    [ID(target)],
                    ID(type_identifier) * "Structure",
                )
            )
            post_call.append(
                CallStatement(
                    ID(type_identifier) * "To_Context",
                    [
                        Variable(ID(target)),
                        Variable(context_id(target)),
                    ],
                ),
            )

        for a in call_expr.args:
            if not isinstance(a, (expr.Number, expr.Variable, expr.Selected, expr.String)) and not (
                isinstance(a, expr.Opaque)
                and isinstance(a.prefix, expr.Variable)
                and a.type_ == rty.OPAQUE
            ):
                _unsupported_expression(a, "as function argument")

            if isinstance(a, expr.Variable) and isinstance(a.type_, rty.Message):
                type_identifier = self._ada_type(a.type_.identifier)
                local_declarations.append(
                    ObjectDeclaration(
                        [ID(a.identifier)],
                        ID(type_identifier) * "Structure",
                    )
                )
                pre_call.append(
                    CallStatement(
                        ID(type_identifier) * "To_Structure",
                        [
                            Variable(context_id(a.identifier)),
                            Variable(ID(a.identifier)),
                        ],
                    ),
                )

        call = [
            CallStatement(
                ID(call_expr.identifier),
                [
                    Variable(ID(target)),
                    *[a.substituted(self._substitution()).ada_expr() for a in call_expr.args],
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
            )

        return call

    @staticmethod
    def contains_function_name(
        refinement_package: rid.ID, pdu: rid.ID, sdu: rid.ID, field: rid.ID
    ) -> str:
        sdu_name = sdu.name if sdu.parent == refinement_package else sdu
        pdu_name = pdu.name if pdu.parent == refinement_package else pdu
        return f"{sdu_name.flat}_In_{pdu_name.flat}_{field}"

    def _assign_to_conversion(
        self,
        target: rid.ID,
        conversion: expr.Conversion,
        exception_handler: ExceptionHandler,
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
            IfStatement(
                [
                    (
                        Call(
                            ID(contains_package)
                            * self.contains_function_name(
                                refinement.package, pdu.identifier, sdu.identifier, field
                            ),
                            [Variable(context_id(conversion.argument.prefix.identifier))],
                        ),
                        [
                            CallStatement(
                                ID(contains_package) * f"Copy_{field}",
                                [
                                    Variable(context_id(conversion.argument.prefix.identifier)),
                                    Variable(context_id(target)),
                                ],
                            ),
                            CallStatement(
                                ID(sdu.identifier) * "Verify_Message",
                                [
                                    Variable(context_id(target)),
                                ],
                            ),
                        ],
                    )
                ],
                # ISSUE: Componolit/RecordFlux#569
                [
                    *self._debug_output(f'Error: invalid conversion "{conversion}"'),
                    *exception_handler.execute(),
                ],
            )
        ]

    def _append(
        self,
        append: stmt.Append,
        exception_handler: ExceptionHandler,
    ) -> Sequence[Statement]:
        assert isinstance(append.type_, rty.Sequence)

        self._session_context.used_types_body.append(const.TYPES_BIT_LENGTH)

        def check(sequence_type: ID, required_space: Expr) -> Statement:
            return IfStatement(
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
            )

        if isinstance(append.type_.element, (rty.Integer, rty.Enumeration)):
            assert isinstance(append.parameter, expr.Variable)

            sequence_type = ID(append.type_.identifier)
            sequence_context = context_id(append.identifier)
            element_type = ID(append.type_.element.identifier)

            return [
                check(sequence_type, Size(element_type)),
                CallStatement(
                    sequence_type * "Append_Element",
                    [Variable(sequence_context), append.parameter.ada_expr()],
                ),
            ]

        if isinstance(append.type_.element, rty.Message):
            sequence_type = ID(append.type_.identifier)
            sequence_context = context_id(append.identifier)
            element_type = ID(append.type_.element.identifier)
            element_context = context_id("RFLX_Element_" + append.identifier)

            self._session_context.referenced_types_body.append(element_type)

            if isinstance(append.parameter, expr.MessageAggregate):
                required_space = (
                    self._message_size(append.parameter)
                    .substituted(self._substitution())
                    .ada_expr()
                )
            else:
                _unsupported_expression(append.parameter, "in Append statement")

            with exception_handler.local() as local_exception_handler:
                return [
                    check(sequence_type, required_space),
                    Declare(
                        [ObjectDeclaration([element_context], element_type * "Context")],
                        [
                            CallStatement(
                                sequence_type * "Switch",
                                [Variable(sequence_context), Variable(element_context)],
                            ),
                            *(
                                self._set_message_fields(
                                    element_type,
                                    element_context,
                                    append.parameters[0],
                                    local_exception_handler,
                                )
                                if isinstance(append.parameters[0], expr.MessageAggregate)
                                else []
                            ),
                            *self._update_context(sequence_context, element_context, sequence_type),
                        ],
                    ),
                    *exception_handler.execute_deferred(),
                ]

        fatal_fail(
            f"unexpected element type {append.type_.element} in Append statement",
            Subsystem.GENERATOR,
            location=append.parameter.location,
        )

    @staticmethod
    def _read(read: stmt.Read) -> Sequence[Statement]:
        assert isinstance(read.parameter.type_, rty.Message)

        if not isinstance(read.parameter, expr.Variable):
            _unsupported_expression(read.parameter, "in Read statement")

        target_type = ID(read.parameter.type_.identifier)
        target_context = context_id(read.parameter.identifier)
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

    @staticmethod
    def _reset(
        reset: stmt.Reset,
    ) -> Sequence[Statement]:
        assert isinstance(reset.type_, rty.Message)

        target_type = ID(reset.type_.identifier)
        target_context = context_id(reset.identifier)
        return [
            CallStatement(
                target_type * "Reset",
                [Variable(target_context)],
                {ID(n): e.ada_expr() for n, e in reset.associations.items()},
            ),
        ]

    def _message_size(self, message_aggregate: expr.MessageAggregate) -> expr.Expr:
        message = self._model_type(message_aggregate.identifier)
        assert isinstance(message, model.Message)
        return message.size({model.Field(f): v for f, v in message_aggregate.field_values.items()})

    def _substitution(self) -> Callable[[expr.Expr], expr.Expr]:
        # pylint: disable = too-many-statements

        def func(expression: expr.Expr) -> expr.Expr:
            # pylint: disable = too-many-branches, too-many-return-statements
            if isinstance(expression, (expr.Relation, expr.MathBinExpr)):
                for e in [expression.left, expression.right]:
                    if isinstance(e.type_, (rty.Integer, rty.Enumeration)):
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
                            expr.Variable(context_id(expression.prefix.identifier)),
                            expression.selector,
                        )
                    return expr.Call(
                        ID(expression.prefix.type_.identifier) * ID(f"Get_{expression.selector}"),
                        [expr.Variable(context_id(expression.prefix.identifier))],
                    )

                assert False

            if isinstance(expression, expr.Valid):
                if isinstance(expression.prefix, expr.Variable):
                    if isinstance(expression.prefix.type_, rty.Message):
                        return expr.Call(
                            ID(expression.prefix.type_.identifier) * ID("Structural_Valid_Message"),
                            [expr.Variable(context_id(expression.prefix.identifier))],
                        )

                    if isinstance(expression.prefix.type_, rty.Sequence):
                        return expr.Call(
                            ID(expression.prefix.type_.identifier) * ID("Valid"),
                            [expr.Variable(context_id(expression.prefix.identifier))],
                        )

                    assert False

                if isinstance(expression.prefix, expr.Selected):
                    assert isinstance(expression.prefix.prefix, expr.Variable)
                    assert isinstance(expression.prefix.prefix.type_, rty.Message)
                    type_name = ID(expression.prefix.prefix.type_.identifier)
                    return expr.Call(
                        type_name
                        * (
                            ID("Valid")
                            if isinstance(expression.prefix.type_, (rty.Integer, rty.Enumeration))
                            else ID("Structural_Valid")
                        ),
                        [
                            expr.Variable(context_id(expression.prefix.prefix.identifier)),
                            expr.Variable(type_name * f"F_{expression.prefix.selector}"),
                        ],
                    )

                assert False

            if isinstance(expression, expr.Present):
                if isinstance(expression.prefix, expr.Selected):
                    assert isinstance(expression.prefix.prefix, expr.Variable)
                    assert isinstance(expression.prefix.prefix.type_, rty.Message)
                    type_name = ID(expression.prefix.prefix.type_.identifier)
                    return expr.Call(
                        type_name * ID("Present"),
                        [
                            expr.Variable(context_id(expression.prefix.prefix.identifier)),
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

            if (
                isinstance(expression, expr.Equal)
                and isinstance(expression.left, expr.Variable)
                and isinstance(expression.right, expr.Variable)
            ):
                if expression.left == expr.Variable("True"):
                    return expression.right
                if expression.right == expr.Variable("True"):
                    return expression.left
                if expression.left == expr.Variable("False"):
                    return expr.Not(expression.right)
                if expression.right == expr.Variable("False"):
                    return expr.Not(expression.left)

            if (
                isinstance(expression, expr.NotEqual)
                and isinstance(expression.left, expr.Variable)
                and isinstance(expression.right, expr.Variable)
            ):
                if expression.left == expr.Variable("True"):
                    return expr.Not(expression.right)
                if expression.right == expr.Variable("True"):
                    return expr.Not(expression.left)
                if expression.left == expr.Variable("False"):
                    return expression.right
                if expression.right == expr.Variable("False"):
                    return expression.left

            if isinstance(expression, (expr.Equal, expr.NotEqual)) and isinstance(
                expression.left.type_, rty.Enumeration
            ):
                relation = expression.__class__
                if expression.left.type_.always_valid:
                    selected = None
                    literal = None

                    if isinstance(expression.left, expr.Selected):
                        selected = expression.left.substituted(self._substitution())
                        literal = expression.right
                    elif isinstance(expression.right, expr.Selected):
                        selected = expression.right.substituted(self._substitution())
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
                if isinstance(expression.prefix, expr.Variable):
                    if (
                        isinstance(expression.prefix.type_, rty.AnyInteger)
                        or (
                            isinstance(expression.prefix.type_, (rty.Sequence, rty.Aggregate))
                            and isinstance(expression.prefix.type_.element, rty.AnyInteger)
                        )
                        or (
                            isinstance(expression.prefix.type_, (rty.Integer, rty.Enumeration))
                            and expression.prefix.identifier == expression.prefix.type_.identifier
                        )
                    ):
                        return expression

                    if isinstance(expression.prefix.type_, (rty.Message, rty.Sequence)):
                        type_ = ID(expression.prefix.type_.identifier)
                        context = context_id(expression.prefix.identifier)
                        return expr.Call(type_ * "Size", [expr.Variable(context)])

                    _unexpected_expression(expression.prefix, "in Size attribute")

                if isinstance(expression.prefix, expr.Selected):
                    assert isinstance(expression.prefix.prefix, expr.Variable)
                    assert isinstance(expression.prefix.prefix.type_, rty.Message)
                    type_ = ID(expression.prefix.prefix.type_.identifier)
                    context = context_id(expression.prefix.prefix.identifier)
                    return expr.Call(
                        type_ * "Field_Size",
                        [
                            expr.Variable(context),
                            expr.Variable(type_ * "F_" + expression.prefix.selector),
                        ],
                    )

                _unsupported_expression(expression.prefix, "in Size attribute")

            if isinstance(expression, expr.HasData):
                assert isinstance(expression.prefix, expr.Variable)
                assert isinstance(expression.prefix.type_, rty.Message)
                type_ = ID(expression.prefix.type_.identifier)
                context = context_id(expression.prefix.identifier)
                return expr.Greater(
                    expr.Call(type_ * "Byte_Size", [expr.Variable(context)]), expr.Number(0)
                )

            if isinstance(expression, expr.Opaque):
                if isinstance(expression.prefix, expr.Variable):
                    assert expression.type_ == rty.OPAQUE
                    assert isinstance(expression.prefix.type_, rty.Message)
                    message_type = ID(expression.prefix.type_.identifier)
                    message_context = context_id(expression.prefix.identifier)
                    return expr.Call(
                        message_type * "Message_Data", [expr.Variable(message_context)]
                    )

                _unsupported_expression(expression.prefix, "in Opaque attribute")

            if isinstance(expression, expr.Head):
                _unsupported_expression(expression, "in expression")

            return expression

        return func

    def _if_valid_sequence(
        self,
        sequence_type: ID,
        sequence_context: ID,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        # ISSUE: Componlit/RecordFlux#569
        return IfStatement(
            [
                (
                    Call(sequence_type * "Valid", [Variable(sequence_context)]),
                    statements,
                )
            ],
            # ISSUE: Componolit/RecordFlux#569
            [
                *self._debug_output(f'Error: invalid sequence "{sequence_context}"'),
                *exception_handler.execute(),
            ],
        )

    def _if_structural_valid_message(
        self,
        message_type: ID,
        message_context: ID,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        # ISSUE: Componlit/RecordFlux#569
        return IfStatement(
            [
                (
                    Call(
                        message_type * "Structural_Valid_Message",
                        [Variable(message_context)],
                    ),
                    statements,
                )
            ],
            # ISSUE: Componolit/RecordFlux#569
            [
                *self._debug_output(f'Error: invalid message "{message_context}"'),
                *exception_handler.execute(),
            ],
        )

    def _if_structural_valid_message_field(
        self,
        message_type: ID,
        message_context: ID,
        message_field: ID,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        # ISSUE: Componlit/RecordFlux#569
        return IfStatement(
            [
                (
                    Call(
                        message_type * "Structural_Valid",
                        [
                            Variable(message_context),
                            Variable(message_type * model.Field(message_field).affixed_name),
                        ],
                    ),
                    statements,
                )
            ],
            # ISSUE: Componolit/RecordFlux#569
            [
                *self._debug_output(
                    f'Error: invalid message field "{message_type * message_field}"'
                ),
                *exception_handler.execute(),
            ],
        )

    def _if_valid_fields(
        self,
        expression: expr.Expr,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
    ) -> Sequence[Statement]:
        """Ensure that all referenced fields in the expression are valid."""
        # ISSUE: Componlit/RecordFlux#569

        selected = expression.findall(lambda x: isinstance(x, expr.Selected))

        if selected:
            expressions = " or ".join(f'"{s}"' for s in selected)
            return [
                IfStatement(
                    [
                        (
                            expr.AndThen(*[expr.Valid(e) for e in selected])
                            .substituted(self._substitution())
                            .ada_expr(),
                            statements,
                        )
                    ],
                    # ISSUE: Componolit/RecordFlux#569
                    [
                        *self._debug_output(
                            f"Error: reference to invalid message field in {expressions}"
                        ),
                        *exception_handler.execute(),
                    ],
                )
            ]

        return statements

    def _if_sufficient_space(
        self,
        required_space: Expr,
        target_context: ID,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        return IfStatement(
            [
                (
                    GreaterEqual(
                        Add(
                            Call(
                                const.TYPES_TO_FIRST_BIT_INDEX,
                                [
                                    Variable(f"{target_context}.Buffer_Last"),
                                ],
                            ),
                            -Call(
                                const.TYPES_TO_FIRST_BIT_INDEX,
                                [
                                    Variable(f"{target_context}.Buffer_First"),
                                ],
                            ),
                            Number(1),
                        ),
                        required_space,
                    ),
                    statements,
                )
            ],
            [
                *self._debug_output(f'Error: insufficient space in message "{target_context}"'),
                *exception_handler.execute(),
            ],
        )

    def _if_sufficient_space_in_sequence(
        self,
        required_space: Expr,
        sequence_type: ID,
        sequence_context: ID,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
    ) -> IfStatement:
        return IfStatement(
            [
                (
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
                )
            ],
            [
                *self._debug_output(f'Error: insufficient space in sequence "{sequence_context}"'),
                *exception_handler.execute(),
            ],
        )

    def _ensure(
        self,
        statements: List[Statement],
        property_expression: Expr,
        error_message: str,
        exception_handler: ExceptionHandler,
    ) -> List[Statement]:
        nested: List[Statement] = []
        statements.append(
            IfStatement(
                [
                    (
                        property_expression,
                        nested,
                    )
                ],
                # ISSUE: Componolit/RecordFlux#569
                [
                    *self._debug_output(error_message),
                    *exception_handler.execute(),
                ],
            )
        )
        return nested

    @staticmethod
    def _exit_on_deferred_exception() -> ExitStatement:
        # ISSUE: Componolit/Workarounds#569
        return ExitStatement(Variable("RFLX_Exception"))

    def _set_message_fields(
        self,
        target_type: ID,
        target_context: ID,
        message_aggregate: expr.MessageAggregate,
        exception_handler: ExceptionHandler,
    ) -> Sequence[Statement]:
        # pylint: disable = too-many-statements, too-many-branches, too-many-locals

        assert isinstance(message_aggregate.type_, rty.Message)

        statements: List[Statement] = []
        result = statements

        for f, v in message_aggregate.field_values.items():
            if f not in message_aggregate.type_.field_types:
                continue

            field_type = message_aggregate.type_.field_types[f]

            if isinstance(field_type, rty.Sequence):
                size: Expr
                if isinstance(v, expr.Aggregate):
                    size = Mul(Number(len(v.elements)), Size(const.TYPES_BYTE))
                elif isinstance(v, expr.Variable) and isinstance(
                    v.type_, (rty.Message, rty.Sequence)
                ):
                    type_ = ID(v.type_.identifier)
                    context = context_id(v.identifier)
                    size = Call(type_ * "Size", [Variable(context)])
                elif isinstance(v, expr.Selected):
                    assert isinstance(v.prefix, expr.Variable)
                    assert isinstance(v.prefix.type_, rty.Message)
                    message_type = ID(v.prefix.type_.identifier)
                    message_context = context_id(v.prefix.identifier)
                    statements = self._ensure(
                        statements,
                        Call(
                            message_type * "Valid_Next",
                            [
                                Variable(message_context),
                                Variable(message_type * f"F_{v.selector}"),
                            ],
                        ),
                        f'Error: access to invalid next message field for "{v}"',
                        exception_handler,
                    )
                    size = Call(
                        message_type * "Field_Size",
                        [
                            Variable(message_context),
                            Variable(message_type * f"F_{v.selector}"),
                        ],
                    )
                else:
                    size = Size(v.substituted(self._substitution()).ada_expr())
                statements = self._ensure(
                    statements,
                    Equal(
                        Call(
                            target_type * "Field_Size",
                            [
                                Variable(target_context),
                                Variable(target_type * f"F_{f}"),
                            ],
                        ),
                        size,
                    ),
                    f'Error: invalid message field size for "{v}"',
                    exception_handler,
                )

            if isinstance(v, (expr.Number, expr.Aggregate)) or (
                isinstance(v, (expr.Variable, expr.MathBinExpr, expr.MathAssExpr, expr.Size))
                and isinstance(v.type_, (rty.AnyInteger, rty.Enumeration, rty.Aggregate))
            ):
                field_type = message_aggregate.type_.types[f]
                if isinstance(v, expr.Aggregate) and len(v.elements) == 0:
                    statements.append(
                        CallStatement(target_type * f"Set_{f}_Empty", [Variable(target_context)])
                    )
                else:
                    value = self._convert_type(v, field_type).substituted(self._substitution())
                    statements.append(
                        CallStatement(
                            target_type * f"Set_{f}",
                            [
                                Variable(target_context),
                                value.ada_expr(),
                            ],
                        )
                    )
            elif isinstance(v, expr.Variable) and isinstance(v.type_, rty.Sequence):
                sequence_context = context_id(v.identifier)
                statements.extend(
                    [
                        CallStatement(
                            target_type * f"Set_{f}",
                            [Variable(target_context), Variable(sequence_context)],
                        ),
                    ]
                )
            elif isinstance(v, expr.Variable) and isinstance(v.type_, rty.Message):
                _unsupported_expression(v, "in message aggregate")
            elif (
                isinstance(v, expr.Selected)
                and isinstance(v.prefix, expr.Variable)
                and isinstance(v.prefix.type_, rty.Message)
            ):
                message_type = ID(v.prefix.type_.identifier)
                message_context = context_id(v.prefix.identifier)
                target_field_type = message_aggregate.type_.types[f]
                assert isinstance(target_field_type, (rty.Integer, rty.Enumeration, rty.Sequence))
                statements = self._ensure(
                    statements,
                    Call(
                        message_type
                        * ("Structural_Valid" if isinstance(v.type_, rty.Sequence) else "Valid"),
                        [
                            Variable(message_context),
                            Variable(message_type * f"F_{v.selector}"),
                        ],
                    ),
                    f'Error: access to invalid message field in "{v}"',
                    exception_handler,
                )
                get_field_value = self._convert_type(
                    expr.Call(message_type * f"Get_{v.selector}", [expr.Variable(message_context)]),
                    target_field_type,
                    v.type_,
                ).ada_expr()
                statements.extend(
                    [
                        CallStatement(
                            target_type * f"Set_{f}",
                            [
                                Variable(target_context),
                                get_field_value,
                            ],
                        ),
                    ]
                )
            elif isinstance(v, expr.Opaque) and isinstance(v.prefix, expr.Variable):
                assert v.type_ == rty.OPAQUE
                assert isinstance(v.prefix.type_, rty.Message)
                message_type = ID(v.prefix.type_.identifier)
                message_context = context_id(v.prefix.identifier)
                statements.extend(
                    [
                        CallStatement(
                            target_type * f"Set_{f}",
                            [
                                Variable(target_context),
                                Call(message_type * "Message_Data", [Variable(message_context)]),
                            ],
                        ),
                    ]
                )
            else:
                _unsupported_expression(v, "in message aggregate")
        return result

    def _declare_context_buffer(self, identifier: ID, type_: ID) -> List[Declaration]:
        return [
            self._declare_context(identifier, type_),
            self._declare_buffer(identifier),
        ]

    def _declare_context(self, identifier: ID, type_: ID) -> Declaration:
        self._session_context.referenced_types_body.append(type_)
        return ObjectDeclaration([context_id(identifier)], type_ * "Context")

    @staticmethod
    def _declare_buffer(identifier: ID) -> Declaration:
        return ObjectDeclaration([buffer_id(identifier)], const.TYPES_BYTES_PTR)

    def _declare_sequence_copy(
        self,
        sequence_identifier: ID,
        sequence_type: ID,
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
        alloc_id: Optional[Location],
    ) -> IfStatement:
        # ISSUE: Componolit/RecordFlux#577
        with exception_handler.local() as local_exception_handler:
            return self._if_valid_sequence(
                sequence_type,
                context_id(sequence_identifier),
                [
                    Declare(
                        self._declare_context_buffer(copy_id(sequence_identifier), sequence_type),
                        [
                            *self._allocate_buffer(copy_id(sequence_identifier), alloc_id),
                            self._copy_to_buffer(
                                sequence_type,
                                context_id(sequence_identifier),
                                ID(f"{copy_id(buffer_id(sequence_identifier))}"),
                                local_exception_handler,
                            ),
                            self._initialize_context(
                                copy_id(sequence_identifier),
                                sequence_type,
                                last=Call(
                                    sequence_type * "Sequence_Last",
                                    [Variable(context_id(sequence_identifier))],
                                ),
                            ),
                            *statements,
                            *self._free_context_buffer(
                                copy_id(sequence_identifier), sequence_type, alloc_id
                            ),
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
        statements: Sequence[Statement],
        exception_handler: ExceptionHandler,
        alloc_id: Optional[Location],
    ) -> Declare:
        # ISSUE: Componolit/RecordFlux#577
        with exception_handler.local() as local_exception_handler:
            return Declare(
                self._declare_context_buffer(sequence_identifier, sequence_type),
                [
                    *self._allocate_buffer(sequence_identifier, alloc_id),
                    self._copy_to_buffer(
                        message_type,
                        context_id(message_identifier),
                        ID(f"{buffer_id(sequence_identifier)}"),
                        local_exception_handler,
                    ),
                    self._if_structural_valid_message_field(
                        message_type,
                        context_id(message_identifier),
                        message_field,
                        [
                            self._initialize_context(
                                sequence_identifier,
                                sequence_type,
                                first=Call(
                                    message_type * "Field_First",
                                    [
                                        Variable(context_id(message_identifier)),
                                        Variable(
                                            message_type * model.Field(message_field).affixed_name
                                        ),
                                    ],
                                ),
                                last=Call(
                                    message_type * "Field_Last",
                                    [
                                        Variable(context_id(message_identifier)),
                                        Variable(
                                            message_type * model.Field(message_field).affixed_name
                                        ),
                                    ],
                                ),
                            ),
                            *statements,
                            *self._take_buffer(sequence_identifier, sequence_type),
                        ],
                        local_exception_handler,
                    ),
                    *self._free_buffer(sequence_identifier, alloc_id),
                ],
            )

    def _comprehension(  # pylint: disable = too-many-arguments
        self,
        sequence_identifier: ID,
        sequence_type: ID,
        target_identifier: ID,
        target_type: rty.Sequence,
        iterator_identifier: ID,
        iterator_type: ID,
        selector: expr.Expr,
        condition: expr.Expr,
        exception_handler: ExceptionHandler,
    ) -> While:
        assert not isinstance(selector, expr.MessageAggregate)

        assert isinstance(target_type.element, (rty.Integer, rty.Enumeration, rty.Message))

        target_type_id = ID(target_type.identifier)

        with exception_handler.local() as local_exception_handler:
            return While(
                Call(
                    sequence_type * "Has_Element",
                    [Variable(context_id(sequence_identifier))],
                ),
                [
                    PragmaStatement(
                        "Loop_Invariant",
                        [
                            Call(
                                sequence_type * "Has_Buffer",
                                [Variable(context_id(sequence_identifier))],
                            )
                        ],
                    ),
                    PragmaStatement(
                        "Loop_Invariant",
                        [
                            Call(
                                target_type_id * "Has_Buffer",
                                [Variable(context_id(target_identifier))],
                            )
                        ],
                    ),
                    *[
                        PragmaStatement(
                            "Loop_Invariant",
                            [
                                Equal(
                                    Selected(Variable(context_id(x)), "Buffer_First"),
                                    LoopEntry(Selected(Variable(context_id(x)), "Buffer_First")),
                                ),
                            ],
                        )
                        for x in [sequence_identifier, target_identifier]
                    ],
                    *[
                        PragmaStatement(
                            "Loop_Invariant",
                            [
                                Equal(
                                    Selected(Variable(context_id(x)), "Buffer_Last"),
                                    LoopEntry(Selected(Variable(context_id(x)), "Buffer_Last")),
                                ),
                            ],
                        )
                        for x in [sequence_identifier, target_identifier]
                    ],
                    PragmaStatement(
                        "Loop_Invariant",
                        [
                            Call(
                                target_type_id * "Valid",
                                [Variable(context_id(target_identifier))],
                            )
                        ],
                    ),
                    Declare(
                        [self._declare_context(iterator_identifier, iterator_type)],
                        [
                            CallStatement(
                                sequence_type * "Switch",
                                [
                                    Variable(context_id(sequence_identifier)),
                                    Variable(context_id(iterator_identifier)),
                                ],
                            ),
                            CallStatement(
                                iterator_type * "Verify_Message",
                                [Variable(context_id(iterator_identifier))],
                            ),
                            *self._if_valid_fields(
                                condition,
                                [
                                    IfStatement(
                                        [
                                            (
                                                condition.substituted(
                                                    self._substitution()
                                                ).ada_expr(),
                                                [
                                                    self._if_sufficient_space_in_sequence(
                                                        Size(
                                                            ID(
                                                                target_type.element.identifier
                                                                + "_Enum"
                                                                if isinstance(
                                                                    target_type.element,
                                                                    rty.Enumeration,
                                                                )
                                                                and target_type.element.always_valid
                                                                else target_type.element.identifier
                                                            )
                                                        ),
                                                        target_type_id,
                                                        context_id(target_identifier),
                                                        [
                                                            CallStatement(
                                                                target_type_id * "Append_Element",
                                                                [
                                                                    Variable(
                                                                        context_id(
                                                                            target_identifier
                                                                        )
                                                                    ),
                                                                    selector.substituted(
                                                                        self._substitution()
                                                                    ).ada_expr(),
                                                                ],
                                                            )
                                                        ],
                                                        local_exception_handler,
                                                    ),
                                                ],
                                            )
                                        ]
                                    )
                                ],
                                exception_handler,
                            ),
                            *self._update_context(
                                context_id(sequence_identifier),
                                context_id(iterator_identifier),
                                sequence_type,
                            ),
                        ],
                    ),
                    self._exit_on_deferred_exception(),
                ],
            )

    def _free_context_buffer(
        self, identifier: ID, type_: ID, alloc_id: Optional[Location]
    ) -> Sequence[Statement]:
        return [
            *self._take_buffer(identifier, type_),
            *self._free_buffer(identifier, alloc_id),
        ]

    def _free_buffer(self, identifier: ID, alloc_id: Optional[Location]) -> Sequence[Statement]:
        return self._allocator.free_buffer(buffer_id(identifier), alloc_id)

    @staticmethod
    def _take_buffer(identifier: ID, type_: ID, buf: ID = None) -> Sequence[Statement]:
        context = context_id(identifier)
        buf = buf or buffer_id(identifier)
        return [
            # WORKAROUND: Componolit/Workarounds#32
            PragmaStatement(
                "Warnings",
                [
                    Variable("Off"),
                    String(f'unused assignment to "{context}"'),
                ],
            ),
            PragmaStatement(
                "Warnings",
                [
                    Variable("Off"),
                    String(f'"{context}" is set by "Take_Buffer" but not used after the call'),
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
                    String(f'"{context}" is set by "Take_Buffer" but not used after the call'),
                ],
            ),
            PragmaStatement(
                "Warnings",
                [
                    Variable("On"),
                    String(f'unused assignment to "{context}"'),
                ],
            ),
        ]

    @staticmethod
    def _update_context(
        sequence_context: ID, element_context: ID, sequence_type: ID
    ) -> Sequence[Statement]:
        return [
            # WORKAROUND: Componolit/Workarounds#32
            PragmaStatement(
                "Warnings",
                [
                    Variable("Off"),
                    String(f'unused assignment to "{element_context}"'),
                ],
            ),
            PragmaStatement(
                "Warnings",
                [
                    Variable("Off"),
                    String(f'"{element_context}" is set by "Update" but not used after the call'),
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
                    String(f'"{element_context}" is set by "Update" but not used after the call'),
                ],
            ),
            PragmaStatement(
                "Warnings",
                [
                    Variable("On"),
                    String(f'unused assignment to "{element_context}"'),
                ],
            ),
        ]

    def _allocate_buffer(
        self, identifier: rid.ID, alloc_id: Optional[Location]
    ) -> Sequence[Statement]:
        self._session_context.used_types_body.append(const.TYPES_INDEX)
        slot_id = self._allocator.get_slot_ptr(alloc_id)
        return [
            Assignment(buffer_id(identifier), slot_id),
            PragmaStatement("Warnings", [Variable("Off"), String("unused assignment")]),
            Assignment(slot_id, Variable("null")),
            PragmaStatement("Warnings", [Variable("On"), String("unused assignment")]),
        ]

    @staticmethod
    def _initialize_context(
        identifier: rid.ID,
        type_: ID,
        first: Expr = None,
        last: Expr = None,
        parameters: Mapping[ID, Expr] = None,
    ) -> CallStatement:
        return CallStatement(
            type_ * "Initialize",
            [
                Variable(context_id(identifier)),
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
            ],
            parameters,
        )

    def _copy_to_buffer(
        self, type_: ID, source_context: ID, target_buffer: ID, exception_handler: ExceptionHandler
    ) -> IfStatement:
        self._session_context.used_types_body.append(const.TYPES_LENGTH)
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
                        CallStatement(
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
                        ),
                    ],
                )
            ],
            exception_handler.execute(),
        )

    def _convert_type(
        self, expression: expr.Expr, target_type: rty.Type, expression_type: rty.Type = None
    ) -> expr.Expr:
        if not expression_type:
            expression_type = expression.type_

        if expression_type.is_compatible_strong(target_type):
            return expression

        assert isinstance(target_type, (rty.Integer, rty.Enumeration)), target_type

        self._session_context.referenced_types_body.append(ID(target_type.identifier))
        return expr.Conversion(target_type.identifier, expression)

    def _debug_output(self, string: str) -> List[CallStatement]:
        return [CallStatement("Ada.Text_IO.Put_Line", [String(string)])] if self._debug else []


def copy_id(identifier: ID) -> ID:
    return "RFLX_Copy_" + identifier


def context_id(identifier: rid.ID) -> ID:
    return ID(identifier + "_Ctx")


def buffer_id(identifier: rid.ID) -> ID:
    return ID(identifier + "_Buffer")


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

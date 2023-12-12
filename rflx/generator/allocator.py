from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass
from itertools import zip_longest
from typing import Optional

from rflx import ir, typing_ as rty
from rflx.ada import (
    Add,
    And,
    AndThen,
    Assignment,
    Call,
    Component,
    ContextItem,
    DynamicPredicate,
    Equal,
    ExpressionFunctionDeclaration,
    First,
    FunctionSpecification,
    InOutParameter,
    Last,
    NamedAggregate,
    NotEqual,
    NullStatement,
    Number,
    OrElse,
    OutParameter,
    Parameter,
    Postcondition,
    ProcedureSpecification,
    RecordType,
    Slice,
    SparkMode,
    SubprogramBody,
    SubprogramDeclaration,
    Subtype,
    UnitPart,
    UnrestrictedAccess,
    UseTypeClause,
    Variable,
    WithClause,
)
from rflx.error import Location
from rflx.identifier import ID
from rflx.integration import Integration

from . import const


@dataclass
class SlotInfo:
    size: int
    locations: Sequence[Location]


@dataclass
class NumberedSlotInfo(SlotInfo):
    slot_id: int
    global_: bool


class AllocatorGenerator:
    def __init__(self, session: ir.Session, integration: Integration, prefix: str = "") -> None:
        self._session = session
        self._prefix = prefix
        self._declaration_context: list[ContextItem] = []
        self._body_context: list[ContextItem] = []
        self._allocation_slots: dict[Location, int] = {}
        self._unit_part = UnitPart()
        self._integration = integration

        global_slots: list[SlotInfo] = self._allocate_global_slots()
        local_slots: list[SlotInfo] = self._allocate_local_slots()
        numbered_slots: list[NumberedSlotInfo] = []
        count = 1

        for slot in global_slots:
            numbered_slots.append(NumberedSlotInfo(slot.size, slot.locations, count, global_=True))
            count += 1
        for slot in local_slots:
            numbered_slots.append(NumberedSlotInfo(slot.size, slot.locations, count, global_=False))
            count += 1
        for slot in numbered_slots:
            for location in slot.locations:
                self._allocation_slots[location] = slot.slot_id

        self._create(numbered_slots)

        self._numbered_slots = numbered_slots

    @property
    def unit_identifier(self) -> ID:
        return self._session.identifier.parent * f"{self._session.identifier.name}_Allocator"

    @property
    def declaration_context(self) -> list[ContextItem]:
        return self._declaration_context

    @property
    def body_context(self) -> list[ContextItem]:
        return self._body_context

    @property
    def unit_part(self) -> UnitPart:
        return self._unit_part

    @property
    def required(self) -> bool:
        return bool(self._allocation_slots)

    def get_global_slot_ptrs(self) -> list[ID]:
        return [self._slot_name(s.slot_id) for s in self._numbered_slots if s.global_]

    def get_local_slot_ptrs(self) -> list[ID]:
        return [self._slot_name(s.slot_id) for s in self._numbered_slots if not s.global_]

    def get_slot_ptr(self, location: Optional[Location]) -> ID:
        assert location is not None
        slot_id: int = self._allocation_slots[location]
        return self._slot_name(slot_id)

    def get_size(self, variable: Optional[ID] = None, state: Optional[ID] = None) -> int:
        return self._integration.get_size(self._session.identifier, variable, state)

    @staticmethod
    def _slot_name(slot_id: int) -> ID:
        return ID(f"Slot_Ptr_{slot_id}")

    @staticmethod
    def _ptr_type(size: int) -> ID:
        return ID(f"Slot_Ptr_Type_{size}")

    def _create(self, slots: Sequence[NumberedSlotInfo]) -> None:
        self._unit_part += self._create_memory(slots)
        if slots:
            self._unit_part += self._create_ptr_subtypes(slots)
        self._unit_part += self._create_slots(slots)
        self._unit_part += self._create_init_pred(slots)
        self._unit_part += self._create_uninitialized_pred(slots)
        self._unit_part += self._create_init_proc(slots)
        self._unit_part += self._create_finalize_proc(slots)
        self._unit_part += self._create_global_allocated_pred(slots)

    @staticmethod
    def _create_memory(slots: Sequence[NumberedSlotInfo]) -> UnitPart:
        return UnitPart(
            [
                RecordType(
                    "Memory",
                    [
                        Component(
                            f"Slot_{slot.slot_id}",
                            Slice(
                                Variable(const.TYPES_BYTES),
                                First(const.TYPES_INDEX),
                                Add(First(const.TYPES_INDEX), Number(slot.size - 1)),
                            ),
                            NamedAggregate(("others", Number(0))),
                            aliased=True,
                        )
                        for slot in slots
                    ],
                ),
            ],
        )

    def _create_ptr_subtypes(self, slots: Sequence[NumberedSlotInfo]) -> UnitPart:
        unit = UnitPart(
            specification=[
                Subtype(
                    self._ptr_type(size),
                    const.TYPES_BYTES_PTR,
                    aspects=[
                        DynamicPredicate(
                            OrElse(
                                Equal(Variable(self._ptr_type(size)), Variable("null")),
                                AndThen(
                                    Equal(First(self._ptr_type(size)), First(const.TYPES_INDEX)),
                                    Equal(
                                        Last(self._ptr_type(size)),
                                        Add(First(const.TYPES_INDEX), Number(size - 1)),
                                    ),
                                ),
                            ),
                        ),
                    ],
                )
                for size in sorted({slot.size for slot in slots})
            ],
        )
        self._declaration_context.append(WithClause(self._prefix * const.TYPES_PACKAGE))
        self._declaration_context.append(UseTypeClause(self._prefix * const.TYPES_INDEX))
        self._declaration_context.append(UseTypeClause(self._prefix * const.TYPES_BYTES_PTR))
        return unit

    def _create_slots(self, slots: Sequence[NumberedSlotInfo]) -> UnitPart:
        self._declaration_context.append(WithClause(self._prefix * const.TYPES_PACKAGE))

        return UnitPart(
            [
                RecordType(
                    "Slots",
                    [
                        Component(self._slot_name(slot.slot_id), self._ptr_type(slot.size))
                        for slot in slots
                    ],
                ),
            ],
        )

    def _create_init_pred(self, slots: Sequence[NumberedSlotInfo]) -> UnitPart:
        return UnitPart(
            [
                ExpressionFunctionDeclaration(
                    FunctionSpecification("Initialized", "Boolean", [Parameter(["S"], "Slots")]),
                    And(
                        *[
                            NotEqual(
                                Variable("S" * self._slot_name(slot.slot_id)),
                                Variable("null"),
                            )
                            for slot in slots
                        ],
                    ),
                ),
            ],
        )

    def _create_uninitialized_pred(self, slots: Sequence[NumberedSlotInfo]) -> UnitPart:
        return UnitPart(
            [
                ExpressionFunctionDeclaration(
                    FunctionSpecification("Uninitialized", "Boolean", [Parameter(["S"], "Slots")]),
                    And(
                        *[
                            Equal(
                                Variable("S" * self._slot_name(slot.slot_id)),
                                Variable("null"),
                            )
                            for slot in slots
                        ],
                    ),
                ),
            ],
        )

    def _create_global_allocated_pred(self, slots: Sequence[NumberedSlotInfo]) -> UnitPart:
        return UnitPart(
            [
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "Global_Allocated",
                        "Boolean",
                        [Parameter(["S"], "Slots")],
                    ),
                    And(
                        *[
                            Equal(
                                Variable("S" * self._slot_name(slot.slot_id)),
                                Variable("null"),
                            )
                            for slot in slots
                            if slot.global_
                        ],
                        *[
                            NotEqual(
                                Variable("S" * self._slot_name(slot.slot_id)),
                                Variable("null"),
                            )
                            for slot in slots
                            if not slot.global_
                        ],
                    ),
                ),
            ],
        )

    def _create_init_proc(self, slots: Sequence[NumberedSlotInfo]) -> UnitPart:
        proc = ProcedureSpecification(
            "Initialize",
            [OutParameter(["S"], "Slots"), Parameter(["M"], "Memory")],
        )
        return UnitPart(
            [
                SubprogramDeclaration(proc, [Postcondition(Call("Initialized", [Variable("S")]))]),
            ],
            [
                SubprogramBody(
                    proc,
                    declarations=[],
                    statements=(
                        [
                            Assignment(
                                "S" * self._slot_name(slot.slot_id),
                                UnrestrictedAccess(Variable(ID(f"M.Slot_{slot.slot_id}"))),
                            )
                            for slot in slots
                        ]
                        if slots
                        else [NullStatement()]
                    ),
                    aspects=[SparkMode(off=True)],
                ),
            ],
        )

    def _create_finalize_proc(self, slots: Sequence[NumberedSlotInfo]) -> UnitPart:
        proc = ProcedureSpecification("Finalize", [InOutParameter(["S"], "Slots")])
        return UnitPart(
            [
                SubprogramDeclaration(
                    proc,
                    [Postcondition(Call("Uninitialized", [Variable("S")]))],
                ),
            ],
            [
                SubprogramBody(
                    proc,
                    declarations=[],
                    statements=(
                        [
                            Assignment(
                                "S" * self._slot_name(slot.slot_id),
                                Variable("null"),
                            )
                            for slot in slots
                        ]
                        if slots
                        else [NullStatement()]
                    ),
                    aspects=[SparkMode(off=True)],
                ),
            ],
        )

    @staticmethod
    def _needs_allocation(type_: rty.Type) -> bool:
        return isinstance(type_, (rty.Message, rty.Sequence))

    def _allocate_global_slots(self) -> list[SlotInfo]:
        slots = []
        for d in self._session.declarations:
            if self._needs_allocation(d.type_):
                assert d.location is not None
                slots.append(SlotInfo(self.get_size(d.identifier), [d.location]))
        return slots

    @staticmethod
    def _scope(state: ir.State, var_id: ID) -> Optional[ID]:
        """
        Return the scope of the variable var_id.

        Return the ID of the state if var_id was defined in that state, otherwise
        return None to indicate that var_id is a global variable of the session.
        """
        if any(var_id == d.identifier for d in state.declarations):
            return state.identifier.name
        return None

    def _allocate_local_slots(
        self,
    ) -> list[SlotInfo]:
        """
        Allocate slots for state variables and state actions.

        We collect the allocation requirements (pairs of locations and sizes) for each
        state in a list, then sort these lists in descending order of sizes.
        Then, we imagine a matrix where each line is one of these lists (filling
        up with zero sizes to make the lists the same length). For each column of
        that matrix, we create a SlotInfo object whose size is the maximal size for
        that column, and the locations are all locations in that column.
        """

        @dataclass
        class AllocationRequirement:
            location: Optional[Location]
            size: int

        def determine_allocation_requirements(
            statements: Sequence[ir.Stmt],
            state: ir.State,
        ) -> list[AllocationRequirement]:
            alloc_requirements = []

            for statement in statements:
                if isinstance(statement, ir.VarDecl) and self._needs_allocation(statement.type_):
                    alloc_requirements.append(
                        AllocationRequirement(
                            statement.location,
                            self.get_size(statement.identifier, state.identifier.name),
                        ),
                    )
                if (
                    isinstance(statement, ir.Assign)
                    and isinstance(statement.expression, ir.Comprehension)
                    and isinstance(statement.expression.sequence.type_, rty.Sequence)
                    and isinstance(statement.expression.sequence.type_.element, rty.Message)
                    and isinstance(statement.expression.sequence, (ir.Var, ir.FieldAccess))
                ):
                    if isinstance(statement.expression.sequence, ir.FieldAccess):
                        identifier = statement.expression.sequence.message
                    else:
                        assert isinstance(statement.expression.sequence, ir.Var)
                        identifier = statement.expression.sequence.identifier
                    alloc_requirements.append(
                        AllocationRequirement(
                            statement.location,
                            self.get_size(identifier, self._scope(state, identifier)),
                        ),
                    )
                if isinstance(statement, ir.Assign) and isinstance(statement.expression, ir.Head):
                    identifier = statement.expression.prefix
                    alloc_requirements.append(
                        AllocationRequirement(
                            statement.location,
                            self.get_size(identifier, self._scope(state, identifier)),
                        ),
                    )
                if isinstance(statement, ir.Assign) and isinstance(statement.expression, ir.Find):
                    if isinstance(statement.expression.sequence, ir.Var):
                        identifier = statement.expression.sequence.identifier
                    elif isinstance(statement.expression.sequence, ir.FieldAccess):
                        identifier = statement.expression.sequence.message
                    else:
                        assert False
                    alloc_requirements.append(
                        AllocationRequirement(
                            statement.location,
                            self.get_size(identifier, self._scope(state, identifier)),
                        ),
                    )
                if isinstance(statement, ir.Assign) and isinstance(
                    statement.expression,
                    (ir.Comprehension, ir.Find),
                ):
                    alloc_requirements.extend(
                        determine_allocation_requirements(
                            statement.expression.selector.stmts,
                            state,
                        ),
                    )
                    alloc_requirements.extend(
                        determine_allocation_requirements(
                            statement.expression.condition.stmts,
                            state,
                        ),
                    )

            return alloc_requirements

        alloc_requirements_per_state = [
            determine_allocation_requirements(state.actions, state)
            for state in self._session.states
        ]

        for state_requirements in alloc_requirements_per_state:
            state_requirements.sort(key=lambda x: x.size, reverse=True)

        slots = []
        for requirements_per_slot in zip_longest(
            *alloc_requirements_per_state,
            fillvalue=AllocationRequirement(None, 0),
        ):
            size = max(x.size for x in requirements_per_slot)
            locations = [x.location for x in requirements_per_slot if x.location is not None]
            slots.append(SlotInfo(size, locations))
        return slots

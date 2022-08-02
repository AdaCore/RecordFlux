from __future__ import annotations

from dataclasses import dataclass
from itertools import zip_longest
from typing import Dict, List, Optional, Sequence

from rflx import expression as expr, typing_ as rty
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
from rflx.model import Session, State, declaration as decl, statement as stmt

from . import const


@dataclass
class SlotInfo:
    size: int
    locations: List[Location]


@dataclass
class NumberedSlotInfo(SlotInfo):
    slot_id: int
    global_: bool


class AllocatorGenerator:  # pylint: disable = too-many-instance-attributes
    def __init__(self, session: Session, integration: Integration, prefix: str = "") -> None:
        self._session = session
        self._prefix = prefix
        self._declaration_context: List[ContextItem] = []
        self._body_context: List[ContextItem] = []
        self._allocation_slots: Dict[Location, int] = {}
        self._unit_part = UnitPart()
        self._integration = integration

        global_slots: List[SlotInfo] = self._allocate_global_slots()
        local_slots: List[SlotInfo] = self._allocate_local_slots()
        numbered_slots: List[NumberedSlotInfo] = []
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
    def declaration_context(self) -> List[ContextItem]:
        return self._declaration_context

    @property
    def body_context(self) -> List[ContextItem]:
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
                )
            ]
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
                            )
                        )
                    ],
                )
                for size in sorted({slot.size for slot in slots})
            ]
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
                )
            ]
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
                        ]
                    ),
                )
            ]
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
                        ]
                    ),
                )
            ]
        )

    def _create_global_allocated_pred(self, slots: Sequence[NumberedSlotInfo]) -> UnitPart:
        return UnitPart(
            [
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "Global_Allocated", "Boolean", [Parameter(["S"], "Slots")]
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
                )
            ]
        )

    def _create_init_proc(self, slots: Sequence[NumberedSlotInfo]) -> UnitPart:
        proc = ProcedureSpecification(
            "Initialize", [OutParameter(["S"], "Slots"), Parameter(["M"], "Memory")]
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
                )
            ],
        )

    def _create_finalize_proc(self, slots: Sequence[NumberedSlotInfo]) -> UnitPart:
        proc = ProcedureSpecification("Finalize", [InOutParameter(["S"], "Slots")])
        return UnitPart(
            [
                SubprogramDeclaration(
                    proc, [Postcondition(Call("Uninitialized", [Variable("S")]))]
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
                )
            ],
        )

    @staticmethod
    def _needs_allocation(type_: rty.Type) -> bool:
        return isinstance(type_, (rty.Message, rty.Sequence))

    def _allocate_global_slots(self) -> List[SlotInfo]:
        slots = []
        for d in self._session.declarations.values():
            if isinstance(d, decl.VariableDeclaration) and self._needs_allocation(d.type_):
                assert d.location is not None
                slots.append(SlotInfo(self.get_size(d.identifier), [d.location]))
        return slots

    @staticmethod
    def _scope(state: State, var_id: ID) -> Optional[ID]:
        """
        Return the scope of the variable var_id.

        Return the ID of the state if var_id was defined in that state, otherwise
        return None to indicate that var_id is a global variable of the session.
        """
        if var_id in state.declarations.keys():
            return state.identifier.name
        return None

    def _allocate_local_slots(self) -> List[SlotInfo]:  # pylint: disable-next = too-many-branches
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

        alloc_requirements_per_state: List[List[AllocationRequirement]] = []
        for s in self._session.states:
            state_requirements = []
            for d in s.declarations.values():
                if isinstance(d, decl.VariableDeclaration) and self._needs_allocation(d.type_):
                    state_requirements.append(
                        AllocationRequirement(
                            d.location, self.get_size(d.identifier, s.identifier.name)
                        )
                    )
            for a in s.actions:
                if (
                    isinstance(a, stmt.VariableAssignment)
                    and isinstance(a.expression, expr.Comprehension)
                    and isinstance(a.expression.sequence.type_, rty.Sequence)
                    and isinstance(a.expression.sequence.type_.element, rty.Message)
                    and isinstance(a.expression.sequence, (expr.Selected, expr.Variable))
                ):
                    if isinstance(a.expression.sequence, expr.Selected) and isinstance(
                        a.expression.sequence.prefix, expr.Variable
                    ):
                        identifier = a.expression.sequence.prefix.identifier
                    else:
                        assert isinstance(a.expression.sequence, expr.Variable)
                        identifier = a.expression.sequence.identifier
                    state_requirements.append(
                        AllocationRequirement(
                            a.location,
                            self.get_size(identifier, self._scope(s, identifier)),
                        )
                    )
                if isinstance(a, stmt.VariableAssignment) and isinstance(a.expression, expr.Head):
                    if isinstance(a.expression.prefix, expr.Comprehension) and isinstance(
                        a.expression.prefix.sequence, expr.Variable
                    ):
                        identifier = a.expression.prefix.sequence.identifier
                    elif (
                        isinstance(a.expression.prefix, expr.Comprehension)
                        and isinstance(a.expression.prefix.sequence, expr.Selected)
                        and isinstance(a.expression.prefix.sequence.prefix, expr.Variable)
                    ):
                        identifier = a.expression.prefix.sequence.prefix.identifier
                    else:
                        assert isinstance(a.expression.prefix, expr.Variable)
                        identifier = a.expression.prefix.identifier
                    state_requirements.append(
                        AllocationRequirement(
                            a.location,
                            self.get_size(identifier, self._scope(s, identifier)),
                        )
                    )

            alloc_requirements_per_state.append(state_requirements)

        for state_requirements in alloc_requirements_per_state:
            state_requirements.sort(key=lambda x: x.size, reverse=True)

        slots = []
        for requirements_per_slot in zip_longest(
            *alloc_requirements_per_state, fillvalue=AllocationRequirement(None, 0)
        ):
            size = max(x.size for x in requirements_per_slot)
            locations = [x.location for x in requirements_per_slot if x.location is not None]
            slots.append(SlotInfo(size, locations))
        return slots

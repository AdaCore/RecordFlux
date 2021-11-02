from typing import Dict, List, Optional, Sequence, Set

from rflx import expression as expr, typing_ as rty
from rflx.ada import (
    ID,
    Add,
    And,
    AndThen,
    Assignment,
    Call,
    ContextItem,
    Declaration,
    DynamicPredicate,
    Equal,
    ExpressionFunctionDeclaration,
    First,
    FunctionSpecification,
    Last,
    NamedAggregate,
    NotEqual,
    Number,
    ObjectDeclaration,
    OrElse,
    Postcondition,
    Pragma,
    PragmaStatement,
    ProcedureSpecification,
    SparkMode,
    Statement,
    String,
    SubprogramBody,
    SubprogramDeclaration,
    Subtype,
    UnitPart,
    UnrestrictedAccess,
    UseTypeClause,
    ValueRange,
    Variable,
    WithClause,
)
from rflx.error import Location
from rflx.model import Session, declaration as decl, statement as stmt

from . import const


class AllocatorGenerator:  # pylint: disable = too-many-instance-attributes
    def __init__(self, session: Session, prefix: str = "") -> None:
        self._session = session
        self._prefix = prefix
        self._declaration_context: List[ContextItem] = []
        self._body_context: List[ContextItem] = []
        self._allocation_map: Dict[Location, int] = {}
        self._unit_part = UnitPart(specification=[Pragma("Elaborate_Body")])
        self._all_slots: Set[int] = set()
        self._global_slots: Set[int] = set()
        self._allocate_slots()
        self._create()

    @property
    def unit_identifier(self) -> ID:
        return ID(self._session.identifier.parent * f"{self._session.identifier.name}_Allocator")

    @property
    def declaration_context(self) -> List[ContextItem]:
        return self._declaration_context

    @property
    def body_context(self) -> List[ContextItem]:
        return self._body_context

    @property
    def unit_part(self) -> UnitPart:
        return self._unit_part

    def _slot_id(self, slot_id: int, qualified: bool = False) -> ID:
        base = f"Slot_Ptr_{slot_id}"
        return ID(self.unit_identifier * base) if qualified else ID(base)

    def get_slot_ptr(self, location: Optional[Location]) -> Variable:
        assert location is not None
        slot_id: int = self._allocation_map[location]
        return Variable(self._slot_id(slot_id, qualified=True))

    def free_buffer(self, identifier: ID, location: Optional[Location]) -> Sequence[Statement]:
        assert location is not None
        slot_id = self._allocation_map[location]
        return [
            PragmaStatement("Warnings", [Variable("Off"), String("unused assignment")]),
            Assignment(
                self._slot_id(slot_id, qualified=True),
                Variable(identifier),
            ),
            PragmaStatement("Warnings", [Variable("On"), String("unused assignment")]),
        ]

    @staticmethod
    def _ptr_type() -> ID:
        return ID("Slot_Ptr_Type")

    def _create(self) -> None:
        self._unit_part += self._create_ptr_subtype()
        self._unit_part += self._create_slots()
        self._unit_part += self._create_init_pred()
        self._unit_part += self._create_init_proc()
        self._unit_part += self._create_global_allocated_pred()

    def _create_ptr_subtype(self) -> UnitPart:
        pred = OrElse(
            Equal(Variable(self._ptr_type()), Variable("null")),
            AndThen(
                Equal(First(self._ptr_type()), First(const.TYPES_INDEX)),
                Equal(Last(self._ptr_type()), Add(First(const.TYPES_INDEX), Number(4095))),
            ),
        )
        self._declaration_context.append(WithClause(self._prefix * const.TYPES_PACKAGE))
        self._declaration_context.append(UseTypeClause(self._prefix * const.TYPES_INDEX))
        self._declaration_context.append(UseTypeClause(self._prefix * const.TYPES_BYTES_PTR))
        unit = UnitPart(
            specification=[
                Subtype(self._ptr_type(), const.TYPES_BYTES_PTR, aspects=[DynamicPredicate(pred)]),
            ]
        )
        return unit

    def _create_slots(self) -> UnitPart:
        array_decls: List[Declaration] = [
            ObjectDeclaration(
                [ID(f"Slot_{i}")],
                const.TYPES_BYTES,
                aliased=True,
                expression=NamedAggregate(
                    (
                        ValueRange(
                            First(const.TYPES_INDEX), Add(First(const.TYPES_INDEX), Number(4095))
                        ),
                        First(const.TYPES_BYTE),
                    )
                ),
            )
            for i in self._all_slots
        ]

        pointer_decls: List[Declaration] = [
            ObjectDeclaration(
                [self._slot_id(i)],
                self._ptr_type(),
            )
            for i in self._all_slots
        ]
        self._declaration_context.append(WithClause(self._prefix * const.TYPES_PACKAGE))
        unit = UnitPart(specification=pointer_decls, body=array_decls)
        return unit

    def _create_init_pred(self) -> UnitPart:
        return UnitPart(
            [
                ExpressionFunctionDeclaration(
                    FunctionSpecification("Initialized", "Boolean"),
                    And(
                        *[
                            NotEqual(
                                Variable(self._slot_id(i)),
                                Variable("null"),
                            )
                            for i in self._all_slots
                        ]
                    ),
                )
            ]
        )

    def _create_global_allocated_pred(self) -> UnitPart:
        return UnitPart(
            [
                ExpressionFunctionDeclaration(
                    FunctionSpecification("Global_Allocated", "Boolean"),
                    And(
                        *[
                            Equal(
                                Variable(self._slot_id(i)),
                                Variable("null"),
                            )
                            for i in self._all_slots
                            if i in self._global_slots
                        ],
                        *[
                            NotEqual(
                                Variable(self._slot_id(i)),
                                Variable("null"),
                            )
                            for i in self._all_slots
                            if i not in self._global_slots
                        ],
                    ),
                )
            ]
        )

    def _create_init_proc(self) -> UnitPart:
        assignments = [
            Assignment(self._slot_id(i), UnrestrictedAccess(Variable(ID(f"Slot_{i}"))))
            for i in self._all_slots
        ]
        proc = ProcedureSpecification(ID("Initialize"))
        return UnitPart(
            [SubprogramDeclaration(proc, [Postcondition(Call("Initialized"))])],
            [
                SubprogramBody(
                    proc, declarations=[], statements=assignments, aspects=[SparkMode(off=True)]
                )
            ],
        )

    @staticmethod
    def _needs_allocation(type_: rty.Type) -> bool:
        return isinstance(type_, (rty.Message, rty.Sequence))

    def _allocate_slots(self) -> None:
        """Create memory slots for each construct in the session that requires memory."""
        count: int = 0

        def insert(loc: Optional[Location]) -> None:
            nonlocal count
            count += 1
            assert loc is not None
            self._allocation_map[loc] = count
            self._all_slots.add(count)

        for d in self._session.declarations.values():
            if isinstance(d, decl.VariableDeclaration) and self._needs_allocation(d.type_):
                insert(d.location)
                self._global_slots.add(count)

        global_count = count

        for s in self._session.states:
            count = global_count
            for d in s.declarations.values():
                if isinstance(d, decl.VariableDeclaration) and self._needs_allocation(d.type_):
                    insert(d.location)
            for a in s.actions:
                if (
                    isinstance(a, stmt.Assignment)
                    and isinstance(a.expression, expr.Comprehension)
                    and isinstance(a.expression.sequence.type_, rty.Sequence)
                    and isinstance(a.expression.sequence.type_.element, rty.Message)
                    and isinstance(a.expression.sequence, (expr.Selected, expr.Variable))
                ):
                    insert(a.location)
                if isinstance(a, stmt.Assignment) and isinstance(a.expression, expr.Head):
                    insert(a.location)
                if (
                    isinstance(a, stmt.Write)
                    and isinstance(a.parameter.type_, rty.Message)
                    and not isinstance(a.parameter, expr.Variable)
                ):
                    insert(a.location)

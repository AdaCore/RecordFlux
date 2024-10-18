from __future__ import annotations

from collections.abc import Callable

import pytest

from rflx import ir, ty
from rflx.generator.allocator import AllocatorGenerator
from rflx.identifier import ID, id_generator
from rflx.integration import Integration, IntegrationFile, StateMachineIntegration
from rflx.rapidflux import NO_LOCATION, Location
from tests.data import models
from tests.unit.generator.generator_test import create_integration
from tests.unit.generator.state_machine_test import dummy_state_machine


@pytest.mark.parametrize(
    ("state_machine", "required", "global_slot_ptrs", "local_slot_ptrs"),
    [
        (
            dummy_state_machine,
            False,
            [],
            [],
        ),
        (
            lambda: ir.StateMachine(
                identifier=ID("P::S"),
                states=[
                    ir.State(
                        "State",
                        [
                            ir.Transition(
                                "Final",
                                ir.ComplexExpr([], ir.BoolVal(value=True)),
                                None,
                                NO_LOCATION,
                            ),
                        ],
                        None,
                        [
                            ir.VarDecl(
                                "Y",
                                ty.Message("T"),
                                origin=ir.ConstructedOrigin("X : T", Location((2, 2))),
                            ),
                        ],
                        None,
                        NO_LOCATION,
                    ),
                ],
                declarations=[
                    ir.VarDecl(
                        "X",
                        ty.Message("T"),
                        origin=ir.ConstructedOrigin("X : T", Location((1, 1))),
                    ),
                ],
                parameters=[],
                types={t.identifier: t for t in models.universal_model().types},
                location=NO_LOCATION,
                variable_id=id_generator(),
            ),
            True,
            [ID("Slot_Ptr_1")],
            [ID("Slot_Ptr_2")],
        ),
        (
            lambda: ir.StateMachine(
                identifier=ID("P::S"),
                states=[
                    ir.State(
                        "State",
                        [
                            ir.Transition(
                                "Final",
                                ir.ComplexExpr([], ir.BoolVal(value=True)),
                                None,
                                NO_LOCATION,
                            ),
                        ],
                        None,
                        [ir.Read("C", ir.ObjVar("X", ty.Message("T")))],
                        None,
                        NO_LOCATION,
                    ),
                ],
                declarations=[
                    ir.VarDecl(
                        "X",
                        ty.Message("T"),
                        origin=ir.ConstructedOrigin("X : T", Location((1, 1))),
                    ),
                ],
                parameters=[],
                types={t.identifier: t for t in models.universal_model().types},
                location=NO_LOCATION,
                variable_id=id_generator(),
            ),
            True,
            [ID("Slot_Ptr_1")],
            [],
        ),
    ],
)
def test_allocator(
    state_machine: Callable[[], ir.StateMachine],
    required: bool,
    global_slot_ptrs: list[ID],
    local_slot_ptrs: list[ID],
) -> None:
    allocator = AllocatorGenerator(state_machine(), Integration())
    assert allocator.required == required
    assert allocator.get_global_slot_ptrs() == global_slot_ptrs
    assert allocator.get_local_slot_ptrs() == local_slot_ptrs


@pytest.mark.parametrize(
    ("state_machine", "required", "global_slot_ptrs", "local_slot_ptrs"),
    [
        (
            lambda: ir.StateMachine(
                identifier=ID("P::S"),
                states=[
                    ir.State(
                        "State",
                        [
                            ir.Transition(
                                "Final",
                                ir.ComplexExpr([], ir.BoolVal(value=True)),
                                None,
                                NO_LOCATION,
                            ),
                        ],
                        None,
                        [ir.Read("C", ir.ObjVar("X", ty.Message("T")))],
                        None,
                        NO_LOCATION,
                    ),
                ],
                declarations=[
                    ir.VarDecl(
                        "X",
                        ty.Message("T"),
                        origin=ir.ConstructedOrigin("X : T", Location((1, 1))),
                    ),
                ],
                parameters=[],
                types={},
                location=NO_LOCATION,
                variable_id=id_generator(),
            ),
            False,
            [],
            [],
        ),
        (
            lambda: ir.StateMachine(
                identifier=ID("P::S"),
                states=[
                    ir.State(
                        "State",
                        [
                            ir.Transition(
                                "Final",
                                ir.ComplexExpr([], ir.BoolVal(value=True)),
                                None,
                                NO_LOCATION,
                            ),
                        ],
                        None,
                        [ir.Read("C", ir.ObjVar("X", ty.Message("T")))],
                        None,
                        NO_LOCATION,
                    ),
                ],
                declarations=[
                    ir.VarDecl(
                        "X",
                        ty.Message("T"),
                        origin=ir.ConstructedOrigin("X : T", Location((1, 1))),
                    ),
                    ir.VarDecl(
                        "Y",
                        ty.Message("T"),
                        origin=ir.ConstructedOrigin("X : T", Location((2, 2))),
                    ),
                ],
                parameters=[],
                types={},
                location=NO_LOCATION,
                variable_id=id_generator(),
            ),
            True,
            [ID("Slot_Ptr_1")],
            [],
        ),
    ],
)
def test_allocator_with_external_io_buffers(
    state_machine: Callable[[], ir.StateMachine],
    required: bool,
    global_slot_ptrs: list[ID],
    local_slot_ptrs: list[ID],
) -> None:
    allocator = AllocatorGenerator(
        state_machine(),
        create_integration(
            {
                "p": IntegrationFile(
                    Machine={
                        "S": StateMachineIntegration(Buffer_Size=None, External_IO_Buffers=True),
                    },
                ),
            },
        ),
    )
    assert allocator.required == required
    assert allocator.get_global_slot_ptrs() == global_slot_ptrs
    assert allocator.get_local_slot_ptrs() == local_slot_ptrs
    assert allocator.is_externally_managed(Location((1, 1)))
    assert not allocator.is_externally_managed(Location((2, 2)))

import pytest

from rflx import ada, ada_prefix
from rflx.identifier import ID


@pytest.mark.parametrize(
    ("model", "expected"),
    [
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "Template.P",
                    declarations=[
                        ada.SignedIntegerType(
                            "T",
                            first=ada.Number(0),
                            last=ada.Variable("Template.D"),
                        ),
                    ],
                    aspects=[
                        ada.Initializes("Local", "Template.External"),
                        ada.AbstractState("Local", "Template.External"),
                        ada.InitialCondition(
                            ada.Less(ada.Variable("C"), ada.Variable("Template.P")),
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "New.Prefix.P",
                    declarations=[
                        ada.SignedIntegerType(
                            "T",
                            first=ada.Number(0),
                            last=ada.Variable("New.Prefix.D"),
                        ),
                    ],
                    aspects=[
                        ada.Initializes("Local", "New.Prefix.External"),
                        ada.AbstractState("Local", "New.Prefix.External"),
                        ada.InitialCondition(
                            ada.Less(ada.Variable("C"), ada.Variable("New.Prefix.P")),
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody("Template.P"),
                formal_parameters=[
                    ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                ],
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
                formal_parameters=[
                    ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                ],
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[ada.WithClause("Template.Q")],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[ada.WithClause("New.Prefix.Q")],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="Template.P",
                    declarations=[
                        ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                        ada.SubprogramDeclaration(
                            specification=ada.ProcedureSpecification(
                                "S",
                                parameters=[
                                    ada.Parameter(["P1"], "Template.T"),
                                    ada.AccessParameter(["P2"], "Template.U"),
                                ],
                            ),
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="New.Prefix.P",
                    declarations=[
                        ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                        ada.SubprogramDeclaration(
                            specification=ada.ProcedureSpecification(
                                "S",
                                parameters=[
                                    ada.Parameter(["P1"], "New.Prefix.T"),
                                    ada.AccessParameter(["P2"], "New.Prefix.U"),
                                ],
                            ),
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="Template.P",
                    declarations=[
                        ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                        ada.SubprogramDeclaration(
                            specification=ada.FunctionSpecification(
                                "F",
                                return_type="Template.T",
                                parameters=[ada.Parameter(["P1"], "Template.T")],
                            ),
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="New.Prefix.P",
                    declarations=[
                        ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                        ada.SubprogramDeclaration(
                            specification=ada.FunctionSpecification(
                                "F",
                                return_type="New.Prefix.T",
                                parameters=[ada.Parameter(["P1"], "New.Prefix.T")],
                            ),
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "Template.P",
                    declarations=[ada.Subtype(identifier="T", base_identifier="Template.U")],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "New.Prefix.P",
                    declarations=[ada.Subtype(identifier="T", base_identifier="New.Prefix.U")],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "Template.P",
                    declarations=[
                        ada.SubprogramRenamingDeclaration(
                            specification=ada.ProcedureSpecification("Proc"),
                            subprogram_identifier="Template.Q",
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "New.Prefix.P",
                    declarations=[
                        ada.SubprogramRenamingDeclaration(
                            specification=ada.ProcedureSpecification("Proc"),
                            subprogram_identifier="New.Prefix.Q",
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[ada.UseTypeClause("Template.Q")],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[ada.UseTypeClause("New.Prefix.Q")],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[ada.UseTypeClause("Template.Q", "Template.R")],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[ada.UseTypeClause("New.Prefix.Q", "New.Prefix.R")],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[ada.UsePackageClause("Template.Q")],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[ada.UsePackageClause("New.Prefix.Q")],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[ada.UsePackageClause("Template.Q")],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[ada.UsePackageClause("New.Prefix.Q")],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody("Template.P"),
                formal_parameters=[
                    ada.FormalPackageDeclaration("O", "Template.Q"),
                ],
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
                formal_parameters=[
                    ada.FormalPackageDeclaration("O", "New.Prefix.Q"),
                ],
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "Template.P",
                    private_declarations=[
                        ada.UseTypeClause("Template.Q"),
                        ada.UseTypeClause("Template.R"),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "New.Prefix.P",
                    private_declarations=[
                        ada.UseTypeClause("New.Prefix.Q"),
                        ada.UseTypeClause("New.Prefix.R"),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody("Template.P", statements=[ada.CallStatement("Template.C")]),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody(
                    "New.Prefix.P",
                    statements=[ada.CallStatement("New.Prefix.C")],
                ),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody(
                    "Template.P",
                    statements=[
                        ada.CallStatement(
                            "Template.C",
                            named_arguments={ID("P1"): ada.Variable("Template.V")},
                        ),
                    ],
                ),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody(
                    "New.Prefix.P",
                    statements=[
                        ada.CallStatement(
                            "New.Prefix.C",
                            named_arguments={ID("P1"): ada.Variable("New.Prefix.V")},
                        ),
                    ],
                ),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody(
                    "Template.P",
                    statements=[
                        ada.CallStatement("Template.C", arguments=[ada.Variable("Template.X")]),
                    ],
                ),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody(
                    "New.Prefix.P",
                    statements=[
                        ada.CallStatement("New.Prefix.C", arguments=[ada.Variable("New.Prefix.X")]),
                    ],
                ),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody(
                    "Template.P",
                    statements=[
                        ada.Assignment("Template.V", expression=ada.Variable("Template.X")),
                    ],
                ),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody(
                    "New.Prefix.P",
                    statements=[
                        ada.Assignment("New.Prefix.V", expression=ada.Variable("New.Prefix.X")),
                    ],
                ),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody(
                    "Template.P",
                    statements=[
                        ada.Assignment(
                            ada.Variable("Template.X"),
                            expression=ada.Call("Template.Y", arguments=[ada.Number(5)]),
                        ),
                    ],
                ),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody(
                    "New.Prefix.P",
                    statements=[
                        ada.Assignment(
                            ada.Variable("New.Prefix.X"),
                            expression=ada.Call("New.Prefix.Y", arguments=[ada.Number(5)]),
                        ),
                    ],
                ),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody(
                    "Template.P",
                    statements=[
                        ada.Assignment(
                            ada.Variable("Template.X"),
                            expression=ada.Add(
                                ada.Variable("Template.Y"),
                                ada.Variable("Template.Z"),
                            ),
                        ),
                    ],
                ),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody(
                    "New.Prefix.P",
                    statements=[
                        ada.Assignment(
                            ada.Variable("New.Prefix.X"),
                            expression=ada.Add(
                                ada.Variable("New.Prefix.Y"),
                                ada.Variable("New.Prefix.Z"),
                            ),
                        ),
                    ],
                ),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody(
                    "Template.P",
                    statements=[
                        ada.Assignment(
                            ada.Variable("Template.X"),
                            expression=ada.NamedAggregate(
                                ("A", ada.Variable("Template.Y")),
                                ("B", ada.Variable("Template.Z")),
                            ),
                        ),
                    ],
                ),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody(
                    "New.Prefix.P",
                    statements=[
                        ada.Assignment(
                            ada.Variable("New.Prefix.X"),
                            expression=ada.NamedAggregate(
                                ("A", ada.Variable("New.Prefix.Y")),
                                ("B", ada.Variable("New.Prefix.Z")),
                            ),
                        ),
                    ],
                ),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody("Template.P"),
                formal_parameters=[
                    ada.FormalSubprogramDeclaration(
                        specification=ada.ProcedureSpecification(
                            "P",
                            parameters=[ada.Parameter(["P1"], "Template.T", ada.Number(42))],
                        ),
                    ),
                ],
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
                formal_parameters=[
                    ada.FormalSubprogramDeclaration(
                        specification=ada.ProcedureSpecification(
                            "P",
                            parameters=[ada.Parameter(["P1"], "New.Prefix.T", ada.Number(42))],
                        ),
                    ),
                ],
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody("Template.P"),
                formal_parameters=[
                    ada.FormalSubprogramDeclaration(
                        specification=ada.FunctionSpecification(
                            "P",
                            return_type="Template.RT",
                            parameters=[
                                ada.Parameter(["P1"], "Template.T", ada.Variable("Template.C")),
                            ],
                        ),
                    ),
                ],
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
                formal_parameters=[
                    ada.FormalSubprogramDeclaration(
                        specification=ada.FunctionSpecification(
                            "P",
                            return_type="New.Prefix.RT",
                            parameters=[
                                ada.Parameter(["P1"], "New.Prefix.T", ada.Variable("New.Prefix.C")),
                            ],
                        ),
                    ),
                ],
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="Template.P",
                    declarations=[
                        ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                        ada.SubprogramDeclaration(
                            specification=ada.ProcedureSpecification(
                                "S",
                                parameters=[ada.Parameter(["P1"], "Template.T")],
                            ),
                            aspects=[
                                ada.Precondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("Template.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                                ada.Postcondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("Template.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="New.Prefix.P",
                    declarations=[
                        ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                        ada.SubprogramDeclaration(
                            specification=ada.ProcedureSpecification(
                                "S",
                                parameters=[ada.Parameter(["P1"], "New.Prefix.T")],
                            ),
                            aspects=[
                                ada.Precondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("New.Prefix.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                                ada.Postcondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("New.Prefix.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="Template.P",
                    declarations=[
                        ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                        ada.SubprogramDeclaration(
                            specification=ada.ProcedureSpecification(
                                "S",
                                parameters=[ada.Parameter(["P1"], "Template.T")],
                            ),
                            aspects=[
                                ada.ClassPrecondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("Template.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                                ada.ClassPostcondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("Template.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="New.Prefix.P",
                    declarations=[
                        ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                        ada.SubprogramDeclaration(
                            specification=ada.ProcedureSpecification(
                                "S",
                                parameters=[ada.Parameter(["P1"], "New.Prefix.T")],
                            ),
                            aspects=[
                                ada.ClassPrecondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("New.Prefix.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                                ada.ClassPostcondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("New.Prefix.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="Template.P",
                    declarations=[
                        ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                        ada.SubprogramDeclaration(
                            specification=ada.FunctionSpecification(
                                "S",
                                return_type="T",
                                parameters=[ada.Parameter(["P1"], "Template.T")],
                            ),
                            aspects=[
                                ada.Precondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("Template.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                                ada.Postcondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("Template.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="New.Prefix.P",
                    declarations=[
                        ada.SignedIntegerType("T", first=ada.Number(0), last=ada.Number(124)),
                        ada.SubprogramDeclaration(
                            specification=ada.FunctionSpecification(
                                "S",
                                return_type="T",
                                parameters=[ada.Parameter(["P1"], "New.Prefix.T")],
                            ),
                            aspects=[
                                ada.Precondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("New.Prefix.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                                ada.Postcondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("New.Prefix.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="Template.P",
                    declarations=[
                        ada.RecordType(
                            identifier="T",
                            components=[
                                ada.Component("C1", "Template.T", ada.Variable("Template.C")),
                            ],
                            aspects=[
                                ada.DynamicPredicate(
                                    ada.Less(ada.Variable("Template.C"), ada.Number(42)),
                                ),
                            ],
                        ),
                        ada.DerivedRecordType(
                            "D",
                            "Template.R",
                            [
                                ada.Component(
                                    "E",
                                    "Template.T",
                                    ada.Variable("Template.C"),
                                    aliased=True,
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="New.Prefix.P",
                    declarations=[
                        ada.RecordType(
                            identifier="T",
                            components=[
                                ada.Component("C1", "New.Prefix.T", ada.Variable("New.Prefix.C")),
                            ],
                            aspects=[
                                ada.DynamicPredicate(
                                    ada.Less(ada.Variable("New.Prefix.C"), ada.Number(42)),
                                ),
                            ],
                        ),
                        ada.DerivedRecordType(
                            "D",
                            "New.Prefix.R",
                            [
                                ada.Component(
                                    "E",
                                    "New.Prefix.T",
                                    ada.Variable("New.Prefix.C"),
                                    aliased=True,
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="Template.P",
                    declarations=[
                        ada.ExpressionFunctionDeclaration(
                            specification=ada.FunctionSpecification(
                                "S",
                                return_type="T",
                                parameters=[ada.Parameter(["P1"], "Template.T")],
                            ),
                            expression=ada.Less(ada.Variable("Template.C"), ada.Number(42)),
                            aspects=[
                                ada.SubprogramVariant(
                                    ada.Decreases(
                                        ada.Add(
                                            ada.Variable("P1"),
                                            ada.Variable("Template.C"),
                                        ),
                                    ),
                                ),
                                ada.SubprogramVariant(
                                    ada.Increases(
                                        ada.Sub(
                                            ada.Variable("P1"),
                                            ada.Variable("Template.C"),
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="New.Prefix.P",
                    declarations=[
                        ada.ExpressionFunctionDeclaration(
                            specification=ada.FunctionSpecification(
                                "S",
                                return_type="T",
                                parameters=[ada.Parameter(["P1"], "New.Prefix.T")],
                            ),
                            expression=ada.Less(ada.Variable("New.Prefix.C"), ada.Number(42)),
                            aspects=[
                                ada.SubprogramVariant(
                                    ada.Decreases(
                                        ada.Add(
                                            ada.Variable("P1"),
                                            ada.Variable("New.Prefix.C"),
                                        ),
                                    ),
                                ),
                                ada.SubprogramVariant(
                                    ada.Increases(
                                        ada.Sub(
                                            ada.Variable("P1"),
                                            ada.Variable("New.Prefix.C"),
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="Template.P",
                    declarations=[
                        ada.ExpressionFunctionDeclaration(
                            specification=ada.FunctionSpecification(
                                "S",
                                return_type="T",
                                parameters=[ada.Parameter(["P1"], "Template.T")],
                            ),
                            expression=ada.Less(ada.Variable("Template.C"), ada.Number(42)),
                            aspects=[
                                ada.Precondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("Template.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                                ada.Postcondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("Template.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    identifier="New.Prefix.P",
                    declarations=[
                        ada.ExpressionFunctionDeclaration(
                            specification=ada.FunctionSpecification(
                                "S",
                                return_type="T",
                                parameters=[ada.Parameter(["P1"], "New.Prefix.T")],
                            ),
                            expression=ada.Less(ada.Variable("New.Prefix.C"), ada.Number(42)),
                            aspects=[
                                ada.Precondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("New.Prefix.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                                ada.Postcondition(
                                    ada.And(
                                        ada.Call("C", [ada.Variable("P1")]),
                                        ada.Add(
                                            ada.Call("D", [ada.Variable("P1")]),
                                            ada.Call("New.Prefix.C", [ada.Variable("P1")]),
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.GenericPackageInstantiation(
                    identifier="Template.P",
                    generic_package="Template.Q",
                    associations=[("Template.A", "Template.B"), ("Template.C", "Template.D")],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.GenericPackageInstantiation(
                    identifier="New.Prefix.P",
                    generic_package="New.Prefix.Q",
                    associations=[
                        ("New.Prefix.A", "New.Prefix.B"),
                        ("New.Prefix.C", "New.Prefix.D"),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody(
                    "Template.P",
                    statements=[
                        ada.Assignment(
                            "Template.V",
                            expression=ada.New(ada.Variable("Template.X")),
                        ),
                    ],
                ),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody(
                    "New.Prefix.P",
                    statements=[
                        ada.Assignment(
                            "New.Prefix.V",
                            expression=ada.New(ada.Variable("New.Prefix.X")),
                        ),
                    ],
                ),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody(
                    "Template.P",
                    statements=[
                        ada.Assignment(
                            "Template.V",
                            expression=ada.Raise("Template.E", ada.String("Template.Unchanged")),
                        ),
                    ],
                ),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody(
                    "New.Prefix.P",
                    statements=[
                        ada.Assignment(
                            "New.Prefix.V",
                            expression=ada.Raise("New.Prefix.E", ada.String("Template.Unchanged")),
                        ),
                    ],
                ),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "Template.P",
                    declarations=[ada.PackageRenamingDeclaration("P2", "Template.P")],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "New.Prefix.P",
                    declarations=[ada.PackageRenamingDeclaration("P2", "New.Prefix.P")],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody(
                    "Template.P",
                    statements=[
                        ada.Assignment(
                            "Template.V",
                            expression=ada.Slice(
                                ada.Variable("Template.X"),
                                ada.Variable("Template.F"),
                                ada.Variable("Template.L"),
                            ),
                        ),
                    ],
                ),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody(
                    "New.Prefix.P",
                    statements=[
                        ada.Assignment(
                            "New.Prefix.V",
                            expression=ada.Slice(
                                ada.Variable("New.Prefix.X"),
                                ada.Variable("New.Prefix.F"),
                                ada.Variable("New.Prefix.L"),
                            ),
                        ),
                    ],
                ),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody(
                    "Template.P",
                    statements=[
                        ada.RaiseStatement("Template.E", ada.String("Template.Unchanged")),
                    ],
                ),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("New.Prefix.P"),
                body_context=[],
                body=ada.PackageBody(
                    "New.Prefix.P",
                    statements=[
                        ada.RaiseStatement("New.Prefix.E", ada.String("Template.Unchanged")),
                    ],
                ),
            ),
        ),
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "Template.P",
                    declarations=[
                        ada.GenericFunctionInstantiation(
                            "F",
                            "Template.F",
                            [("P1", ada.Variable("Template.P"))],
                            overriding=True,
                        ),
                        ada.GenericFunctionInstantiation(
                            "F",
                            "Template.F",
                            [(None, ada.Variable("P1")), (None, ada.Variable("Template.P2"))],
                            overriding=True,
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration(
                    "New.Prefix.P",
                    declarations=[
                        ada.GenericFunctionInstantiation(
                            "F",
                            "New.Prefix.F",
                            [("P1", ada.Variable("New.Prefix.P"))],
                            overriding=True,
                        ),
                        ada.GenericFunctionInstantiation(
                            "F",
                            "New.Prefix.F",
                            [(None, ada.Variable("P1")), (None, ada.Variable("New.Prefix.P2"))],
                            overriding=True,
                        ),
                    ],
                ),
                body_context=[],
                body=ada.PackageBody("New.Prefix.P"),
            ),
        ),
    ],
)
def test_change_prefix(model: ada.PackageUnit, expected: ada.PackageUnit) -> None:
    assert ada_prefix.change_prefix(model, ID("Template"), ID("New.Prefix")) == expected


@pytest.mark.parametrize(
    ("model", "expected"),
    [
        (
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("Template.P"),
                body_context=[],
                body=ada.PackageBody("Template.P"),
            ),
            ada.PackageUnit(
                declaration_context=[],
                declaration=ada.PackageDeclaration("P"),
                body_context=[],
                body=ada.PackageBody("P"),
            ),
        ),
    ],
)
def test_remove_prefix(model: ada.PackageUnit, expected: ada.PackageUnit) -> None:
    assert ada_prefix.change_prefix(model, ID("Template"), None) == expected

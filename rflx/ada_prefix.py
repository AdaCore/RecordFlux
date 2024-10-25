from __future__ import annotations

from collections.abc import Mapping, Sequence
from functools import singledispatch
from typing import TypeVar

from rflx import ada
from rflx.identifier import ID

T = TypeVar("T")
U = TypeVar("U")


def change_prefix(element: T, old: ID, new: ID | None) -> T:
    result = _change_prefix(element, old, new)
    assert isinstance(
        result,
        element.__class__,
    ), f"got {result.__class__.__name__}, expected {element.__class__.__name__}"
    return result


# Helper functions


def _change_prefix_elem(element: T, old: ID, new: ID | None) -> T:
    assert not isinstance(element, list), "lists are not supported"
    result = _change_prefix(element, old, new)
    assert isinstance(
        result,
        element.__class__,
    ), f"got {result.__class__.__name__}, expected {element.__class__.__name__}"
    return result


def _change_prefix_seq(element: Sequence[T], old: ID, new: ID | None) -> list[T]:
    assert not element or not isinstance(element[0], tuple), "tuples are not supported"
    return [_change_prefix_elem(e, old, new) for e in element]


def _change_prefix_tuple_seq(
    element: Sequence[tuple[T, U]],
    old: ID,
    new: ID | None,
) -> list[tuple[T, U]]:
    return [
        (_change_prefix_elem(l, old, new), _change_prefix_elem(r, old, new)) for l, r in element
    ]


def _change_prefix_opt_tuple_seq(
    element: Sequence[tuple[T, U]] | None,
    old: ID,
    new: ID | None,
) -> Sequence[tuple[T, U]] | None:
    if element is None:
        return None
    return _change_prefix_tuple_seq(element, old, new)


def _change_prefix_opt_seq(
    element: Sequence[T] | None,
    old: ID,
    new: ID | None,
) -> list[T] | None:
    if element is None:
        return None
    return _change_prefix_seq(element, old, new)


def _change_prefix_mapping(
    element: Mapping[T, U],
    old: ID,
    new: ID | None,
) -> Mapping[T, U] | None:
    if not element:
        return None
    return {
        _change_prefix_elem(k, old, new): _change_prefix_elem(v, old, new)
        for k, v in element.items()
    }


@singledispatch
def _change_prefix(element: object, old: ID, new: ID | None) -> object:  # noqa: ARG001
    raise NotImplementedError(f"implement change_prefix for {element.__class__.__name__}")


# Basic types


@_change_prefix.register
def _(element: ID, old: ID, new: ID | None) -> ID:
    if element.parts[0] != old:
        return element
    if new is None:
        return ID([*element.parts[1:]], location=element.location)
    return ID([*new.parts, *element.parts[1:]], location=element.location)


@_change_prefix.register
def _(element: None, old: ID, new: ID | None) -> None:  # noqa: ARG001
    return None


@_change_prefix.register
def _(element: str, old: ID, new: ID | None) -> str:  # noqa: ARG001
    return element


# rflx.ada types


@_change_prefix.register
def _(element: ada.Not, old: ID, new: ID | None) -> ada.Not:
    return element.__class__(_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.Neg, old: ID, new: ID | None) -> ada.Neg:
    return element.__class__(_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.BinExpr, old: ID, new: ID | None) -> ada.BinExpr:
    return element.__class__(
        _change_prefix_elem(element.left, old, new),
        _change_prefix_elem(element.right, old, new),
    )


@_change_prefix.register
def _(element: ada.AssExpr, old: ID, new: ID | None) -> ada.AssExpr:
    return element.__class__(
        *_change_prefix_seq(element.terms, old, new),
    )


@_change_prefix.register
def _(element: ada.Number, old: ID, new: ID | None) -> ada.Number:  # noqa: ARG001
    return element


@_change_prefix.register
def _(element: ada.New, old: ID, new: ID | None) -> ada.New:
    return element.__class__(_change_prefix_elem(element.expr, old, new))


@_change_prefix.register
def _(element: ada.Literal, old: ID, new: ID | None) -> ada.Literal:
    return element.__class__(_change_prefix_elem(element.identifier, old, new))


@_change_prefix.register
def _(element: ada.Variable, old: ID, new: ID | None) -> ada.Variable:
    return element.__class__(_change_prefix_elem(element.identifier, old, new))


@_change_prefix.register
def _(element: ada.Attribute, old: ID, new: ID | None) -> ada.Attribute:
    return element.__class__(prefix=_change_prefix_elem(element.prefix, old, new))


@_change_prefix.register
def _(element: ada.AttributeExpr, old: ID, new: ID | None) -> ada.AttributeExpr:
    return element.__class__(
        prefix=_change_prefix_elem(element.prefix, old, new),
        expression=_change_prefix_elem(element.expression, old, new),
    )


@_change_prefix.register
def _(element: ada.BinAttributeExpr, old: ID, new: ID | None) -> ada.BinAttributeExpr:
    return element.__class__(
        prefix=_change_prefix_elem(element.prefix, old, new),
        left=_change_prefix_elem(element.left, old, new),
        right=_change_prefix_elem(element.right, old, new),
    )


@_change_prefix.register
def _(element: ada.NamedAttributeExpr, old: ID, new: ID | None) -> ada.NamedAttributeExpr:
    return element.__class__(
        _change_prefix_elem(element.prefix, old, new),
        *_change_prefix_tuple_seq(element.associations, old, new),
    )


@_change_prefix.register
def _(element: ada.Indexed, old: ID, new: ID | None) -> ada.Indexed:
    return element.__class__(
        _change_prefix_elem(element.prefix, old, new),
        *_change_prefix_seq(element.elements, old, new),
    )


@_change_prefix.register
def _(element: ada.Selected, old: ID, new: ID | None) -> ada.Selected:
    return element.__class__(
        prefix=_change_prefix_elem(element.prefix, old, new),
        selector=_change_prefix_elem(element.selector, old, new),
    )


@_change_prefix.register
def _(element: ada.Call, old: ID, new: ID | None) -> ada.Call:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        arguments=_change_prefix_opt_seq(element.arguments, old, new),
        named_arguments=_change_prefix_mapping(element.named_arguments, old, new),
    )


@_change_prefix.register
def _(element: ada.Slice, old: ID, new: ID | None) -> ada.Slice:
    return element.__class__(
        prefix=_change_prefix_elem(element.prefix, old, new),
        first=_change_prefix_elem(element.first, old, new),
        last=_change_prefix_elem(element.last, old, new),
    )


@_change_prefix.register
def _(element: ada.Aggregate, old: ID, new: ID | None) -> ada.Aggregate:
    return element.__class__(*_change_prefix_seq(element.elements, old, new))


@_change_prefix.register
def _(element: ada.String, old: ID, new: ID | None) -> ada.String:  # noqa: ARG001
    return element.__class__(element.data)


@_change_prefix.register
def _(element: ada.NamedAggregate, old: ID, new: ID | None) -> ada.NamedAggregate:
    return element.__class__(
        *[
            (_change_prefix_elem(k, old, new), _change_prefix_elem(v, old, new))
            for k, v in element.elements
        ],
    )


@_change_prefix.register
def _(element: ada.IfExpr, old: ID, new: ID | None) -> ada.IfExpr:
    return element.__class__(
        condition_expressions=[
            (_change_prefix_elem(c, old, new), _change_prefix_elem(e, old, new))
            for c, e in element.condition_expressions
        ],
        else_expression=_change_prefix_elem(element.else_expression, old, new),
    )


@_change_prefix.register
def _(element: ada.CaseExpr, old: ID, new: ID | None) -> ada.CaseExpr:
    return element.__class__(
        control_expression=_change_prefix_elem(element.control_expression, old, new),
        case_expressions=[
            (_change_prefix_elem(c, old, new), _change_prefix_elem(e, old, new))
            for c, e in element.case_expressions
        ],
    )


@_change_prefix.register
def _(element: ada.QuantifiedExpr, old: ID, new: ID | None) -> ada.QuantifiedExpr:
    return element.__class__(
        parameter_identifier=_change_prefix_elem(element.parameter_identifier, old, new),
        iterable=_change_prefix_elem(element.iterable, old, new),
        predicate=_change_prefix_elem(element.predicate, old, new),
    )


@_change_prefix.register
def _(element: ada.ValueRange, old: ID, new: ID | None) -> ada.ValueRange:
    return element.__class__(
        lower=_change_prefix_elem(element.lower, old, new),
        upper=_change_prefix_elem(element.upper, old, new),
        type_identifier=_change_prefix_elem(element.type_identifier, old, new),
    )


@_change_prefix.register
def _(element: ada.Conversion, old: ID, new: ID | None) -> ada.Conversion:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        argument=_change_prefix_elem(element.argument, old, new),
    )


@_change_prefix.register
def _(element: ada.QualifiedExpr, old: ID, new: ID | None) -> ada.QualifiedExpr:
    return element.__class__(
        type_identifier=_change_prefix_elem(element.type_identifier, old, new),
        expression=_change_prefix_elem(element.expression, old, new),
    )


@_change_prefix.register
def _(element: ada.Raise, old: ID, new: ID | None) -> ada.Raise:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        string=_change_prefix_elem(element.string, old, new),
    )


@_change_prefix.register
def _(element: ada.ChoiceList, old: ID, new: ID | None) -> ada.ChoiceList:
    return element.__class__(*_change_prefix_seq(element.expressions, old, new))


@_change_prefix.register
def _(element: ada.ContextItem, old: ID, new: ID | None) -> ada.ContextItem:
    return element.__class__(identifier=_change_prefix_elem(element.identifier, old, new))


@_change_prefix.register
def _(element: ada.UseTypeClause, old: ID, new: ID | None) -> ada.UseTypeClause:
    return element.__class__(*_change_prefix_seq(element.identifiers, old, new))


@_change_prefix.register
def _(element: ada.Aspect, old: ID, new: ID | None) -> ada.Aspect:  # noqa: ARG001
    return element.__class__()


@_change_prefix.register
def _(element: ada.Precondition, old: ID, new: ID | None) -> ada.Precondition:
    return element.__class__(_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.Postcondition, old: ID, new: ID | None) -> ada.Postcondition:
    return element.__class__(_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.ClassPrecondition, old: ID, new: ID | None) -> ada.ClassPrecondition:
    return element.__class__(expression=_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.ClassPostcondition, old: ID, new: ID | None) -> ada.ClassPostcondition:
    return element.__class__(expression=_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.ContractCases, old: ID, new: ID | None) -> ada.ContractCases:
    return element.__class__(*_change_prefix_tuple_seq(element.cases, old, new))


@_change_prefix.register
def _(element: ada.Depends, old: ID, new: ID | None) -> ada.Depends:
    return element.__class__(
        dependencies={
            _change_prefix_elem(k, old, new): _change_prefix_seq(v, old, new)
            for k, v in element.dependencies.items()
        },
    )


@_change_prefix.register
def _(element: ada.AlwaysTerminates, old: ID, new: ID | None) -> ada.AlwaysTerminates:
    return element.__class__(expression=_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.ChangeDirection, old: ID, new: ID | None) -> ada.ChangeDirection:
    return element.__class__(expression=_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.SubprogramVariant, old: ID, new: ID | None) -> ada.SubprogramVariant:
    return element.__class__(direction=_change_prefix_elem(element.direction, old, new))


@_change_prefix.register
def _(element: ada.DynamicPredicate, old: ID, new: ID | None) -> ada.DynamicPredicate:
    return element.__class__(_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.SizeAspect, old: ID, new: ID | None) -> ada.SizeAspect:
    return element.__class__(_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.InitialCondition, old: ID, new: ID | None) -> ada.InitialCondition:
    return element.__class__(expression=_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.DefaultInitialCondition, old: ID, new: ID | None) -> ada.DefaultInitialCondition:
    return element.__class__(_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.SparkMode, old: ID, new: ID | None) -> ada.SparkMode:  # noqa: ARG001
    return element.__class__(element.off)


@_change_prefix.register
def _(element: ada.AbstractState, old: ID, new: ID | None) -> ada.AbstractState:
    return element.__class__(*_change_prefix_seq(element.identifiers, old, new))


@_change_prefix.register
def _(element: ada.Initializes, old: ID, new: ID | None) -> ada.Initializes:
    return element.__class__(*_change_prefix_seq(element.identifiers, old, new))


@_change_prefix.register
def _(element: ada.Global, old: ID, new: ID | None) -> ada.Global:
    return element.__class__(
        inputs=_change_prefix_opt_seq(element.inputs, old, new),
        outputs=_change_prefix_opt_seq(element.outputs, old, new),
        in_outs=_change_prefix_opt_seq(element.in_outs, old, new),
    )


@_change_prefix.register
def _(element: ada.Annotate, old: ID, new: ID | None) -> ada.Annotate:  # noqa: ARG001
    return element.__class__(*element.args)


@_change_prefix.register
def _(element: ada.ConventionKind, old: ID, new: ID | None) -> ada.ConventionKind:  # noqa: ARG001
    return element


@_change_prefix.register
def _(element: ada.Convention, old: ID, new: ID | None) -> ada.Convention:
    return element.__class__(convention=_change_prefix_elem(element.convention, old, new))


@_change_prefix.register
def _(
    element: ada.FormalPackageDeclaration,
    old: ID,
    new: ID | None,
) -> ada.FormalPackageDeclaration:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        generic_identifier=_change_prefix_elem(element.generic_identifier, old, new),
        associations=_change_prefix_opt_tuple_seq(element.associations, old, new),
    )


@_change_prefix.register
def _(element: ada.PackageDeclaration, old: ID, new: ID | None) -> ada.PackageDeclaration:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        declarations=_change_prefix_opt_seq(element.declarations, old, new),
        private_declarations=_change_prefix_opt_seq(element.private_declarations, old, new),
        formal_parameters=_change_prefix_opt_seq(element.formal_parameters, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(element: ada.PackageBody, old: ID, new: ID | None) -> ada.PackageBody:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        declarations=_change_prefix_opt_seq(element.declarations, old, new),
        statements=_change_prefix_opt_seq(element.statements, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(
    element: ada.GenericPackageInstantiation,
    old: ID,
    new: ID | None,
) -> ada.GenericPackageInstantiation:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        generic_package=_change_prefix_elem(element.generic_package, old, new),
        associations=[
            (_change_prefix_elem(a, old, new), _change_prefix_elem(e, old, new))
            for a, e in element.associations
        ],
    )


@_change_prefix.register
def _(
    element: ada.PackageRenamingDeclaration,
    old: ID,
    new: ID | None,
) -> ada.PackageRenamingDeclaration:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        package_identifier=_change_prefix_elem(element.package_identifier, old, new),
    )


@_change_prefix.register
def _(element: ada.ObjectDeclaration, old: ID, new: ID | None) -> ada.ObjectDeclaration:
    return element.__class__(
        identifiers=_change_prefix_seq(element.identifiers, old, new),
        type_identifier=_change_prefix_elem(element.type_identifier, old, new),
        expression=_change_prefix_elem(element.expression, old, new),
        constant=element.constant,
        aliased=element.aliased,
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(element: ada.Discriminant, old: ID, new: ID | None) -> ada.Discriminant:
    return element.__class__(
        identifiers=_change_prefix_seq(element.identifiers, old, new),
        type_identifier=_change_prefix_elem(element.type_identifier, old, new),
        default=_change_prefix_elem(element.default, old, new),
    )


@_change_prefix.register
def _(element: ada.TypeDeclaration, old: ID, new: ID | None) -> ada.TypeDeclaration:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        discriminants=_change_prefix_opt_seq(element.discriminants, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(element: ada.ModularType, old: ID, new: ID | None) -> ada.ModularType:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        modulus=_change_prefix_elem(element.modulus, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(element: ada.RangeType, old: ID, new: ID | None) -> ada.RangeType:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        first=_change_prefix_elem(element.first, old, new),
        last=_change_prefix_elem(element.last, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(element: ada.EnumerationType, old: ID, new: ID | None) -> ada.EnumerationType:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        literals={
            _change_prefix_elem(k, old, new): _change_prefix_elem(v, old, new)
            for k, v in element.literals.items()
        },
        size=_change_prefix_elem(element.size, old, new),
    )


@_change_prefix.register
def _(element: ada.Subtype, old: ID, new: ID | None) -> ada.Subtype:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        base_identifier=_change_prefix_elem(element.base_identifier, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(element: ada.RangeSubtype, old: ID, new: ID | None) -> ada.RangeSubtype:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        base_identifier=_change_prefix_elem(element.base_identifier, old, new),
        first=_change_prefix_elem(element.first, old, new),
        last=_change_prefix_elem(element.last, old, new),
    )


@_change_prefix.register
def _(element: ada.DerivedType, old: ID, new: ID | None) -> ada.DerivedType:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        type_identifier=_change_prefix_elem(element.type_identifier, old, new),
    )


@_change_prefix.register
def _(element: ada.DerivedRangeType, old: ID, new: ID | None) -> ada.DerivedRangeType:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        type_identifier=_change_prefix_elem(element.type_identifier, old, new),
        first=_change_prefix_elem(element.first, old, new),
        last=_change_prefix_elem(element.last, old, new),
    )


@_change_prefix.register
def _(element: ada.DerivedRecordType, old: ID, new: ID | None) -> ada.DerivedRecordType:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        type_identifier=_change_prefix_elem(element.type_identifier, old, new),
        record_extension=_change_prefix_seq(element.record_extension, old, new),
    )


@_change_prefix.register
def _(element: ada.ArrayType, old: ID, new: ID | None) -> ada.ArrayType:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        index_type=_change_prefix_elem(element.index_type, old, new),
        component_identifier=_change_prefix_elem(element.component_identifier, old, new),
    )


@_change_prefix.register
def _(element: ada.AccessType, old: ID, new: ID | None) -> ada.AccessType:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        object_identifier=_change_prefix_elem(element.object_identifier, old, new),
    )


@_change_prefix.register
def _(element: ada.Component, old: ID, new: ID | None) -> ada.Component:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        type_identifier=_change_prefix_elem(element.type_identifier, old, new),
        default=_change_prefix_elem(element.default, old, new),
        aliased=element.aliased,
    )


@_change_prefix.register
def _(element: ada.Variant, old: ID, new: ID | None) -> ada.Variant:
    return element.__class__(
        discrete_choices=_change_prefix_seq(element.discrete_choices, old, new),
        components=_change_prefix_seq(element.components, old, new),
    )


@_change_prefix.register
def _(element: ada.VariantPart, old: ID, new: ID | None) -> ada.VariantPart:
    return element.__class__(
        discriminant_identifier=_change_prefix_elem(element.discriminant_identifier, old, new),
        variants=_change_prefix_seq(element.variants, old, new),
    )


@_change_prefix.register
def _(element: ada.RecordType, old: ID, new: ID | None) -> ada.RecordType:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        components=_change_prefix_seq(element.components, old, new),
        discriminants=_change_prefix_opt_seq(element.discriminants, old, new),
        variant_part=_change_prefix_elem(element.variant_part, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
        abstract=element.abstract,
        tagged=element.tagged,
        limited=element.limited,
    )


@_change_prefix.register
def _(element: ada.NullStatement, old: ID, new: ID | None) -> ada.NullStatement:  # noqa: ARG001
    return element.__class__()


@_change_prefix.register
def _(element: ada.Assignment, old: ID, new: ID | None) -> ada.Assignment:
    return element.__class__(
        name=_change_prefix_elem(element.name, old, new),
        expression=_change_prefix_elem(element.expression, old, new),
    )


@_change_prefix.register
def _(element: ada.CallStatement, old: ID, new: ID | None) -> ada.CallStatement:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        arguments=_change_prefix_opt_seq(element.arguments, old, new),
        named_arguments=_change_prefix_mapping(element.named_arguments, old, new),
    )


@_change_prefix.register
def _(element: ada.PragmaStatement, old: ID, new: ID | None) -> ada.PragmaStatement:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        parameters=_change_prefix_seq(element.pragma_parameters, old, new),
    )


@_change_prefix.register
def _(element: ada.ReturnStatement, old: ID, new: ID | None) -> ada.ReturnStatement:
    return element.__class__(expression=_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.ExitStatement, old: ID, new: ID | None) -> ada.ExitStatement:
    return element.__class__(expression=_change_prefix_elem(element.expression, old, new))


@_change_prefix.register
def _(element: ada.GotoStatement, old: ID, new: ID | None) -> ada.GotoStatement:
    return element.__class__(label=_change_prefix_elem(element.label, old, new))


@_change_prefix.register
def _(element: ada.Label, old: ID, new: ID | None) -> ada.Label:
    return element.__class__(identifier=_change_prefix_elem(element.identifier, old, new))


@_change_prefix.register
def _(element: ada.Comment, old: ID, new: ID | None) -> ada.Comment:
    return element.__class__(comment=_change_prefix_elem(element.comment, old, new))


@_change_prefix.register
def _(element: ada.VerticalSpace, old: ID, new: ID | None) -> ada.VerticalSpace:  # noqa: ARG001
    return element.__class__()


@_change_prefix.register
def _(element: ada.IfStatement, old: ID, new: ID | None) -> ada.IfStatement:
    return element.__class__(
        condition_statements=[
            (_change_prefix_elem(c, old, new), _change_prefix_seq(s, old, new))
            for c, s in element.condition_statements
        ],
        else_statements=_change_prefix_opt_seq(element.else_statements, old, new),
    )


@_change_prefix.register
def _(element: ada.CaseStatement, old: ID, new: ID | None) -> ada.CaseStatement:
    return element.__class__(
        control_expression=_change_prefix_elem(element.control_expression, old, new),
        case_statements=[
            (_change_prefix_elem(c, old, new), _change_prefix_seq(s, old, new))
            for c, s in element.case_statements
        ],
        case_grouping=element.case_grouping,
    )


@_change_prefix.register
def _(element: ada.While, old: ID, new: ID | None) -> ada.While:
    return element.__class__(
        condition=_change_prefix_elem(element.condition, old, new),
        statements=_change_prefix_seq(element.statements, old, new),
    )


@_change_prefix.register
def _(element: ada.ForLoop, old: ID, new: ID | None) -> ada.ForLoop:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        iterator=_change_prefix_elem(element.iterator, old, new),
        statements=_change_prefix_seq(element.statements, old, new),
        reverse=element.reverse,
    )


@_change_prefix.register
def _(element: ada.RaiseStatement, old: ID, new: ID | None) -> ada.RaiseStatement:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        string=_change_prefix_elem(element.string, old, new),
    )


@_change_prefix.register
def _(element: ada.Declare, old: ID, new: ID | None) -> ada.Declare:
    return element.__class__(
        declarations=_change_prefix_seq(element.declarations, old, new),
        statements=_change_prefix_seq(element.statements, old, new),
    )


@_change_prefix.register
def _(element: ada.Parameter, old: ID, new: ID | None) -> ada.Parameter:
    return element.__class__(
        identifiers=_change_prefix_seq(element.identifiers, old, new),
        type_identifier=_change_prefix_elem(element.type_identifier, old, new),
        default=_change_prefix_elem(element.default, old, new),
    )


@_change_prefix.register
def _(element: ada.AccessParameter, old: ID, new: ID | None) -> ada.AccessParameter:
    return element.__class__(
        identifiers=_change_prefix_seq(element.identifiers, old, new),
        type_identifier=_change_prefix_elem(element.type_identifier, old, new),
        default=_change_prefix_elem(element.default, old, new),
        constant=element.constant,
    )


@_change_prefix.register
def _(
    element: ada.ParameterizedSubprogramSpecification,
    old: ID,
    new: ID | None,
) -> ada.ParameterizedSubprogramSpecification:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        parameters=_change_prefix_opt_seq(element.parameters, old, new),
        overriding=element.overriding,
    )


@_change_prefix.register
def _(element: ada.FunctionSpecification, old: ID, new: ID | None) -> ada.FunctionSpecification:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        return_type=_change_prefix_elem(element.return_type, old, new),
        parameters=_change_prefix_seq(element.parameters, old, new),
        overriding=element.overriding,
        not_null=element.not_null,
    )


@_change_prefix.register
def _(element: ada.SubprogramDeclaration, old: ID, new: ID | None) -> ada.SubprogramDeclaration:
    return element.__class__(
        specification=_change_prefix_elem(element.specification, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
        formal_parameters=_change_prefix_opt_seq(element.formal_parameters, old, new),
    )


@_change_prefix.register
def _(
    element: ada.FormalSubprogramDeclaration,
    old: ID,
    new: ID | None,
) -> ada.FormalSubprogramDeclaration:
    return element.__class__(
        specification=_change_prefix_elem(element.specification, old, new),
        default=_change_prefix_elem(element.default, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(element: ada.SubprogramBody, old: ID, new: ID | None) -> ada.SubprogramBody:
    return element.__class__(
        specification=_change_prefix_elem(element.specification, old, new),
        declarations=_change_prefix_seq(element.declarations, old, new),
        statements=_change_prefix_seq(element.statements, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(
    element: ada.ExpressionFunctionDeclaration,
    old: ID,
    new: ID | None,
) -> ada.ExpressionFunctionDeclaration:
    assert isinstance(element.specification, ada.FunctionSpecification)
    return element.__class__(
        specification=_change_prefix_elem(element.specification, old, new),
        expression=_change_prefix_elem(element.expression, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(
    element: ada.GenericProcedureInstantiation,
    old: ID,
    new: ID | None,
) -> ada.GenericProcedureInstantiation:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        generic_name=_change_prefix_elem(element.generic_name, old, new),
        associations=_change_prefix_opt_tuple_seq(element.associations, old, new),
        overriding=element.overriding,
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(
    element: ada.GenericFunctionInstantiation,
    old: ID,
    new: ID | None,
) -> ada.GenericFunctionInstantiation:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        generic_name=_change_prefix_elem(element.generic_name, old, new),
        associations=_change_prefix_opt_tuple_seq(element.associations, old, new),
        overriding=element.overriding,
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(
    element: ada.SubprogramRenamingDeclaration,
    old: ID,
    new: ID | None,
) -> ada.SubprogramRenamingDeclaration:
    return element.__class__(
        specification=_change_prefix_elem(element.specification, old, new),
        subprogram_identifier=_change_prefix_elem(element.subprogram_identifier, old, new),
        aspects=_change_prefix_opt_seq(element.aspects, old, new),
    )


@_change_prefix.register
def _(element: ada.Pragma, old: ID, new: ID | None) -> ada.Pragma:
    return element.__class__(
        identifier=_change_prefix_elem(element.identifier, old, new),
        parameters=_change_prefix_opt_seq(element.pragma_parameters, old, new),
    )


@_change_prefix.register
def _(element: ada.PackageUnit, old: ID, new: ID | None) -> ada.PackageUnit:
    return element.__class__(
        declaration_context=_change_prefix_seq(element.declaration_context, old, new),
        declaration=_change_prefix_elem(element.declaration, old, new),
        body_context=_change_prefix_seq(element.body_context, old, new),
        body=_change_prefix_elem(element.body, old, new),
        formal_parameters=_change_prefix_opt_seq(element.formal_parameters, old, new),
    )


@_change_prefix.register
def _(element: ada.InstantiationUnit, old: ID, new: ID | None) -> ada.InstantiationUnit:
    return element.__class__(
        context=_change_prefix_seq(element.context, old, new),
        declaration=_change_prefix_elem(element.declaration, old, new),
    )

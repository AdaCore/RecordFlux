from __future__ import annotations

from collections.abc import Generator
from functools import singledispatch

from rflx import ada, expr, ir, ty
from rflx.error import fail
from rflx.identifier import ID


@singledispatch
def to_ada(expression: expr.Expr) -> ada.Expr:  # noqa: ARG001
    raise NotImplementedError


@to_ada.register
def _(expression: expr.Not) -> ada.Expr:
    return ada.Not(to_ada(expression.expr))


@to_ada.register
def _(expression: expr.And) -> ada.Expr:
    return ada.And(*[to_ada(t) for t in expression.terms])


@to_ada.register
def _(expression: expr.AndThen) -> ada.Expr:
    return ada.AndThen(*[to_ada(t) for t in expression.terms])


@to_ada.register
def _(expression: expr.Or) -> ada.Expr:
    return ada.Or(*[to_ada(t) for t in expression.terms])


@to_ada.register
def _(expression: expr.OrElse) -> ada.Expr:
    return ada.OrElse(*[to_ada(t) for t in expression.terms])


@to_ada.register
def _(expression: expr.Number) -> ada.Expr:
    return ada.Number(expression.value, expression.base)


@to_ada.register
def _(expression: expr.Neg) -> ada.Expr:
    return ada.Neg(to_ada(expression.expr))


@to_ada.register
def _(expression: expr.Add) -> ada.Expr:
    return ada.Add(*[to_ada(t) for t in expression.terms])


@to_ada.register
def _(expression: expr.Mul) -> ada.Expr:
    return ada.Mul(*[to_ada(t) for t in expression.terms])


@to_ada.register
def _(expression: expr.Sub) -> ada.Expr:
    return ada.Sub(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.Div) -> ada.Expr:
    return ada.Div(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.Pow) -> ada.Expr:
    return ada.Pow(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.Mod) -> ada.Expr:
    return ada.Mod(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.Rem) -> ada.Expr:
    return ada.Rem(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.TypeName) -> ada.Expr:
    return ada.Literal(expression.identifier)


@to_ada.register
def _(expression: expr.Literal) -> ada.Expr:
    return ada.Literal(expression.identifier)


@to_ada.register
def _(expression: expr.Variable) -> ada.Expr:
    return ada.Variable(expression.identifier)


@to_ada.register
def _(expression: expr.Attribute) -> ada.Expr:
    result = getattr(ada, expression.__class__.__name__)(to_ada(expression.prefix))
    assert isinstance(result, ada.Expr)
    return result


@to_ada.register
def _(expression: expr.Val) -> ada.Expr:
    result = getattr(ada, expression.__class__.__name__)(
        to_ada(expression.prefix),
        to_ada(expression.expression),
    )
    assert isinstance(result, ada.Expr)
    return result


@to_ada.register
def _(expression: expr.Indexed) -> ada.Expr:
    return ada.Indexed(
        to_ada(expression.prefix),
        *[to_ada(e) for e in expression.elements],
    )


@to_ada.register
def _(expression: expr.Selected) -> ada.Expr:
    return ada.Selected(to_ada(expression.prefix), ID(expression.selector))


@to_ada.register
def _(expression: expr.Call) -> ada.Expr:
    return ada.Call(expression.identifier, [to_ada(a) for a in expression.args], {})


@to_ada.register
def _(expression: expr.Aggregate) -> ada.Expr:
    return ada.Aggregate(*[to_ada(e) for e in expression.elements])


@to_ada.register
def _(expression: expr.String) -> ada.Expr:
    return ada.String(expression.data)


@to_ada.register
def _(expression: expr.Less) -> ada.Expr:
    return ada.Less(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.LessEqual) -> ada.Expr:
    return ada.LessEqual(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.Equal) -> ada.Expr:
    return ada.Equal(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.GreaterEqual) -> ada.Expr:
    return ada.GreaterEqual(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.Greater) -> ada.Expr:
    return ada.Greater(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.NotEqual) -> ada.Expr:
    return ada.NotEqual(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.In) -> ada.Expr:
    return ada.In(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.NotIn) -> ada.Expr:
    return ada.NotIn(to_ada(expression.left), to_ada(expression.right))


@to_ada.register
def _(expression: expr.IfExpr) -> ada.Expr:
    result = getattr(ada, expression.__class__.__name__)(
        [(to_ada(c), to_ada(e)) for c, e in expression.condition_expressions],
        to_ada(expression.else_expression) if expression.else_expression else None,
    )
    assert isinstance(result, ada.Expr)
    return result


@to_ada.register
def _(expression: expr.QuantifiedExpr) -> ada.Expr:
    result = getattr(ada, expression.__class__.__name__)(
        expression.parameter_identifier,
        to_ada(expression.iterable),
        to_ada(expression.predicate),
    )
    assert isinstance(result, ada.Expr)
    return result


@to_ada.register
def _(expression: expr.ValueRange) -> ada.Expr:
    return ada.ValueRange(to_ada(expression.lower), to_ada(expression.upper))


@to_ada.register
def _(expression: expr.Conversion) -> ada.Expr:
    return ada.Conversion(expression.identifier, to_ada(expression.argument))


@to_ada.register
def _(expression: expr.QualifiedExpr) -> ada.Expr:
    return ada.QualifiedExpr(expression.type_identifier, to_ada(expression.expression))


@singledispatch
def to_ir(
    expression: expr.Expr,  # noqa: ARG001
    variable_id: Generator[ID, None, None],  # noqa: ARG001
) -> ir.ComplexExpr:
    raise NotImplementedError


@to_ir.register
def _(expression: expr.Not, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    inner_stmts, inner_expr = _to_ir_basic_bool(expression.expr, variable_id)
    return ir.ComplexBoolExpr(inner_stmts, ir.Not(inner_expr, origin=expression))


@to_ir.register
def _(expression: expr.BoolAssExpr, variable_id: Generator[ID, None, None]) -> ir.ComplexBoolExpr:
    if len(expression.terms) == 0:
        return ir.ComplexBoolExpr([], ir.BoolVal(value=True, origin=expression))

    if len(expression.terms) == 1:
        first_stmts, first_expr = _to_ir_basic_bool(expression.terms[0], variable_id)
        return ir.ComplexBoolExpr(first_stmts, first_expr)

    if len(expression.terms) == 2:
        left_stmts, left_expr = _to_ir_basic_bool(expression.terms[0], variable_id)
        right_stmts, right_expr = _to_ir_basic_bool(expression.terms[1], variable_id)
        return ir.ComplexBoolExpr(
            [*left_stmts, *right_stmts],
            getattr(ir, expression.__class__.__name__)(left_expr, right_expr, origin=expression),
        )

    left_stmts, left_expr = _to_ir_basic_bool(expression.terms[0], variable_id)
    right_id = next(variable_id)
    right_origin = expression.__class__(*expression.terms[1:])
    right = to_ir(right_origin, variable_id)
    return ir.ComplexBoolExpr(
        [
            *right.stmts,
            ir.VarDecl(right_id, ty.BOOLEAN, None, origin=right_origin),
            ir.Assign(right_id, right.expr, ty.BOOLEAN, origin=right_origin),
            *left_stmts,
        ],
        getattr(ir, expression.__class__.__name__)(
            left_expr,
            ir.BoolVar(right_id),
            origin=expression,
        ),
    )


@to_ir.register
def _(expression: expr.Number, _variable_id: Generator[ID, None, None]) -> ir.ComplexIntExpr:
    return ir.ComplexIntExpr([], ir.IntVal(expression.value, origin=expression))


@to_ir.register
def _(expression: expr.Neg, variable_id: Generator[ID, None, None]) -> ir.ComplexIntExpr:
    assert isinstance(expression.type_, ty.AnyInteger)
    inner_stmts, inner_expr = _to_ir_basic_int(expression.expr, variable_id)
    return ir.ComplexIntExpr(inner_stmts, ir.Neg(inner_expr, origin=expression))


@to_ir.register
def _(expression: expr.MathAssExpr, variable_id: Generator[ID, None, None]) -> ir.ComplexIntExpr:
    if len(expression.terms) == 0:
        return ir.ComplexIntExpr([], ir.IntVal(0, origin=expression))

    assert isinstance(expression.type_, ty.AnyInteger)

    if len(expression.terms) == 1:
        first_stmts, first_expr = _to_ir_basic_int(expression.terms[0], variable_id)
        return ir.ComplexIntExpr(first_stmts, first_expr)

    if len(expression.terms) == 2:
        left_stmts, left_expr = _to_ir_basic_int(expression.terms[0], variable_id)
        right_stmts, right_expr = _to_ir_basic_int(expression.terms[1], variable_id)
        return ir.ComplexIntExpr(
            [*left_stmts, *right_stmts],
            getattr(ir, expression.__class__.__name__)(left_expr, right_expr, origin=expression),
        )

    right_id = next(variable_id)
    left_stmts, left_expr = _to_ir_basic_int(expression.terms[0], variable_id)
    right_origin = expression.__class__(
        *expression.terms[1:],
        location=expression.terms[1].location,
    )

    assert isinstance(right_origin.type_, ty.AnyInteger)

    right = to_ir(right_origin, variable_id)

    return ir.ComplexIntExpr(
        [
            *right.stmts,
            ir.VarDecl(right_id, ir.to_integer(expression.type_), None, origin=right_origin),
            ir.Assign(right_id, right.expr, ir.to_integer(expression.type_), origin=right_origin),
            *left_stmts,
        ],
        getattr(ir, expression.__class__.__name__)(
            left_expr,
            ir.IntVar(right_id, right_origin.type_, origin=right_origin),
            origin=expression,
        ),
    )


@to_ir.register
def _(expression: expr.MathBinExpr, variable_id: Generator[ID, None, None]) -> ir.ComplexIntExpr:
    assert isinstance(expression.type_, ty.AnyInteger)

    left_stmts, left_expr = _to_ir_basic_int(expression.left, variable_id)
    right_stmts, right_expr = _to_ir_basic_int(expression.right, variable_id)
    return ir.ComplexIntExpr(
        [*left_stmts, *right_stmts],
        getattr(ir, expression.__class__.__name__)(left_expr, right_expr, origin=expression),
    )


@to_ir.register
def _(expression: expr.Literal, _variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.Enumeration)

    if expression.type_ == ty.BOOLEAN:
        if expression.identifier == ID("True"):
            return ir.ComplexBoolExpr([], ir.BoolVal(value=True, origin=expression))
        assert expression.identifier == ID("False")
        return ir.ComplexBoolExpr([], ir.BoolVal(value=False, origin=expression))

    return ir.ComplexExpr([], ir.EnumLit(expression.name, expression.type_, origin=expression))


@to_ir.register
def _(expression: expr.Variable, _variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    if expression.type_ == ty.BOOLEAN:
        return ir.ComplexBoolExpr([], ir.BoolVar(expression.name, origin=expression))
    if isinstance(expression.type_, ty.Integer):
        return ir.ComplexIntExpr(
            [],
            ir.IntVar(expression.name, expression.type_, origin=expression),
        )

    assert isinstance(expression.type_, ty.Any)

    return ir.ComplexExpr([], ir.ObjVar(expression.name, expression.type_, origin=expression))


@to_ir.register
def _(expression: expr.Attribute, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.Any)

    prefix_stmts, prefix_expr = to_ir_basic_expr(expression.prefix, variable_id)

    assert isinstance(prefix_expr, ir.Var)

    return ir.ComplexExpr(prefix_stmts, _attribute_to_ir(expression, prefix_expr.identifier))


@to_ir.register
def _(expression: expr.Size, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.Any)

    if isinstance(expression.prefix, expr.Selected):
        assert isinstance(expression.prefix.prefix.type_, ty.Compound)
        assert isinstance(expression.prefix.prefix, expr.Variable)
        return ir.ComplexExpr(
            [],
            ir.FieldSize(
                expression.prefix.prefix.identifier,
                expression.prefix.selector,
                expression.prefix.prefix.type_,
            ),
        )

    if isinstance(expression.prefix, expr.TypeName):
        return ir.ComplexExpr([], _attribute_to_ir(expression, expression.prefix.identifier))

    prefix_stmts, prefix_expr = to_ir_basic_expr(expression.prefix, variable_id)

    assert isinstance(prefix_expr, ir.Var)

    return ir.ComplexExpr(prefix_stmts, _attribute_to_ir(expression, prefix_expr.identifier))


@singledispatch
def _attribute_to_ir(
    expression: expr.Attribute,  # noqa: ARG001
    prefix: ID,  # noqa: ARG001
) -> ir.Expr:
    raise NotImplementedError


@_attribute_to_ir.register
def _(expression: expr.Size, prefix: ID) -> ir.Expr:
    assert isinstance(expression.prefix.type_, ty.Any)
    return ir.Size(prefix, expression.prefix.type_, origin=expression)


@_attribute_to_ir.register
def _(expression: expr.Length, prefix: ID) -> ir.Expr:
    assert isinstance(expression.prefix.type_, ty.Any)
    return ir.Length(prefix, expression.prefix.type_, origin=expression)


@_attribute_to_ir.register
def _(expression: expr.First, prefix: ID) -> ir.Expr:
    assert isinstance(expression.prefix.type_, ty.Any)
    return ir.First(prefix, expression.prefix.type_, origin=expression)


@_attribute_to_ir.register
def _(expression: expr.Last, prefix: ID) -> ir.Expr:
    assert isinstance(expression.prefix.type_, ty.Any)
    return ir.Last(prefix, expression.prefix.type_, origin=expression)


@_attribute_to_ir.register
def _(expression: expr.ValidChecksum, prefix: ID) -> ir.Expr:
    assert isinstance(expression.prefix.type_, ty.Any)
    return ir.ValidChecksum(prefix, expression.prefix.type_, origin=expression)


@to_ir.register
def _(expression: expr.Valid, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.Any)

    if isinstance(expression.prefix, expr.Selected):
        assert isinstance(expression.prefix.prefix, expr.Variable)
        assert isinstance(expression.prefix.prefix.type_, ty.Compound)
        return ir.ComplexExpr(
            [],
            ir.FieldValid(
                expression.prefix.prefix.identifier,
                expression.prefix.selector,
                expression.prefix.prefix.type_,
            ),
        )

    prefix_stmts, prefix_expr = to_ir_basic_expr(expression.prefix, variable_id)

    assert isinstance(prefix_expr, ir.Var)

    return ir.ComplexExpr(prefix_stmts, _attribute_to_ir(expression, prefix_expr.identifier))


@_attribute_to_ir.register
def _(expression: expr.Valid, prefix: ID) -> ir.Expr:
    assert isinstance(expression.prefix.type_, ty.Any)
    return ir.Valid(prefix, expression.prefix.type_, origin=expression)


@to_ir.register
def _(expression: expr.Present, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.Any)

    if isinstance(expression.prefix, expr.Selected):
        assert isinstance(expression.prefix.prefix, expr.Variable)
        assert isinstance(expression.prefix.prefix.type_, ty.Compound)
        return ir.ComplexExpr(
            [],
            ir.FieldPresent(
                expression.prefix.prefix.identifier,
                expression.prefix.selector,
                expression.prefix.prefix.type_,
            ),
        )

    prefix_stmts, prefix_expr = to_ir_basic_expr(expression.prefix, variable_id)

    assert isinstance(prefix_expr, ir.Var)

    return ir.ComplexExpr(prefix_stmts, _attribute_to_ir(expression, prefix_expr.identifier))


@_attribute_to_ir.register
def _(expression: expr.Present, prefix: ID) -> ir.Expr:
    assert isinstance(expression.prefix.type_, ty.Any)
    return ir.Present(prefix, expression.prefix.type_, origin=expression)


@_attribute_to_ir.register
def _(expression: expr.HasData, prefix: ID) -> ir.Expr:
    assert isinstance(expression.prefix.type_, ty.Any)
    return ir.HasData(prefix, expression.prefix.type_, origin=expression)


@to_ir.register
def _(expression: expr.Head, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.Any)

    if isinstance(expression.prefix, expr.Comprehension):
        comprehension = to_ir(expression.prefix, variable_id)
        assert isinstance(comprehension.expr, ir.Comprehension)
        return ir.ComplexExpr(
            comprehension.stmts,
            ir.Find(
                comprehension.expr.iterator,
                comprehension.expr.sequence,
                comprehension.expr.selector,
                comprehension.expr.condition,
                comprehension.expr.origin,
            ),
        )

    prefix_stmts, prefix_expr = to_ir_basic_expr(expression.prefix, variable_id)

    assert isinstance(prefix_expr, ir.Var)

    return ir.ComplexExpr(prefix_stmts, _attribute_to_ir(expression, prefix_expr.identifier))


@_attribute_to_ir.register
def _(expression: expr.Head, prefix: ID) -> ir.Expr:
    assert isinstance(expression.prefix.type_, ty.Composite)
    assert isinstance(expression.prefix.type_.element, ty.Any)
    return ir.Head(prefix, expression.prefix.type_, origin=expression)


@_attribute_to_ir.register
def _(expression: expr.Opaque, prefix: ID) -> ir.Expr:
    assert isinstance(expression.prefix.type_, (ty.Sequence, ty.Message))
    return ir.Opaque(prefix, expression.prefix.type_, origin=expression)


@_attribute_to_ir.register
def _(
    expression: expr.Constrained,  # noqa: ARG001
    prefix: ID,  # noqa: ARG001
) -> ir.Expr:
    raise NotImplementedError


@_attribute_to_ir.register
def _(
    expression: expr.Val,  # noqa: ARG001
    prefix: ID,  # noqa: ARG001
) -> ir.Expr:
    raise NotImplementedError


@to_ir.register
def _(
    expression: expr.Indexed,  # noqa: ARG001
    variable_id: Generator[ID, None, None],  # noqa: ARG001
) -> ir.ComplexExpr:
    raise NotImplementedError


@to_ir.register
def _(expression: expr.Selected, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.Any)
    assert isinstance(expression.prefix.type_, ty.Compound)
    stmts, msg = to_ir_basic_expr(expression.prefix, variable_id)
    assert isinstance(msg, ir.ObjVar)
    if expression.type_ == ty.BOOLEAN:
        return ir.ComplexExpr(
            stmts,
            ir.BoolFieldAccess(
                msg.identifier,
                expression.selector,
                expression.prefix.type_,
                origin=expression,
            ),
        )
    if isinstance(expression.type_, ty.Integer):
        return ir.ComplexExpr(
            stmts,
            ir.IntFieldAccess(
                msg.identifier,
                expression.selector,
                expression.prefix.type_,
                origin=expression,
            ),
        )
    if isinstance(expression.type_, (ty.Enumeration, ty.Sequence)):
        return ir.ComplexExpr(
            stmts,
            ir.ObjFieldAccess(
                msg.identifier,
                expression.selector,
                expression.prefix.type_,
                origin=expression,
            ),
        )
    assert False, expression.type_


@to_ir.register
def _(expression: expr.Call, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    arguments_stmts = []
    arguments_exprs = []

    for a in expression.args:
        a_ir = to_ir(a, variable_id)
        arguments_stmts.extend(a_ir.stmts)
        arguments_exprs.append(a_ir.expr)

    assert all(isinstance(t, ty.Any) for t in expression.argument_types)
    argument_types = [t for t in expression.argument_types if isinstance(t, ty.Any)]

    if expression.type_ is ty.BOOLEAN:
        return ir.ComplexExpr(
            arguments_stmts,
            ir.BoolCall(
                expression.identifier,
                arguments_exprs,
                argument_types,
                origin=expression,
            ),
        )

    if isinstance(expression.type_, ty.Integer):
        return ir.ComplexExpr(
            arguments_stmts,
            ir.IntCall(
                expression.identifier,
                arguments_exprs,
                argument_types,
                expression.type_,
                origin=expression,
            ),
        )

    assert isinstance(expression.type_, (ty.Enumeration, ty.Structure, ty.Message))
    return ir.ComplexExpr(
        arguments_stmts,
        ir.ObjCall(
            expression.identifier,
            arguments_exprs,
            argument_types,
            expression.type_,
            origin=expression,
        ),
    )


@to_ir.register
def _(
    expression: expr.Slice,  # noqa: ARG001
    variable_id: Generator[ID, None, None],  # noqa: ARG001
) -> ir.ComplexExpr:
    raise NotImplementedError


@to_ir.register
def _(
    expression: expr.UndefinedExpr,  # noqa: ARG001
    variable_id: Generator[ID, None, None],  # noqa: ARG001
) -> ir.ComplexExpr:
    raise NotImplementedError


@to_ir.register
def _(expression: expr.Aggregate, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.Any)

    elements = []
    stmts = []

    for e in expression.elements:
        e_stmts, e_expr = to_ir_basic_expr(e, variable_id)
        elements.append(e_expr)
        stmts.extend(e_stmts)

    return ir.ComplexExpr(stmts, ir.Agg(elements, origin=expression))


@to_ir.register
def _(
    expression: expr.NamedAggregate,  # noqa: ARG001
    variable_id: Generator[ID, None, None],  # noqa: ARG001
) -> ir.ComplexExpr:
    raise NotImplementedError


@to_ir.register
def _(expression: expr.Relation, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    left_stmts, left_expr = to_ir_basic_expr(expression.left, variable_id)
    right_stmts, right_expr = to_ir_basic_expr(expression.right, variable_id)
    return ir.ComplexBoolExpr(
        [*left_stmts, *right_stmts],
        getattr(ir, expression.__class__.__name__)(left_expr, right_expr, origin=expression),
    )


@to_ir.register
def _(
    expression: expr.In,  # noqa: ARG001
    variable_id: Generator[ID, None, None],  # noqa: ARG001
) -> ir.ComplexExpr:
    raise NotImplementedError


@to_ir.register
def _(
    expression: expr.NotIn,  # noqa: ARG001
    variable_id: Generator[ID, None, None],  # noqa: ARG001
) -> ir.ComplexExpr:
    raise NotImplementedError


@to_ir.register
def _(expression: expr.IfExpr, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert len(expression.condition_expressions) == 1
    assert expression.else_expression is not None

    condition = expression.condition_expressions[0][0]
    condition_stmts, condition_expr = _to_ir_basic_bool(condition, variable_id)

    assert condition.type_ is ty.BOOLEAN

    then_expression = expression.condition_expressions[0][1]

    if then_expression.type_ is ty.BOOLEAN and expression.else_expression.type_ is ty.BOOLEAN:
        then_expr = to_ir(then_expression, variable_id)
        else_expr = to_ir(expression.else_expression, variable_id)
        assert isinstance(then_expr, ir.ComplexBoolExpr)
        assert isinstance(else_expr, ir.ComplexBoolExpr)
        return ir.ComplexBoolExpr(
            [*condition_stmts],
            ir.BoolIfExpr(
                condition_expr,
                then_expr,
                else_expr,
                origin=expression,
            ),
        )

    assert isinstance(expression.type_, ty.AnyInteger)
    assert isinstance(then_expression.type_, ty.AnyInteger)
    assert isinstance(expression.else_expression.type_, ty.AnyInteger)
    then_expr = to_ir(then_expression, variable_id)
    else_expr = to_ir(expression.else_expression, variable_id)
    assert isinstance(then_expr, ir.ComplexIntExpr)
    assert isinstance(else_expr, ir.ComplexIntExpr)
    return ir.ComplexIntExpr(
        [*condition_stmts],
        ir.IntIfExpr(
            condition_expr,
            then_expr,
            else_expr,
            expression.type_,
            origin=expression,
        ),
    )


@to_ir.register
def _(
    expression: expr.QuantifiedExpr,
    variable_id: Generator[ID, None, None],  # noqa: ARG001
) -> ir.ComplexExpr:
    fail(
        "quantified expressions not yet supported",
        location=expression.location,
    )


@to_ir.register
def _(
    expression: expr.ValueRange,  # noqa: ARG001
    variable_id: Generator[ID, None, None],  # noqa: ARG001
) -> ir.ComplexExpr:
    raise NotImplementedError


@to_ir.register
def _(expression: expr.Conversion, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.NamedTypeClass)
    argument = to_ir(expression.argument, variable_id)
    return ir.ComplexExpr(
        argument.stmts,
        ir.Conversion(expression.type_, argument.expr, origin=expression),
    )


@to_ir.register
def _(
    expression: expr.QualifiedExpr,  # noqa: ARG001
    variable_id: Generator[ID, None, None],  # noqa: ARG001
) -> ir.ComplexExpr:
    raise NotImplementedError


@to_ir.register
def _(expression: expr.Comprehension, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    sequence = to_ir(expression.sequence, variable_id)
    selector = to_ir(expression.selector, variable_id)
    condition = to_ir(expression.condition.simplified(), variable_id)
    assert isinstance(sequence.expr, (ir.Var, ir.FieldAccess))
    assert isinstance(condition, ir.ComplexBoolExpr)
    return ir.ComplexExpr(
        sequence.stmts,
        ir.Comprehension(
            expression.iterator,
            sequence.expr,
            selector,
            condition,
            origin=expression,
        ),
    )


@to_ir.register
def _(expression: expr.MessageAggregate, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.Message)
    field_values = {}
    stmts = []
    for i, e in expression.field_values.items():
        e_ir = to_ir(e, variable_id)
        field_values[i] = e_ir.expr
        stmts.extend(e_ir.stmts)
    return ir.ComplexExpr(
        stmts,
        ir.MsgAgg(expression.identifier, field_values, expression.type_, expression),
    )


@to_ir.register
def _(
    expression: expr.DeltaMessageAggregate,
    variable_id: Generator[ID, None, None],
) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.Message)
    field_values = {}
    stmts = []
    for i, e in expression.field_values.items():
        e_ir = to_ir(e, variable_id)
        field_values[i] = e_ir.expr
        stmts.extend(e_ir.stmts)
    return ir.ComplexExpr(
        stmts,
        ir.DeltaMsgAgg(expression.identifier, field_values, expression.type_, expression),
    )


@to_ir.register
def _(expression: expr.CaseExpr, variable_id: Generator[ID, None, None]) -> ir.ComplexExpr:
    assert isinstance(expression.type_, ty.Any)

    expression_stmts, expression_expr = to_ir_basic_expr(expression.expr, variable_id)
    choices = []

    for choice, e in expression.choices:
        e_stmts, e_expr = to_ir_basic_expr(e, variable_id)
        # TODO(eng/recordflux/RecordFlux#633): Check for unsupported case expressions in model
        assert not e_stmts
        cs: list[ir.BasicExpr]
        if isinstance(expression.expr.type_, ty.Enumeration):
            assert all(isinstance(c, ID) for c in choice)
            cs = [ir.EnumLit(c, expression.expr.type_) for c in choice if isinstance(c, ID)]
        else:
            assert isinstance(expression.expr.type_, ty.AnyInteger)
            assert all(isinstance(c, expr.Number) for c in choice)
            cs = [ir.IntVal(int(c)) for c in choice if isinstance(c, expr.Number)]
        choices.append((cs, e_expr))

    return ir.ComplexExpr(
        expression_stmts,
        ir.CaseExpr(
            expression_expr,
            choices,
            expression.type_,
            origin=expression,
        ),
    )


def _to_ir_basic_int(
    expression: expr.Expr,
    variable_id: Generator[ID, None, None],
) -> tuple[list[ir.Stmt], ir.BasicIntExpr]:
    assert isinstance(expression.type_, ty.AnyInteger)

    result = to_ir(expression, variable_id)
    if isinstance(result.expr, ir.BasicIntExpr):
        result_expr = result.expr
        result_stmts = result.stmts
    else:
        result_id = next(variable_id)
        result_type = ir.to_integer(expression.type_)
        result_expr = ir.IntVar(result_id, result_type, origin=expression)
        result_stmts = [
            *result.stmts,
            ir.VarDecl(result_id, result_type, None, origin=expression),
            ir.Assign(result_id, result.expr, result_type, origin=expression),
        ]
    return (result_stmts, result_expr)


def _to_ir_basic_bool(
    expression: expr.Expr,
    variable_id: Generator[ID, None, None],
) -> tuple[list[ir.Stmt], ir.BasicBoolExpr]:
    assert expression.type_ == ty.BOOLEAN

    result = to_ir(expression, variable_id)
    if isinstance(result.expr, ir.BasicBoolExpr):
        result_expr = result.expr
        result_stmts = result.stmts
    else:
        result_id = next(variable_id)
        result_expr = ir.BoolVar(result_id, origin=expression)
        result_stmts = [
            *result.stmts,
            ir.VarDecl(result_id, ty.BOOLEAN, None, origin=expression),
            ir.Assign(result_id, result.expr, ty.BOOLEAN, origin=expression),
        ]
    return (result_stmts, result_expr)


def to_ir_basic_expr(
    expression: expr.Expr,
    variable_id: Generator[ID, None, None],
) -> tuple[list[ir.Stmt], ir.BasicExpr]:
    result = to_ir(expression, variable_id)
    if isinstance(result.expr, ir.BasicExpr):
        result_expr = result.expr
        result_stmts = result.stmts
    else:
        result_id = next(variable_id)

        if isinstance(result.expr, ir.BoolExpr):
            result_expr = ir.BoolVar(result_id, origin=expression)
        elif isinstance(result.expr, ir.IntExpr):
            assert isinstance(expression.type_, ty.AnyInteger)
            result_expr = ir.IntVar(result_id, ir.to_integer(expression.type_), origin=expression)
        else:
            assert isinstance(expression.type_, ty.Any)
            result_expr = ir.ObjVar(result_id, expression.type_, origin=expression)

        if isinstance(result_expr.type_, ty.Aggregate):
            # TODO(eng/recordflux/RecordFlux#1497): Support comparisons of opaque fields
            result_stmts = [  # pragma: no cover
                *result.stmts,
                ir.VarDecl(
                    result_id,
                    ty.OPAQUE,
                    ir.ComplexExpr([], result.expr),
                    origin=expression,
                ),
            ]
        else:
            result_type = result_expr.type_

            assert isinstance(result_type, ty.NamedTypeClass)

            result_stmts = [
                *result.stmts,
                ir.VarDecl(result_id, result_type, None, origin=expression),
                ir.Assign(result_id, result.expr, result_type, origin=expression),
            ]

    return (result_stmts, result_expr)

import math

from hypothesis import HealthCheck, given, settings, strategies as st

import rflx.expression as expr
from rflx.model import Model
from rflx.specification import Parser
from tests.property import strategies
from tests.utils import parse_bool_expression, parse_expression, parse_math_expression


@given(
    strategies.mathematical_expressions(
        st.one_of(
            strategies.numbers()
            | strategies.variables(strategies.identifiers())
            | strategies.attributes(strategies.identifiers())
        )
    )
)
@settings(deadline=None, suppress_health_check=[HealthCheck.filter_too_much, HealthCheck.too_slow])
def test_parsing_mathematical_expressions(expression: expr.Expr) -> None:
    parsed_expression = parse_math_expression(str(expression), True)
    assert parsed_expression == expression


@given(
    strategies.boolean_expressions(
        st.one_of(
            strategies.mathematical_expressions(
                st.one_of(
                    strategies.numbers()
                    | strategies.variables(strategies.identifiers())
                    | strategies.attributes(strategies.identifiers())
                )
            )
        )
    )
)
@settings(deadline=None, suppress_health_check=[HealthCheck.filter_too_much, HealthCheck.too_slow])
def test_parsing_boolean_expressions(expression: expr.Expr) -> None:
    parsed_expression = parse_bool_expression(str(expression), True)
    assert parsed_expression == expression


@given(
    st.one_of(
        strategies.mathematical_expressions(
            st.one_of(
                strategies.numbers()
                | strategies.variables(strategies.identifiers())
                | strategies.attributes(strategies.identifiers())
            )
        ),
        strategies.boolean_expressions(
            st.one_of(
                strategies.mathematical_expressions(
                    st.one_of(
                        strategies.numbers()
                        | strategies.variables(strategies.identifiers())
                        | strategies.attributes(strategies.identifiers())
                    )
                )
            )
        ),
        strategies.calls(
            st.one_of(
                strategies.numbers()
                | strategies.variables(strategies.identifiers())
                | strategies.attributes(strategies.identifiers())
            )
        ),
        strategies.quantified_expressions(
            st.one_of(
                strategies.numbers()
                | strategies.variables(strategies.identifiers())
                | strategies.attributes(strategies.identifiers())
            )
        ),
        strategies.strings(),
        strategies.aggregates(strategies.numbers()),
    )
)
@settings(deadline=None, suppress_health_check=[HealthCheck.filter_too_much, HealthCheck.too_slow])
def test_parsing_expressions(expression: expr.Expr) -> None:
    parsed_expression = parse_expression(str(expression))
    assert parsed_expression == expression


@given(strategies.models())
@settings(
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
    max_examples=math.ceil(settings.default.max_examples / 10),
)
def test_parsing_model(model: Model) -> None:
    parser = Parser()
    parser.parse_string(
        f"""
        package Test is
            {model}
        end Test;
        """
    )
    parsed_model = parser.create_model()
    assert parsed_model.types == model.types
    assert parsed_model == model

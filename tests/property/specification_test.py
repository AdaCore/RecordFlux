import math

from hypothesis import HealthCheck, given, settings, strategies as st
from librecordfluxdsllang import AnalysisContext

import rflx.expression as expr
from rflx.model import Model
from rflx.specification import Parser
from rflx.specification.parser import GrammarRule, create_expression
from tests.property import strategies


def parse_expression(data: str, rule: GrammarRule) -> expr.Expr:
    unit = AnalysisContext().get_from_buffer("<stdin>", data, rule=rule)
    assert unit.root, "\n".join(str(d) for d in unit.diagnostics)
    return create_expression(unit.root)


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
    parsed_expression = parse_expression(str(expression), GrammarRule.mathematical_expression_rule)
    assert parsed_expression == expression


@given(
    strategies.boolean_expressions(
        st.one_of(
            strategies.aggregates(strategies.numbers())
            | strategies.strings()
            | strategies.mathematical_expressions(
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
    parsed_expression = parse_expression(
        str(expression), GrammarRule.extended_boolean_expression_rule
    )
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
                strategies.aggregates(strategies.numbers())
                | strategies.strings()
                | strategies.mathematical_expressions(
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
    )
)
@settings(deadline=None, suppress_health_check=[HealthCheck.filter_too_much, HealthCheck.too_slow])
def test_parsing_expressions(expression: expr.Expr) -> None:
    parsed_expression = parse_expression(str(expression), GrammarRule.extended_expression_rule)
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

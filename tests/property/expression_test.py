from hypothesis import given, settings, strategies as st

from rflx import expr, expr_conv
from tests.property import strategies


@given(
    st.one_of(
        strategies.mathematical_expressions(
            st.one_of(
                strategies.numbers(),
                strategies.variables(strategies.identifiers()),
                strategies.attributes(strategies.identifiers()),
            ),
        ),
        strategies.boolean_relations(
            st.one_of(strategies.aggregates(strategies.numbers()), strategies.strings()),
        ),
        strategies.boolean_expressions(
            st.one_of(
                strategies.mathematical_expressions(
                    st.one_of(
                        strategies.numbers(),
                        strategies.variables(strategies.identifiers()),
                        strategies.attributes(strategies.identifiers()),
                    ),
                ),
            ),
        ),
        strategies.calls(
            st.one_of(
                strategies.numbers(),
                strategies.variables(strategies.identifiers()),
                strategies.attributes(strategies.identifiers()),
            ),
        ),
        strategies.quantified_expressions(
            st.one_of(
                strategies.numbers(),
                strategies.variables(strategies.identifiers()),
                strategies.attributes(strategies.identifiers()),
            ),
        ),
        strategies.strings(),
        strategies.aggregates(strategies.numbers()),
    ),
)
@settings(deadline=None)
def test_conversion(expression: expr.Expr) -> None:
    assert expr_conv.to_ada(expression).rflx_expr() == expression

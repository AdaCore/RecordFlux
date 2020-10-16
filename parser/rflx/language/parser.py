from langkit.dsl import ASTNode, abstract  # type: ignore
from langkit.parsers import Grammar  # type: ignore


@abstract
class RFLXNode(ASTNode):
    """
    Root node class for RecordFluxDSL AST nodes.
    """


class ExampleNode(RFLXNode):
    """
    Example node.
    """


rflx_grammar = Grammar("main_rule")
rflx_grammar.add_rules(main_rule=ExampleNode("example"))

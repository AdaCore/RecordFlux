import astroid
from pylint.checkers import BaseChecker
from pylint.interfaces import IAstroidChecker


class NamedBooleanArgumentsChecker(BaseChecker):  # type: ignore[misc]
    __implements__ = IAstroidChecker

    name = "named-boolean-arguments"
    priority = -1
    msgs = {
        "C0001": {
            "Anonymous boolean argument",
            "anon-bool-arg",
            "Boolean arguments must be passed as keywords",
        }
    }

    def visit_call(self, node: astroid.Call) -> None:
        num_args = (len(node.args) if node.args else 0) + (
            len(node.keywords) if node.keywords else 0
        )
        if num_args > 1:
            for arg in node.args:
                if isinstance(arg, astroid.Const) and arg.pytype() == "builtins.bool":
                    self.add_message("C0001", node=arg)


def register(linter: BaseChecker) -> None:
    linter.register_checker(NamedBooleanArgumentsChecker(linter))

import astroid
import pylint.testutils

import tools.rflx_pylint_checker


class TestNamedBooleanArgumentsChecker(pylint.testutils.CheckerTestCase):  # type: ignore[misc]
    CHECKER_CLASS = tools.rflx_pylint_checker.NamedBooleanArgumentsChecker

    def test_valid_calls(self) -> None:
        nodes = astroid.extract_node(
            """
        def test():
            fun1(True) #@
            fun2(arg1=True, arg2=False) #@
            fun3(3) #@
            fun4(7, 12) #@
            fun5("some string", 12) #@
        """
        )

        for n in nodes:
            self.checker.visit_call(n)

    def test_invalid_call_bool(self) -> None:
        call = astroid.extract_node(
            """
        def test():
            fun1 (True, False) #@
        """
        )
        with self.assertAddsMessages(
            pylint.testutils.Message(msg_id="C0001", node=call.args[0]),
            pylint.testutils.Message(msg_id="C0001", node=call.args[1]),
        ):
            self.checker.visit_call(call)

    def test_invalid_call_mixed(self) -> None:
        call = astroid.extract_node(
            """
        def test():
            fun1 (False, param=True) #@
        """
        )
        with self.assertAddsMessages(
            pylint.testutils.Message(msg_id="C0001", node=call.args[0]),
        ):
            self.checker.visit_call(call)

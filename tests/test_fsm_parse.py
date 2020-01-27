from rflx.expression import FALSE, TRUE, And, Equal, NotEqual, Number, Or, Variable
from rflx.fsm_expression import Contains, Convert, ForAll, ForSome, NotContains, Valid
from rflx.fsm_parser import FSMParser


def test_simple_equation() -> None:
    result = FSMParser.condition().parseString("Foo.Bar = abc")[0]
    assert result == Equal(Variable("Foo.Bar"), Variable("abc"))


def test_simple_inequation() -> None:
    result = FSMParser.condition().parseString("Foo.Bar /= abc")[0]
    assert result == NotEqual(Variable("Foo.Bar"), Variable("abc"))


def test_valid() -> None:
    result = FSMParser.condition().parseString("Something'Valid")[0]
    assert result == Valid(Variable("Something"))


def test_conjunction_valid() -> None:
    result = FSMParser.condition().parseString("Foo'Valid and Bar'Valid")[0]
    assert result == And(Valid(Variable("Foo")), Valid(Variable("Bar")))


def test_conjunction() -> None:
    result = FSMParser.condition().parseString("Foo = Bar and Bar /= Baz")[0]
    assert result == And(
        Equal(Variable("Foo"), Variable("Bar")), NotEqual(Variable("Bar"), Variable("Baz"))
    )


def test_disjunction() -> None:
    result = FSMParser.condition().parseString("Foo = Bar or Bar /= Baz")[0]
    assert result == Or(
        Equal(Variable("Foo"), Variable("Bar")), NotEqual(Variable("Bar"), Variable("Baz"))
    )


def test_in_operator() -> None:
    result = FSMParser.condition().parseString("Foo in Bar")[0]
    assert result == Contains(Variable("Foo"), Variable("Bar"))


def test_not_in_operator() -> None:
    result = FSMParser.condition().parseString("Foo not in Bar")[0]
    assert result == NotContains(Variable("Foo"), Variable("Bar"))


def test_parenthesized_expression() -> None:
    result = FSMParser.condition().parseString("Foo = True and (Bar = False or Baz = False)")[0]
    assert result == And(
        Equal(Variable("Foo"), TRUE),
        Or(Equal(Variable("Bar"), FALSE), Equal(Variable("Baz"), FALSE)),
    )


def test_parenthesized_expression2() -> None:
    result = FSMParser.condition().parseString("Foo'Valid and (Bar'Valid or Baz'Valid)")[0]
    assert result == And(Valid(Variable("Foo")), Or(Valid(Variable("Bar")), Valid(Variable("Baz"))))


def test_numeric_constant_expression() -> None:
    result = FSMParser.condition().parseString("Keystore_Message.Length = 0")[0]
    assert result == Equal(Variable("Keystore_Message.Length"), Number(0))


def test_complex_expression() -> None:
    expression = (
        "Keystore_Message'Valid = False "
        "or Keystore_Message.Tag /= KEYSTORE_RESPONSE "
        "or Keystore_Message.Request /= KEYSTORE_REQUEST_PSK_IDENTITIES "
        "or (Keystore_Message.Length = 0 "
        "    and TLS_Handshake.PSK_DHE_KE not in Configuration.PSK_Key_Exchange_Modes)"
    )
    result = FSMParser.condition().parseString(expression)[0]
    expected = Or(
        Equal(Valid(Variable("Keystore_Message")), FALSE),
        NotEqual(Variable("Keystore_Message.Tag"), Variable("KEYSTORE_RESPONSE")),
        NotEqual(Variable("Keystore_Message.Request"), Variable("KEYSTORE_REQUEST_PSK_IDENTITIES")),
        And(
            Equal(Variable("Keystore_Message.Length"), Number(0)),
            NotContains(
                Variable("TLS_Handshake.PSK_DHE_KE"),
                Variable("Configuration.PSK_Key_Exchange_Modes"),
            ),
        ),
    )
    assert result == expected


def test_existential_quantification() -> None:
    result = FSMParser.condition().parseString("for some X in Y => X = 3")[0]
    assert result == ForSome(Variable("X"), Variable("Y"), Equal(Variable("X"), Number(3)))


def test_universal_quantification() -> None:
    result = FSMParser.condition().parseString("for all X in Y => X = Bar")[0]
    assert result == ForAll(Variable("X"), Variable("Y"), Equal(Variable("X"), Variable("Bar")))


def test_type_conversion_simple() -> None:
    expr = "Foo (Bar) = 5"
    result = FSMParser.condition().parseString(expr)[0]
    print(f"result: {result}")
    expected = Equal(Convert(Variable("Bar"), Variable("Foo")), Number(5))
    assert result == expected


def test_type_conversion() -> None:
    expr = "TLS_Handshake.Supported_Versions (E.Data) = 5"
    result = FSMParser.condition().parseString(expr)[0]
    expected = Equal(
        Convert(Variable("E.Data"), Variable("TLS_Handshake.Supported_Versions")), Number(5)
    )
    assert result == expected


def test_use_type_conversion() -> None:
    expr = "GreenTLS.TLS_1_3 not in TLS_Handshake.Supported_Versions (E.Data)"
    result = FSMParser.condition().parseString(expr)[0]
    expected = NotContains(
        Variable("GreenTLS.TLS_1_3"),
        Convert(Variable("E.Data"), Variable("TLS_Handshake.Supported_Versions")),
    )
    assert result == expected

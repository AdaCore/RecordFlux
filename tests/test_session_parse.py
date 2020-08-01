from rflx.expression import (
    FALSE,
    TRUE,
    Add,
    And,
    Binding,
    Call,
    Comprehension,
    Conversion,
    Div,
    Equal,
    ForAllIn,
    ForSomeIn,
    Greater,
    Head,
    In,
    Length,
    Less,
    MessageAggregate,
    Mul,
    NotEqual,
    NotIn,
    Number,
    Opaque,
    Or,
    Present,
    Selected,
    String,
    Sub,
    Valid,
    Variable,
)
from rflx.identifier import ID
from rflx.parser.session import expression


def test_simple_equation() -> None:
    result = expression("Foo.Bar = abc")
    expected = Equal(Selected(Variable("Foo"), "Bar"), Variable("abc"))
    assert result == expected


def test_simple_inequation() -> None:
    result = expression("Foo.Bar /= abc")
    assert result == NotEqual(Selected(Variable("Foo"), "Bar"), Variable("abc"))


def test_valid() -> None:
    result = expression("Something'Valid")
    assert result == Valid(Variable("Something"))


def test_opaque() -> None:
    result = expression("Something'Opaque")
    assert result == Opaque(Variable("Something"))


def test_conjunction() -> None:
    result = expression("Foo = Bar and Bar /= Baz")
    assert result == And(
        Equal(Variable("Foo"), Variable("Bar")), NotEqual(Variable("Bar"), Variable("Baz"))
    )


def test_conjunction_valid() -> None:
    result = expression("Foo'Valid and Bar'Valid")
    assert result == And(Valid(Variable("Foo")), Valid(Variable("Bar")))


def test_disjunction_multi() -> None:
    result = expression("Foo = Bar or Bar /= Baz or Baz'Valid = False")
    assert result == Or(
        Equal(Variable("Foo"), Variable("Bar")),
        NotEqual(Variable("Bar"), Variable("Baz")),
        Equal(Valid(Variable("Baz")), FALSE),
    )


def test_not_in_whitespace_operator() -> None:
    result = expression("Foo not   in  Bar")
    assert result == NotIn(Variable("Foo"), Variable("Bar"))


def test_disjunction() -> None:
    result = expression("Foo = Bar or Bar /= Baz")
    assert result == Or(
        Equal(Variable("Foo"), Variable("Bar")), NotEqual(Variable("Bar"), Variable("Baz"))
    )


def test_in_operator() -> None:
    result = expression("Foo in Bar")
    assert result == In(Variable("Foo"), Variable("Bar"))


def test_not_in_operator() -> None:
    result = expression("Foo not in Bar")
    assert result == NotIn(Variable("Foo"), Variable("Bar"))


def test_parenthesized_expression() -> None:
    result = expression("Foo = True and (Bar = False or Baz = False)")
    assert result == And(
        Equal(Variable("Foo"), TRUE),
        Or(Equal(Variable("Bar"), FALSE), Equal(Variable("Baz"), FALSE)),
    )


def test_parenthesized_expression2() -> None:
    result = expression("Foo'Valid and (Bar'Valid or Baz'Valid)")
    assert result == And(Valid(Variable("Foo")), Or(Valid(Variable("Bar")), Valid(Variable("Baz"))))


def test_numeric_constant_expression() -> None:
    result = expression("Keystore_Message.Length = 0")
    assert result == Equal(Selected(Variable("Keystore_Message"), "Length"), Number(0))


def test_complex_expression() -> None:
    expr = (
        "Keystore_Message'Valid = False "
        "or Keystore_Message.Tag /= KEYSTORE_RESPONSE "
        "or Keystore_Message.Request /= KEYSTORE_REQUEST_PSK_IDENTITIES "
        "or (Keystore_Message.Length = 0 "
        "    and TLS_Handshake.PSK_DHE_KE not in Configuration.PSK_Key_Exchange_Modes)"
    )
    result = expression(expr)
    expected = Or(
        Equal(Valid(Variable("Keystore_Message")), FALSE),
        NotEqual(Selected(Variable("Keystore_Message"), "Tag"), Variable("KEYSTORE_RESPONSE")),
        NotEqual(
            Selected(Variable("Keystore_Message"), "Request"),
            Variable("KEYSTORE_REQUEST_PSK_IDENTITIES"),
        ),
        And(
            Equal(Selected(Variable("Keystore_Message"), "Length"), Number(0)),
            NotIn(
                Selected(Variable("TLS_Handshake"), "PSK_DHE_KE"),
                Selected(Variable("Configuration"), "PSK_Key_Exchange_Modes"),
            ),
        ),
    )
    assert result == expected


def test_existential_quantification() -> None:
    result = expression("for some X in Y => X = 3")
    assert result == ForSomeIn("X", Variable("Y"), Equal(Variable("X"), Number(3)))


def test_complex_existential_quantification() -> None:
    expr = (
        "for some E in Server_Hello_Message.Extensions => "
        "(E.Tag = TLS_Handshake.EXTENSION_SUPPORTED_VERSIONS and "
        "(GreenTLS.TLS_1_3 not in TLS_Handshake.Supported_Versions (E.Data).Versions))"
    )
    result = expression(expr)
    expected = ForSomeIn(
        "E",
        Selected(Variable("Server_Hello_Message"), "Extensions"),
        And(
            Equal(
                Selected(Variable("E"), "Tag"),
                Selected(Variable("TLS_Handshake"), "EXTENSION_SUPPORTED_VERSIONS"),
            ),
            NotIn(
                Selected(Variable("GreenTLS"), "TLS_1_3"),
                Selected(
                    Conversion("TLS_Handshake.Supported_Versions", Selected(Variable("E"), "Data")),
                    "Versions",
                ),
            ),
        ),
    )
    assert result == expected


def test_conjunction_multi() -> None:
    result = expression("Foo = Bar and Bar /= Baz and Baz = Foo")
    expected = And(
        Equal(Variable("Foo"), Variable("Bar")),
        NotEqual(Variable("Bar"), Variable("Baz")),
        Equal(Variable("Baz"), Variable("Foo")),
    )
    assert result == expected


def test_universal_quantification() -> None:
    result = expression("for all X in Y => X = Bar")
    assert result == ForAllIn("X", Variable("Y"), Equal(Variable("X"), Variable("Bar")))


def test_type_conversion_simple() -> None:
    expr = "Foo.T (Bar) = 5"
    result = expression(expr)
    expected = Equal(Conversion("Foo.T", Variable("Bar")), Number(5))
    assert result == expected


def test_field_simple() -> None:
    result = expression("Bar (Foo).Fld")
    assert result == Selected(Call("Bar", [Variable("Foo")]), "Fld")


def test_field_variable() -> None:
    result = expression("Types.Bar")
    assert result == Selected(Variable("Types"), "Bar")


def test_field_length() -> None:
    result = expression("Bar (Foo).Fld'Length")
    assert result == Length(Selected(Call("Bar", [Variable("Foo")]), "Fld"))


def test_type_conversion() -> None:
    expr = "TLS_Handshake.Supported_Versions (E.Data) = 5"
    result = expression(expr)
    expected = Equal(
        Conversion("TLS_Handshake.Supported_Versions", Selected(Variable("E"), "Data")), Number(5)
    )
    assert result == expected


def test_use_type_conversion() -> None:
    expr = "GreenTLS.TLS_1_3 not in TLS_Handshake.Supported_Versions (E.Data).Versions"
    result = expression(expr)
    expected = NotIn(
        Selected(Variable("GreenTLS"), "TLS_1_3"),
        Selected(
            Conversion("TLS_Handshake.Supported_Versions", Selected(Variable("E"), "Data")),
            "Versions",
        ),
    )
    assert result == expected


def test_present() -> None:
    result = expression("Something'Present")
    assert result == Present(Variable("Something"))


def test_list_comprehension_without_condition() -> None:
    result = expression("[for K in PSKs => K.Identity]")
    expected = Comprehension("K", Variable("PSKs"), Selected(Variable("K"), "Identity"), TRUE)
    assert result == expected


def test_conjunction_present() -> None:
    result = expression("Foo'Present and Bar'Present")
    assert result == And(Present(Variable("Foo")), Present(Variable("Bar")))


def test_length_lt() -> None:
    result = expression("Foo'Length < 100")
    assert result == Less(Length(Variable("Foo")), Number(100))


def test_field_length_lt() -> None:
    result = expression("Bar (Foo).Fld'Length < 100")
    assert result == Less(Length(Selected(Call("Bar", [Variable("Foo")]), "Fld")), Number(100))


def test_list_comprehension() -> None:
    result = expression("[for E in List => E.Bar when E.Tag = Foo]")
    assert result == Comprehension(
        "E",
        Variable("List"),
        Selected(Variable("E"), "Bar"),
        Equal(Selected(Variable("E"), "Tag"), Variable("Foo")),
    )


def test_head_attribute() -> None:
    result = expression("Foo'Head")
    assert result == Head(Variable("Foo"))


def test_head_attribute_comprehension() -> None:
    result = expression("[for E in List => E.Bar when E.Tag = Foo]'Head")
    assert result == Head(
        Comprehension(
            "E",
            Variable("List"),
            Selected(Variable("E"), "Bar"),
            Equal(Selected(Variable("E"), "Tag"), Variable("Foo")),
        )
    )


def test_gt() -> None:
    result = expression("Server_Name_Extension.Data_Length > 0")
    assert result == Greater(Selected(Variable("Server_Name_Extension"), "Data_Length"), Number(0))


def test_list_head_field_simple() -> None:
    result = expression("Foo'Head.Data")
    assert result == Selected(Head(Variable("Foo")), "Data")


def test_list_head_field() -> None:
    result = expression("[for E in List => E.Bar when E.Tag = Foo]'Head.Data")
    assert result == Selected(
        Head(
            Comprehension(
                "E",
                Variable("List"),
                Selected(Variable("E"), "Bar"),
                Equal(Selected(Variable("E"), "Tag"), Variable("Foo")),
            )
        ),
        "Data",
    )


def test_complex() -> None:
    result = expression(
        "(for some S in TLS_Handshake.Key_Share_CH ([for E in Client_Hello_Message.Extensions "
        "=> E when E.Tag = TLS_Handshake.EXTENSION_KEY_SHARE]'Head.Data).Shares => S.Group = "
        "Selected_Group) = False"
    )
    expected = Equal(
        ForSomeIn(
            "S",
            Selected(
                Conversion(
                    "TLS_Handshake.Key_Share_CH",
                    Selected(
                        Head(
                            Comprehension(
                                "E",
                                Selected(Variable("Client_Hello_Message"), "Extensions"),
                                Variable("E"),
                                Equal(
                                    Selected(Variable("E"), "Tag"),
                                    Selected(Variable("TLS_Handshake"), "EXTENSION_KEY_SHARE"),
                                ),
                            )
                        ),
                        "Data",
                    ),
                ),
                "Shares",
            ),
            Equal(Selected(Variable("S"), "Group"), Variable("Selected_Group")),
        ),
        FALSE,
    )
    assert result == expected


def test_simple_aggregate() -> None:
    result = expression("Message'(Data => Foo)")
    expected = MessageAggregate("Message", {ID("Data"): Variable("Foo")})
    assert result == expected


def test_null_aggregate() -> None:
    result = expression("Message'(null message)")
    expected = MessageAggregate("Message", {})
    assert result == expected


def test_complex_aggregate() -> None:
    result = expression("Complex.Message'(Data1 => Foo, Data2 => Bar, Data3 => Baz)")
    expected = MessageAggregate(
        "Complex.Message",
        {ID("Data1"): Variable("Foo"), ID("Data2"): Variable("Bar"), ID("Data3"): Variable("Baz")},
    )
    assert result == expected


def test_simple_function_call() -> None:
    result = expression("Fun (Parameter)")
    expected = Call("Fun", [Variable("Parameter")])
    assert result == expected


def test_complex_function_call() -> None:
    result = expression("Complex_Function (Param1, Param2, Param3)")
    expected = Call(
        "Complex_Function", [Variable("Param1"), Variable("Param2"), Variable("Param3")],
    )
    assert result == expected


def test_simple_binding() -> None:
    result = expression("M1'(Data => B1) where B1 = M2'(Data => B2)")
    expected = Binding(
        MessageAggregate("M1", {ID("Data"): Variable("B1")}),
        {ID("B1"): MessageAggregate("M2", {ID("Data"): Variable("B2")})},
    )
    assert result == expected


def test_multi_binding() -> None:
    result = expression(
        "M1'(Data1 => B1, Data2 => B2) where B1 = M2'(Data => B2), B2 = M2'(Data => B3)"
    )
    expected = Binding(
        MessageAggregate("M1", {ID("Data1"): Variable("B1"), ID("Data2"): Variable("B2")}),
        {
            ID("B1"): MessageAggregate("M2", {ID("Data"): Variable("B2")}),
            ID("B2"): MessageAggregate("M2", {ID("Data"): Variable("B3")}),
        },
    )
    assert result == expected


def test_nested_binding() -> None:
    result = expression("M1'(Data => B1) where B1 = M2'(Data => B2) where B2 = M3'(Data => B3)")
    expected = Binding(
        MessageAggregate("M1", {ID("Data"): Variable("B1")}),
        {
            ID("B1"): Binding(
                MessageAggregate("M2", {ID("Data"): Variable("B2")}),
                {ID("B2"): MessageAggregate("M3", {ID("Data"): Variable("B3")})},
            )
        },
    )
    assert result == expected


def test_simple_add() -> None:
    result = expression("Foo + Bar")
    expected = Add(Variable("Foo"), Variable("Bar"))
    assert result == expected


def test_simple_sub() -> None:
    result = expression("Foo - Bar")
    expected = Sub(Variable("Foo"), Variable("Bar"))
    assert result == expected


def test_simple_mul() -> None:
    result = expression("Foo * Bar")
    expected = Mul(Variable("Foo"), Variable("Bar"))
    assert result == expected


def test_simple_div() -> None:
    result = expression("Foo / Bar")
    expected = Div(Variable("Foo"), Variable("Bar"))
    assert result == expected


def test_arith_expression() -> None:
    result = expression("Foo + Bar - Foo2 / Bar * Baz + 3")
    expected = Add(
        Sub(
            Add(Variable("Foo"), Variable("Bar")),
            Mul(Div(Variable("Foo2"), Variable("Bar")), Variable("Baz")),
        ),
        Number(3),
    )
    assert result == expected


def test_string() -> None:
    result = expression('"SomeString"')
    expected = String("SomeString")
    assert result == expected


def test_string_with_whitespace() -> None:
    result = expression('"Some String With Whitespace"')
    expected = String("Some String With Whitespace")
    assert result == expected

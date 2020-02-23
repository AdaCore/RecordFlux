from rflx.expression import (
    FALSE,
    TRUE,
    Add,
    And,
    Div,
    Equal,
    Greater,
    Length,
    Less,
    Mul,
    NotEqual,
    Number,
    Or,
    Sub,
    Variable,
)
from rflx.fsm_expression import (
    Binding,
    Comprehension,
    Contains,
    Conversion,
    Field,
    ForAll,
    ForSome,
    Head,
    MessageAggregate,
    NotContains,
    Opaque,
    Present,
    String,
    SubprogramCall,
    Valid,
)
from rflx.fsm_parser import FSMParser
from rflx.identifier import ID


def test_simple_equation() -> None:
    result = FSMParser.expression().parseString("Foo.Bar = abc")[0]
    assert result == Equal(Variable("Foo.Bar"), Variable("abc"))


def test_simple_inequation() -> None:
    result = FSMParser.expression().parseString("Foo.Bar /= abc")[0]
    assert result == NotEqual(Variable("Foo.Bar"), Variable("abc"))


def test_valid() -> None:
    result = FSMParser.expression().parseString("Something'Valid")[0]
    assert result == Valid(Variable("Something"))


def test_opaque() -> None:
    result = FSMParser.expression().parseString("Something'Opaque")[0]
    assert result == Opaque(Variable("Something"))


def test_conjunction() -> None:
    result = FSMParser.expression().parseString("Foo = Bar and Bar /= Baz")[0]
    assert result == And(
        Equal(Variable("Foo"), Variable("Bar")), NotEqual(Variable("Bar"), Variable("Baz"))
    )


def test_conjunction_valid() -> None:
    result = FSMParser.expression().parseString("Foo'Valid and Bar'Valid")[0]
    assert result == And(Valid(Variable("Foo")), Valid(Variable("Bar")))


def test_disjunction_multi() -> None:
    result = FSMParser.expression().parseString("Foo = Bar or Bar /= Baz or Baz'Valid = False")[0]
    assert result == Or(
        Equal(Variable("Foo"), Variable("Bar")),
        NotEqual(Variable("Bar"), Variable("Baz")),
        Equal(Valid(Variable("Baz")), FALSE),
    )


def test_not_in_whitespace_operator() -> None:
    result = FSMParser.expression().parseString("Foo not   in  Bar")[0]
    assert result == NotContains(Variable("Foo"), Variable("Bar"))


def test_disjunction() -> None:
    result = FSMParser.expression().parseString("Foo = Bar or Bar /= Baz")[0]
    assert result == Or(
        Equal(Variable("Foo"), Variable("Bar")), NotEqual(Variable("Bar"), Variable("Baz"))
    )


def test_in_operator() -> None:
    result = FSMParser.expression().parseString("Foo in Bar")[0]
    assert result == Contains(Variable("Foo"), Variable("Bar"))


def test_not_in_operator() -> None:
    result = FSMParser.expression().parseString("Foo not in Bar")[0]
    assert result == NotContains(Variable("Foo"), Variable("Bar"))


def test_parenthesized_expression() -> None:
    result = FSMParser.expression().parseString("Foo = True and (Bar = False or Baz = False)")[0]
    assert result == And(
        Equal(Variable("Foo"), TRUE),
        Or(Equal(Variable("Bar"), FALSE), Equal(Variable("Baz"), FALSE)),
    )


def test_parenthesized_expression2() -> None:
    result = FSMParser.expression().parseString("Foo'Valid and (Bar'Valid or Baz'Valid)")[0]
    assert result == And(Valid(Variable("Foo")), Or(Valid(Variable("Bar")), Valid(Variable("Baz"))))


def test_numeric_constant_expression() -> None:
    result = FSMParser.expression().parseString("Keystore_Message.Length = 0")[0]
    assert result == Equal(Variable("Keystore_Message.Length"), Number(0))


def test_complex_expression() -> None:
    expr = (
        "Keystore_Message'Valid = False "
        "or Keystore_Message.Tag /= KEYSTORE_RESPONSE "
        "or Keystore_Message.Request /= KEYSTORE_REQUEST_PSK_IDENTITIES "
        "or (Keystore_Message.Length = 0 "
        "    and TLS_Handshake.PSK_DHE_KE not in Configuration.PSK_Key_Exchange_Modes)"
    )
    result = FSMParser.expression().parseString(expr)[0]
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
    result = FSMParser.expression().parseString("for some X in Y => X = 3")[0]
    assert result == ForSome("X", Variable("Y"), Equal(Variable("X"), Number(3)))


def test_complex_existential_quantification() -> None:
    expr = (
        "for some E in Server_Hello_Message.Extensions => "
        "(E.Tag = TLS_Handshake.EXTENSION_SUPPORTED_VERSIONS and "
        "(GreenTLS.TLS_1_3 not in TLS_Handshake.Supported_Versions (E.Data).Versions))"
    )
    result = FSMParser.expression().parseString(expr)[0]
    expected = ForSome(
        "E",
        Variable("Server_Hello_Message.Extensions"),
        And(
            Equal(Variable("E.Tag"), Variable("TLS_Handshake.EXTENSION_SUPPORTED_VERSIONS")),
            NotContains(
                Variable("GreenTLS.TLS_1_3"),
                Field(
                    Conversion("TLS_Handshake.Supported_Versions", Variable("E.Data")), "Versions",
                ),
            ),
        ),
    )
    assert result == expected


def test_conjunction_multi() -> None:
    result = FSMParser.expression().parseString("Foo = Bar and Bar /= Baz and Baz = Foo")[0]
    expected = And(
        Equal(Variable("Foo"), Variable("Bar")),
        NotEqual(Variable("Bar"), Variable("Baz")),
        Equal(Variable("Baz"), Variable("Foo")),
    )
    assert result == expected


def test_universal_quantification() -> None:
    result = FSMParser.expression().parseString("for all X in Y => X = Bar")[0]
    assert result == ForAll("X", Variable("Y"), Equal(Variable("X"), Variable("Bar")))


def test_type_conversion_simple() -> None:
    expr = "Foo (Bar) = 5"
    result = FSMParser.expression().parseString(expr)[0]
    expected = Equal(Conversion("Foo", Variable("Bar")), Number(5))
    assert result == expected


def test_field_simple() -> None:
    result = FSMParser.expression().parseString("Bar (Foo).Fld")[0]
    assert result == Field(Conversion("Bar", Variable("Foo")), "Fld")


def test_field_length() -> None:
    result = FSMParser.expression().parseString("Bar (Foo).Fld'Length")[0]
    assert result == Length(Field(Conversion("Bar", Variable("Foo")), "Fld"))


def test_type_conversion() -> None:
    expr = "TLS_Handshake.Supported_Versions (E.Data) = 5"
    result = FSMParser.expression().parseString(expr)[0]
    expected = Equal(Conversion("TLS_Handshake.Supported_Versions", Variable("E.Data")), Number(5))
    assert result == expected


def test_use_type_conversion() -> None:
    expr = "GreenTLS.TLS_1_3 not in TLS_Handshake.Supported_Versions (E.Data).Versions"
    result = FSMParser.expression().parseString(expr)[0]
    expected = NotContains(
        Variable("GreenTLS.TLS_1_3"),
        Field(Conversion("TLS_Handshake.Supported_Versions", Variable("E.Data")), "Versions",),
    )
    assert result == expected


def test_present() -> None:
    result = FSMParser.expression().parseString("Something'Present")[0]
    assert result == Present(Variable("Something"))


def test_list_comprehension_without_condition() -> None:
    result = FSMParser.expression().parseString("[for K in PSKs => K.Identity]")[0]
    expected = Comprehension("K", Variable("PSKs"), Variable("K.Identity"), TRUE)
    assert result == expected


def test_conjunction_present() -> None:
    result = FSMParser.expression().parseString("Foo'Present and Bar'Present")[0]
    assert result == And(Present(Variable("Foo")), Present(Variable("Bar")))


def test_length_lt() -> None:
    result = FSMParser.expression().parseString("Foo'Length < 100")[0]
    assert result == Less(Length(Variable("Foo")), Number(100))


def test_field_length_lt() -> None:
    result = FSMParser.expression().parseString("Bar (Foo).Fld'Length < 100")[0]
    assert result == Less(Length(Field(Conversion("Bar", Variable("Foo")), "Fld")), Number(100))


def test_list_comprehension() -> None:
    result = FSMParser.expression().parseString("[for E in List => E.Bar when E.Tag = Foo]")[0]
    assert result == Comprehension(
        "E", Variable("List"), Variable("E.Bar"), Equal(Variable("E.Tag"), Variable("Foo")),
    )


def test_head_attribute() -> None:
    result = FSMParser.expression().parseString("Foo'Head")[0]
    assert result == Head(Variable("Foo"))


def test_head_attribute_comprehension() -> None:
    result = FSMParser.expression().parseString("[for E in List => E.Bar when E.Tag = Foo]'Head")[0]
    assert result == Head(
        Comprehension(
            "E", Variable("List"), Variable("E.Bar"), Equal(Variable("E.Tag"), Variable("Foo")),
        )
    )


def test_gt() -> None:
    result = FSMParser.expression().parseString("Server_Name_Extension.Data_Length > 0")[0]
    assert result == Greater(Variable("Server_Name_Extension.Data_Length"), Number(0))


def test_list_head_field_simple() -> None:
    result = FSMParser.expression().parseString("Foo'Head.Data")[0]
    assert result == Field(Head(Variable("Foo")), "Data")


def test_list_head_field() -> None:
    result = FSMParser.expression().parseString(
        "[for E in List => E.Bar when E.Tag = Foo]'Head.Data"
    )[0]
    assert result == Field(
        Head(
            Comprehension(
                "E", Variable("List"), Variable("E.Bar"), Equal(Variable("E.Tag"), Variable("Foo")),
            )
        ),
        "Data",
    )


def test_complex() -> None:
    result = FSMParser.expression().parseString(
        "(for some S in TLS_Handshake.Key_Share_CH ([for E in Client_Hello_Message.Extensions "
        "=> E when E.Tag = TLS_Handshake.EXTENSION_KEY_SHARE]'Head.Data).Shares => S.Group = "
        "Selected_Group) = False"
    )[0]
    expected = Equal(
        ForSome(
            "S",
            Field(
                Conversion(
                    "TLS_Handshake.Key_Share_CH",
                    Field(
                        Head(
                            Comprehension(
                                "E",
                                Variable("Client_Hello_Message.Extensions"),
                                Variable("E"),
                                Equal(
                                    Variable("E.Tag"),
                                    Variable("TLS_Handshake.EXTENSION_KEY_SHARE"),
                                ),
                            )
                        ),
                        "Data",
                    ),
                ),
                "Shares",
            ),
            Equal(Variable("S.Group"), Variable("Selected_Group")),
        ),
        FALSE,
    )
    assert result == expected


def test_simple_aggregate() -> None:
    result = FSMParser.expression().parseString("Message'(Data => Foo)")[0]
    expected = MessageAggregate("Message", {ID("Data"): Variable("Foo")})
    assert result == expected


def test_null_aggregate() -> None:
    result = FSMParser.expression().parseString("Message'(null message)")[0]
    expected = MessageAggregate("Message", {})
    assert result == expected


def test_complex_aggregate() -> None:
    result = FSMParser.expression().parseString(
        "Complex.Message'(Data1 => Foo, Data2 => Bar, Data3 => Baz)"
    )[0]
    expected = MessageAggregate(
        "Complex.Message",
        {ID("Data1"): Variable("Foo"), ID("Data2"): Variable("Bar"), ID("Data3"): Variable("Baz")},
    )
    assert result == expected


def test_simple_function_call() -> None:
    result = FSMParser.expression().parseString("Fun (Parameter)")[0]
    expected = Conversion("Fun", Variable("Parameter"))
    assert result == expected


def test_complex_function_call() -> None:
    result = FSMParser.expression().parseString("Complex_Function (Param1, Param2, Param3)")[0]
    expected = SubprogramCall(
        "Complex_Function", [Variable("Param1"), Variable("Param2"), Variable("Param3")],
    )
    assert result == expected


def test_simple_binding() -> None:
    result = FSMParser.expression().parseString("M1'(Data => B1) where B1 = M2'(Data => B2)")[0]
    expected = Binding(
        MessageAggregate("M1", {ID("Data"): Variable("B1")}),
        {ID("B1"): MessageAggregate("M2", {ID("Data"): Variable("B2")})},
    )
    assert result == expected


def test_multi_binding() -> None:
    result = FSMParser.expression().parseString(
        "M1'(Data1 => B1, Data2 => B2) where B1 = M2'(Data => B2), B2 = M2'(Data => B3)"
    )[0]
    expected = Binding(
        MessageAggregate("M1", {ID("Data1"): Variable("B1"), ID("Data2"): Variable("B2")}),
        {
            ID("B1"): MessageAggregate("M2", {ID("Data"): Variable("B2")}),
            ID("B2"): MessageAggregate("M2", {ID("Data"): Variable("B3")}),
        },
    )
    assert result == expected


def test_nested_binding() -> None:
    result = FSMParser.expression().parseString(
        "M1'(Data => B1) where B1 = M2'(Data => B2) where B2 = M3'(Data => B3)"
    )[0]
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
    result = FSMParser.expression().parseString("Foo + Bar")[0]
    expected = Add(Variable("Foo"), Variable("Bar"))
    assert result == expected


def test_simple_sub() -> None:
    result = FSMParser.expression().parseString("Foo - Bar")[0]
    expected = Sub(Variable("Foo"), Variable("Bar"))
    assert result == expected


def test_simple_mul() -> None:
    result = FSMParser.expression().parseString("Foo * Bar")[0]
    expected = Mul(Variable("Foo"), Variable("Bar"))
    assert result == expected


def test_simple_div() -> None:
    result = FSMParser.expression().parseString("Foo / Bar")[0]
    expected = Div(Variable("Foo"), Variable("Bar"))
    assert result == expected


def test_arith_expression() -> None:
    result = FSMParser.expression().parseString("Foo + Bar - Foo2 / Bar * Baz + 3")[0]
    expected = Add(
        Sub(
            Add(Variable("Foo"), Variable("Bar")),
            Mul(Div(Variable("Foo2"), Variable("Bar")), Variable("Baz")),
        ),
        Number(3),
    )
    assert result == expected


def test_string() -> None:
    result = FSMParser.expression().parseString('"SomeString"')[0]
    expected = String("SomeString")
    assert result == expected


def test_string_with_whitespace() -> None:
    result = FSMParser.expression().parseString('"Some String With Whitespace"')[0]
    expected = String("Some String With Whitespace")
    assert result == expected

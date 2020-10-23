import librecordfluxdsllang as rflxdsl  # type: ignore

ctx = rflxdsl.AnalysisContext()


def test_empty_file() -> None:
    unit = ctx.get_from_buffer("empty.rflx", "")
    assert unit.root is None


def test_empty_package() -> None:
    unit = ctx.get_from_buffer(
        "empty_package.rflx",
        """
            package Empty_Package is
            end Empty_Package;
        """,
    )
    assert unit.root.f_package_declaration.f_name_start.text == "Empty_Package"
    assert not unit.root.f_package_declaration.f_content.text
    assert unit.root.f_package_declaration.f_name_end.text == "Empty_Package"


def test_modular_type() -> None:
    unit = ctx.get_from_buffer(
        "modular.rflx",
        """
            type Modular_Type is mod 2 ** 9;
        """,
        rule=rflxdsl.GrammarRule.type_declaration_rule,
    )
    assert unit.root.f_identifier.text == "Modular_Type"
    assert unit.root.f_type_definition.kind_name == "ModularTypeDef"
    assert unit.root.f_type_definition.f_mod.kind_name == "MathematicalExpression"
    assert unit.root.f_type_definition.f_mod.text == "2 ** 9"

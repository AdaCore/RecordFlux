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
    assert unit.root.f_name_start.text == "Empty_Package"
    assert not unit.root.f_content.text
    assert unit.root.f_name_end.text == "Empty_Package"

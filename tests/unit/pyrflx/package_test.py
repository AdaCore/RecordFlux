from rflx.pyrflx.package import Package


def test_package_name() -> None:
    p = Package("Test")
    assert p.name == "Test"


def test_package_iterator(tlv_package: Package) -> None:
    assert [m.name for m in tlv_package] == ["Message"]

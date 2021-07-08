import re
from pathlib import Path
from xml.etree import ElementTree
from xml.etree.ElementTree import Element

import pytest

import tools
from tools.iana_to_rflx import (
    IANAError,
    _get_name_tag,
    _normalize_hex_value,
    _normalize_name,
    _normalize_value,
    cli,
    iana_to_rflx,
)


@pytest.mark.parametrize(
    "registry",
    [
        "test_all_name_tags",
        "test_duplicate_identifiers",
        "test_duplicate_values",
        "test_empty_registry",
        "test_long_comment",
        "test_no_value_no_name_tags",
        "test_numeric_identifiers",
        "test_registry_comma_sep_hex_values",
        "test_registry_decimal_values",
        "test_registry_decimal_values_code_tag",
        "test_registry_decimal_values_name_and_description_tag",
        "test_registry_decimal_values_type_tag",
        "test_registry_hex_values",
        "test_registry_no_title_last_updated",
        "test_registry_no_type_name",
        "test_reserved_unassigned",
        "test_reserved_word",
    ],
)
def test_registry_output(registry: Path, tmp_path: Path) -> None:
    registry_xml_file = f"tests/iana_to_rflx/test_registries/{registry}.xml"
    generated_spec_file = Path(f"{tmp_path / registry}.rflx")
    with open(registry_xml_file, "r") as f:
        iana_to_rflx(f, True, generated_spec_file)

    with open(generated_spec_file, "r") as generated_file, open(
        Path(f"tests/iana_to_rflx/generated_valid_specs/{registry}.rflx"), "r"
    ) as known_valid_file:
        generated = generated_file.read()
        known_valid = known_valid_file.read()
        assert re.sub(
            r"Generation date: [0-9]{4}-[0-9]{2}-[0-9]{2}",
            "Generation date: .",
            generated,
        ) == re.sub(
            r"Generation date: [0-9]{4}-[0-9]{2}-[0-9]{2}",
            "Generation date: .",
            known_valid,
        )


def test_no_package_name() -> None:
    with pytest.raises(IANAError, match=r"^no registry ID found$"):
        with open("tests/iana_to_rflx/test_registries/test_registry_no_package_name.xml", "r") as f:
            iana_to_rflx(f, True)


def test_normalize_hex_value() -> None:
    assert _normalize_hex_value("0xAA,0x11") == "16#AA11#"
    assert _normalize_hex_value("0x0123") == "16#0123#"
    assert _normalize_hex_value("0x081D-0x08FF") == "16#08FF#"
    assert _normalize_hex_value("0xCD-CF,*") == "16#CFFF#"
    assert _normalize_hex_value("0xCB,*") == "16#CBFF#"
    assert _normalize_hex_value("0xCC,0xAF-FF") == "16#CCFF#"
    with pytest.raises(IANAError, match=r'^cannot normalize hex value range "XY-XY"$'):
        _normalize_hex_value("XY-XY")
    with pytest.raises(IANAError, match=r'^cannot normalize hex value "XY"$'):
        _normalize_hex_value("XY")


def test_normalize_value() -> None:
    assert _normalize_value("1") == ("1", 1)
    assert _normalize_value("8") == ("8", 4)
    assert _normalize_value("0-15") == ("15", 4)
    assert _normalize_value("0xAA,0x11") == ("16#AA11#", 16)
    assert _normalize_value("0x0123") == ("16#0123#", 12)


def test_normalize_name() -> None:
    assert _normalize_name("A") == "A"
    assert _normalize_name("TLS_RSA_WITH_NULL_MD5") == "TLS_RSA_WITH_NULL_MD5"
    assert _normalize_name("Quotes Server") == "Quotes_Server"
    assert _normalize_name("Ethernet (10Mb)") == "Ethernet_10Mb"
    assert _normalize_name("NSFNET-IGP") == "NSFNET_IGP"
    assert _normalize_name("RSVP-E2E-\nIGNORE") == "RSVP_E2E_IGNORE"
    assert (
        _normalize_name("TIA-102 Project 25 Common Air Interface (CAI)")
        == "TIA_102_Project_25_Common_Air_Interface_CAI"
    )


def test_get_name_tag() -> None:
    with open("tests/iana_to_rflx/test_registries/test_all_name_tags.xml", "r") as f:
        registry = ElementTree.fromstring(f.read())
        sub_registry = registry.find("iana:registry", tools.iana_to_rflx.NAMESPACE)
        assert isinstance(sub_registry, Element)
        record = sub_registry.find("iana:record", tools.iana_to_rflx.NAMESPACE)
        assert isinstance(record, Element)
        assert _get_name_tag(record) == "name"


def test_cli(tmp_path: Path) -> None:
    out = tmp_path / "test_all_name_tags.rflx"
    assert (
        cli(["-a", "-o", str(out), "tests/iana_to_rflx/test_registries/test_all_name_tags.xml"])
        == 0
    )
    assert (
        cli(
            [
                "-a",
                "-o",
                str(out),
                "tests/iana_to_rflx/test_registries/test_registry_invalid_hex_value.xml",
            ]
        )
        == 'cannot normalize hex value "0xXX,0xZZ"'
    )

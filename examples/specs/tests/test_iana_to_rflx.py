import filecmp
import re
from pathlib import Path
from typing import List
from xml.etree import ElementTree
from xml.etree.ElementTree import Element

import pytest

from tools.iana_to_rflx import (
    IANAError,
    _get_name_tag,
    _normalize_hex_value,
    _normalize_name,
    _normalize_records,
    _normalize_value,
    cli,
    iana_to_rflx,
    write_registry,
)

NAMESPACE = {"iana": "http://www.iana.org/assignments"}


@pytest.fixture(name="root_element", scope="session")
def fixture_root_element() -> Element:
    with open("tests/iana_to_rflx/test_registries/test.xml", "r") as xml_f:
        xml_str = xml_f.read()
    root = ElementTree.fromstring(xml_str)
    return root


@pytest.fixture(name="registries", scope="session")
def fixture_registries(root_element: Element) -> List[Element]:
    return root_element.findall(root_element.tag, NAMESPACE)


@pytest.fixture(name="all_records", scope="session")
def fixture_all_records(registries: Element) -> List[Element]:
    return [
        record for registry in registries for record in registry.findall("iana:record", NAMESPACE)
    ]


@pytest.fixture(name="records_registry_hex_values", scope="session")
def fixture_hex_value_records(registries: Element) -> List[Element]:
    return [
        record
        for registry in registries
        for record in registry.findall("iana:record", NAMESPACE)
        if registry.get("id") == "test_registry_hex_values"
    ]


@pytest.fixture(name="duplicate_value_records", scope="session")
def fixture_duplicate_value_records(registries: Element) -> List[Element]:
    return [
        record
        for registry in registries
        for record in registry.findall("iana:record", NAMESPACE)
        if registry.get("id") == "test_duplicate_values"
    ]


@pytest.fixture(name="all_name_tags_record", scope="session")
def fixture_all_name_tags_record(registries: Element) -> Element:
    for registry in registries:
        if registry.get("id") == "test_all_name_tags":
            elem = registry.find("iana:record", NAMESPACE)
            assert elem is not None
            return elem
    assert False


@pytest.fixture(name="no_value_no_name_tags_record", scope="session")
def fixture_no_value_no_name_tags(registries: Element) -> List[Element]:
    return [
        record
        for registry in registries
        for record in registry.findall("iana:record", NAMESPACE)
        if registry.get("id") == "test_no_value_no_name_tags"
    ]


@pytest.fixture(name="reserved_unassigned_records", scope="session")
def fixture_unassigned_reserved(registries: Element) -> List[Element]:
    return [
        record
        for registry in registries
        for record in registry.findall("iana:record", NAMESPACE)
        if registry.get("id") == "test_reserved_unassigned"
    ]


def test_normalize_hex_value() -> None:
    assert _normalize_hex_value("0xAA,0x11") == "16#AA11#"
    assert _normalize_hex_value("0x0123") == "16#0123#"
    with pytest.raises(IANAError, match="Cannot normalize hex value XY"):
        _normalize_hex_value("XY")


def test_normalize_value() -> None:
    assert _normalize_value("1") == ("1", 1)
    assert _normalize_value("8") == ("8", 4)
    assert _normalize_value("0xAA,0x11") == ("16#AA11#", 16)
    assert _normalize_value("0x0123") == ("16#0123#", 12)


def test_normalize_name() -> None:
    assert _normalize_name("TLS_RSA_WITH_NULL_MD5") == "Tls_Rsa_With_Null_Md5"
    assert _normalize_name("Quotes Server") == "Quotes_Server"
    assert _normalize_name("Ethernet (10Mb)") == "Ethernet_10mb"
    assert _normalize_name("NSFNET-IGP") == "Nsfnet_Igp"
    assert _normalize_name("RSVP-E2E-\nIGNORE") == "Rsvp_E2e_Ignore"
    assert (
        _normalize_name("TIA-102 Project 25 Common Air Interface (CAI)")
        == "Tia_102_Project_25_Common_Air_Interface_Cai"
    )


def test_normalize_records(records_registry_hex_values: List[Element]) -> None:
    normalized_records, highest_bit_length = _normalize_records(
        records_registry_hex_values, "REGISTRY_NAME", []
    )
    assert len(normalized_records) == 2
    record_sha_256_legacy = normalized_records[0]
    record_sha384 = normalized_records[1]
    assert record_sha_256_legacy.name == "Rsa_Pkcs1_Sha256_Legacy"
    assert record_sha_256_legacy.value == "16#0420#"
    assert record_sha_256_legacy.comment == ["recommended = N, Ref: draft-davidben-tls13-pkcs1-00"]
    assert record_sha_256_legacy.bit_length == 12
    assert record_sha384.name == "Rsa_Pkcs1_Sha384"
    assert record_sha384.value == "16#0501#"
    assert record_sha384.comment == []
    assert record_sha384.bit_length == 12
    assert highest_bit_length == 12


def test_normalize_duplicate_value_records(duplicate_value_records: List[Element]) -> None:
    normalized_records, highest_bit_length = _normalize_records(
        duplicate_value_records, "REGISTRY_NAME", []
    )
    assert len(normalized_records) == 1
    unified_record = normalized_records.pop()
    assert unified_record.name == "REGISTRY_NAME_5"
    assert unified_record.comment == [
        "comment = comment from duplicate value 1, comment = comment from duplicate value",
        " 2, comment = comment from duplicate value 3, alternative_name = Duplicate_Value",
        "_2, alternative_name = Duplicate_Value_3",
    ]
    assert unified_record.value
    assert highest_bit_length == 3


def test_normalize_rec_missing_tag(no_value_no_name_tags_record: List[Element]) -> None:
    normalized_records, highest_bit_length = _normalize_records(
        no_value_no_name_tags_record, "REGISTRY_NAME", []
    )
    assert normalized_records == []
    assert highest_bit_length == 0


def test_normalize_reserved_values(reserved_unassigned_records: List[Element]) -> None:
    normalized_records, highest_bit_length = _normalize_records(
        reserved_unassigned_records, "REGISTRY_NAME", []
    )
    assert normalized_records == []
    assert highest_bit_length == 12


def test_get_name_tag(all_name_tags_record: Element) -> None:
    assert _get_name_tag(all_name_tags_record) == "name"


def test_write_registries(tmp_path: Path, registries: List[Element]) -> None:
    for registry in registries:
        identifier = registry.get("id")
        assert identifier is not None
        tmp_registry_path = tmp_path / identifier

        with open(tmp_registry_path, "w+") as f:
            write_registry(registry, True, f, [])
        assert filecmp.cmp(
            tmp_registry_path, Path("tests/iana_to_rflx/generated_types") / identifier
        )


def test_iana_to_rflx(tmp_path: Path) -> None:
    out_path = tmp_path / "iana_to_rflx_test.rflx"
    with open("tests/iana_to_rflx/test_registries/test.xml", "r") as f:
        iana_to_rflx(f, True, out_path)

    with open(out_path, "r") as generated_file:
        generated = generated_file.read()
        generated = re.sub(
            r"Generation date: [0-9]{4}-[0-9]{2}-[0-9]{2}", "Generation date: .", generated
        )
        generated = re.sub(
            r"AUTOMATICALLY GENERATED BY .* DO NOT EDIT\.",
            "AUTOMATICALLY GENERATED BY . DO NOT EDIT.",
            generated,
        )
    with open("tests/iana_to_rflx/test_registries/test_parameters.rflx", "r") as known_valid_file:
        known_valid = known_valid_file.read()
    assert generated == known_valid


def test_no_package_name() -> None:
    with pytest.raises(IANAError, match=r"^No registry ID found$"):
        with open("tests/iana_to_rflx/test_registries/test_registry_no_package_name.xml", "r") as f:
            iana_to_rflx(f, True)


def test_no_type_name(tmp_path: Path) -> None:
    out_path = tmp_path / "test_registry_no_type_name.rflx"
    with pytest.raises(IANAError, match=r"^could not find registry title$"):
        with open("tests/iana_to_rflx/test_registries/test_registry_no_type_name.xml", "r") as f:
            iana_to_rflx(f, True, out_path)


def test_no_title_last_updated(tmp_path: Path) -> None:
    out_path = tmp_path / "test_registry_no_title_last_updated.rflx"
    with open(
        "tests/iana_to_rflx/test_registries/test_registry_no_title_last_updated.xml", "r"
    ) as f:
        iana_to_rflx(f, True, out_path)

    with open(out_path, "r") as generated_file:
        generated = generated_file.read()
        generated = re.sub(
            r"Generation date: [0-9]{4}-[0-9]{2}-[0-9]{2}", "Generation date: .", generated
        )
        generated = re.sub(
            r"AUTOMATICALLY GENERATED BY .* DO NOT EDIT\.",
            "AUTOMATICALLY GENERATED BY . DO NOT EDIT.",
            generated,
        )
    with open(
        "tests/iana_to_rflx/test_registries/test_registry_no_title_last_updated.rflx", "r"
    ) as known_valid_file:
        known_valid = known_valid_file.read()
    assert generated == known_valid


def test_cli(tmp_path: Path) -> None:
    out = tmp_path / "test_paramerers.rflx"
    assert cli(["-a", "-o", str(out), "tests/iana_to_rflx/test_registries/test.xml"]) == 0
    assert (
        cli(["-a", "-o", str(out), "tests/iana_to_rflx/test_registries/test_invalid_hex_value.xml"])
        == "Cannot normalize hex value 0xXX,0xZZ"
    )

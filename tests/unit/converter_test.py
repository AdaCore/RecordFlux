import textwrap
from pathlib import Path
from typing import List, Union, cast

import asn1tools as asn1
import hypothesis as hypot
import hypothesis.strategies as strats
import pytest
from asn1tools.codecs.ber import encode_signed_integer

from rflx.common import STDIN
from rflx.converter import iana
from rflx.converter.asn1 import AsnTypeConverter
from rflx.error import RecordFluxError
from rflx.model.model import Model
from rflx.pyrflx import PyRFLX
from rflx.pyrflx.typevalue import MessageValue
from tests.const import DATA_DIR

ASN_SHORT_LEN = 10
ASN_SHORT_INTS = strats.integers(
    min_value=-(256 ** (ASN_SHORT_LEN - 1)),
    max_value=256 ** (ASN_SHORT_LEN - 1) - 1,
)
ASN_SHORT_IA5STRINGS = strats.text(
    alphabet=strats.characters(max_codepoint=127),
    max_size=ASN_SHORT_LEN,
)
ASN_SHORT_OCTET_STRINGS = strats.text(max_size=ASN_SHORT_LEN)


def test_convert_iana_invalid_xml(tmp_path: Path) -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^<stdin>:1:0: converter: error: invalid XML document: "
            r"syntax error: line 1, column 0$"
        ),
    ):
        iana.convert(
            data="NOT A VALID XML DOCUMENT",
            source=STDIN,
            always_valid=False,
            output_dir=tmp_path,
        )


@pytest.mark.parametrize(
    "xml_str,error",
    [
        pytest.param(
            """\
                <registry xmlns="http://www.iana.org/assignments" id="test_registry">
                    <title>Test registry</title>
                    <registry id="test">
                        <title>Test registry</title>
                        <record>
                            <value>0xcejunk</value>
                            <name>UNASSIGNED</name>
                            <description>UNASSIGNED range ignored</description>
                        </record>
                    </registry>
                </registry>
            """,
            r'^converter: error: cannot normalize hex value "0xcejunk"',
            id="value",
        ),
        pytest.param(
            """\
                <registry xmlns="http://www.iana.org/assignments" id="test_registry">
                    <title>Test registry</title>
                    <registry id="test">
                        <title>Test registry</title>
                        <record>
                            <value>0xce-0xdf</value>
                            <name>UNASSIGNED</name>
                            <description>UNASSIGNED range ignored</description>
                        </record>
                    </registry>
                </registry>
            """,
            r'^converter: error: cannot normalize hex value range "0xce-0xdf"',
            id="range",
        ),
    ],
)
def test_convert_iana_invalid_hex_value(xml_str: str, error: str, tmp_path: Path) -> None:
    with pytest.raises(RecordFluxError, match=error):
        iana.convert(
            data=textwrap.dedent(xml_str),
            source=STDIN,
            always_valid=False,
            output_dir=tmp_path,
            reproducible=True,
        )


def test_convert_iana_invalid_no_registry_id(tmp_path: Path) -> None:
    with pytest.raises(
        RecordFluxError,
        match=(r"^<stdin>:0:0: converter: error: no registry ID found$"),
    ):
        iana.convert(
            data=textwrap.dedent(
                """\
                <registry xmlns="http://www.iana.org/assignments">
                </registry>
                """
            ),
            source=STDIN,
            always_valid=False,
            output_dir=tmp_path,
        )


@pytest.mark.parametrize(
    "xml_str,rflx_str",
    [
        pytest.param(
            """
                <registry xmlns="http://www.iana.org/assignments" id="test_registry">
                </registry>
            """,
            """\
                package Test_Registry is

                end Test_Registry;
            """,
            id="empty",
        ),
        pytest.param(
            """
                <registry xmlns="http://www.iana.org/assignments" id="test_registry">
                    <updated>YYYY-MM-DD</updated>
                </registry>
            """,
            """\
                -- Registry last updated on YYYY-MM-DD

                package Test_Registry is

                end Test_Registry;
            """,
            id="empty, last updated",
        ),
        pytest.param(
            """
                <registry xmlns="http://www.iana.org/assignments" id="test_registry">
                    <title>Test registry</title>
                    <registry id="test">
                        <title>Test registry</title>
                        <record>
                            <value>10</value>
                            <name>Value</name>
                            <description>First value</description>
                        </record>
                        <record>
                            <value>11</value>
                            <name>Value</name>
                            <description>Second value with same name as first</description>
                        </record>
                        <record>
                            <value>12</value>
                            <name>42</name>
                            <description>Third value starting with digit</description>
                        </record>
                        <record>
                            <value>13</value>
                            <name>Begin</name>
                            <description>Reserved words are suffixed with value</description>
                        </record>
                        <record>
                            <name>Unused</name>
                            <description>Records without a value are ignored</description>
                        </record>
                        <record>
                            <invalid>Record skipped: no child element usable as name</invalid>
                        </record>
                        <record>
                            <value>14</value>
                            <name>UNASSIGNED</name>
                            <description>Name UNASSIGNED/RESERVED ignored</description>
                        </record>
                        <record>
                            <value>15</value>
                            <name>RESERVED</name>
                            <description>Name UNASSIGNED/RESERVED ignored</description>
                        </record>
                        <record>
                            <value>0xa1a1</value>
                            <name>UNASSIGNED</name>
                            <description>UNASSIGNED range ignored</description>
                        </record>
                        <record>
                            <value>0x0a,0xFF</value>
                            <name>UNASSIGNED</name>
                            <description>UNASSIGNED range ignored</description>
                        </record>
                        <record>
                            <value>0xCC,0xAF-FF</value>
                            <name>UNASSIGNED</name>
                            <description>UNASSIGNED range ignored</description>
                        </record>
                        <record>
                            <value>0xCE,*</value>
                            <name>UNASSIGNED</name>
                            <description>UNASSIGNED range ignored</description>
                        </record>
                        <record>
                            <value>0xCE-DF,*</value>
                            <name>UNASSIGNED</name>
                            <description>UNASSIGNED range ignored</description>
                        </record>
                        <record>
                            <value>0xff00-0xffff</value>
                            <name>UNASSIGNED</name>
                            <description>UNASSIGNED range ignored</description>
                        </record>
                    </registry>

                    <registry id="has_no_title">
                        <record>
                            <value>5</value>
                            <name>Elem</name>
                            <description>First value, won't be used</description>
                        </record>
                    </registry>

                    <registry id="empty">
                        <title>Empty registry</title>
                        <record>
                            <name>Ignored1</name>
                        </record>
                        <record>
                            <name>Ignored2</name>
                        </record>
                    </registry>
                </registry>
            """,
            """\
                -- Test registry
                package Test_Registry is

                   type Test_Registry is
                      (-- description = First value
                       Value_10 => 10,

                       -- description = Second value with same name as first
                       Value_11 => 11,

                       -- description = Third value starting with digit
                       Test_Registry_42 => 12,

                       -- description = Reserved words are suffixed with value
                       Begin_13 => 13)
                   with Size => 16;

                end Test_Registry;
            """,
            id="special records",
        ),
        pytest.param(
            """
                <registry xmlns="http://www.iana.org/assignments" id="test_registry">
                    <title>Test registry</title>
                    <registry id="test">
                        <title>Test registry</title>
                        <record>
                            <value>11</value>
                            <name>Value</name>
                            <description>First value</description>
                        </record>
                        <record>
                            <value>12</value>
                            <name>Value</name>
                            <description>Second value (same name as first!)</description>
                        </record>
                    </registry>
                </registry>
            """,
            """\
                -- Test registry
                package Test_Registry is

                   type Test_Registry is
                      (-- description = First value
                       Value_11 => 11,

                       -- description = Second value (same name as first!)
                       Value_12 => 12)
                   with Size => 4;

                end Test_Registry;
            """,
            id="simple",
        ),
    ],
)
def test_convert_iana(xml_str: str, rflx_str: str, tmp_path: Path) -> None:
    iana.convert(
        data=textwrap.dedent(xml_str),
        source=STDIN,
        always_valid=False,
        output_dir=tmp_path,
        reproducible=True,
    )
    output_file = tmp_path / "test_registry.rflx"
    assert output_file.exists(), "No output file generated"
    assert output_file.is_file(), "Output is not a file"
    assert output_file.read_text(
        encoding="utf-8"
    ) == "-- AUTOMATICALLY GENERATED. DO NOT EDIT.\n" + textwrap.dedent(rflx_str)


@hypot.given(identifier=ASN_SHORT_INTS, question=ASN_SHORT_IA5STRINGS)
@hypot.example(identifier=0, question="")
@hypot.settings(deadline=None)
@pytest.mark.xdist_group(name="test")
def test_convert_asn1_test_decode(
    identifier: int,
    question: str,
) -> None:
    test_spec = asn1.compile_files(str(DATA_DIR / "test.asn"))
    test = AsnTypeConverter(skip_proof=False).convert_spec(test_spec)

    types = test.values()
    model = PyRFLX(model=Model(types=[*types]))
    pkg = model.package("Test")

    (expected := pkg.new_message("Question")).parse(
        test_spec.encode("Question", {"id": identifier, "question": question})
    )

    assert expected.get("Tag_Class") == 0
    assert expected.get("Tag_Form") == 1
    assert expected.get("Tag_Num") == 16

    assert expected.get("Untagged_Value_id_Untagged_Value") == encode_signed_integer(identifier)

    assert expected.get("Untagged_Value_question_Untagged_Value") == question.encode()


@hypot.given(
    range_=ASN_SHORT_INTS,
    name=ASN_SHORT_OCTET_STRINGS,
    payload=strats.one_of(
        ASN_SHORT_INTS,
        strats.lists(ASN_SHORT_INTS, max_size=3),
    ),
)
@hypot.example(range_=0, name="", payload=0)
@hypot.example(range_=0, name="", payload=[])
@hypot.settings(deadline=None)
@pytest.mark.xdist_group(name="rocket")
def test_convert_asn1_rocket_decode(
    range_: int,
    name: str,
    payload: Union[int, List[int]],
) -> None:
    rocket_spec = asn1.compile_files(str(DATA_DIR / "rocket_mod.asn"))
    rocket = AsnTypeConverter(skip_proof=False).convert_spec(rocket_spec)

    types = rocket.values()
    model = PyRFLX(model=Model(types=[*types]))
    pkg = model.package("World_Schema")

    name1 = name.encode()
    is_one = isinstance(payload, int)
    payload1 = ("one", payload) if is_one else ("many", payload)
    (expected := pkg.new_message("Rocket")).parse(
        rocket_spec.encode(
            "Rocket",
            {"range": range_, "name": name1, "ident": "1.2.3.4", "payload": payload1},
        )
    )

    assert expected.get("Untagged_Value_range_Untagged_Value") == encode_signed_integer(range_)
    assert expected.get("Untagged_Value_name_Untagged_Value") == name1
    assert expected.get("Untagged_Value_ident_Untagged_Value") == b"\x2a\x03\x04"

    got_payload = expected.get(f"Untagged_Value_payload_{'one' if is_one else 'many'}_Value")
    if is_one:
        assert got_payload == encode_signed_integer(payload)
    else:
        assert [i.bytestring[2:] for i in cast(List[MessageValue], got_payload)] == [
            encode_signed_integer(i) for i in cast(List[int], payload)
        ]


@hypot.given(
    name=ASN_SHORT_OCTET_STRINGS,
    variant=strats.from_regex("[ac][ie]", fullmatch=True),
    payload=strats.one_of(
        ASN_SHORT_INTS,
        strats.lists(ASN_SHORT_INTS, max_size=4),
    ),
)
@hypot.example(name="", variant="ai", payload=[])
@hypot.example(name="", variant="ae", payload=[])
@hypot.example(name="", variant="ci", payload=[])
@hypot.example(name="", variant="ce", payload=[])
@hypot.settings(deadline=None)
@pytest.mark.xdist_group(name="tagged")
def test_convert_asn1_tagged_decode(
    name: str,
    variant: str,
    payload: Union[int, List[int]],
) -> None:
    tagged_spec = asn1.compile_files(str(DATA_DIR / "tagged.asn"))
    tagged = AsnTypeConverter(skip_proof=False).convert_spec(tagged_spec)

    types = tagged.values()
    model = PyRFLX(model=Model(types=[*types]))
    pkg = model.package("Tagged_Test")

    name1 = name.encode()
    is_one = isinstance(payload, int)
    choice = variant + ("p" if is_one else "c")
    payload1 = (choice, payload)
    (expected := pkg.new_message("Tagged")).parse(
        tagged_spec.encode("Tagged", {"name": name1, "payload": payload1})
    )

    assert expected.get("Untagged_Value_name_Untagged_Value") == name1

    payload_field = f"Untagged_Value_payload_{choice}_Value"
    if variant.endswith("e"):  # Explicit tagging
        payload_field += "_Inner_Untagged_Value"
    if is_one:
        assert expected.get(payload_field) == encode_signed_integer(payload)
    else:
        assert [
            i.bytestring[2:] for i in cast(List[MessageValue], expected.get(payload_field))
        ] == [encode_signed_integer(i) for i in cast(List[int], payload)]


def test_convert_asn1_snmpv1_decode() -> None:
    snmpv1_spec = asn1.compile_files(
        [
            str(DATA_DIR / "rfc1155.asn"),
            str(DATA_DIR / "rfc1157.asn"),
        ]
    )
    snmpv1 = AsnTypeConverter(skip_proof=True).convert_spec(snmpv1_spec)

    types = snmpv1.values()
    model = PyRFLX(model=Model(types=[*types]))
    pkg = model.package("RFC1157_SNMP")

    (expected := pkg.new_message("Message")).parse(
        b"0G\x02\x01\x00\x04\x06public\xa2:\x02\x01'\x02\x01\x00\x02\x01\x000/"
        b"0\x11\x06\x08+\x06\x01\x02\x01\x01\x05\x00\x04\x05B6300"
        b"0\x1a\x06\x08+\x06\x01\x02\x01\x01\x06\x00\x04\x0eChandra's cube"
    )

    assert expected.get("Tag_Class") == 0
    assert expected.get("Tag_Form") == 1
    assert expected.get("Tag_Num") == 16

    pdu = "Untagged_Value_data_get_response_Value_"

    assert expected.get(pdu + "request_id_Untagged_Value") == b"\x27"
    assert expected.get(pdu + "error_status_Untagged_Value") == b"\x00"
    assert expected.get(pdu + "error_index_Untagged_Value") == b"\x00"

    variable_bindings = pdu + "variable_bindings_Untagged_Value"
    assert [
        i.bytestring[2:] for i in cast(List[MessageValue], expected.get(variable_bindings))
    ] == [
        b"\x06\x08+\x06\x01\x02\x01\x01\x05\x00" + b"\x04\x05B6300",
        b"\x06\x08+\x06\x01\x02\x01\x01\x06\x00" + b"\x04\x0eChandra's cube",
    ]

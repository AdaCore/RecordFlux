import textwrap
from pathlib import Path

import pytest

from rflx.common import STDIN
from rflx.converter import iana
from rflx.error import RecordFluxError


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
    ("xml_str", "error"),
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
    ("xml_str", "rflx_str"),
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
                        <record>
                            <value>100-200</value>
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
                        </record>
                        <record>
                            <value>12</value>
                            <name>Value</name>
                            <description>Second value (same name as first!)</description>
                        </record>
                        <record>
                            <value>0xB</value>
                            <name>Hex</name>
                            <description>Third value (hex)</description>
                        </record>
                        <record>
                            <value>0xB</value>
                            <name>Duplicate</name>
                            <description>Third value (duplicate)</description>
                        </record>
                    </registry>
                </registry>
            """,
            """\
                -- Test registry
                package Test_Registry is

                   type Test_Registry is
                      (Value_11 => 11,

                       -- description = Second value (same name as first!)
                       Value_12 => 12,

                       -- description = Third value (hex)
                       -- description = Third value (duplicate)
                       -- alternative_name = Hex
                       -- alternative_name = Duplicate
                       Test_Registry_B => 16#B#)
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

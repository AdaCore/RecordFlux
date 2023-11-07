from __future__ import annotations

import re
import string
import textwrap
from abc import ABC, abstractmethod
from collections.abc import Sequence
from datetime import datetime
from pathlib import Path
from typing import Optional, Union
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, ParseError

import rflx.specification.const
from rflx.common import file_name
from rflx.error import Location, RecordFluxError, Subsystem, fail, warn

NAMESPACE = {"iana": "http://www.iana.org/assignments"}
RESERVED_WORDS = "|".join(rflx.specification.const.RESERVED_WORDS)
OUTPUT_INDENT_CHAR = " "
OUTPUT_INDENT = OUTPUT_INDENT_CHAR * 3
OUTPUT_WIDTH = 80


def convert(
    data: str,
    source: Path,
    always_valid: bool,
    output_dir: Path,
    reproducible: bool = False,
) -> None:
    try:
        # TODO(eng/recordflux/RecordFlux#1375): Prevent XML attacks by replacing xml by defusedxml
        root = ElementTree.fromstring(data)  # noqa: S314
    except ParseError as e:
        fail(
            f"invalid XML document: {e}",
            subsystem=Subsystem.CONVERTER,
            location=Location(start=e.position, source=source),
        )
    registry_id = root.get("id")
    if registry_id is None:
        fail(
            "no registry ID found",
            subsystem=Subsystem.CONVERTER,
            location=Location(start=(0, 0), source=source),
        )
    package_name = _normalize_name(registry_id)
    registry_last_updated = root.find("iana:updated", NAMESPACE)
    registry_title = root.find("iana:title", NAMESPACE)
    package_entries: list[SpecificationElement] = []
    for sub_registry in root.findall(root.tag):
        try:
            enum_type = _convert_registry_to_enum_type(sub_registry, always_valid)
            if enum_type is not None:
                package_entries.append(enum_type)
        except RecordFluxError as e:  # noqa: PERF203
            sub_registry_id = sub_registry.get("id")
            sub_registry_title = sub_registry.find("iana:title", NAMESPACE)
            title_str = f'"{sub_registry_title.text}"' if sub_registry_title is not None else ""
            id_str = f' (id={sub_registry_id if sub_registry_id is not None else "<Unknown>"})'
            err_msg = f"{e}"[len("converter: error: ") :]
            warn(
                f"registry {title_str}{id_str} skipped due to conversion error: {err_msg}",
                subsystem=Subsystem.CONVERTER,
            )
            package_entries.append(
                CommentBlock(
                    f"type {_normalize_name(title_str)}\n"
                    "... Skipped due to unsupported content in the registry ...",
                    trailing_lines=1,
                ),
            )
    enum_types = [x for x in package_entries if isinstance(x, EnumType)]
    resolve_duplicate_literals(enum_types)
    file = output_dir / Path(file_name(package_name) + ".rflx")
    write_rflx_specification(
        file,
        package_entries,
        package_name,
        registry_title,
        registry_last_updated,
        reproducible,
    )


def write_rflx_specification(
    file: Path,
    package_entries: list[SpecificationElement],
    package_name: str,
    registry_title: Optional[Element],
    registry_last_updated: Optional[Element],
    reproducible: bool,
) -> None:
    package_header: list[CommentBlock] = []

    package_header.append(
        CommentBlock(
            "AUTOMATICALLY GENERATED. DO NOT EDIT."
            + (
                ""
                if reproducible
                else f"\nGeneration date: {datetime.now().strftime('%Y-%m-%d')}"  # noqa: DTZ005
            ),
        ),
    )
    if registry_title is not None:
        package_header.append(CommentBlock(f"{registry_title.text}"))
    if registry_last_updated is not None:
        package_header.append(
            CommentBlock(
                f"Registry last updated on {registry_last_updated.text}",
                trailing_lines=1,
            ),
        )

    package = Package(package_name, package_header, package_entries)
    file.write_text(package.rflx_str(), encoding="utf-8")


def resolve_duplicate_literals(enum_types: Sequence[EnumType]) -> None:
    literal_names = [
        enum_literal.name for enum_type in enum_types for enum_literal in enum_type.enum_literals
    ]
    for enum_type in enum_types:
        for enum_literal in enum_type.enum_literals:
            if literal_names.count(enum_literal.name) > 1:
                enum_literal.name += f"_{enum_literal.value.replace('16#', '').replace('#', '')}"


def _convert_registry_to_enum_type(
    registry: Element,
    always_valid: bool,
) -> Optional[EnumType]:
    records = registry.findall("iana:record", NAMESPACE)
    title = registry.find("iana:title", NAMESPACE)
    if len(records) == 0 or title is None or title.text is None:
        return None

    registry_title = _normalize_name(title.text)
    enum_literals: dict[str, EnumLiteral] = {}
    enum_literal_highest_bit_len = 0
    for record in records:
        name_tag = _get_name_tag(record)
        value_tag = "value"

        if name_tag is None:
            continue

        name_element = record.find(f"iana:{name_tag}", NAMESPACE)
        value_element = record.find(f"iana:{value_tag}", NAMESPACE)

        if value_element is None or name_element is None:
            # https://github.com/nedbat/coveragepy/issues/772
            # A dummy statement is needed to disable the peephole optimizer, so that the continue
            # statement is detected during coverage analysis.
            # CPython 3.8 and 3.9 are affected. The issue is fixed in CPython 3.10.
            dummy = 0  # noqa: F841
            continue

        assert isinstance(value_element.text, str)
        assert isinstance(name_element.text, str)

        rflx_value, bit_length = _normalize_value(value_element.text)

        rflx_name = _normalize_literal_name(name_element.text)

        if bit_length > enum_literal_highest_bit_len:
            enum_literal_highest_bit_len = bit_length

        if re.search(r"RESERVED|UNASSIGNED", rflx_name, flags=re.I):
            continue

        if re.fullmatch(RESERVED_WORDS, rflx_name, re.I | re.X) is not None:
            rflx_name += f"_{rflx_value.replace('16#', '').replace('#', '')}"
        if rflx_name.startswith(tuple(d for d in string.digits)):
            rflx_name = f"{registry_title}_{rflx_name}"

        comment = [
            element
            for element in record.iterfind("*", NAMESPACE)
            if element.tag
            not in [f"{{{NAMESPACE['iana']}}}{name_tag}", f"{{{NAMESPACE['iana']}}}{value_tag}"]
        ]

        enum_literal = EnumLiteral(rflx_name, rflx_value, bit_length, comment)
        if rflx_value in enum_literals:
            enum_literals[rflx_value].join(enum_literal, registry_title)
        else:
            enum_literals[rflx_value] = enum_literal

    if len(enum_literals.values()) > 0:
        return EnumType(
            registry_title,
            list(enum_literals.values()),
            enum_literal_highest_bit_len,
            always_valid,
        )
    return None


def _get_name_tag(record: Element) -> Optional[str]:
    sub_elements = record.findall("*", NAMESPACE)
    child_names = {c.tag[c.tag.index("}") + 1 :] for c in sub_elements}
    possible_name_tags = ["name", "code", "type", "description"]
    for tag in possible_name_tags:
        if tag in child_names:
            return tag
    return None


class SpecificationElement(ABC):
    """Base class for RecordFlux specification elements."""

    def __str__(self) -> str:
        """
        Render the current element in RecordFlux concrete syntax.

        Note: If relative indentation is needed, then the method 'rflx_str' should be used instead.
        """
        return self.rflx_str()

    @abstractmethod
    def rflx_str(self, indent_level: int = 0) -> str:
        """Render the current element in RecordFlux concrete syntax with given base indentation."""


class Package(SpecificationElement):
    def __init__(
        self,
        name: str,
        header: Sequence[CommentBlock],
        entries: Sequence[SpecificationElement],
    ):
        self.name = name
        self.header = header
        self.entries = entries

    def rflx_str(self, indent_level: int = 0) -> str:
        base_indent = OUTPUT_INDENT * indent_level
        str_repr = ""
        str_repr += "".join(r.rflx_str(indent_level) for r in self.header)
        str_repr += f"{base_indent}package {self.name} is\n\n"
        str_repr += "".join(r.rflx_str(indent_level + 1) for r in self.entries)
        str_repr += f"{base_indent}end {self.name};\n"
        return str_repr


class EnumType(SpecificationElement):
    def __init__(
        self,
        type_name: str,
        enum_literals: Sequence[EnumLiteral],
        type_size: int,
        always_valid: bool,
    ) -> None:
        self.type_name = type_name
        self.enum_literals = enum_literals
        self.type_size = type_size
        self.always_valid = always_valid and len(self.enum_literals) < 2**self.type_size

    def rflx_str(self, indent_level: int = 0) -> str:
        base_indent = OUTPUT_INDENT * indent_level
        formatted_enum_literals = ",\n".join(
            r.rflx_str(indent_level + 1) for r in self.enum_literals
        )
        return (
            f"{base_indent}type {self.type_name} is\n"
            f"{base_indent + OUTPUT_INDENT}({formatted_enum_literals.lstrip()})\n"
            f"{base_indent}with Size => {self.type_size}"
            f"{', Always_Valid;' if self.always_valid else ';'}"
            f"\n\n"
        )


class EnumLiteral(SpecificationElement):
    def __init__(
        self,
        rflx_name: str,
        rflx_value: str,
        bit_length: int,
        comments: Optional[list[Element]] = None,
    ):
        self.name = rflx_name
        self.value = rflx_value
        self.bit_length = bit_length
        self.comment_list = comments or []
        self.alternative_names: list[str] = [f"alternative_name = {self.name}"]

    def join(self, duplicate: EnumLiteral, registry_name: str) -> None:
        self.name = f"{registry_name}_{self.value.replace('16#', '').replace('#', '')}"
        self.comment_list.extend(duplicate.comment_list)
        self.alternative_names.append(f"alternative_name = {duplicate.name}")

    def comment(self, indent_level: int) -> list[str]:
        base_indent = OUTPUT_INDENT * indent_level
        comments = [
            f"{c.tag[c.tag.index('}') + 1:]} = {c.text}"
            if c.tag is not None and c.text is not None
            else f"Ref: {c.attrib['data']}"
            for c in self.comment_list
        ]
        if len(self.alternative_names) > 1:
            comments.extend(self.alternative_names)

        formatted_comments = [
            textwrap.wrap(comment, width=OUTPUT_WIDTH, subsequent_indent=f"{base_indent} --  ")
            for comment in comments
        ]
        if len(formatted_comments) > 0:
            return ["\n".join(formatted_comment) for formatted_comment in formatted_comments]
        return []

    def rflx_str(self, indent_level: int = 0) -> str:
        base_indent = OUTPUT_INDENT * indent_level
        str_repr = ""
        comment = self.comment(indent_level)
        if comment:
            str_repr += "\n"
            for comment_line in comment:
                str_repr += f"{base_indent} -- {comment_line}\n"
        name = (
            self.name
            if len(self.name) <= 100
            else self.name[:100]
            + ("_" if self.name[99] != "_" else "")
            + self.value.replace("16#", "").replace("#", "")
        )
        str_repr += f"{base_indent} {f'{name} => {self.value}'}"
        return str_repr


class CommentBlock(SpecificationElement):
    """
    A textual comment block in the generated source code.

    An instance of this class holds one text string that can contain multiple lines. Comment
    characters (i.e., '--' for RecordFlux) and leading whitespace should *not* be included. When the
    object is converted to string leading whitespace and comment characters are added automatically.
    Lines that would be longer than 80 characters are wrapped. However, existing line-endings are
    preserved.
    """

    def __init__(
        self,
        text: str,
        trailing_lines: int = 0,
    ) -> None:
        self.text = text
        self.trailing_lines = trailing_lines

    def rflx_str(self, indent_level: int = 0) -> str:
        def wrap_line(line: str) -> str:
            commented_section = textwrap.wrap(
                f"{OUTPUT_INDENT*indent_level}-- {line}",
                width=OUTPUT_WIDTH,
                subsequent_indent=f"{OUTPUT_INDENT*indent_level}-- ",
            )
            return "\n".join(commented_section)

        lines = self.text.split("\n")
        wrapped_lines = map(wrap_line, lines)
        return "\n".join(wrapped_lines) + "\n" + "\n" * self.trailing_lines


def _normalize_name(description_text: str) -> str:
    t: dict[str, Union[int, str, None]] = {c: " " for c in string.punctuation + "\n"}
    name = description_text.translate(str.maketrans(t))
    return "_".join([s[0].upper() + s[1:] for s in name.split()])


def _normalize_literal_name(description_text: str) -> str:
    """
    Normalize the given text to be used as the name of an enum literal.

    The input text is expected to be a potentially multiline string that
    sometimes contains additional comments in parentheses. Only the part up to
    the first parenthesis is kept. The rest is normalized in the same way
    as in _normalize_name. In addition, if the original string contains either
    "(deprecated)" or "(obsolete)", then those words are prefixed to the
    normalized name.
    """
    description_prefix = re.sub(r"\(.*", "", description_text, flags=re.S)
    rflx_name = _normalize_name(description_prefix)
    if re.search(r"\(obsolete\)", description_text, re.IGNORECASE | re.S):
        rflx_name = "Obsolete_" + rflx_name
    if re.search(r"\(deprecated\)", description_text, re.IGNORECASE | re.S):
        rflx_name = "Deprecated_" + rflx_name
    return rflx_name


def _normalize_value(value: str) -> tuple[str, int]:
    """
    Return hex string and required bit length.

    In case of a range, just return the upper bound of that range. Ranges can only occur for
    UNASSIGNED or RESERVED values, which are filtered out.
    """
    if value.find("0x") != -1:
        rflx_hex = _normalize_hex_value(value)
        return rflx_hex, len(rflx_hex[3 : len(rflx_hex) - 1].lstrip("0")) * 4
    if value.find("-") != -1:
        range_upper = value[value.index("-") + 1 :]
        return range_upper, _convert_int_value(range_upper).bit_length()
    return str(value), _convert_int_value(value).bit_length()


def _normalize_hex_value(hex_value: str) -> str:
    if hex_value.find("-") != -1 or re.match(r"^0x[0-9A-Fa-f]{2},\*", hex_value) is not None:
        if re.match(r"^0x[0-9A-Fa-f]{4}-0x[0-9A-Fa-f]{4}$", hex_value) is not None:  # 0x081D-0x08FF
            return f"16#{hex_value[9:]}#".upper()
        if re.match(r"^0x[0-9A-Fa-f]{2}-[0-9A-Fa-f]{2},\*$", hex_value) is not None:  # 0xCD-CF,*
            return f"16#{hex_value[5:7]}FF#".upper()
        if re.match(r"^0x[0-9A-F]{2},\*", hex_value) is not None:  # 0xCB,*
            return f"16#{hex_value[2:4]}FF#".upper()
        if (
            re.match(r"^0x[0-9A-Fa-f]{2},0x[0-9A-Fa-f]{2}-[0-9A-Fa-f]{2}$", hex_value) is not None
        ):  # 0xCC,0xAF-FF
            return f"16#{hex_value[2:4]}{hex_value[10:12]}#".upper()
        fail(f'cannot normalize hex value range "{hex_value}"', subsystem=Subsystem.CONVERTER)
    if re.match(r"^0x[0-9A-Fa-f]{2},0x[0-9A-Fa-f]{2}$", hex_value) is not None:  # 0x0A,0xFF
        return f"16#{hex_value.replace('0x', '').replace(',', '')}#".upper()
    if re.match(r"^0x[0-9A-Fa-f]+$", hex_value) is not None:  # 0xA1A1
        return f"16#{hex_value[2:]}#".upper()
    fail(f'cannot normalize hex value "{hex_value}"', subsystem=Subsystem.CONVERTER)


def _convert_int_value(value: str) -> int:
    try:
        return int(value)
    except ValueError as e:
        fail(f"{e}", subsystem=Subsystem.CONVERTER)

import re
import string
from collections import defaultdict
from datetime import datetime
from typing import Iterator, TextIO, List
from urllib.error import HTTPError
from urllib.request import urlopen

from defusedxml import ElementTree

NAMESPACE = {"iana": "http://www.iana.org/assignments"}

DUPLICATE_CACHE = []


def iana_to_rflx(url: str):
    if not re.match(r"^https://www\.iana\.org/assignments/.*\.xml$", url):
        raise IANAError(f"{url} not a valid IANA url")

    try:
        with urlopen(url) as xml_response:
            xml_document: str = xml_response.read().decode("utf-8")
    except HTTPError as e:
        raise IANAError(f"cannot fetch url {e}") from e

    with open("test.rflx", "w+") as file:
        root = ElementTree.fromstring(xml_document)
        file.write(f"-- AUTOMATICALLY GENERATED BY {__file__}. DO NOT EDIT.\n")
        file.write(f"-- SOURCE: {url}")
        file.write(f"-- Generation date: {datetime.now().strftime('%Y-%m-%d')}\n")
        file.write(f"-- {root.find('iana:title', NAMESPACE).text}\n")
        file.write(f"-- Registry last updated on {root.find('iana:updated', NAMESPACE).text}\n\n")
        file.write(f"package {root.get('id').upper().replace('-', '_')} is\n\n")
        for registry in root.findall(root.tag):
            write_registry(registry, 8, True, file)  # how to get size of field? always_valid?
        file.write(f"end {root.get('id').upper().replace('-', '_')}")


def write_registry(
    registry: ElementTree,
    type_size: int,
    always_valid: bool,
    file: TextIO,
) -> None:
    file.write(f"{'':<3}type {registry.find('iana:title', NAMESPACE).text.replace(' ', '_')} is\n")
    file.write(f"{'':<6}(\n")
    for record in registry.iterfind("iana:record", NAMESPACE):
        write_record(record, file)
    file.write(f"{'':<6})\n")
    file.write(f"{'':<3}with Size => {type_size}; ")
    if always_valid:
        file.write("Always_Valid;")
    file.write("\n\n")


def write_record(record: ElementTree, file: TextIO) -> None:
    references = []
    name = ""
    value = ""
    sub_elements = record.findall("*", NAMESPACE)

    child_names = set(c.tag[c.tag.index("}") + 1 :] for c in sub_elements)
    possible_literal_elements = ["name", "description"]
    if all((p in child_names for p in possible_literal_elements)):
        literal_name = r"name"
    else:
        literal_name = child_names.intersection(possible_literal_elements).pop()

    for element in sub_elements:
        if re.search(literal_name, element.tag, flags=re.IGNORECASE):
            name = element.text
        elif re.search(r"value", element.tag, flags=re.IGNORECASE):
            value = element.text
        else:
            references.append(element)

    if name == "" or value == "":
        print(f"DEBUG: {name} => {value}")
        return

    if re.search(r"RESERVED|UNASSIGNED", name, flags=re.IGNORECASE):
        return

    comment = [
        f"{r.tag[r.tag.index('}')+1:]} = {r.text}"
        if r.tag is not None and r.text is not None
        else f"Ref: {r.attrib['data']}"
        for r in references
    ]

    literal = _normalize_description(name)
    for rflx_value in _normalize_value(value):
        if literal in DUPLICATE_CACHE:
            literal = f"{literal}{rflx_value}"
        else:
            DUPLICATE_CACHE.append(literal)
        if (c := ", ".join(comment)) != "":
            file.write("\n")
            for comment_line in _normalize_comment(c):
                file.write(f"{'':<9}-- {comment_line}\n")
            file.write(f"{'':<9}--\n")
        file.write(f"{'':<9}{f'{literal} => {rflx_value},'}\n")


def _normalize_comment(comment: str) -> List[str]:
    c = comment.replace("\n", " ")
    c = " ".join(c.split())
    if len(c) > 80:
        lines = len(c) // 80 +1
        return [c[i*80:i*80+80] for i in range(lines)]
    return [c]


def _normalize_description(description_text: str) -> str:

    t = {c: "" for c in string.punctuation + "\n" if c != "_"}
    t["-"] = "_"
    desc = description_text.translate(str.maketrans(t))
    desc = "_".join(desc.split())
    return desc.upper()


def _normalize_value(value: str) -> Iterator[str]:
    if value.find("0x") != -1:
        yield from _normalize_hex_value(value)
    else:
        yield from _normalize_dec_value(value)


def _normalize_dec_value(dec_value: str) -> Iterator[str]:
    if dec_value.find("-") != -1:
        range_lower = int(dec_value[: dec_value.index("-")])
        range_upper = int(dec_value[dec_value.index("-") :])
        for i in range(range_lower, range_upper + 1):
            yield str(i)
    else:
        yield dec_value


def _normalize_hex_value(hex_value: str) -> Iterator[str]:
    if hex_value.find("-") != -1:
        for k in _expand_hex_range(hex_value):
            yield k
    else:
        if re.match(r"^0x[0-9A-Fa-f]{2},0x[0-9A-Fa-f]{2}$", hex_value) is not None:  # 0x0A,0xFF
            yield f"16#{hex_value.replace('0x', '').replace(',', '')}#"
        elif re.match(r"^0x[0-9A-Fa-f]+$", hex_value) is not None:  # 0xA1A1
            yield f"16#{hex_value[2:]}#"


def _expand_hex_range(hex_value_range: str) -> Iterator[str]:
    # never used atm
    if (
        re.match(r"^0x[0-9A-Fa-f]{4}-0x[0-9A-Fa-f]{4}$", hex_value_range) is not None
    ):  # 0x081D-0x08FF
        range_lower = int(hex_value_range[:6], 0)
        range_upper = int(hex_value_range[7:], 0)
    elif (
        re.match(r"^0x[0-9A-Fa-f]{2}-[0-9A-Fa-f]{2},\*$", hex_value_range) is not None
    ):  # 0xCD-CF,*
        range_lower = int(f"{hex_value_range[:4]}00", 0)
        range_upper = int(f"{hex_value_range[5:7]}FF", 16)
    elif (
        re.match(r"^0x[0-9A-Fa-f]{2},0x[0-9A-Fa-f]{2}-[0-9A-Fa-f]{2}$", hex_value_range) is not None
    ):  # 0xCC,0xAF-FF
        range_lower = int(f"{hex_value_range[:4]}{hex_value_range[7:9]}", 0)
        range_upper = int(f"{hex_value_range[:4]}{hex_value_range[10:12]}", 0)
    else:
        raise IANAError(hex_value_range)

    for i in range(range_lower, range_upper + 1):
        yield f"16#{hex(i)}#"


class IANAError(Exception):
    pass


if __name__ == "__main__":
    iana_to_rflx("https://www.iana.org/assignments/bootp-dhcp-parameters/bootp-dhcp-parameters.xml")

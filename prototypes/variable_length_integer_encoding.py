# pylint: disable = unnecessary-lambda-assignment

from collections.abc import Callable

import pytest


def sequence_value(
    sequence: list[int],
    initial_value: int,
    func_next_value: Callable[[int, int], int],
    func_until: Callable[[int], int],
) -> int:
    if not sequence:
        return initial_value

    value = func_next_value(initial_value, sequence[0])

    if func_until(sequence[0]):
        return value

    return sequence_value(
        sequence[1:],
        value,
        func_next_value,
        func_until,
    )


# VLQ (MIDI, SNMP, X.509)

VLQ_INITIAL = 0
VLQ_NEXT = lambda current, element: current * 128 + (element & 127)
VLQ_UNTIL = lambda element: (element & 128) == 0


@pytest.mark.parametrize(
    ["input_sequence", "expected"],
    [
        ([0x00], 0),
        ([0x7F], 127),
        ([0x81, 0x00], 128),
        ([0xC0, 0x00], 8192),
        ([0xFF, 0x7F], 16383),
        ([0x81, 0x80, 0x00], 16384),
        ([0xFF, 0xFF, 0x7F], 2097151),
        ([0x81, 0x80, 0x80, 0x00], 2097152),
        ([0xC0, 0x80, 0x80, 0x00], 134217728),
        ([0xFF, 0xFF, 0xFF, 0x7F], 268435455),
    ],
)
def test_vlq(input_sequence: list[int], expected: int) -> None:
    assert sequence_value(input_sequence, VLQ_INITIAL, VLQ_NEXT, VLQ_UNTIL) == expected
    assert sequence_value([*input_sequence, 0, 0], VLQ_INITIAL, VLQ_NEXT, VLQ_UNTIL) == expected
    assert sequence_value([*input_sequence, 255, 255], VLQ_INITIAL, VLQ_NEXT, VLQ_UNTIL) == expected


def sequence_value_variant(
    sequence: list[int],
    initial_value: int,
    func_next_value: Callable[[int, int, int], int],
    func_until: Callable[[int], int],
    i: int = 1,
) -> int:
    if not sequence:
        return initial_value

    value = func_next_value(initial_value, sequence[0], i)

    if func_until(sequence[0]):
        return value

    return sequence_value_variant(
        sequence[1:],
        value,
        func_next_value,
        func_until,
        i + 1,
    )


# ULEB128 (MQTT, WebAssembly, DWARF)

LEB128_INITIAL = 0
LEB128_NEXT = lambda current, element, position: current + (element & 127) * 128 ** (position - 1)
LEB128_UNTIL = lambda element: (element & 128) == 0


@pytest.mark.parametrize(
    ["input_sequence", "expected"],
    [
        ([0x00], 0),
        ([0x7F], 127),
        ([0x80, 0x01], 128),
        ([0xFF, 0x7F], 16383),
        ([0x80, 0x80, 0x01], 16384),
        ([0xE5, 0x8E, 0x26], 624485),
        ([0x80, 0x80, 0x80, 0x01], 2097152),
        ([0xFF, 0xFF, 0xFF, 0x7F], 268435455),
    ],
)
def test_leb128(input_sequence: list[int], expected: int) -> None:
    assert (
        sequence_value_variant(input_sequence, LEB128_INITIAL, LEB128_NEXT, LEB128_UNTIL)
        == expected
    )
    assert (
        sequence_value_variant([*input_sequence, 0, 0], LEB128_INITIAL, LEB128_NEXT, LEB128_UNTIL)
        == expected
    )
    assert (
        sequence_value_variant(
            [*input_sequence, 255, 255], LEB128_INITIAL, LEB128_NEXT, LEB128_UNTIL
        )
        == expected
    )


# BT L2CAP PSM

# Protocol/Service Multiplexer - PSM (2 octets (minimum))
# The PSM field is at least two octets in length. All PSM values shall have the
# least significant bit of the most significant octet equal to 0 and the least
# significant bit of all other octets equal to 1.
#
# Note: This means that all PSMs are odd numbers and that the end of a PSM
# can be easily found by searching for the first even octet.
#
# PSM values are separated into two ranges. Valid values in the first range are
# assigned by the Bluetooth SIG and indicate protocols. The second range of
# values are dynamically allocated and used in conjunction with the Service
# Discovery protocol (SDP). The dynamically assigned values may be used to
# support multiple implementations of a particular protocol.
#
# 0x0001-0x0EFF Fixed, SIG assigned
# >0x1000 Dynamic
#
# Since PSMs are odd and the least significant bit of the most significant byte is zero,
# the following ranges do not contain valid PSMs: 0x0100-0x01FF, 0x0300-0x03FF,
# 0x0500-0x05FF, 0x0700-0x07FF, 0x0900-0x09FF, 0x0B00-0x0BFF, 0x0D00-
# 0x0DFF. All even values are also not valid as PSMs.

PSM_INITIAL = 0
PSM_NEXT = lambda current, element, position: current + element * 256 ** (position - 1)
PSM_UNTIL = lambda element: (element & 1) == 0


@pytest.mark.parametrize(
    ["input_sequence", "expected"],
    [
        ([0x01, 0x02], 0x0201),
        ([0xFF, 0x0E], 0x0EFF),
    ],
)
def test_psm(input_sequence: list[int], expected: int) -> None:
    assert sequence_value_variant(input_sequence, PSM_INITIAL, PSM_NEXT, PSM_UNTIL) == expected
    assert (
        sequence_value_variant([*input_sequence, 0, 0], PSM_INITIAL, PSM_NEXT, PSM_UNTIL)
        == expected
    )
    assert (
        sequence_value_variant([*input_sequence, 255, 255], PSM_INITIAL, PSM_NEXT, PSM_UNTIL)
        == expected
    )

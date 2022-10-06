def icmp_checksum(message: bytes, **kwargs: object) -> int:
    first_arg = kwargs.get("Tag'First .. Checksum'First - 1")
    assert isinstance(first_arg, tuple)
    tag_first, checksum_first_minus_one = first_arg
    assert tag_first == 0 and checksum_first_minus_one == 15
    second_arg = kwargs.get("Checksum'Last + 1 .. Message'Last")
    assert isinstance(second_arg, tuple)
    checksum_last_plus_one, data_last = second_arg
    assert checksum_last_plus_one == 32 and data_last == 511
    checksum_size = kwargs.get("Checksum'Size")
    assert isinstance(checksum_size, int)
    assert checksum_size == 16

    checksum_bytes = message[tag_first : (checksum_first_minus_one + 1) // 8]
    checksum_bytes += b"\x00" * (checksum_size // 8)
    checksum_bytes += message[(checksum_last_plus_one // 8) : (data_last + 1) // 8]
    return internet_checksum(checksum_bytes)


def internet_checksum(checksum_bytes: bytes) -> int:
    def add_ones_complement(num1: int, num2: int) -> int:
        mod = 1 << 16
        result = num1 + num2
        return result if result < mod else (result + 1) % mod

    message_in_sixteen_bit_chunks = [
        int.from_bytes(checksum_bytes[i : i + 2], "big") for i in range(0, len(checksum_bytes), 2)
    ]
    intermediary_result = message_in_sixteen_bit_chunks[0]
    for i in range(1, len(message_in_sixteen_bit_chunks)):
        intermediary_result = add_ones_complement(
            intermediary_result, message_in_sixteen_bit_chunks[i]
        )

    return intermediary_result ^ 0xFFFF


checksum_functions = {"ICMP::Message": {"Checksum": icmp_checksum}}

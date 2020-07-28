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

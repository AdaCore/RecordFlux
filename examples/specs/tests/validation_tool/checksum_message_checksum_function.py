def checksum_function(message: bytes, **kwargs: object) -> int:
    field_values = kwargs.get("Field_A'First .. Field_A'Last")
    assert isinstance(field_values, tuple)
    field_a_first = field_values[0]
    field_a_last = field_values[1]
    assert field_a_first == 0 and field_a_last == 7

    field_b_values = kwargs.get("Field_B'First .. Field_B'Last")
    assert isinstance(field_b_values, tuple)
    field_b_first = field_b_values[0]
    field_b_last = field_b_values[1]
    assert field_b_first == 8 and field_b_last == 15

    field_a = message[(field_a_first + 1) // 8 : (field_a_last + 1) // 8]
    field_b = message[(field_b_first + 1) // 8 : (field_b_last + 1) // 8]

    return (int.from_bytes(field_a, "big") + int.from_bytes(field_b, "big")) % 0xFF


checksum_functions = {"Checksum_Message::Message": {"Checksum": checksum_function}}

def checksum_function(message: bytes, **kwargs: object) -> int:
    field_a = kwargs.get("Field_A")
    field_b = kwargs.get("Field_B")
    assert isinstance(field_a, int)
    assert isinstance(field_b, int)
    return field_a + field_b % 0xFF


checksum_functions = {"Checksum_Message::Message": {"Checksum": checksum_function}}

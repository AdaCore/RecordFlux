def checksum_function(message: bytes, **kwargs: object) -> int:
    return 171  # 0xAB


checksum_functions = {"Checksum_Message::Message": {"Checksum": checksum_function}}

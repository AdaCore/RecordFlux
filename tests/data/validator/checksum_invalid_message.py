def checksum_test() -> int:
    return 0


checksum_functions = {
    "Checksum_Message::Message": {"C": checksum_test},
    "Checksum_Message::Invalid_Message": {"C": checksum_test},
}

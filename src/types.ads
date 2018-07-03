package Types is

    type Byte is mod 2**8;
    type Bytes is array (Positive range <>) of Byte;
    type Payload_Type is new Bytes;

    type U16 is mod 2**16;
    type U32 is mod 2**32;

    procedure Bytes_Put (Buffer : Bytes);

    generic
        type UXX is mod <>;
    function Convert_To (Buffer : Bytes) return UXX with
        Depends => (Convert_To'Result => Buffer),
        Pre => UXX'Size rem 8 = 0 and then Buffer'Length = UXX'Size / 8;

end Types;

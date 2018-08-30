package Types is

   type Byte is mod 2**8;
   type Bytes is array (Positive range <>) of Byte;
   type Payload_Type is new Bytes;

   procedure Bytes_Put (Buffer : Bytes);

   generic
      type UXX is mod <>;
   function Convert_To_Mod (Buffer : Bytes) return UXX
     with
        Pre => UXX'Size rem 8 = 0 and then Buffer'Length = UXX'Size / 8;

   generic
      type Int is range <>;
   function Convert_To_Int (Buffer : Bytes) return Int
     with
       Pre => Int'Size rem 8 = 0 and then Buffer'Length = Int'Size / 8;

end Types;

package Types
  with SPARK_Mode
is

   type Byte is mod 2**8;

   type Length_Type is new Natural;
   subtype Index_Type is Length_Type range 1 .. Length_Type'Last;

   type Bytes is array (Index_Type range <>) of Byte
      with Predicate => Bytes'Length > 0;

   type Payload_Type is new Bytes;

   procedure Bytes_Put (Buffer : Bytes);

   generic
      type Int is mod <>;
   function Convert_To_Mod (Buffer : Bytes; Offset : Natural := 0) return Int
     with
       Pre => Offset < 8 and then Buffer'Length = ((Int'Size + Offset - 1) / 8) + 1;

   generic
      type Int is range <>;
   function Convert_To_Int (Buffer : Bytes; Offset : Natural := 0) return Int
     with
       Pre => Offset < 8 and then Buffer'Length = ((Int'Size + Offset - 1) / 8) + 1;

end Types;

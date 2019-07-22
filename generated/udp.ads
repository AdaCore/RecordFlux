with Types;
use type Types.Bytes, Types.Index_Type, Types.Length_Type, Types.Bit_Index_Type, Types.Bit_Length_Type;

package UDP with
  SPARK_Mode
is

   type Port_Type is mod (2**16);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Port_Type return Port_Type is
     (Port_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Port_Type is new Types.Convert_To_Mod (Port_Type);

   function Valid_Port_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (True)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Port_Type'Size + Types.Bit_Length_Type (Offset)));

   type Length_Type_Base is range 0 .. ((2**16) - 1) with Size => 16;

   subtype Length_Type is Length_Type_Base range 8 .. ((2**16) - 1);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Length_Type return Length_Type is
     (Length_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Length_Type_Base is new Types.Convert_To_Int (Length_Type_Base);

   function Valid_Length_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (Convert_To_Length_Type_Base (Buffer, Offset) >= 8)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Length_Type_Base'Size + Types.Bit_Length_Type (Offset)));

   type Checksum_Type is mod (2**16);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Checksum_Type return Checksum_Type is
     (Checksum_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Checksum_Type is new Types.Convert_To_Mod (Checksum_Type);

   function Valid_Checksum_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Boolean is
     (True)
    with
     Pre => Buffer'Length = Types.Byte_Index ((Checksum_Type'Size + Types.Bit_Length_Type (Offset)));

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Types_Index_Type return Types.Index_Type is
     (Types.Index_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Types_Length_Type return Types.Length_Type is
     (Types.Length_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

end UDP;

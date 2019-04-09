with Types;
use type Types.Bytes, Types.Index_Type, Types.Length_Type;

package UDP
  with SPARK_Mode
is

   type Port_Type is mod (2**16);
   function Convert_To_Port_Type is new Types.Convert_To_Mod (Port_Type);

   type Length_Type_Base is range 0 .. ((2**16) - 1) with Size => 16;
   function Convert_To_Length_Type_Base is new Types.Convert_To_Int (Length_Type_Base);

   subtype Length_Type is Length_Type_Base range 8 .. ((2**16) - 1);

   type Checksum_Type is mod (2**16);
   function Convert_To_Checksum_Type is new Types.Convert_To_Mod (Checksum_Type);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Port_Type return Port_Type is
      (Port_Type'First)
     with
       Pre => False;

   function Unreachable_Length_Type return Length_Type is
      (Length_Type'First)
     with
       Pre => False;

   function Unreachable_Checksum_Type return Checksum_Type is
      (Checksum_Type'First)
     with
       Pre => False;

   function Unreachable_Types_Index_Type return Types.Index_Type is
      (Types.Index_Type'First)
     with
       Pre => False;

   function Unreachable_Types_Length_Type return Types.Length_Type is
      (Types.Length_Type'First)
     with
       Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Valid_Port_Type (Buffer : Types.Bytes; Offset : Natural) return Boolean is
      (True)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((Port_Type'Size + Offset + (-1)) / 8) + 1));

   function Valid_Length_Type (Buffer : Types.Bytes; Offset : Natural) return Boolean is
      (Convert_To_Length_Type_Base (Buffer, Offset) >= 8)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((Length_Type_Base'Size + Offset + (-1)) / 8) + 1));

   function Valid_Checksum_Type (Buffer : Types.Bytes; Offset : Natural) return Boolean is
      (True)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((Checksum_Type'Size + Offset + (-1)) / 8) + 1));

end UDP;

with Types;
use type Types.Bytes, Types.Index_Type, Types.Length_Type;

package Ethernet
  with SPARK_Mode
is

   type UINT48 is mod (2**48);

   type UINT16 is range 0 .. ((2**16) - 1) with Size => 16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_UINT48 return UINT48 is
      (UINT48'First)
     with
       Pre => False;

   function Unreachable_UINT16 return UINT16 is
      (UINT16'First)
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

   function Convert_To_UINT48 is new Types.Convert_To_Mod (UINT48);

   function Valid_UINT48 (Buffer : Types.Bytes; Offset : Natural) return Boolean is
      (True)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((UINT48'Size + Offset + (-1)) / 8) + 1));

   function Convert_To_UINT16 is new Types.Convert_To_Int (UINT16);

   function Valid_UINT16 (Buffer : Types.Bytes; Offset : Natural) return Boolean is
      (True)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((UINT16'Size + Offset + (-1)) / 8) + 1));

end Ethernet;

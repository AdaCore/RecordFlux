with Types;

package Ethernet
  with SPARK_Mode
is

   type UINT48 is mod (2**48);
   function Convert_To_UINT48 is new Types.Convert_To_Mod (UINT48);

   type UINT16 is range 0 .. ((2**16) - 1) with Size => 16;
   function Convert_To_UINT16 is new Types.Convert_To_Int (UINT16);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_UINT48 return UINT48 is
      (UINT48'First)
     with
       Pre => False;

   function Unreachable_UINT16 return UINT16 is
      (UINT16'First)
     with
       Pre => False;

   function Unreachable_Natural return Natural is
      (Natural'First)
     with
       Pre => False;

   pragma Warnings (On, "precondition is statically false");

end Ethernet;

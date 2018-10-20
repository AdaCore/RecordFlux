with Types; use Types;

package Ethernet
  with SPARK_Mode
is

   type UINT48 is mod (2**48);
   function Convert_To_UINT48 is new Convert_To_Mod (UINT48);

   type UINT16 is range 0 .. ((2**16) - 1) with Size => 16;
   function Convert_To_UINT16 is new Convert_To_Int (UINT16);

end Ethernet;

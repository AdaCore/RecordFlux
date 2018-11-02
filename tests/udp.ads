with Types; use Types;

package UDP
  with SPARK_Mode
is

   type Port_Type is mod (2**16);
   function Convert_To_Port_Type is new Convert_To_Mod (Port_Type);

   type Length_Type_Base is range 0 .. ((2**16) - 1) with Size => 16;
   function Convert_To_Length_Type_Base is new Convert_To_Int (Length_Type_Base);

   subtype Length_Type is Length_Type_Base range 8 .. ((2**16) - 1);

   type Checksum_Type is mod (2**16);
   function Convert_To_Checksum_Type is new Convert_To_Mod (Checksum_Type);

end UDP;

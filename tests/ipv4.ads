with Types; use Types;

package IPv4
  with SPARK_Mode
is

   type Version_Type_Base is range 0 .. ((2**4) - 1) with Size => 4;
   function Convert_To_Version_Type_Base is new Convert_To_Int (Version_Type_Base);

   subtype Version_Type is Version_Type_Base range 4 .. 4;

   type IHL_Type_Base is range 0 .. ((2**4) - 1) with Size => 4;
   function Convert_To_IHL_Type_Base is new Convert_To_Int (IHL_Type_Base);

   subtype IHL_Type is IHL_Type_Base range 5 .. 15;

   type DCSP_Type is mod (2**6);
   function Convert_To_DCSP_Type is new Convert_To_Mod (DCSP_Type);

   type ECN_Type is mod (2**2);
   function Convert_To_ECN_Type is new Convert_To_Mod (ECN_Type);

   type Total_Length_Type_Base is range 0 .. ((2**16) - 1) with Size => 16;
   function Convert_To_Total_Length_Type_Base is new Convert_To_Int (Total_Length_Type_Base);

   subtype Total_Length_Type is Total_Length_Type_Base range 20 .. ((2**16) - 1);

   type Identification_Type is mod (2**16);
   function Convert_To_Identification_Type is new Convert_To_Mod (Identification_Type);

   type Flag_Type is mod 2;
   function Convert_To_Flag_Type is new Convert_To_Mod (Flag_Type);

   type Fragment_Offset_Type is mod (2**13);
   function Convert_To_Fragment_Offset_Type is new Convert_To_Mod (Fragment_Offset_Type);

   type TTL_Type is mod (2**8);
   function Convert_To_TTL_Type is new Convert_To_Mod (TTL_Type);

   type Protocol_Type is mod (2**8);
   function Convert_To_Protocol_Type is new Convert_To_Mod (Protocol_Type);

   type Header_Checksum_Type is mod (2**16);
   function Convert_To_Header_Checksum_Type is new Convert_To_Mod (Header_Checksum_Type);

   type Address_Type is mod (2**32);
   function Convert_To_Address_Type is new Convert_To_Mod (Address_Type);

end IPv4;

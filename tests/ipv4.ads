with Types; use Types;

package IPv4
  with SPARK_Mode
is

   type Flag_Type_Base is mod (2**1);
   function Convert_To_Flag_Type_Base is new Convert_To_Mod (Flag_Type_Base);

   type Flag_Type is (Flag_False, Flag_True) with Size => 1;
   for Flag_Type use (Flag_False => 0, Flag_True => 1);

   type Option_Class_Type_Base is mod (2**2);
   function Convert_To_Option_Class_Type_Base is new Convert_To_Mod (Option_Class_Type_Base);

   type Option_Class_Type is (Control, Debugging_And_Measurement) with Size => 2;
   for Option_Class_Type use (Control => 0, Debugging_And_Measurement => 2);

   type Option_Number_Type is mod (2**5);
   function Convert_To_Option_Number_Type is new Convert_To_Mod (Option_Number_Type);

   type Option_Length_Type is mod (2**8);
   function Convert_To_Option_Length_Type is new Convert_To_Mod (Option_Length_Type);

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

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Flag_Type return Flag_Type is
      (Flag_Type'First)
     with
       Pre => False;

   function Unreachable_Option_Class_Type return Option_Class_Type is
      (Option_Class_Type'First)
     with
       Pre => False;

   function Unreachable_Option_Number_Type return Option_Number_Type is
      (Option_Number_Type'First)
     with
       Pre => False;

   function Unreachable_Option_Length_Type return Option_Length_Type is
      (Option_Length_Type'First)
     with
       Pre => False;

   function Unreachable_Natural return Natural is
      (Natural'First)
     with
       Pre => False;

   function Unreachable_Version_Type return Version_Type is
      (Version_Type'First)
     with
       Pre => False;

   function Unreachable_IHL_Type return IHL_Type is
      (IHL_Type'First)
     with
       Pre => False;

   function Unreachable_DCSP_Type return DCSP_Type is
      (DCSP_Type'First)
     with
       Pre => False;

   function Unreachable_ECN_Type return ECN_Type is
      (ECN_Type'First)
     with
       Pre => False;

   function Unreachable_Total_Length_Type return Total_Length_Type is
      (Total_Length_Type'First)
     with
       Pre => False;

   function Unreachable_Identification_Type return Identification_Type is
      (Identification_Type'First)
     with
       Pre => False;

   function Unreachable_Fragment_Offset_Type return Fragment_Offset_Type is
      (Fragment_Offset_Type'First)
     with
       Pre => False;

   function Unreachable_TTL_Type return TTL_Type is
      (TTL_Type'First)
     with
       Pre => False;

   function Unreachable_Protocol_Type return Protocol_Type is
      (Protocol_Type'First)
     with
       Pre => False;

   function Unreachable_Header_Checksum_Type return Header_Checksum_Type is
      (Header_Checksum_Type'First)
     with
       Pre => False;

   function Unreachable_Address_Type return Address_Type is
      (Address_Type'First)
     with
       Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Valid_Flag_Type (Buffer : Bytes; Offset : Natural) return Boolean is
      (case Convert_To_Flag_Type_Base (Buffer, Offset) is when 0 | 1 => True, when others => False)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((Flag_Type_Base'Size + Offset + (-1)) / 8) + 1));

   function Convert_To_Flag_Type (Buffer : Bytes; Offset : Natural) return Flag_Type is
      (case Convert_To_Flag_Type_Base (Buffer, Offset) is when 0 => Flag_False, when 1 => Flag_True, when others => Unreachable_Flag_Type)
     with
       Pre => ((Offset < 8 and then Buffer'Length = (((Flag_Type_Base'Size + Offset + (-1)) / 8) + 1)) and then Valid_Flag_Type (Buffer, Offset));

   function Valid_Option_Class_Type (Buffer : Bytes; Offset : Natural) return Boolean is
      (case Convert_To_Option_Class_Type_Base (Buffer, Offset) is when 0 | 2 => True, when others => False)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((Option_Class_Type_Base'Size + Offset + (-1)) / 8) + 1));

   function Convert_To_Option_Class_Type (Buffer : Bytes; Offset : Natural) return Option_Class_Type is
      (case Convert_To_Option_Class_Type_Base (Buffer, Offset) is when 0 => Control, when 2 => Debugging_And_Measurement, when others => Unreachable_Option_Class_Type)
     with
       Pre => ((Offset < 8 and then Buffer'Length = (((Option_Class_Type_Base'Size + Offset + (-1)) / 8) + 1)) and then Valid_Option_Class_Type (Buffer, Offset));

end IPv4;

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

   function Unreachable_Natural return Natural is
      (Natural'First)
     with
       Pre => False;

   pragma Warnings (On, "precondition is statically false");

end UDP;

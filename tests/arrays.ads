with Types;
use type Types.Bytes, Types.Index_Type, Types.Length_Type;

package Arrays
  with SPARK_Mode
is

   type Length_Type is mod (2**8);
   function Convert_To_Length_Type is new Types.Convert_To_Mod (Length_Type);

   type Modular_Integer is mod (2**16);
   function Convert_To_Modular_Integer is new Types.Convert_To_Mod (Modular_Integer);

   type Range_Integer_Base is range 0 .. ((2**8) - 1) with Size => 8;
   function Convert_To_Range_Integer_Base is new Types.Convert_To_Int (Range_Integer_Base);

   subtype Range_Integer is Range_Integer_Base range 1 .. 100;

   type Enumeration_Base is mod (2**8);
   function Convert_To_Enumeration_Base is new Types.Convert_To_Mod (Enumeration_Base);

   type Enumeration is (ZERO, ONE, TWO) with Size => 8;
   for Enumeration use (ZERO => 0, ONE => 1, TWO => 2);

   type AV_Enumeration_Base is mod (2**8);
   function Convert_To_AV_Enumeration_Base is new Types.Convert_To_Mod (AV_Enumeration_Base);

   type AV_Enumeration_Enum is (AV_ZERO, AV_ONE, AV_TWO) with Size => 8;
   for AV_Enumeration_Enum use (AV_ZERO => 0, AV_ONE => 1, AV_TWO => 2);

   type AV_Enumeration (Known : Boolean := False) is
      record
         case Known is
            when True =>
               Enum : AV_Enumeration_Enum;
            when False =>
               Raw : AV_Enumeration_Base;
         end case;
      end record;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Length_Type return Length_Type is
      (Length_Type'First)
     with
       Pre => False;

   function Unreachable_Modular_Integer return Modular_Integer is
      (Modular_Integer'First)
     with
       Pre => False;

   function Unreachable_Types_Index_Type return Types.Index_Type is
      (Types.Index_Type'First)
     with
       Pre => False;

   function Unreachable_Range_Integer return Range_Integer is
      (Range_Integer'First)
     with
       Pre => False;

   function Unreachable_Enumeration return Enumeration is
      (Enumeration'First)
     with
       Pre => False;

   function Unreachable_AV_Enumeration return AV_Enumeration is
      ((False, AV_Enumeration_Base'First))
     with
       Pre => False;

   function Unreachable_Types_Length_Type return Types.Length_Type is
      (Types.Length_Type'First)
     with
       Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Valid_Length_Type (Buffer : Types.Bytes; Offset : Natural) return Boolean is
      (True)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((Length_Type'Size + Offset + (-1)) / 8) + 1));

   function Valid_Modular_Integer (Buffer : Types.Bytes; Offset : Natural) return Boolean is
      (True)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((Modular_Integer'Size + Offset + (-1)) / 8) + 1));

   function Valid_Range_Integer (Buffer : Types.Bytes; Offset : Natural) return Boolean is
      ((Convert_To_Range_Integer_Base (Buffer, Offset) >= 1 and then Convert_To_Range_Integer_Base (Buffer, Offset) <= 100))
     with
       Pre => (Offset < 8 and then Buffer'Length = (((Range_Integer_Base'Size + Offset + (-1)) / 8) + 1));

   function Valid_Enumeration (Buffer : Types.Bytes; Offset : Natural) return Boolean is
      (case Convert_To_Enumeration_Base (Buffer, Offset) is when 0 | 1 | 2 => True, when others => False)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((Enumeration_Base'Size + Offset + (-1)) / 8) + 1));

   function Convert_To_Enumeration (Buffer : Types.Bytes; Offset : Natural) return Enumeration is
      (case Convert_To_Enumeration_Base (Buffer, Offset) is when 0 => ZERO, when 1 => ONE, when 2 => TWO, when others => Unreachable_Enumeration)
     with
       Pre => ((Offset < 8 and then Buffer'Length = (((Enumeration_Base'Size + Offset + (-1)) / 8) + 1)) and then Valid_Enumeration (Buffer, Offset));

   function Convert_To_Enumeration_Base (Enum : Enumeration) return Enumeration_Base is
      (case Enum is when ZERO => 0, when ONE => 1, when TWO => 2);

   function Valid_AV_Enumeration (Buffer : Types.Bytes; Offset : Natural) return Boolean is
      (True)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((AV_Enumeration_Base'Size + Offset + (-1)) / 8) + 1));

   function Convert_To_AV_Enumeration (Buffer : Types.Bytes; Offset : Natural) return AV_Enumeration
     with
       Pre => ((Offset < 8 and then Buffer'Length = (((AV_Enumeration_Base'Size + Offset + (-1)) / 8) + 1)) and then Valid_AV_Enumeration (Buffer, Offset));

   function Convert_To_AV_Enumeration_Base (Enum : AV_Enumeration_Enum) return AV_Enumeration_Base is
      (case Enum is when AV_ZERO => 0, when AV_ONE => 1, when AV_TWO => 2);

end Arrays;

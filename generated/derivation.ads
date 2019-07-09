with Types;
use type Types.Bytes, Types.Index_Type, Types.Length_Type;
with Arrays;

package Derivation with
  SPARK_Mode
is

   subtype Length_Type is Arrays.Length_Type;

   function Convert_To_Length_Type (Buffer : Types.Bytes; Offset : Natural) return Length_Type renames Arrays.Convert_To_Length_Type;

   function Valid_Length_Type (Buffer : Types.Bytes; Offset : Natural) return Boolean renames Arrays.Valid_Length_Type;

   subtype Modular_Integer is Arrays.Modular_Integer;

   function Convert_To_Modular_Integer (Buffer : Types.Bytes; Offset : Natural) return Modular_Integer renames Arrays.Convert_To_Modular_Integer;

   function Valid_Modular_Integer (Buffer : Types.Bytes; Offset : Natural) return Boolean renames Arrays.Valid_Modular_Integer;

   subtype Range_Integer_Base is Arrays.Range_Integer_Base;

   subtype Range_Integer is Arrays.Range_Integer;

   function Convert_To_Range_Integer_Base (Buffer : Types.Bytes; Offset : Natural) return Range_Integer_Base renames Arrays.Convert_To_Range_Integer_Base;

   function Valid_Range_Integer (Buffer : Types.Bytes; Offset : Natural) return Boolean renames Arrays.Valid_Range_Integer;

   subtype Enumeration_Base is Arrays.Enumeration_Base;

   subtype Enumeration is Arrays.Enumeration;

   function Convert_To_Enumeration_Base (Buffer : Types.Bytes; Offset : Natural) return Enumeration_Base renames Arrays.Convert_To_Enumeration_Base;

   function Valid_Enumeration (Buffer : Types.Bytes; Offset : Natural) return Boolean renames Arrays.Valid_Enumeration;

   function Convert_To_Enumeration (Buffer : Types.Bytes; Offset : Natural) return Enumeration renames Arrays.Convert_To_Enumeration;

   function Convert_To_Enumeration_Base (Enum : Enumeration) return Enumeration_Base renames Arrays.Convert_To_Enumeration_Base;

   ZERO : constant Enumeration := Arrays.ZERO;

   ONE : constant Enumeration := Arrays.ONE;

   TWO : constant Enumeration := Arrays.TWO;

   subtype AV_Enumeration_Base is Arrays.AV_Enumeration_Base;

   subtype AV_Enumeration_Enum is Arrays.AV_Enumeration_Enum;

   subtype AV_Enumeration is Arrays.AV_Enumeration;

   function Convert_To_AV_Enumeration_Base (Buffer : Types.Bytes; Offset : Natural) return AV_Enumeration_Base renames Arrays.Convert_To_AV_Enumeration_Base;

   function Valid_AV_Enumeration (Buffer : Types.Bytes; Offset : Natural) return Boolean renames Arrays.Valid_AV_Enumeration;

   function Convert_To_AV_Enumeration (Buffer : Types.Bytes; Offset : Natural) return AV_Enumeration renames Arrays.Convert_To_AV_Enumeration;

   function Convert_To_AV_Enumeration_Base (Enum : AV_Enumeration_Enum) return AV_Enumeration_Base renames Arrays.Convert_To_AV_Enumeration_Base;

   AV_ZERO : constant AV_Enumeration_Enum := Arrays.AV_ZERO;

   AV_ONE : constant AV_Enumeration_Enum := Arrays.AV_ONE;

   AV_TWO : constant AV_Enumeration_Enum := Arrays.AV_TWO;

end Derivation;

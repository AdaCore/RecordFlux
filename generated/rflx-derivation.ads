with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index_Type, RFLX.Types.Length_Type, RFLX.Types.Bit_Index_Type, RFLX.Types.Bit_Length_Type;
with RFLX.Arrays;

package RFLX.Derivation with
  SPARK_Mode
is

   subtype Length_Type is Arrays.Length_Type;

   function Convert (Buffer : RFLX.Types.Bytes; Offset : RFLX.Types.Offset_Type) return Length_Type renames Arrays.Convert;

   function Valid (Value : Length_Type) return Boolean renames Arrays.Valid;

   function Convert (Value : Length_Type) return Length_Type renames Arrays.Convert;

   subtype Modular_Integer is Arrays.Modular_Integer;

   function Convert (Buffer : RFLX.Types.Bytes; Offset : RFLX.Types.Offset_Type) return Modular_Integer renames Arrays.Convert;

   function Valid (Value : Modular_Integer) return Boolean renames Arrays.Valid;

   function Convert (Value : Modular_Integer) return Modular_Integer renames Arrays.Convert;

   subtype Range_Integer_Base is Arrays.Range_Integer_Base;

   subtype Range_Integer is Arrays.Range_Integer;

   function Convert (Buffer : RFLX.Types.Bytes; Offset : RFLX.Types.Offset_Type) return Range_Integer_Base renames Arrays.Convert;

   function Valid (Value : Range_Integer_Base) return Boolean renames Arrays.Valid;

   function Convert (Value : Range_Integer_Base) return Range_Integer renames Arrays.Convert;

   subtype Enumeration_Base is Arrays.Enumeration_Base;

   subtype Enumeration is Arrays.Enumeration;

   function Convert (Buffer : RFLX.Types.Bytes; Offset : RFLX.Types.Offset_Type) return Enumeration_Base renames Arrays.Convert;

   function Valid (Value : Enumeration_Base) return Boolean renames Arrays.Valid;

   function Convert (Value : Enumeration_Base) return Enumeration renames Arrays.Convert;

   function Convert (Enum : Enumeration) return Enumeration_Base renames Arrays.Convert;

   ZERO : constant Enumeration := Arrays.ZERO;

   ONE : constant Enumeration := Arrays.ONE;

   TWO : constant Enumeration := Arrays.TWO;

   subtype AV_Enumeration_Base is Arrays.AV_Enumeration_Base;

   subtype AV_Enumeration_Enum is Arrays.AV_Enumeration_Enum;

   subtype AV_Enumeration is Arrays.AV_Enumeration;

   function Convert (Buffer : RFLX.Types.Bytes; Offset : RFLX.Types.Offset_Type) return AV_Enumeration_Base renames Arrays.Convert;

   function Valid (Value : AV_Enumeration_Base) return Boolean renames Arrays.Valid;

   function Convert (Value : AV_Enumeration_Base) return AV_Enumeration renames Arrays.Convert;

   function Convert (Enum : AV_Enumeration_Enum) return AV_Enumeration_Base renames Arrays.Convert;

   AV_ZERO : constant AV_Enumeration_Enum := Arrays.AV_ZERO;

   AV_ONE : constant AV_Enumeration_Enum := Arrays.AV_ONE;

   AV_TWO : constant AV_Enumeration_Enum := Arrays.AV_TWO;

end RFLX.Derivation;

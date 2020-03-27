with RFLX.Arrays;

package RFLX.Derivation with
  SPARK_Mode
is

   subtype Length is Arrays.Length;

   function Valid (Val : RFLX.Arrays.Length) return Boolean renames Arrays.Valid;

   function Convert (Val : RFLX.Arrays.Length) return RFLX.Arrays.Length renames Arrays.Convert;

   subtype Modular_Integer is Arrays.Modular_Integer;

   function Valid (Val : RFLX.Arrays.Modular_Integer) return Boolean renames Arrays.Valid;

   function Convert (Val : RFLX.Arrays.Modular_Integer) return RFLX.Arrays.Modular_Integer renames Arrays.Convert;

   subtype Range_Integer_Base is Arrays.Range_Integer_Base;

   subtype Range_Integer is Arrays.Range_Integer;

   function Valid (Val : RFLX.Arrays.Range_Integer_Base) return Boolean renames Arrays.Valid;

   function Convert (Val : RFLX.Arrays.Range_Integer_Base) return RFLX.Arrays.Range_Integer renames Arrays.Convert;

   subtype Enumeration_Base is Arrays.Enumeration_Base;

   subtype Enumeration is Arrays.Enumeration;

   function Valid (Val : RFLX.Arrays.Enumeration_Base) return Boolean renames Arrays.Valid;

   function Convert (Enum : RFLX.Arrays.Enumeration) return RFLX.Arrays.Enumeration_Base renames Arrays.Convert;

   function Convert (Val : RFLX.Arrays.Enumeration_Base) return RFLX.Arrays.Enumeration renames Arrays.Convert;

   ZERO : constant Enumeration := Arrays.ZERO;

   ONE : constant Enumeration := Arrays.ONE;

   TWO : constant Enumeration := Arrays.TWO;

   subtype AV_Enumeration_Base is Arrays.AV_Enumeration_Base;

   subtype AV_Enumeration_Enum is Arrays.AV_Enumeration_Enum;

   subtype AV_Enumeration is Arrays.AV_Enumeration;

   function Valid (Val : RFLX.Arrays.AV_Enumeration_Base) return Boolean renames Arrays.Valid;

   function Convert (Enum : RFLX.Arrays.AV_Enumeration_Enum) return RFLX.Arrays.AV_Enumeration_Base renames Arrays.Convert;

   function Convert (Enum : AV_Enumeration_Enum) return RFLX.Arrays.AV_Enumeration renames Arrays.Convert;

   function Convert (Val : RFLX.Arrays.AV_Enumeration_Base) return RFLX.Arrays.AV_Enumeration renames Arrays.Convert;

   function Convert (Val : RFLX.Arrays.AV_Enumeration) return RFLX.Arrays.AV_Enumeration_Base renames Arrays.Convert;

   AV_ZERO : constant AV_Enumeration_Enum := Arrays.AV_ZERO;

   AV_ONE : constant AV_Enumeration_Enum := Arrays.AV_ONE;

   AV_TWO : constant AV_Enumeration_Enum := Arrays.AV_TWO;

end RFLX.Derivation;

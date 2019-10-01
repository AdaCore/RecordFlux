with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index, RFLX.Types.Length, RFLX.Types.Bit_Index, RFLX.Types.Bit_Length;

package RFLX.Arrays with
  SPARK_Mode
is

   type Length is mod 2**8;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Length return Length is
     (Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Length);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Length) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Length) return Length is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Modular_Integer is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Modular_Integer return Modular_Integer is
     (Modular_Integer'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Modular_Integer);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Modular_Integer) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Modular_Integer) return Modular_Integer is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Range_Integer_Base is range 0 .. 2**8 - 1 with
     Size =>
       8;

   subtype Range_Integer is Range_Integer_Base range 1 .. 100;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Range_Integer return Range_Integer is
     (Range_Integer'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Range_Integer_Base);

   function Valid (Value : Range_Integer_Base) return Boolean is
     (Value >= 1
      and then Value <= 100);

   function Convert (Value : Range_Integer_Base) return Range_Integer is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Enumeration_Base is mod 2**8;

   type Enumeration is (ZERO, ONE, TWO) with
     Size =>
       8;
   for Enumeration use (ZERO => 0, ONE => 1, TWO => 2);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Enumeration return Enumeration is
     (Enumeration'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Enumeration_Base);

   function Valid (Value : Enumeration_Base) return Boolean is
     ((case Value is
         when 0 | 1 | 2 =>
            True,
         when others =>
            False));

   function Convert (Value : Enumeration_Base) return Enumeration is
     ((case Value is
         when 0 =>
            ZERO,
         when 1 =>
            ONE,
         when 2 =>
            TWO,
         when others =>
            Unreachable_Enumeration))
    with
     Pre =>
       Valid (Value);

   function Convert (Enum : Enumeration) return Enumeration_Base is
     ((case Enum is
         when ZERO =>
            0,
         when ONE =>
            1,
         when TWO =>
            2));

   type AV_Enumeration_Base is mod 2**8;

   type AV_Enumeration_Enum is (AV_ZERO, AV_ONE, AV_TWO) with
     Size =>
       8;
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

   function Unreachable_AV_Enumeration return AV_Enumeration is
     ((False, AV_Enumeration_Base'First))
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, AV_Enumeration_Base);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : AV_Enumeration_Base) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : AV_Enumeration_Base) return AV_Enumeration is
     ((case Value is
         when 0 =>
            (True, AV_ZERO),
         when 1 =>
            (True, AV_ONE),
         when 2 =>
            (True, AV_TWO),
         when others =>
            (False, Value)))
    with
     Pre =>
       Valid (Value);

   function Convert (Enum : AV_Enumeration_Enum) return AV_Enumeration_Base is
     ((case Enum is
         when AV_ZERO =>
            0,
         when AV_ONE =>
            1,
         when AV_TWO =>
            2));

end RFLX.Arrays;

package RFLX.Arrays with
  SPARK_Mode
is

   type Length is mod 2**8;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Arrays_Length return RFLX.Arrays.Length is
     (RFLX.Arrays.Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : RFLX.Arrays.Length) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : RFLX.Arrays.Length) return RFLX.Arrays.Length is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Modular_Integer is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Arrays_Modular_Integer return RFLX.Arrays.Modular_Integer is
     (RFLX.Arrays.Modular_Integer'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : RFLX.Arrays.Modular_Integer) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : RFLX.Arrays.Modular_Integer) return RFLX.Arrays.Modular_Integer is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Range_Integer_Base is range 0 .. 2**8 - 1 with
     Size =>
       8;

   subtype Range_Integer is Range_Integer_Base range 1 .. 100;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Arrays_Range_Integer return RFLX.Arrays.Range_Integer is
     (RFLX.Arrays.Range_Integer'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : RFLX.Arrays.Range_Integer_Base) return Boolean is
     (Val >= 1
      and Val <= 100);

   function Convert (Val : RFLX.Arrays.Range_Integer_Base) return RFLX.Arrays.Range_Integer is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Enumeration_Base is mod 2**8;

   type Enumeration is (ZERO, ONE, TWO) with
     Size =>
       8;
   for Enumeration use (ZERO => 0, ONE => 1, TWO => 2);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Arrays_Enumeration return RFLX.Arrays.Enumeration is
     (RFLX.Arrays.Enumeration'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : RFLX.Arrays.Enumeration_Base) return Boolean is
     ((case Val is
         when 0 | 1 | 2 =>
            True,
         when others =>
            False));

   function Convert (Enum : RFLX.Arrays.Enumeration) return RFLX.Arrays.Enumeration_Base is
     ((case Enum is
         when ZERO =>
            0,
         when ONE =>
            1,
         when TWO =>
            2));

   function Convert (Val : RFLX.Arrays.Enumeration_Base) return RFLX.Arrays.Enumeration is
     ((case Val is
         when 0 =>
            ZERO,
         when 1 =>
            ONE,
         when 2 =>
            TWO,
         when others =>
            Unreachable_Arrays_Enumeration))
    with
     Pre =>
       Valid (Val);

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

   function Unreachable_Arrays_AV_Enumeration return RFLX.Arrays.AV_Enumeration is
     ((False, RFLX.Arrays.AV_Enumeration_Base'First))
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : RFLX.Arrays.AV_Enumeration_Base) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Enum : RFLX.Arrays.AV_Enumeration_Enum) return RFLX.Arrays.AV_Enumeration_Base is
     ((case Enum is
         when AV_ZERO =>
            0,
         when AV_ONE =>
            1,
         when AV_TWO =>
            2));

   function Convert (Enum : AV_Enumeration_Enum) return RFLX.Arrays.AV_Enumeration is
     ((True, Enum));

   function Convert (Val : RFLX.Arrays.AV_Enumeration_Base) return RFLX.Arrays.AV_Enumeration is
     ((case Val is
         when 0 =>
            (True, AV_ZERO),
         when 1 =>
            (True, AV_ONE),
         when 2 =>
            (True, AV_TWO),
         when others =>
            (False, Val)))
    with
     Pre =>
       Valid (Val);

   function Convert (Val : RFLX.Arrays.AV_Enumeration) return RFLX.Arrays.AV_Enumeration_Base is
     ((if Val.Known then
       Convert (Val.Enum)
    else
       Val.Raw));

end RFLX.Arrays;

pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.Sequence with
  SPARK_Mode
is

   type Length is mod 2**8 with
     Size =>
       8;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_Sequence_Length return RFLX.Sequence.Length is
     (RFLX.Sequence.Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.Sequence.Length) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.Sequence.Length) return RFLX.Sequence.Length is
     (Val);

   function To_Actual (Val : RFLX.Sequence.Length) return RFLX.Sequence.Length is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Modular_Integer is mod 2**16 with
     Size =>
       16;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_Sequence_Modular_Integer return RFLX.Sequence.Modular_Integer is
     (RFLX.Sequence.Modular_Integer'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.Sequence.Modular_Integer) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.Sequence.Modular_Integer) return RFLX.Sequence.Modular_Integer is
     (Val);

   function To_Actual (Val : RFLX.Sequence.Modular_Integer) return RFLX.Sequence.Modular_Integer is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Range_Integer_Base is mod 2**8 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Range_Integer is range 1 .. 100 with
     Size =>
       8;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_Sequence_Range_Integer return RFLX.Sequence.Range_Integer is
     (RFLX.Sequence.Range_Integer'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.Sequence.Range_Integer_Base) return Boolean is
     (Val >= 1
      and Val <= 100);

   function To_Base (Val : RFLX.Sequence.Range_Integer) return RFLX.Sequence.Range_Integer_Base is
     (RFLX.Sequence.Range_Integer_Base (Val));

   function To_Actual (Val : RFLX.Sequence.Range_Integer_Base) return RFLX.Sequence.Range_Integer is
     (RFLX.Sequence.Range_Integer (Val))
    with
     Pre =>
       Valid (Val);

   type Enumeration_Base is mod 2**8;

   type Enumeration is (Zero, One, Two) with
     Size =>
       8;
   for Enumeration use (Zero => 0, One => 1, Two => 2);

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_Sequence_Enumeration return RFLX.Sequence.Enumeration is
     (RFLX.Sequence.Enumeration'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.Sequence.Enumeration_Base) return Boolean is
     ((case Val is
          when 0 | 1 | 2 =>
             True,
          when others =>
             False));

   function To_Base (Enum : RFLX.Sequence.Enumeration) return RFLX.Sequence.Enumeration_Base is
     ((case Enum is
          when Zero =>
             0,
          when One =>
             1,
          when Two =>
             2));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.Sequence.Enumeration_Base) return RFLX.Sequence.Enumeration is
     ((case Val is
          when 0 =>
             Zero,
          when 1 =>
             One,
          when 2 =>
             Two,
          when others =>
             Unreachable_Sequence_Enumeration))
    with
     Pre =>
       Valid (Val);

   pragma Warnings (On, "unreachable branch");

   type AV_Enumeration_Base is mod 2**8;

   type AV_Enumeration_Enum is (AV_Zero, AV_One, AV_Two) with
     Size =>
       8;
   for AV_Enumeration_Enum use (AV_Zero => 0, AV_One => 1, AV_Two => 2);

   type AV_Enumeration (Known : Boolean := False) is
      record
         case Known is
            when True =>
               Enum : AV_Enumeration_Enum;
            when False =>
               Raw : AV_Enumeration_Base;
         end case;
      end record;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_Sequence_AV_Enumeration return RFLX.Sequence.AV_Enumeration is
     ((False, RFLX.Sequence.AV_Enumeration_Base'First))
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.Sequence.AV_Enumeration_Base) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Enum : RFLX.Sequence.AV_Enumeration_Enum) return RFLX.Sequence.AV_Enumeration_Base is
     ((case Enum is
          when AV_Zero =>
             0,
          when AV_One =>
             1,
          when AV_Two =>
             2));

   function To_Actual (Enum : AV_Enumeration_Enum) return RFLX.Sequence.AV_Enumeration is
     ((True, Enum));

   function To_Actual (Val : RFLX.Sequence.AV_Enumeration_Base) return RFLX.Sequence.AV_Enumeration is
     ((case Val is
          when 0 =>
             (True, AV_Zero),
          when 1 =>
             (True, AV_One),
          when 2 =>
             (True, AV_Two),
          when others =>
             (False, Val)))
    with
     Pre =>
       Valid (Val);

   function To_Base (Val : RFLX.Sequence.AV_Enumeration) return RFLX.Sequence.AV_Enumeration_Base is
     ((if
          Val.Known
       then
          To_Base (Val.Enum)
       else
          Val.Raw));

end RFLX.Sequence;

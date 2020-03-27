package RFLX.Enumeration with
  SPARK_Mode
is

   type Priority_Base is mod 2**3;

   type Priority_Enum is (LOW, MEDIUM, HIGH) with
     Size =>
       3;
   for Priority_Enum use (LOW => 1, MEDIUM => 4, HIGH => 7);

   type Priority (Known : Boolean := False) is
      record
         case Known is
            when True =>
               Enum : Priority_Enum;
            when False =>
               Raw : Priority_Base;
         end case;
      end record;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Enumeration_Priority return RFLX.Enumeration.Priority is
     ((False, RFLX.Enumeration.Priority_Base'First))
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : RFLX.Enumeration.Priority_Base) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Enum : RFLX.Enumeration.Priority_Enum) return RFLX.Enumeration.Priority_Base is
     ((case Enum is
         when LOW =>
            1,
         when MEDIUM =>
            4,
         when HIGH =>
            7));

   function Convert (Enum : Priority_Enum) return RFLX.Enumeration.Priority is
     ((True, Enum));

   function Convert (Val : RFLX.Enumeration.Priority_Base) return RFLX.Enumeration.Priority is
     ((case Val is
         when 1 =>
            (True, LOW),
         when 4 =>
            (True, MEDIUM),
         when 7 =>
            (True, HIGH),
         when others =>
            (False, Val)))
    with
     Pre =>
       Valid (Val);

   function Convert (Val : RFLX.Enumeration.Priority) return RFLX.Enumeration.Priority_Base is
     ((if Val.Known then
       Convert (Val.Enum)
    else
       Val.Raw));

end RFLX.Enumeration;

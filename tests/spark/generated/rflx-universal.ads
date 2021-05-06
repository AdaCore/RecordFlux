pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.Universal with
  SPARK_Mode
is

   type Option_Type_Base is mod 2**8;

   type Option_Type_Enum is (OT_Null, OT_Data) with
     Size =>
       8;
   for Option_Type_Enum use (OT_Null => 0, OT_Data => 1);

   type Option_Type (Known : Boolean := False) is
      record
         case Known is
            when True =>
               Enum : Option_Type_Enum;
            when False =>
               Raw : Option_Type_Base;
         end case;
      end record;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.Universal.Option_Type_Base) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Enum : RFLX.Universal.Option_Type_Enum) return RFLX.Universal.Option_Type_Base is
     ((case Enum is
          when OT_Null =>
             0,
          when OT_Data =>
             1));

   function To_Actual (Enum : Option_Type_Enum) return RFLX.Universal.Option_Type is
     ((True, Enum));

   function To_Actual (Val : RFLX.Universal.Option_Type_Base) return RFLX.Universal.Option_Type is
     ((case Val is
          when 0 =>
             (True, OT_Null),
          when 1 =>
             (True, OT_Data),
          when others =>
             (False, Val)))
    with
     Pre =>
       Valid (Val);

   function To_Base (Val : RFLX.Universal.Option_Type) return RFLX.Universal.Option_Type_Base is
     ((if
          Val.Known
       then
          To_Base (Val.Enum)
       else
          Val.Raw));

end RFLX.Universal;

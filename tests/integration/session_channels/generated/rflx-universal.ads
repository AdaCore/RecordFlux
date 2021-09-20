pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.Universal with
  SPARK_Mode
is

   type Message_Type_Base is mod 2**8;

   type Message_Type is (MT_Null, MT_Data, MT_Value, MT_Values, MT_Option_Types, MT_Options, MT_Unconstrained_Data, MT_Unconstrained_Options) with
     Size =>
       8;
   for Message_Type use (MT_Null => 0, MT_Data => 1, MT_Value => 2, MT_Values => 3, MT_Option_Types => 4, MT_Options => 5, MT_Unconstrained_Data => 6, MT_Unconstrained_Options => 7);

   function Valid (Val : RFLX.Universal.Message_Type_Base) return Boolean is
     ((case Val is
          when 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 =>
             True,
          when others =>
             False));

   function To_Base (Enum : RFLX.Universal.Message_Type) return RFLX.Universal.Message_Type_Base is
     ((case Enum is
          when MT_Null =>
             0,
          when MT_Data =>
             1,
          when MT_Value =>
             2,
          when MT_Values =>
             3,
          when MT_Option_Types =>
             4,
          when MT_Options =>
             5,
          when MT_Unconstrained_Data =>
             6,
          when MT_Unconstrained_Options =>
             7));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.Universal.Message_Type_Base) return RFLX.Universal.Message_Type is
     ((case Val is
          when 0 =>
             MT_Null,
          when 1 =>
             MT_Data,
          when 2 =>
             MT_Value,
          when 3 =>
             MT_Values,
          when 4 =>
             MT_Option_Types,
          when 5 =>
             MT_Options,
          when 6 =>
             MT_Unconstrained_Data,
          when 7 =>
             MT_Unconstrained_Options,
          when others =>
             raise Program_Error))
    with
     Pre =>
       Valid (Val);

   pragma Warnings (On, "unreachable branch");

   type Length_Base is mod 2**16 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Length is range 0 .. 2**16 - 1 with
     Size =>
       16;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.Universal.Length_Base) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.Universal.Length) return RFLX.Universal.Length_Base is
     (RFLX.Universal.Length_Base (Val));

   function To_Actual (Val : RFLX.Universal.Length_Base) return RFLX.Universal.Length is
     (RFLX.Universal.Length (Val))
    with
     Pre =>
       Valid (Val);

   type Value is mod 256 with
     Size =>
       8;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.Universal.Value) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.Universal.Value) return RFLX.Universal.Value is
     (Val);

   function To_Actual (Val : RFLX.Universal.Value) return RFLX.Universal.Value is
     (Val)
    with
     Pre =>
       Valid (Val);

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
     ((if Val.Known then To_Base (Val.Enum) else Val.Raw));

end RFLX.Universal;

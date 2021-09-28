pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.TLV with
  SPARK_Mode
is

   type Tag_Base is mod 2**8;

   type Tag is (Msg_Data, Msg_Error) with
     Size =>
       8;
   for Tag use (Msg_Data => 1, Msg_Error => 3);

   function Valid (Val : RFLX.TLV.Tag_Base) return Boolean is
     ((case Val is
          when 1 | 3 =>
             True,
          when others =>
             False));

   function To_Base (Enum : RFLX.TLV.Tag) return RFLX.TLV.Tag_Base is
     ((case Enum is
          when Msg_Data =>
             1,
          when Msg_Error =>
             3));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.TLV.Tag_Base) return RFLX.TLV.Tag is
     ((case Val is
          when 1 =>
             Msg_Data,
          when 3 =>
             Msg_Error,
          when others =>
             raise Program_Error))
    with
     Pre =>
       Valid (Val);

   pragma Warnings (On, "unreachable branch");

   type Length is mod 2**16 with
     Size =>
       16;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.TLV.Length) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.TLV.Length) return RFLX.TLV.Length is
     (Val);

   function To_Actual (Val : RFLX.TLV.Length) return RFLX.TLV.Length is
     (Val)
    with
     Pre =>
       Valid (Val);

end RFLX.TLV;
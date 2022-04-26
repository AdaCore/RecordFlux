pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.TLV with
  SPARK_Mode
is

   type Tag is (Msg_Data, Msg_Error) with
     Size =>
       8;
   for Tag use (Msg_Data => 1, Msg_Error => 3);

   use type RFLX.RFLX_Types.S63;

   function Valid_Tag (Val : RFLX.RFLX_Types.S63) return Boolean is
     (Val in 1 | 3);

   function To_S63 (Enum : RFLX.TLV.Tag) return RFLX.RFLX_Types.S63 is
     ((case Enum is
          when Msg_Data =>
             1,
          when Msg_Error =>
             3));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.S63) return RFLX.TLV.Tag is
     ((case Val is
          when 1 =>
             Msg_Data,
          when 3 =>
             Msg_Error,
          when others =>
             RFLX.TLV.Tag'Last))
    with
     Pre =>
       Valid_Tag (Val);

   pragma Warnings (On, "unreachable branch");

   type Length is mod 2**16 with
     Size =>
       16;

   function Valid_Length (Val : RFLX.RFLX_Types.S63) return Boolean is
     (Val <= 65535);

   function To_S63 (Val : RFLX.TLV.Length) return RFLX.RFLX_Types.S63 is
     (RFLX.RFLX_Types.S63 (Val));

   function To_Actual (Val : RFLX.RFLX_Types.S63) return RFLX.TLV.Length is
     (RFLX.TLV.Length (Val))
    with
     Pre =>
       Valid_Length (Val);

end RFLX.TLV;

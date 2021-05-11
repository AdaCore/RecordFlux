pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.TLV;
use RFLX.TLV;
with RFLX.TLV.Message;

package RFLX.In_TLV.Contains with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   function Null_Message_In_TLV_Message_Value (Ctx : TLV.Message.Context) return Boolean is
     (TLV.Message.Has_Buffer (Ctx)
      and then TLV.Message.Structural_Valid (Ctx, TLV.Message.F_Value)
      and then not TLV.Message.Present (Ctx, TLV.Message.F_Value));

end RFLX.In_TLV.Contains;

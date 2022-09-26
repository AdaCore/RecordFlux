pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.TLV;
use RFLX.TLV;
with RFLX.TLV.Message;

package RFLX.In_TLV.Contains with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Always_Return)
is

   function Null_Message_In_TLV_Message_Value (Ctx : RFLX.TLV.Message.Context) return Boolean is
     (RFLX.TLV.Message.Has_Buffer (Ctx)
      and then RFLX.TLV.Message.Structural_Valid (Ctx, RFLX.TLV.Message.F_Value)
      and then not RFLX.TLV.Message.Present (Ctx, RFLX.TLV.Message.F_Value));

end RFLX.In_TLV.Contains;

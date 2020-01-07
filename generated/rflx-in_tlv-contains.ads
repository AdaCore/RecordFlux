with RFLX.TLV;
use RFLX.TLV;
with RFLX.TLV.Message;
use type RFLX.TLV.Message.Field_Cursors;

package RFLX.In_TLV.Contains with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Contains);

   function Null_Message_In_TLV_Message_Value (Ctx : TLV.Message.Context) return Boolean is
     (TLV.Message.Has_Buffer (Ctx)
      and then TLV.Message.Structural_Valid (Ctx, TLV.Message.F_Value)
      and then not TLV.Message.Present (Ctx, TLV.Message.F_Value));

end RFLX.In_TLV.Contains;

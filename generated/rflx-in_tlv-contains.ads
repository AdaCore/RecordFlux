with RFLX.TLV;
use RFLX.TLV;
with RFLX.TLV.Message;

package RFLX.In_TLV.Contains with
  SPARK_Mode
is

   function Null_Message_In_TLV_Message_Value (Context : TLV.Message.Context_Type) return Boolean is
     (TLV.Message.Has_Buffer (Context)
      and then TLV.Message.Structural_Valid (Context, TLV.Message.F_Value)
      and then not TLV.Message.Present (Context, TLV.Message.F_Value));

end RFLX.In_TLV.Contains;

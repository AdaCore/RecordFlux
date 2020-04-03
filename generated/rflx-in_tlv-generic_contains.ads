with RFLX.Generic_Types;
with RFLX.TLV;
use RFLX.TLV;
with RFLX.TLV.Generic_Message;

generic
   with package Types is new RFLX.Generic_Types (<>);
   with package TLV_Message is new RFLX.TLV.Generic_Message (Types, others => <>);
package RFLX.In_TLV.Generic_Contains with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_Contains);

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Length, Types.Bit_Index, Types.Bit_Length;

   function Null_Message_In_TLV_Message_Value (Ctx : TLV_Message.Context) return Boolean is
     (TLV_Message.Has_Buffer (Ctx)
      and then TLV_Message.Structural_Valid (Ctx, TLV_Message.F_Value)
      and then not TLV_Message.Present (Ctx, TLV_Message.F_Value));

end RFLX.In_TLV.Generic_Contains;

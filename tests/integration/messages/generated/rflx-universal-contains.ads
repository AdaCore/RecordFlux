pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.Universal.Message;
with RFLX.Universal.Option;
use RFLX.Universal.Option;

package RFLX.Universal.Contains with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   use type RFLX_Types.Index;

   use type RFLX_Types.Bit_Index;

   function Option_In_Message_Data (Ctx : RFLX.Universal.Message.Context) return Boolean is
     (RFLX.Universal.Message.Has_Buffer (Ctx)
      and then RFLX.Universal.Message.Present (Ctx, RFLX.Universal.Message.F_Data));

   use type RFLX.Universal.Message.Field_Cursors;

   procedure Switch_To_Data (Universal_Message_PDU_Context : in out RFLX.Universal.Message.Context; Universal_Option_SDU_Context : out RFLX.Universal.Option.Context) with
     Pre =>
       not Universal_Message_PDU_Context'Constrained
       and not Universal_Option_SDU_Context'Constrained
       and RFLX.Universal.Message.Has_Buffer (Universal_Message_PDU_Context)
       and RFLX.Universal.Message.Present (Universal_Message_PDU_Context, RFLX.Universal.Message.F_Data)
       and Option_In_Message_Data (Universal_Message_PDU_Context),
     Post =>
       not RFLX.Universal.Message.Has_Buffer (Universal_Message_PDU_Context)
       and RFLX.Universal.Option.Has_Buffer (Universal_Option_SDU_Context)
       and Universal_Message_PDU_Context.Buffer_First = Universal_Option_SDU_Context.Buffer_First
       and Universal_Message_PDU_Context.Buffer_Last = Universal_Option_SDU_Context.Buffer_Last
       and Universal_Option_SDU_Context.First = RFLX.Universal.Message.Field_First (Universal_Message_PDU_Context, RFLX.Universal.Message.F_Data)
       and Universal_Option_SDU_Context.Last = RFLX.Universal.Message.Field_Last (Universal_Message_PDU_Context, RFLX.Universal.Message.F_Data)
       and RFLX.Universal.Option.Initialized (Universal_Option_SDU_Context)
       and Universal_Message_PDU_Context.Buffer_First = Universal_Message_PDU_Context.Buffer_First'Old
       and Universal_Message_PDU_Context.Buffer_Last = Universal_Message_PDU_Context.Buffer_Last'Old
       and Universal_Message_PDU_Context.First = Universal_Message_PDU_Context.First'Old
       and RFLX.Universal.Message.Context_Cursors (Universal_Message_PDU_Context) = RFLX.Universal.Message.Context_Cursors (Universal_Message_PDU_Context)'Old;

   procedure Copy_Data (Universal_Message_PDU_Context : RFLX.Universal.Message.Context; Universal_Option_SDU_Context : in out RFLX.Universal.Option.Context) with
     Pre =>
       not Universal_Option_SDU_Context'Constrained
       and then RFLX.Universal.Option.Has_Buffer (Universal_Option_SDU_Context)
       and then RFLX.Universal.Message.Has_Buffer (Universal_Message_PDU_Context)
       and then RFLX.Universal.Message.Present (Universal_Message_PDU_Context, RFLX.Universal.Message.F_Data)
       and then Option_In_Message_Data (Universal_Message_PDU_Context)
       and then RFLX_Types.To_Last_Bit_Index (Universal_Option_SDU_Context.Buffer_Last) - RFLX_Types.To_First_Bit_Index (Universal_Option_SDU_Context.Buffer_First) + 1 >= RFLX.Universal.Message.Field_Size (Universal_Message_PDU_Context, RFLX.Universal.Message.F_Data)
       and then RFLX_Types.To_First_Bit_Index (Universal_Option_SDU_Context.Buffer_First) + RFLX.Universal.Message.Field_Size (Universal_Message_PDU_Context, RFLX.Universal.Message.F_Data) - 1 < RFLX_Types.Bit_Index'Last,
     Post =>
       RFLX.Universal.Message.Has_Buffer (Universal_Message_PDU_Context)
       and RFLX.Universal.Option.Has_Buffer (Universal_Option_SDU_Context)
       and RFLX.Universal.Option.Initialized (Universal_Option_SDU_Context)
       and Universal_Option_SDU_Context.Buffer_First = Universal_Option_SDU_Context.Buffer_First'Old
       and Universal_Option_SDU_Context.Buffer_Last = Universal_Option_SDU_Context.Buffer_Last'Old;

end RFLX.Universal.Contains;

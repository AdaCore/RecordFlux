pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Universal.Contains with
  SPARK_Mode
is

   procedure Switch_To_Data (Universal_Message_PDU_Context : in out RFLX.Universal.Message.Context; Universal_Option_SDU_Context : out RFLX.Universal.Option.Context) is
      First : constant RFLX_Types.Bit_Index := RFLX.Universal.Message.Field_First (Universal_Message_PDU_Context, RFLX.Universal.Message.F_Data);
      Last : constant RFLX_Types.Bit_Length := RFLX.Universal.Message.Field_Last (Universal_Message_PDU_Context, RFLX.Universal.Message.F_Data);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      RFLX.Universal.Message.Take_Buffer (Universal_Message_PDU_Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      RFLX.Universal.Option.Initialize (Universal_Option_SDU_Context, Buffer, First, Last, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Data;

   procedure Copy_Data (Universal_Message_PDU_Context : RFLX.Universal.Message.Context; Universal_Option_SDU_Context : in out RFLX.Universal.Option.Context) is
      First : constant RFLX_Types.Bit_Index := RFLX_Types.To_First_Bit_Index (Universal_Option_SDU_Context.Buffer_First);
      Size : constant RFLX_Types.Bit_Index := RFLX.Universal.Message.Field_Size (Universal_Message_PDU_Context, RFLX.Universal.Message.F_Data);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Universal_Option_SDU_Context"" is set by ""Take_Buffer"" but not used after the call");
      RFLX.Universal.Option.Take_Buffer (Universal_Option_SDU_Context, Buffer);
      pragma Warnings (On, """Universal_Option_SDU_Context"" is set by ""Take_Buffer"" but not used after the call");
      RFLX.Universal.Message.Get_Data (Universal_Message_PDU_Context, Buffer.all);
      RFLX.Universal.Option.Initialize (Universal_Option_SDU_Context, Buffer, First, First + Size - 1, First + Size - 1);
   end Copy_Data;

end RFLX.Universal.Contains;

pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.In_Ethernet.Contains with
  SPARK_Mode
is

   procedure Switch_To_Payload (Ethernet_Frame_PDU_Context : in out RFLX.Ethernet.Frame.Context; IPv4_Packet_SDU_Context : out RFLX.IPv4.Packet.Context) is
      First : constant RFLX_Types.Bit_Index := RFLX.Ethernet.Frame.Field_First (Ethernet_Frame_PDU_Context, RFLX.Ethernet.Frame.F_Payload);
      Last : constant RFLX_Types.Bit_Length := RFLX.Ethernet.Frame.Field_Last (Ethernet_Frame_PDU_Context, RFLX.Ethernet.Frame.F_Payload);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      RFLX.Ethernet.Frame.Take_Buffer (Ethernet_Frame_PDU_Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      RFLX.IPv4.Packet.Initialize (IPv4_Packet_SDU_Context, Buffer, First, Last, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Payload;

   procedure Copy_Payload (Ethernet_Frame_PDU_Context : RFLX.Ethernet.Frame.Context; IPv4_Packet_SDU_Context : in out RFLX.IPv4.Packet.Context) is
      First : constant RFLX_Types.Bit_Index := RFLX_Types.To_First_Bit_Index (IPv4_Packet_SDU_Context.Buffer_First);
      Size : constant RFLX_Types.Bit_Index := RFLX.Ethernet.Frame.Field_Size (Ethernet_Frame_PDU_Context, RFLX.Ethernet.Frame.F_Payload);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """IPv4_Packet_SDU_Context"" is set by ""Take_Buffer"" but not used after the call");
      RFLX.IPv4.Packet.Take_Buffer (IPv4_Packet_SDU_Context, Buffer);
      pragma Warnings (On, """IPv4_Packet_SDU_Context"" is set by ""Take_Buffer"" but not used after the call");
      RFLX.Ethernet.Frame.Get_Payload (Ethernet_Frame_PDU_Context, Buffer.all);
      RFLX.IPv4.Packet.Initialize (IPv4_Packet_SDU_Context, Buffer, First, First + Size - 1, First + Size - 1);
   end Copy_Payload;

end RFLX.In_Ethernet.Contains;

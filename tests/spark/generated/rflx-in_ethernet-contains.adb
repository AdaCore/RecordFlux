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
      RFLX.IPv4.Packet.Initialize (IPv4_Packet_SDU_Context, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Payload;

end RFLX.In_Ethernet.Contains;

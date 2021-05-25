pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.In_IPv4.Contains with
  SPARK_Mode
is

   procedure Switch_To_Payload (IPv4_Packet_PDU_Context : in out RFLX.IPv4.Packet.Context; UDP_Datagram_SDU_Context : out RFLX.UDP.Datagram.Context) is
      First : constant RFLX_Types.Bit_Index := RFLX.IPv4.Packet.Field_First (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload);
      Last : constant RFLX_Types.Bit_Length := RFLX.IPv4.Packet.Field_Last (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      RFLX.IPv4.Packet.Take_Buffer (IPv4_Packet_PDU_Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      RFLX.UDP.Datagram.Initialize (UDP_Datagram_SDU_Context, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Payload;

   procedure Switch_To_Payload (IPv4_Packet_PDU_Context : in out RFLX.IPv4.Packet.Context; ICMP_Message_SDU_Context : out RFLX.ICMP.Message.Context) is
      First : constant RFLX_Types.Bit_Index := RFLX.IPv4.Packet.Field_First (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload);
      Last : constant RFLX_Types.Bit_Length := RFLX.IPv4.Packet.Field_Last (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload);
      Buffer : RFLX_Types.Bytes_Ptr;
   begin
      RFLX.IPv4.Packet.Take_Buffer (IPv4_Packet_PDU_Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      RFLX.ICMP.Message.Initialize (ICMP_Message_SDU_Context, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Payload;

end RFLX.In_IPv4.Contains;

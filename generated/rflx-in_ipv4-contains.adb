package body RFLX.In_IPv4.Contains with
  SPARK_Mode
is

   procedure Switch_To_Payload (IPv4_Packet_Context : in out IPv4.Packet.Context; UDP_Datagram_Context : out UDP.Datagram.Context) is
      First : constant RFLX.Types.Bit_Index := IPv4.Packet.Field_First (IPv4_Packet_Context, IPv4.Packet.F_Payload);
      Last : constant RFLX.Types.Bit_Index := IPv4.Packet.Field_Last (IPv4_Packet_Context, IPv4.Packet.F_Payload);
      Buffer : RFLX.Types.Bytes_Ptr;
   begin
      IPv4.Packet.Take_Buffer (IPv4_Packet_Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      UDP.Datagram.Initialize (UDP_Datagram_Context, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Payload;

end RFLX.In_IPv4.Contains;

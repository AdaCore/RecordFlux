pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.In_IPv4.Generic_Contains with
  SPARK_Mode
is

   procedure Switch_To_Payload (IPv4_Packet_PDU_Context : in out IPv4_Packet.Context; UDP_Datagram_SDU_Context : out UDP_Datagram.Context) is
      First : constant Types.Bit_Index := IPv4_Packet.Field_First (IPv4_Packet_PDU_Context, IPv4_Packet.F_Payload);
      Last : constant Types.Bit_Index := IPv4_Packet.Field_Last (IPv4_Packet_PDU_Context, IPv4_Packet.F_Payload);
      Buffer : Types.Bytes_Ptr;
   begin
      IPv4_Packet.Take_Buffer (IPv4_Packet_PDU_Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      UDP_Datagram.Initialize (UDP_Datagram_SDU_Context, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Payload;

   procedure Switch_To_Payload (IPv4_Packet_PDU_Context : in out IPv4_Packet.Context; ICMP_Message_SDU_Context : out ICMP_Message.Context) is
      First : constant Types.Bit_Index := IPv4_Packet.Field_First (IPv4_Packet_PDU_Context, IPv4_Packet.F_Payload);
      Last : constant Types.Bit_Index := IPv4_Packet.Field_Last (IPv4_Packet_PDU_Context, IPv4_Packet.F_Payload);
      Buffer : Types.Bytes_Ptr;
   begin
      IPv4_Packet.Take_Buffer (IPv4_Packet_PDU_Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      ICMP_Message.Initialize (ICMP_Message_SDU_Context, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Payload;

end RFLX.In_IPv4.Generic_Contains;

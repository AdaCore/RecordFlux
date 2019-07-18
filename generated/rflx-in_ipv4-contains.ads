with RFLX.IPv4;
use RFLX.IPv4;
with RFLX.IPv4.Packet;
with RFLX.UDP.Datagram;

package RFLX.In_IPv4.Contains with
  SPARK_Mode
is

   function UDP_Datagram_In_IPv4_Packet_Payload (Context : IPv4.Packet.Context_Type) return Boolean is
     (IPv4.Packet.Has_Buffer (Context)
      and then IPv4.Packet.Present (Context, IPv4.Packet.F_Payload)
      and then IPv4.Packet.Valid (Context, IPv4.Packet.F_Protocol)
      and then IPv4.Packet.Get_Protocol (Context).Known
      and then IPv4.Packet.Get_Protocol (Context).Enum = PROTOCOL_UDP);

   procedure Switch (IPv4_Packet_Context : in out IPv4.Packet.Context_Type; UDP_Datagram_Context : out UDP.Datagram.Context_Type) with
     Pre =>
       not IPv4_Packet_Context'Constrained
          and then not UDP_Datagram_Context'Constrained
          and then IPv4.Packet.Has_Buffer (IPv4_Packet_Context)
          and then IPv4.Packet.Present (IPv4_Packet_Context, IPv4.Packet.F_Payload)
          and then IPv4.Packet.Valid (IPv4_Packet_Context, IPv4.Packet.F_Protocol)
          and then UDP_Datagram_In_IPv4_Packet_Payload (IPv4_Packet_Context);

end RFLX.In_IPv4.Contains;

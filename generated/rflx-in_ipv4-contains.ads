with RFLX.IPv4;
use RFLX.IPv4;
with RFLX.IPv4.Packet;
use type RFLX.IPv4.Packet.Field_Cursors;
with RFLX.UDP.Datagram;

package RFLX.In_IPv4.Contains with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Contains);

   function UDP_Datagram_In_IPv4_Packet_Payload (Ctx : IPv4.Packet.Context) return Boolean is
     (IPv4.Packet.Has_Buffer (Ctx)
      and then IPv4.Packet.Present (Ctx, IPv4.Packet.F_Payload)
      and then IPv4.Packet.Valid (Ctx, IPv4.Packet.F_Protocol)
      and then IPv4.Packet.Get_Protocol (Ctx).Known
      and then IPv4.Packet.Get_Protocol (Ctx).Enum = PROTOCOL_UDP);

   procedure Switch_To_Payload (IPv4_Packet_Context : in out IPv4.Packet.Context; UDP_Datagram_Context : out UDP.Datagram.Context) with
     Pre =>
       not IPv4_Packet_Context'Constrained
          and not UDP_Datagram_Context'Constrained
          and IPv4.Packet.Has_Buffer (IPv4_Packet_Context)
          and IPv4.Packet.Present (IPv4_Packet_Context, IPv4.Packet.F_Payload)
          and IPv4.Packet.Valid (IPv4_Packet_Context, IPv4.Packet.F_Protocol)
          and UDP_Datagram_In_IPv4_Packet_Payload (IPv4_Packet_Context),
     Post =>
       not IPv4.Packet.Has_Buffer (IPv4_Packet_Context)
          and UDP.Datagram.Has_Buffer (UDP_Datagram_Context)
          and IPv4_Packet_Context.Buffer_First = UDP_Datagram_Context.Buffer_First
          and IPv4_Packet_Context.Buffer_Last = UDP_Datagram_Context.Buffer_Last
          and UDP_Datagram_Context.First = IPv4.Packet.Field_First (IPv4_Packet_Context, IPv4.Packet.F_Payload)
          and UDP_Datagram_Context.Last = IPv4.Packet.Field_Last (IPv4_Packet_Context, IPv4.Packet.F_Payload)
          and UDP.Datagram.Initialized (UDP_Datagram_Context)
          and IPv4_Packet_Context.Buffer_First = IPv4_Packet_Context.Buffer_First'Old
          and IPv4_Packet_Context.Buffer_Last = IPv4_Packet_Context.Buffer_Last'Old
          and IPv4_Packet_Context.First = IPv4_Packet_Context.First'Old
          and IPv4.Packet.Cursors (IPv4_Packet_Context) = IPv4.Packet.Cursors (IPv4_Packet_Context)'Old;

end RFLX.In_IPv4.Contains;

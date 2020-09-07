pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
with RFLX.RFLX_Generic_Types;
with RFLX.IPv4;
use RFLX.IPv4;
with RFLX.IPv4.Generic_Packet;
with RFLX.UDP.Generic_Datagram;
with RFLX.ICMP.Generic_Message;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
   with package IPv4_Packet is new RFLX.IPv4.Generic_Packet (Types, others => <>);
   with package UDP_Datagram is new RFLX.UDP.Generic_Datagram (Types, others => <>);
   with package ICMP_Message is new RFLX.ICMP.Generic_Message (Types, others => <>);
package RFLX.In_IPv4.Generic_Contains with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   use type Types.Index, Types.Bit_Index;

   function UDP_Datagram_In_IPv4_Packet_Payload (Ctx : IPv4_Packet.Context) return Boolean is
     (IPv4_Packet.Has_Buffer (Ctx)
      and then IPv4_Packet.Present (Ctx, IPv4_Packet.F_Payload)
      and then IPv4_Packet.Valid (Ctx, IPv4_Packet.F_Protocol)
      and then IPv4_Packet.Get_Protocol (Ctx).Known
      and then IPv4_Packet.Get_Protocol (Ctx).Enum = PROTOCOL_UDP);

   use type IPv4_Packet.Field_Cursors;

   procedure Switch_To_Payload (IPv4_Packet_Context : in out IPv4_Packet.Context; UDP_Datagram_Context : out UDP_Datagram.Context) with
     Pre =>
       not IPv4_Packet_Context'Constrained
       and not UDP_Datagram_Context'Constrained
       and IPv4_Packet.Has_Buffer (IPv4_Packet_Context)
       and IPv4_Packet.Present (IPv4_Packet_Context, IPv4_Packet.F_Payload)
       and IPv4_Packet.Valid (IPv4_Packet_Context, IPv4_Packet.F_Protocol)
       and UDP_Datagram_In_IPv4_Packet_Payload (IPv4_Packet_Context),
     Post =>
       not IPv4_Packet.Has_Buffer (IPv4_Packet_Context)
       and UDP_Datagram.Has_Buffer (UDP_Datagram_Context)
       and IPv4_Packet_Context.Buffer_First = UDP_Datagram_Context.Buffer_First
       and IPv4_Packet_Context.Buffer_Last = UDP_Datagram_Context.Buffer_Last
       and UDP_Datagram_Context.First = IPv4_Packet.Field_First (IPv4_Packet_Context, IPv4_Packet.F_Payload)
       and UDP_Datagram_Context.Last = IPv4_Packet.Field_Last (IPv4_Packet_Context, IPv4_Packet.F_Payload)
       and UDP_Datagram.Initialized (UDP_Datagram_Context)
       and IPv4_Packet_Context.Buffer_First = IPv4_Packet_Context.Buffer_First'Old
       and IPv4_Packet_Context.Buffer_Last = IPv4_Packet_Context.Buffer_Last'Old
       and IPv4_Packet_Context.First = IPv4_Packet_Context.First'Old
       and IPv4_Packet.Context_Cursors (IPv4_Packet_Context) = IPv4_Packet.Context_Cursors (IPv4_Packet_Context)'Old;

   function ICMP_Message_In_IPv4_Packet_Payload (Ctx : IPv4_Packet.Context) return Boolean is
     (IPv4_Packet.Has_Buffer (Ctx)
      and then IPv4_Packet.Present (Ctx, IPv4_Packet.F_Payload)
      and then IPv4_Packet.Valid (Ctx, IPv4_Packet.F_Protocol)
      and then IPv4_Packet.Get_Protocol (Ctx).Known
      and then IPv4_Packet.Get_Protocol (Ctx).Enum = PROTOCOL_ICMP);

   procedure Switch_To_Payload (IPv4_Packet_Context : in out IPv4_Packet.Context; ICMP_Message_Context : out ICMP_Message.Context) with
     Pre =>
       not IPv4_Packet_Context'Constrained
       and not ICMP_Message_Context'Constrained
       and IPv4_Packet.Has_Buffer (IPv4_Packet_Context)
       and IPv4_Packet.Present (IPv4_Packet_Context, IPv4_Packet.F_Payload)
       and IPv4_Packet.Valid (IPv4_Packet_Context, IPv4_Packet.F_Protocol)
       and ICMP_Message_In_IPv4_Packet_Payload (IPv4_Packet_Context),
     Post =>
       not IPv4_Packet.Has_Buffer (IPv4_Packet_Context)
       and ICMP_Message.Has_Buffer (ICMP_Message_Context)
       and IPv4_Packet_Context.Buffer_First = ICMP_Message_Context.Buffer_First
       and IPv4_Packet_Context.Buffer_Last = ICMP_Message_Context.Buffer_Last
       and ICMP_Message_Context.First = IPv4_Packet.Field_First (IPv4_Packet_Context, IPv4_Packet.F_Payload)
       and ICMP_Message_Context.Last = IPv4_Packet.Field_Last (IPv4_Packet_Context, IPv4_Packet.F_Payload)
       and ICMP_Message.Initialized (ICMP_Message_Context)
       and IPv4_Packet_Context.Buffer_First = IPv4_Packet_Context.Buffer_First'Old
       and IPv4_Packet_Context.Buffer_Last = IPv4_Packet_Context.Buffer_Last'Old
       and IPv4_Packet_Context.First = IPv4_Packet_Context.First'Old
       and IPv4_Packet.Context_Cursors (IPv4_Packet_Context) = IPv4_Packet.Context_Cursors (IPv4_Packet_Context)'Old;

end RFLX.In_IPv4.Generic_Contains;

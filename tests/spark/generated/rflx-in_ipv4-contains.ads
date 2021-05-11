pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.IPv4;
use RFLX.IPv4;
with RFLX.IPv4.Packet;
with RFLX.UDP.Datagram;
with RFLX.ICMP.Message;

package RFLX.In_IPv4.Contains with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   use type RFLX_Types.Index, RFLX_Types.Bit_Index;

   function UDP_Datagram_In_IPv4_Packet_Payload (Ctx : IPv4.Packet.Context) return Boolean is
     (IPv4.Packet.Has_Buffer (Ctx)
      and then IPv4.Packet.Present (Ctx, IPv4.Packet.F_Payload)
      and then IPv4.Packet.Valid (Ctx, IPv4.Packet.F_Protocol)
      and then IPv4.Packet.Get_Protocol (Ctx).Known
      and then IPv4.Packet.Get_Protocol (Ctx).Enum = IPv4.PROTOCOL_UDP);

   use type IPv4.Packet.Field_Cursors;

   procedure Switch_To_Payload (IPv4_Packet_PDU_Context : in out IPv4.Packet.Context; UDP_Datagram_SDU_Context : out UDP.Datagram.Context) with
     Pre =>
       not IPv4_Packet_PDU_Context'Constrained
       and not UDP_Datagram_SDU_Context'Constrained
       and IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and IPv4.Packet.Present (IPv4_Packet_PDU_Context, IPv4.Packet.F_Payload)
       and IPv4.Packet.Valid (IPv4_Packet_PDU_Context, IPv4.Packet.F_Protocol)
       and UDP_Datagram_In_IPv4_Packet_Payload (IPv4_Packet_PDU_Context),
     Post =>
       not IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and UDP.Datagram.Has_Buffer (UDP_Datagram_SDU_Context)
       and IPv4_Packet_PDU_Context.Buffer_First = UDP_Datagram_SDU_Context.Buffer_First
       and IPv4_Packet_PDU_Context.Buffer_Last = UDP_Datagram_SDU_Context.Buffer_Last
       and UDP_Datagram_SDU_Context.First = IPv4.Packet.Field_First (IPv4_Packet_PDU_Context, IPv4.Packet.F_Payload)
       and UDP_Datagram_SDU_Context.Last = IPv4.Packet.Field_Last (IPv4_Packet_PDU_Context, IPv4.Packet.F_Payload)
       and UDP.Datagram.Initialized (UDP_Datagram_SDU_Context)
       and IPv4_Packet_PDU_Context.Buffer_First = IPv4_Packet_PDU_Context.Buffer_First'Old
       and IPv4_Packet_PDU_Context.Buffer_Last = IPv4_Packet_PDU_Context.Buffer_Last'Old
       and IPv4_Packet_PDU_Context.First = IPv4_Packet_PDU_Context.First'Old
       and IPv4.Packet.Context_Cursors (IPv4_Packet_PDU_Context) = IPv4.Packet.Context_Cursors (IPv4_Packet_PDU_Context)'Old;

   function ICMP_Message_In_IPv4_Packet_Payload (Ctx : IPv4.Packet.Context) return Boolean is
     (IPv4.Packet.Has_Buffer (Ctx)
      and then IPv4.Packet.Present (Ctx, IPv4.Packet.F_Payload)
      and then IPv4.Packet.Valid (Ctx, IPv4.Packet.F_Protocol)
      and then IPv4.Packet.Get_Protocol (Ctx).Known
      and then IPv4.Packet.Get_Protocol (Ctx).Enum = IPv4.PROTOCOL_ICMP);

   procedure Switch_To_Payload (IPv4_Packet_PDU_Context : in out IPv4.Packet.Context; ICMP_Message_SDU_Context : out ICMP.Message.Context) with
     Pre =>
       not IPv4_Packet_PDU_Context'Constrained
       and not ICMP_Message_SDU_Context'Constrained
       and IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and IPv4.Packet.Present (IPv4_Packet_PDU_Context, IPv4.Packet.F_Payload)
       and IPv4.Packet.Valid (IPv4_Packet_PDU_Context, IPv4.Packet.F_Protocol)
       and ICMP_Message_In_IPv4_Packet_Payload (IPv4_Packet_PDU_Context),
     Post =>
       not IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and ICMP.Message.Has_Buffer (ICMP_Message_SDU_Context)
       and IPv4_Packet_PDU_Context.Buffer_First = ICMP_Message_SDU_Context.Buffer_First
       and IPv4_Packet_PDU_Context.Buffer_Last = ICMP_Message_SDU_Context.Buffer_Last
       and ICMP_Message_SDU_Context.First = IPv4.Packet.Field_First (IPv4_Packet_PDU_Context, IPv4.Packet.F_Payload)
       and ICMP_Message_SDU_Context.Last = IPv4.Packet.Field_Last (IPv4_Packet_PDU_Context, IPv4.Packet.F_Payload)
       and ICMP.Message.Initialized (ICMP_Message_SDU_Context)
       and IPv4_Packet_PDU_Context.Buffer_First = IPv4_Packet_PDU_Context.Buffer_First'Old
       and IPv4_Packet_PDU_Context.Buffer_Last = IPv4_Packet_PDU_Context.Buffer_Last'Old
       and IPv4_Packet_PDU_Context.First = IPv4_Packet_PDU_Context.First'Old
       and IPv4.Packet.Context_Cursors (IPv4_Packet_PDU_Context) = IPv4.Packet.Context_Cursors (IPv4_Packet_PDU_Context)'Old;

end RFLX.In_IPv4.Contains;

pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
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
    (GNATprove, Always_Return)
is

   use type RFLX_Types.Index;

   use type RFLX_Types.Bit_Index;

   function UDP_Datagram_In_IPv4_Packet_Payload (Ctx : RFLX.IPv4.Packet.Context) return Boolean is
     (RFLX.IPv4.Packet.Has_Buffer (Ctx)
      and then RFLX.IPv4.Packet.Present (Ctx, RFLX.IPv4.Packet.F_Payload)
      and then RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Protocol)
      and then RFLX.IPv4.Packet.Get_Protocol (Ctx).Known
      and then RFLX.IPv4.Packet.Get_Protocol (Ctx).Enum = RFLX.IPv4.P_UDP);

   use type RFLX.IPv4.Packet.Field_Cursors;

   procedure Switch_To_Payload (IPv4_Packet_PDU_Context : in out RFLX.IPv4.Packet.Context; UDP_Datagram_SDU_Context : out RFLX.UDP.Datagram.Context) with
     Pre =>
       not IPv4_Packet_PDU_Context'Constrained
       and not UDP_Datagram_SDU_Context'Constrained
       and RFLX.IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and RFLX.IPv4.Packet.Present (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload)
       and RFLX.IPv4.Packet.Valid (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Protocol)
       and RFLX.In_IPv4.Contains.UDP_Datagram_In_IPv4_Packet_Payload (IPv4_Packet_PDU_Context),
     Post =>
       not RFLX.IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and RFLX.UDP.Datagram.Has_Buffer (UDP_Datagram_SDU_Context)
       and IPv4_Packet_PDU_Context.Buffer_First = UDP_Datagram_SDU_Context.Buffer_First
       and IPv4_Packet_PDU_Context.Buffer_Last = UDP_Datagram_SDU_Context.Buffer_Last
       and UDP_Datagram_SDU_Context.First = RFLX.IPv4.Packet.Field_First (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload)
       and UDP_Datagram_SDU_Context.Last = RFLX.IPv4.Packet.Field_Last (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload)
       and RFLX.UDP.Datagram.Initialized (UDP_Datagram_SDU_Context)
       and IPv4_Packet_PDU_Context.Buffer_First = IPv4_Packet_PDU_Context.Buffer_First'Old
       and IPv4_Packet_PDU_Context.Buffer_Last = IPv4_Packet_PDU_Context.Buffer_Last'Old
       and IPv4_Packet_PDU_Context.First = IPv4_Packet_PDU_Context.First'Old
       and RFLX.IPv4.Packet.Context_Cursors (IPv4_Packet_PDU_Context) = RFLX.IPv4.Packet.Context_Cursors (IPv4_Packet_PDU_Context)'Old;

   procedure Copy_Payload (IPv4_Packet_PDU_Context : RFLX.IPv4.Packet.Context; UDP_Datagram_SDU_Context : in out RFLX.UDP.Datagram.Context) with
     Pre =>
       not UDP_Datagram_SDU_Context'Constrained
       and then RFLX.UDP.Datagram.Has_Buffer (UDP_Datagram_SDU_Context)
       and then RFLX.IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and then RFLX.IPv4.Packet.Present (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Valid (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Protocol)
       and then RFLX.In_IPv4.Contains.UDP_Datagram_In_IPv4_Packet_Payload (IPv4_Packet_PDU_Context)
       and then RFLX_Types.To_Last_Bit_Index (UDP_Datagram_SDU_Context.Buffer_Last) - RFLX_Types.To_First_Bit_Index (UDP_Datagram_SDU_Context.Buffer_First) + 1 >= RFLX.IPv4.Packet.Field_Size (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload)
       and then RFLX_Types.To_First_Bit_Index (UDP_Datagram_SDU_Context.Buffer_First) + RFLX.IPv4.Packet.Field_Size (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload) - 1 < RFLX_Types.Bit_Index'Last,
     Post =>
       RFLX.IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and RFLX.UDP.Datagram.Has_Buffer (UDP_Datagram_SDU_Context)
       and RFLX.UDP.Datagram.Initialized (UDP_Datagram_SDU_Context)
       and UDP_Datagram_SDU_Context.Buffer_First = UDP_Datagram_SDU_Context.Buffer_First'Old
       and UDP_Datagram_SDU_Context.Buffer_Last = UDP_Datagram_SDU_Context.Buffer_Last'Old;

   function ICMP_Message_In_IPv4_Packet_Payload (Ctx : RFLX.IPv4.Packet.Context) return Boolean is
     (RFLX.IPv4.Packet.Has_Buffer (Ctx)
      and then RFLX.IPv4.Packet.Present (Ctx, RFLX.IPv4.Packet.F_Payload)
      and then RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Protocol)
      and then RFLX.IPv4.Packet.Get_Protocol (Ctx).Known
      and then RFLX.IPv4.Packet.Get_Protocol (Ctx).Enum = RFLX.IPv4.P_ICMP);

   procedure Switch_To_Payload (IPv4_Packet_PDU_Context : in out RFLX.IPv4.Packet.Context; ICMP_Message_SDU_Context : out RFLX.ICMP.Message.Context) with
     Pre =>
       not IPv4_Packet_PDU_Context'Constrained
       and not ICMP_Message_SDU_Context'Constrained
       and RFLX.IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and RFLX.IPv4.Packet.Present (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload)
       and RFLX.IPv4.Packet.Valid (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Protocol)
       and RFLX.In_IPv4.Contains.ICMP_Message_In_IPv4_Packet_Payload (IPv4_Packet_PDU_Context),
     Post =>
       not RFLX.IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and RFLX.ICMP.Message.Has_Buffer (ICMP_Message_SDU_Context)
       and IPv4_Packet_PDU_Context.Buffer_First = ICMP_Message_SDU_Context.Buffer_First
       and IPv4_Packet_PDU_Context.Buffer_Last = ICMP_Message_SDU_Context.Buffer_Last
       and ICMP_Message_SDU_Context.First = RFLX.IPv4.Packet.Field_First (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload)
       and ICMP_Message_SDU_Context.Last = RFLX.IPv4.Packet.Field_Last (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload)
       and RFLX.ICMP.Message.Initialized (ICMP_Message_SDU_Context)
       and IPv4_Packet_PDU_Context.Buffer_First = IPv4_Packet_PDU_Context.Buffer_First'Old
       and IPv4_Packet_PDU_Context.Buffer_Last = IPv4_Packet_PDU_Context.Buffer_Last'Old
       and IPv4_Packet_PDU_Context.First = IPv4_Packet_PDU_Context.First'Old
       and RFLX.IPv4.Packet.Context_Cursors (IPv4_Packet_PDU_Context) = RFLX.IPv4.Packet.Context_Cursors (IPv4_Packet_PDU_Context)'Old;

   procedure Copy_Payload (IPv4_Packet_PDU_Context : RFLX.IPv4.Packet.Context; ICMP_Message_SDU_Context : in out RFLX.ICMP.Message.Context) with
     Pre =>
       not ICMP_Message_SDU_Context'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (ICMP_Message_SDU_Context)
       and then RFLX.IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and then RFLX.IPv4.Packet.Present (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Valid (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Protocol)
       and then RFLX.In_IPv4.Contains.ICMP_Message_In_IPv4_Packet_Payload (IPv4_Packet_PDU_Context)
       and then RFLX_Types.To_Last_Bit_Index (ICMP_Message_SDU_Context.Buffer_Last) - RFLX_Types.To_First_Bit_Index (ICMP_Message_SDU_Context.Buffer_First) + 1 >= RFLX.IPv4.Packet.Field_Size (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload)
       and then RFLX_Types.To_First_Bit_Index (ICMP_Message_SDU_Context.Buffer_First) + RFLX.IPv4.Packet.Field_Size (IPv4_Packet_PDU_Context, RFLX.IPv4.Packet.F_Payload) - 1 < RFLX_Types.Bit_Index'Last,
     Post =>
       RFLX.IPv4.Packet.Has_Buffer (IPv4_Packet_PDU_Context)
       and RFLX.ICMP.Message.Has_Buffer (ICMP_Message_SDU_Context)
       and RFLX.ICMP.Message.Initialized (ICMP_Message_SDU_Context)
       and ICMP_Message_SDU_Context.Buffer_First = ICMP_Message_SDU_Context.Buffer_First'Old
       and ICMP_Message_SDU_Context.Buffer_Last = ICMP_Message_SDU_Context.Buffer_Last'Old;

end RFLX.In_IPv4.Contains;

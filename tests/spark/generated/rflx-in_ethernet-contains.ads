pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.Ethernet;
use RFLX.Ethernet;
with RFLX.Ethernet.Frame;
with RFLX.IPv4.Packet;

package RFLX.In_Ethernet.Contains with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   use type RFLX_Types.Index, RFLX_Types.Bit_Index;

   function IPv4_Packet_In_Ethernet_Frame_Payload (Ctx : RFLX.Ethernet.Frame.Context) return Boolean is
     (RFLX.Ethernet.Frame.Has_Buffer (Ctx)
      and then RFLX.Ethernet.Frame.Present (Ctx, RFLX.Ethernet.Frame.F_Payload)
      and then RFLX.Ethernet.Frame.Valid (Ctx, RFLX.Ethernet.Frame.F_Type_Length)
      and then RFLX.Ethernet.Frame.Get_Type_Length (Ctx) = 16#800#);

   use type RFLX.Ethernet.Frame.Field_Cursors;

   procedure Switch_To_Payload (Ethernet_Frame_PDU_Context : in out RFLX.Ethernet.Frame.Context; IPv4_Packet_SDU_Context : out RFLX.IPv4.Packet.Context) with
     Pre =>
       not Ethernet_Frame_PDU_Context'Constrained
       and not IPv4_Packet_SDU_Context'Constrained
       and RFLX.Ethernet.Frame.Has_Buffer (Ethernet_Frame_PDU_Context)
       and RFLX.Ethernet.Frame.Present (Ethernet_Frame_PDU_Context, RFLX.Ethernet.Frame.F_Payload)
       and RFLX.Ethernet.Frame.Valid (Ethernet_Frame_PDU_Context, RFLX.Ethernet.Frame.F_Type_Length)
       and IPv4_Packet_In_Ethernet_Frame_Payload (Ethernet_Frame_PDU_Context),
     Post =>
       not RFLX.Ethernet.Frame.Has_Buffer (Ethernet_Frame_PDU_Context)
       and RFLX.IPv4.Packet.Has_Buffer (IPv4_Packet_SDU_Context)
       and Ethernet_Frame_PDU_Context.Buffer_First = IPv4_Packet_SDU_Context.Buffer_First
       and Ethernet_Frame_PDU_Context.Buffer_Last = IPv4_Packet_SDU_Context.Buffer_Last
       and IPv4_Packet_SDU_Context.First = RFLX.Ethernet.Frame.Field_First (Ethernet_Frame_PDU_Context, RFLX.Ethernet.Frame.F_Payload)
       and IPv4_Packet_SDU_Context.Last = RFLX.Ethernet.Frame.Field_Last (Ethernet_Frame_PDU_Context, RFLX.Ethernet.Frame.F_Payload)
       and RFLX.IPv4.Packet.Initialized (IPv4_Packet_SDU_Context)
       and Ethernet_Frame_PDU_Context.Buffer_First = Ethernet_Frame_PDU_Context.Buffer_First'Old
       and Ethernet_Frame_PDU_Context.Buffer_Last = Ethernet_Frame_PDU_Context.Buffer_Last'Old
       and Ethernet_Frame_PDU_Context.First = Ethernet_Frame_PDU_Context.First'Old
       and RFLX.Ethernet.Frame.Context_Cursors (Ethernet_Frame_PDU_Context) = RFLX.Ethernet.Frame.Context_Cursors (Ethernet_Frame_PDU_Context)'Old;

end RFLX.In_Ethernet.Contains;

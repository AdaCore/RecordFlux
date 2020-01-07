with RFLX.Ethernet;
use RFLX.Ethernet;
with RFLX.Ethernet.Frame;
use type RFLX.Ethernet.Frame.Field_Cursors;
with RFLX.IPv4.Packet;

package RFLX.In_Ethernet.Contains with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Contains);

   function IPv4_Packet_In_Ethernet_Frame_Payload (Ctx : Ethernet.Frame.Context) return Boolean is
     (Ethernet.Frame.Has_Buffer (Ctx)
      and then Ethernet.Frame.Present (Ctx, Ethernet.Frame.F_Payload)
      and then Ethernet.Frame.Valid (Ctx, Ethernet.Frame.F_Type_Length)
      and then Ethernet.Frame.Get_Type_Length (Ctx) = 16#800#);

   procedure Switch_To_Payload (Ethernet_Frame_Context : in out Ethernet.Frame.Context; IPv4_Packet_Context : out IPv4.Packet.Context) with
     Pre =>
       not Ethernet_Frame_Context'Constrained
          and not IPv4_Packet_Context'Constrained
          and Ethernet.Frame.Has_Buffer (Ethernet_Frame_Context)
          and Ethernet.Frame.Present (Ethernet_Frame_Context, Ethernet.Frame.F_Payload)
          and Ethernet.Frame.Valid (Ethernet_Frame_Context, Ethernet.Frame.F_Type_Length)
          and IPv4_Packet_In_Ethernet_Frame_Payload (Ethernet_Frame_Context),
     Post =>
       not Ethernet.Frame.Has_Buffer (Ethernet_Frame_Context)
          and IPv4.Packet.Has_Buffer (IPv4_Packet_Context)
          and Ethernet_Frame_Context.Buffer_First = IPv4_Packet_Context.Buffer_First
          and Ethernet_Frame_Context.Buffer_Last = IPv4_Packet_Context.Buffer_Last
          and IPv4_Packet_Context.First = Ethernet.Frame.Field_First (Ethernet_Frame_Context, Ethernet.Frame.F_Payload)
          and IPv4_Packet_Context.Last = Ethernet.Frame.Field_Last (Ethernet_Frame_Context, Ethernet.Frame.F_Payload)
          and IPv4.Packet.Initialized (IPv4_Packet_Context)
          and Ethernet_Frame_Context.Buffer_First = Ethernet_Frame_Context.Buffer_First'Old
          and Ethernet_Frame_Context.Buffer_Last = Ethernet_Frame_Context.Buffer_Last'Old
          and Ethernet_Frame_Context.First = Ethernet_Frame_Context.First'Old
          and Ethernet.Frame.Cursors (Ethernet_Frame_Context) = Ethernet.Frame.Cursors (Ethernet_Frame_Context)'Old;

end RFLX.In_Ethernet.Contains;

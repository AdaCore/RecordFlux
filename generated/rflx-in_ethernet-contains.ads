with RFLX.Ethernet;
use RFLX.Ethernet;
with RFLX.Ethernet.Frame;
with RFLX.IPv4.Packet;

package RFLX.In_Ethernet.Contains with
  SPARK_Mode
is

   function IPv4_Packet_In_Ethernet_Frame_Payload (Ctx : Ethernet.Frame.Context) return Boolean is
     (Ethernet.Frame.Has_Buffer (Ctx)
      and then Ethernet.Frame.Present (Ctx, Ethernet.Frame.F_Payload)
      and then Ethernet.Frame.Valid (Ctx, Ethernet.Frame.F_Type_Length)
      and then Ethernet.Frame.Get_Type_Length (Ctx) = 16#800#);

   procedure Switch (Ethernet_Frame_Context : in out Ethernet.Frame.Context; IPv4_Packet_Context : out IPv4.Packet.Context) with
     Pre =>
       not Ethernet_Frame_Context'Constrained
          and then not IPv4_Packet_Context'Constrained
          and then Ethernet.Frame.Has_Buffer (Ethernet_Frame_Context)
          and then Ethernet.Frame.Present (Ethernet_Frame_Context, Ethernet.Frame.F_Payload)
          and then Ethernet.Frame.Valid (Ethernet_Frame_Context, Ethernet.Frame.F_Type_Length)
          and then IPv4_Packet_In_Ethernet_Frame_Payload (Ethernet_Frame_Context);

end RFLX.In_Ethernet.Contains;

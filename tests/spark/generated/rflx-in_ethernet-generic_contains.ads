pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
with RFLX.RFLX_Generic_Types;
with RFLX.Ethernet;
use RFLX.Ethernet;
with RFLX.Ethernet.Generic_Frame;
with RFLX.IPv4.Generic_Packet;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
   with package Ethernet_Frame is new RFLX.Ethernet.Generic_Frame (Types, others => <>);
   with package IPv4_Packet is new RFLX.IPv4.Generic_Packet (Types, others => <>);
package RFLX.In_Ethernet.Generic_Contains with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   use type Types.Index, Types.Bit_Index;

   function IPv4_Packet_In_Ethernet_Frame_Payload (Ctx : Ethernet_Frame.Context) return Boolean is
     (Ethernet_Frame.Has_Buffer (Ctx)
      and then Ethernet_Frame.Present (Ctx, Ethernet_Frame.F_Payload)
      and then Ethernet_Frame.Valid (Ctx, Ethernet_Frame.F_Type_Length)
      and then Ethernet_Frame.Get_Type_Length (Ctx) = 16#800#);

   use type Ethernet_Frame.Field_Cursors;

   procedure Switch_To_Payload (Ethernet_Frame_Context : in out Ethernet_Frame.Context; IPv4_Packet_Context : out IPv4_Packet.Context) with
     Pre =>
       not Ethernet_Frame_Context'Constrained
       and not IPv4_Packet_Context'Constrained
       and Ethernet_Frame.Has_Buffer (Ethernet_Frame_Context)
       and Ethernet_Frame.Present (Ethernet_Frame_Context, Ethernet_Frame.F_Payload)
       and Ethernet_Frame.Valid (Ethernet_Frame_Context, Ethernet_Frame.F_Type_Length)
       and IPv4_Packet_In_Ethernet_Frame_Payload (Ethernet_Frame_Context),
     Post =>
       not Ethernet_Frame.Has_Buffer (Ethernet_Frame_Context)
       and IPv4_Packet.Has_Buffer (IPv4_Packet_Context)
       and Ethernet_Frame_Context.Buffer_First = IPv4_Packet_Context.Buffer_First
       and Ethernet_Frame_Context.Buffer_Last = IPv4_Packet_Context.Buffer_Last
       and IPv4_Packet_Context.First = Ethernet_Frame.Field_First (Ethernet_Frame_Context, Ethernet_Frame.F_Payload)
       and IPv4_Packet_Context.Last = Ethernet_Frame.Field_Last (Ethernet_Frame_Context, Ethernet_Frame.F_Payload)
       and IPv4_Packet.Initialized (IPv4_Packet_Context)
       and Ethernet_Frame_Context.Buffer_First = Ethernet_Frame_Context.Buffer_First'Old
       and Ethernet_Frame_Context.Buffer_Last = Ethernet_Frame_Context.Buffer_Last'Old
       and Ethernet_Frame_Context.First = Ethernet_Frame_Context.First'Old
       and Ethernet_Frame.Context_Cursors (Ethernet_Frame_Context) = Ethernet_Frame.Context_Cursors (Ethernet_Frame_Context)'Old;

end RFLX.In_Ethernet.Generic_Contains;

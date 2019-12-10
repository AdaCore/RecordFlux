package body RFLX.In_Ethernet.Contains with
  SPARK_Mode
is

   procedure Switch (Ethernet_Frame_Context : in out Ethernet.Frame.Context; IPv4_Packet_Context : out IPv4.Packet.Context) is
      First, Last : RFLX.Types.Bit_Index;
      Buffer : RFLX.Types.Bytes_Ptr;
   begin
      Ethernet.Frame.Field_Range (Ethernet_Frame_Context, Ethernet.Frame.F_Payload, First, Last);
      Ethernet.Frame.Take_Buffer (Ethernet_Frame_Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      IPv4.Packet.Initialize (IPv4_Packet_Context, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch;

end RFLX.In_Ethernet.Contains;

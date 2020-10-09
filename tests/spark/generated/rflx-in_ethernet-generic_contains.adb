pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package body RFLX.In_Ethernet.Generic_Contains with
  SPARK_Mode
is

   procedure Switch_To_Payload (Ethernet_Frame_Context : in out Ethernet_Frame.Context; IPv4_Packet_Context : out IPv4_Packet.Context) is
      First : constant Types.Bit_Index := Ethernet_Frame.Field_First (Ethernet_Frame_Context, Ethernet_Frame.F_Payload);
      Last : constant Types.Bit_Index := Ethernet_Frame.Field_Last (Ethernet_Frame_Context, Ethernet_Frame.F_Payload);
      Buffer : Types.Bytes_Ptr;
   begin
      Ethernet_Frame.Take_Buffer (Ethernet_Frame_Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      IPv4_Packet.Initialize (IPv4_Packet_Context, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Payload;

end RFLX.In_Ethernet.Generic_Contains;

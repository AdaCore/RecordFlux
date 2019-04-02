package body In_Ethernet.Contains
  with SPARK_Mode
is

   function IPv4_Packet_In_Ethernet_Frame_Payload (Buffer : Types.Bytes) return Boolean is
      EtherType : UINT16 := Ethernet.Frame.Get_EtherType (Buffer);
   begin
      if EtherType = 2048 then
         pragma Assume (IPv4.Packet.Is_Contained (Buffer (Ethernet.Frame.Get_Payload_First (Buffer) .. Ethernet.Frame.Get_Payload_Last (Buffer))));
         return True;
      end if;
      return False;
   end IPv4_Packet_In_Ethernet_Frame_Payload;

end In_Ethernet.Contains;

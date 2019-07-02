package body In_Ethernet.Contains
  with SPARK_Mode
is

   function IPv4_Packet_In_Ethernet_Frame_Payload (Buffer : Types.Bytes) return Boolean is
      Type_Length : Type_Length_Type := Ethernet.Frame.Get_Type_Length (Buffer);
   begin
      if Type_Length = 2048 then
         pragma Assume (IPv4.Packet.Is_Contained (Buffer (Ethernet.Frame.Get_Payload_First (Buffer) .. Ethernet.Frame.Get_Payload_Last (Buffer))));
         return True;
      end if;
      return False;
   end IPv4_Packet_In_Ethernet_Frame_Payload;

end In_Ethernet.Contains;

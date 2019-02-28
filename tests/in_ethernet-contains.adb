package body In_Ethernet.Contains
  with SPARK_Mode
is

   function IPv4_In_Ethernet (Buffer : Types.Bytes) return Boolean is
   begin
      if Ethernet.Frame.Get_EtherType (Buffer) = 2048 then
         pragma Assume (IPv4.Packet.Is_Contained (Buffer (Ethernet.Frame.Get_Payload_First (Buffer) .. Ethernet.Frame.Get_Payload_Last (Buffer))));
         return True;
      end if;
      return False;
   end IPv4_In_Ethernet;

end In_Ethernet.Contains;

package body In_Ethernet is

   function Contains_IPv4_In_Ethernet (Buffer : Bytes) return Boolean is
   begin
      if Ethernet.Frame.EtherType (Buffer) = 2048 then
         pragma Assume (IPv4.Packet.Is_Contained (Buffer (Ethernet.Frame.Payload_First (Buffer) .. Ethernet.Frame.Payload_Last (Buffer))));
         return True;
      end if;
      return False;
   end Contains_IPv4_In_Ethernet;

end In_Ethernet;

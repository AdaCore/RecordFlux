package body In_IPv4.Contains is

   function UDP_In_IPv4 (Buffer : Bytes) return Boolean is
   begin
      if IPv4.Packet.Protocol (Buffer) = 17 then
         pragma Assume (UDP.Datagram.Is_Contained (Buffer (IPv4.Packet.Payload_First (Buffer) .. IPv4.Packet.Payload_Last (Buffer))));
         return True;
      end if;
      return False;
   end UDP_In_IPv4;

end In_IPv4.Contains;

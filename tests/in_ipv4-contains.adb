package body In_IPv4.Contains with
  SPARK_Mode
is

   function UDP_Datagram_In_IPv4_Packet_Payload (Buffer : Types.Bytes) return Boolean is
      Protocol : Protocol_Type := IPv4.Packet.Get_Protocol (Buffer);
   begin
      if (Protocol.Known and then Protocol.Enum = PROTOCOL_UDP) then
         pragma Assume (UDP.Datagram.Is_Contained (Buffer (IPv4.Packet.Get_Payload_First (Buffer) .. IPv4.Packet.Get_Payload_Last (Buffer))));
         return True;
      end if;
      return False;
   end UDP_Datagram_In_IPv4_Packet_Payload;

end In_IPv4.Contains;

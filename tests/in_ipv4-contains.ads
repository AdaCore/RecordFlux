with IPv4; use IPv4;
with IPv4.Packet;
with UDP.Datagram;

package In_IPv4.Contains
  with SPARK_Mode
is

   function UDP_In_IPv4 (Buffer : Bytes) return Boolean
     with
       Pre => (IPv4.Packet.Is_Contained (Buffer) and then IPv4.Packet.Is_Valid (Buffer)),
       Post => (if UDP_In_IPv4'Result then UDP.Datagram.Is_Contained (Buffer (IPv4.Packet.Get_Payload_First (Buffer) .. IPv4.Packet.Get_Payload_Last (Buffer))));

end In_IPv4.Contains;

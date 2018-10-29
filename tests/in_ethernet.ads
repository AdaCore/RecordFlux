with Types; use Types;
with Ethernet; use Ethernet;
with Ethernet.Frame;
with IPv4.Packet;

package In_Ethernet
  with SPARK_Mode
is

   function Contains_IPv4_In_Ethernet (Buffer : Bytes) return Boolean
     with
       Pre => (Ethernet.Frame.Is_Contained (Buffer) and then Ethernet.Frame.Is_Valid (Buffer)),
       Post => (if Contains_IPv4_In_Ethernet'Result then IPv4.Packet.Is_Contained (Buffer (Ethernet.Frame.Payload_First (Buffer) .. Ethernet.Frame.Payload_Last (Buffer))));

end In_Ethernet;

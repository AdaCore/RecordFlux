with Ethernet; use Ethernet;
with Ethernet.Frame;
with IPv4.Packet;

package In_Ethernet.Contains
  with SPARK_Mode
is

   function IPv4_In_Ethernet (Buffer : Types.Bytes) return Boolean
     with
       Pre => (Ethernet.Frame.Is_Contained (Buffer) and then Ethernet.Frame.Is_Valid (Buffer)),
       Post => (if IPv4_In_Ethernet'Result then IPv4.Packet.Is_Contained (Buffer (Ethernet.Frame.Get_Payload_First (Buffer) .. Ethernet.Frame.Get_Payload_Last (Buffer))));

end In_Ethernet.Contains;

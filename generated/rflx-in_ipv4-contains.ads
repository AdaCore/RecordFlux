pragma SPARK_Mode;
with RFLX.In_IPv4.Generic_Contains;
with RFLX.Types;
with RFLX.IPv4.Packet;
with RFLX.UDP.Datagram;

package RFLX.In_IPv4.Contains is new RFLX.In_IPv4.Generic_Contains (RFLX.Types, RFLX.IPv4.Packet, RFLX.UDP.Datagram);

pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.In_IPv4.Generic_Contains;
with RFLX.IPv4.Packet;
with RFLX.UDP.Datagram;

package RFLX.In_IPv4.Contains is new RFLX.In_IPv4.Generic_Contains (RFLX.RFLX_Types, RFLX.IPv4.Packet, RFLX.UDP.Datagram);

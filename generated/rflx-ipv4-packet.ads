pragma SPARK_Mode;
with RFLX.IPv4.Generic_Packet;
with RFLX.Types;
with RFLX.IPv4.Options;

package RFLX.IPv4.Packet is new RFLX.IPv4.Generic_Packet (RFLX.Types, Options);

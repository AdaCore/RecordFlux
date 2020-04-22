pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma SPARK_Mode;
with RFLX.IPv4.Generic_Packet;
with RFLX.RFLX_Types;
with RFLX.IPv4.Options;

package RFLX.IPv4.Packet is new RFLX.IPv4.Generic_Packet (RFLX.RFLX_Types, RFLX.IPv4.Options);

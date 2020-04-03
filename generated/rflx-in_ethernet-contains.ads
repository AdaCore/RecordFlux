pragma SPARK_Mode;
with RFLX.Types;
with RFLX.In_Ethernet.Generic_Contains;
with RFLX.Ethernet.Frame;
with RFLX.IPv4.Packet;

package RFLX.In_Ethernet.Contains is new RFLX.In_Ethernet.Generic_Contains (RFLX.Types, RFLX.Ethernet.Frame, RFLX.IPv4.Packet);

with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.UDP.Datagram;
with RFLX.IPv4.Packet;
with RFLX.Ethernet.Frame;
with RFLX.In_Ethernet.Contains;
with RFLX.In_IPv4.Contains;

package body RFLX.In_IPv4.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("In_IPv4");
   end Name;

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");
   pragma Warnings (Off, "unused assignment to ""Ethernet_Frame_Context""");
   pragma Warnings (Off, "unused assignment to ""IPv4_Packet_Context""");

   procedure Test_UDP_In_IPv4 (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer               : Types.Bytes_Ptr := Read_File_Ptr ("tests/ipv4_udp.raw");
      IPv4_Packet_Context  : IPv4.Packet.Context_Type := IPv4.Packet.Create;
      UDP_Datagram_Context : UDP.Datagram.Context_Type := UDP.Datagram.Create;
      Valid                : Boolean;
   begin
      IPv4.Packet.Initialize (IPv4_Packet_Context, Buffer);
      IPv4.Packet.Verify_Message (IPv4_Packet_Context);
      Valid := IPv4.Packet.Structural_Valid_Message (IPv4_Packet_Context);
      Assert (Valid, "Structural invalid IPv4 packet");
      if Valid then
         Valid := In_IPv4.Contains.UDP_Datagram_In_IPv4_Packet_Payload (IPv4_Packet_Context);
         Assert (Valid, "Ethernet frame contains no UDP datagram");
         if Valid then
            In_IPv4.Contains.Switch (IPv4_Packet_Context, UDP_Datagram_Context);
            UDP.Datagram.Verify_Message (UDP_Datagram_Context);
            Valid := UDP.Datagram.Structural_Valid_Message (UDP_Datagram_Context);
            Assert (Valid, "Structural invalid UDP datagram");
         end if;
      end if;
   end Test_UDP_In_IPv4;

   procedure Test_UDP_In_IPv4_In_Ethernet (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer                 : Types.Bytes_Ptr := Read_File_Ptr ("tests/ethernet_ipv4_udp.raw");
      Ethernet_Frame_Context : Ethernet.Frame.Context_Type := Ethernet.Frame.Create;
      IPv4_Packet_Context    : IPv4.Packet.Context_Type := IPv4.Packet.Create;
      UDP_Datagram_Context   : UDP.Datagram.Context_Type := UDP.Datagram.Create;
      Valid                  : Boolean;
   begin
      Ethernet.Frame.Initialize (Ethernet_Frame_Context, Buffer);
      Ethernet.Frame.Verify_Message (Ethernet_Frame_Context);
      Valid := Ethernet.Frame.Structural_Valid_Message (Ethernet_Frame_Context);
      Assert (Valid, "Structural invalid Ethernet frame");
      if Valid then
         Valid := In_Ethernet.Contains.IPv4_Packet_In_Ethernet_Frame_Payload (Ethernet_Frame_Context);
         Assert (Valid, "Ethernet frame contains no IPv4 packet");
         if Valid then
            In_Ethernet.Contains.Switch (Ethernet_Frame_Context, IPv4_Packet_Context);
            IPv4.Packet.Verify_Message (IPv4_Packet_Context);
            Valid := IPv4.Packet.Structural_Valid_Message (IPv4_Packet_Context);
            Assert (Valid, "Structural invalid IPv4 packet");
            if Valid then
               Valid := In_IPv4.Contains.UDP_Datagram_In_IPv4_Packet_Payload (IPv4_Packet_Context);
               Assert (Valid, "IPv4 packet contains no UDP datagram");
               if Valid then
                  In_IPv4.Contains.Switch (IPv4_Packet_Context, UDP_Datagram_Context);
                  UDP.Datagram.Verify_Message (UDP_Datagram_Context);
                  Valid := UDP.Datagram.Structural_Valid_Message (UDP_Datagram_Context);
                  Assert (Valid, "Structural invalid UDP datagram");
               end if;
            end if;
         end if;
      end if;
   end Test_UDP_In_IPv4_In_Ethernet;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_UDP_In_IPv4'Access, "UDP in IPv4");
      Register_Routine (T, Test_UDP_In_IPv4_In_Ethernet'Access, "UDP in IPv4 in Ethernet");
   end Register_Tests;

end RFLX.In_IPv4.Tests;

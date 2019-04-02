with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with IPv4.Packet;
with Ethernet.Frame;
with In_Ethernet.Contains;

package body In_Ethernet.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("In_Ethernet");
   end Name;

   procedure Test_IPv4_In_Ethernet (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer          : Types.Bytes := Read_File ("tests/ethernet_ipv4_udp.raw");
      Valid           : Boolean;
      First           : Types.Index_Type;
      Last            : Types.Index_Type;
   begin
      Ethernet.Frame.Label (Buffer);
      Valid := Ethernet.Frame.Is_Valid (Buffer);
      Assert (Valid, "Invalid Ethernet frame");
      if Valid then
         Ethernet.Frame.Get_Payload (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (15), "Invalid Ethernet Payload'First");
         Assert (Last'Image, Types.Index_Type'Image (60), "Invalid Ethernet Payload'Last");
         Valid := In_Ethernet.Contains.IPv4_Packet_In_Ethernet_Frame_Payload (Buffer);
         Assert (Valid, "Ethernet frame contains no IPv4 packet");
         if Valid then
            Valid := IPv4.Packet.Is_Valid (Buffer (First .. Last));
            Assert (Valid, "Invalid IPv4 packet");
            if Valid then
               IPv4.Packet.Get_Payload (Buffer (First .. Last), First, Last);
               Assert (First'Image, Types.Index_Type'Image (35), "Invalid IPv4 Payload'First");
               Assert (Last'Image, Types.Index_Type'Image (60), "Invalid IPv4 Payload'Last");
            end if;
         end if;
      end if;
   end Test_IPv4_In_Ethernet;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_IPv4_In_Ethernet'Access, "IPv4 in Ethernet");
   end Register_Tests;

end In_Ethernet.Tests;

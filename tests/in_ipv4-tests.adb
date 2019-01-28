with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with UDP.Datagram;
with IPv4.Packet;
with Ethernet.Frame;
with In_Ethernet.Contains;
with In_IPv4.Contains;

package body In_IPv4.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("In_IPv4");
   end Name;

   procedure Test_UDP_In_IPv4 (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer : Types.Bytes := Read_File ("tests/ipv4_udp.raw");
      Valid  : Boolean;
      First  : Natural;
      Last   : Natural;
   begin
      IPv4.Packet.Initialize (Buffer);
      Valid := IPv4.Packet.Is_Valid (Buffer);
      Assert (Valid, "Invalid IPv4 packet");
      if Valid then
         IPv4.Packet.Get_Payload (Buffer, First, Last);
         Assert (First'Image, Natural'Image (21), "Invalid IPv4 Payload'First");
         Assert (Last'Image, Natural'Image (44), "Invalid IPv4 Payload'Last");
         Valid := In_IPv4.Contains.UDP_In_IPv4 (Buffer);
         Assert (Valid, "IPv4 packet contains no UDP datagram");
         if Valid then
            Valid := UDP.Datagram.Is_Valid (Buffer (First .. Last));
            Assert (Valid, "Invalid UDP datagram");
            if Valid then
               UDP.Datagram.Get_Payload (Buffer (First .. Last), First, Last);
               Assert (First'Image, Natural'Image (29), "Invalid UDP Payload'First");
               Assert (Last'Image, Natural'Image (44), "Invalid UDP Payload'Last");
            end if;
         end if;
      end if;
   end Test_UDP_In_IPv4;

   procedure Test_UDP_In_IPv4_In_Ethernet (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer     : Types.Bytes := Read_File ("tests/ethernet_ipv4_udp.raw");
      Valid      : Boolean;
      IPv4_First : Natural;
      IPv4_Last  : Natural;
      UDP_First  : Natural;
      UDP_Last   : Natural;
      First      : Natural;
      Last       : Natural;
   begin
      Ethernet.Frame.Initialize (Buffer);
      Valid := Ethernet.Frame.Is_Valid (Buffer);
      Assert (Valid, "Invalid Ethernet frame");
      if Valid then
         Ethernet.Frame.Get_Payload (Buffer, IPv4_First, IPv4_Last);
         Assert (IPv4_First'Image, Natural'Image (15), "Invalid Ethernet Payload'First");
         Assert (IPv4_Last'Image, Natural'Image (60), "Invalid Ethernet Payload'Last");
         Valid := In_Ethernet.Contains.IPv4_In_Ethernet (Buffer);
         Assert (Valid, "Ethernet frame contains no IPv4 packet");
         if Valid then
            Valid := IPv4.Packet.Is_Valid (Buffer (IPv4_First .. IPv4_Last));
            Assert (Valid, "Invalid IPv4 packet");
            if Valid then
               IPv4.Packet.Get_Payload (Buffer (IPv4_First .. IPv4_Last), UDP_First, UDP_Last);
               Assert (UDP_First'Image, Natural'Image (35), "Invalid IPv4 Payload'First");
               Assert (UDP_Last'Image, Natural'Image (60), "Invalid IPv4 Payload'Last");
               Valid := In_IPv4.Contains.UDP_In_IPv4 (Buffer (IPv4_First .. IPv4_Last));
               Assert (Valid, "IPv4 packet contains no UDP datagram");
               if Valid then
                  Valid := UDP.Datagram.Is_Valid (Buffer (UDP_First .. UDP_Last));
                  Assert (Valid, "Invalid UDP datagram");
                  if Valid then
                     UDP.Datagram.Get_Payload (Buffer (UDP_First .. UDP_Last), First, Last);
                     Assert (First'Image, Natural'Image (43), "Invalid UDP Payload'First");
                     Assert (Last'Image, Natural'Image (60), "Invalid UDP Payload'Last");
                  end if;
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

end In_IPv4.Tests;

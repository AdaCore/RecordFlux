with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with IPv4.Packet;

package body IPv4.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("IPv4");
   end Name;

   procedure Test_IPv4 (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer          : Bytes := Read_File ("tests/ipv4_udp.raw");
      Valid           : Boolean;
      Version         : IPv4.Version_Type;
      IHL             : IPv4.IHL_Type;
      DSCP            : IPv4.DCSP_Type;
      ECN             : IPv4.ECN_Type;
      Total_Length    : IPv4.Total_Length_Type;
      Identification  : IPv4.Identification_Type;
      Flag_R          : IPv4.Flag_Type;
      Flag_DF         : IPv4.Flag_Type;
      Flag_MF         : IPv4.Flag_Type;
      Fragment_Offset : IPv4.Fragment_Offset_Type;
      TTL             : IPv4.TTL_Type;
      Protocol        : IPv4.Protocol_Type;
      Header_Checksum : IPv4.Header_Checksum_Type;
      Source          : IPv4.Address_Type;
      Destination     : IPv4.Address_Type;
      First           : Natural;
      Last            : Natural;
   begin
      IPv4.Packet.Initialize (Buffer);
      Valid := IPv4.Packet.Valid_Version (Buffer);
      Assert (Valid, "Invalid Version");
      if Valid then
         Version := IPv4.Packet.Version (Buffer);
         Assert (Version'Image, IPv4.Version_Type'Image (4), "Invalid Version");
         Valid := IPv4.Packet.Valid_IHL (Buffer);
         Assert (Valid, "Invalid IHL");
         if Valid then
            IHL := IPv4.Packet.IHL (Buffer);
            Assert (IHL'Image, IPv4.IHL_Type'Image (5), "Invalid IHL");
            Valid := IPv4.Packet.Valid_DSCP (Buffer);
            Assert (Valid, "Invalid DSCP");
            if Valid then
               DSCP := IPv4.Packet.DSCP (Buffer);
               Assert (DSCP'Image, IPv4.DCSP_Type'Image (0), "Invalid DSCP");
               Valid := IPv4.Packet.Valid_ECN (Buffer);
               Assert (Valid, "Invalid ECN");
               if Valid then
                  ECN := IPv4.Packet.ECN (Buffer);
                  Assert (ECN'Image, IPv4.ECN_Type'Image (0), "Invalid ECN");
                  Valid := IPv4.Packet.Valid_Total_Length (Buffer);
                  Assert (Valid, "Invalid Total_Length");
                  if Valid then
                     Total_Length := IPv4.Packet.Total_Length (Buffer);
                     Assert (Total_Length'Image, IPv4.Total_Length_Type'Image (44), "Invalid Total_Length");
                     Valid := IPv4.Packet.Valid_Identification (Buffer);
                     Assert (Valid, "Invalid Identification");
                     if Valid then
                        Identification := IPv4.Packet.Identification (Buffer);
                        Assert (Identification'Image, IPv4.Identification_Type'Image (1), "Invalid Identification");
                        Valid := IPv4.Packet.Valid_Flag_R (Buffer);
                        Assert (Valid, "Invalid Flag_R");
                        if Valid then
                           Flag_R := IPv4.Packet.Flag_R (Buffer);
                           Assert (Flag_R'Image, IPv4.Flag_Type'Image (0), "Invalid Flag_R");
                           Valid := IPv4.Packet.Valid_Flag_DF (Buffer);
                           Assert (Valid, "Invalid Flag_DF");
                           if Valid then
                              Flag_DF := IPv4.Packet.Flag_DF (Buffer);
                              Assert (Flag_DF'Image, IPv4.Flag_Type'Image (0), "Invalid Flag_DF");
                              Valid := IPv4.Packet.Valid_Flag_MF (Buffer);
                              Assert (Valid, "Invalid Flag_MF");
                              if Valid then
                                 Flag_MF := IPv4.Packet.Flag_MF (Buffer);
                                 Assert (Flag_MF'Image, IPv4.Flag_Type'Image (0), "Invalid Flag_MF");
                                 Valid := IPv4.Packet.Valid_Fragment_Offset (Buffer);
                                 Assert (Valid, "Invalid Fragment_Offset");
                                 if Valid then
                                    Fragment_Offset := IPv4.Packet.Fragment_Offset (Buffer);
                                    Assert (Fragment_Offset'Image, IPv4.Fragment_Offset_Type'Image (0), "Invalid Fragment_Offset");
                                    Valid := IPv4.Packet.Valid_TTL (Buffer);
                                    Assert (Valid, "Invalid TTL");
                                    if Valid then
                                       TTL := IPv4.Packet.TTL (Buffer);
                                       Assert (TTL'Image, IPv4.TTL_Type'Image (64), "Invalid TTL");
                                       Valid := IPv4.Packet.Valid_Protocol (Buffer);
                                       Assert (Valid, "Invalid Protocol");
                                       if Valid then
                                          Protocol := IPv4.Packet.Protocol (Buffer);
                                          Assert (Protocol'Image, IPv4.Protocol_Type'Image (16#11#), "Invalid Protocol");
                                          Valid := IPv4.Packet.Valid_Header_Checksum (Buffer);
                                          Assert (Valid, "Invalid Header_Checksum");
                                          if Valid then
                                             Header_Checksum := IPv4.Packet.Header_Checksum (Buffer);
                                             Assert (Header_Checksum'Image, IPv4.Header_Checksum_Type'Image (16#7CBE#), "Invalid Header_Checksum");
                                             Valid := IPv4.Packet.Valid_Source (Buffer);
                                             Assert (Valid, "Invalid Source");
                                             if Valid then
                                                Source := IPv4.Packet.Source (Buffer);
                                                Assert (Source'Image, IPv4.Address_Type'Image (16#7f000001#), "Invalid Source");
                                                Valid := IPv4.Packet.Valid_Destination (Buffer);
                                                Assert (Valid, "Invalid Destination");
                                                if Valid then
                                                   Destination := IPv4.Packet.Destination (Buffer);
                                                   Assert (Destination'Image, IPv4.Address_Type'Image (16#7f000001#), "Invalid Destination");
                                                   Valid := IPv4.Packet.Valid_Payload (Buffer);
                                                   Assert (Valid, "Invalid Payload");
                                                   if Valid then
                                                      IPv4.Packet.Payload (Buffer, First, Last);
                                                      Assert (First'Image, Natural'Image (21), "Invalid Payload'First");
                                                      Assert (Last'Image, Natural'Image (44), "Invalid Payload'Last");
                                                   end if;
                                                end if;
                                             end if;
                                          end if;
                                       end if;
                                    end if;
                                 end if;
                              end if;
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
      Valid := IPv4.Packet.Is_Valid (Buffer);
      Assert (Valid, "Invalid packet");
   end Test_IPv4;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_IPv4'Access, "IPv4");
   end Register_Tests;

end IPv4.Tests;

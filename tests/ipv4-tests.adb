with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with IPv4.Packet;
with IPv4.Option;
with IPv4.Options;

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
      Buffer          : Types.Bytes := Read_File ("tests/ipv4_udp.raw");
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
         Version := IPv4.Packet.Get_Version (Buffer);
         Assert (Version'Image, IPv4.Version_Type'Image (4), "Invalid Version");
         Valid := IPv4.Packet.Valid_IHL (Buffer);
         Assert (Valid, "Invalid IHL");
         if Valid then
            IHL := IPv4.Packet.Get_IHL (Buffer);
            Assert (IHL'Image, IPv4.IHL_Type'Image (5), "Invalid IHL");
            Valid := IPv4.Packet.Valid_DSCP (Buffer);
            Assert (Valid, "Invalid DSCP");
            if Valid then
               DSCP := IPv4.Packet.Get_DSCP (Buffer);
               Assert (DSCP'Image, IPv4.DCSP_Type'Image (0), "Invalid DSCP");
               Valid := IPv4.Packet.Valid_ECN (Buffer);
               Assert (Valid, "Invalid ECN");
               if Valid then
                  ECN := IPv4.Packet.Get_ECN (Buffer);
                  Assert (ECN'Image, IPv4.ECN_Type'Image (0), "Invalid ECN");
                  Valid := IPv4.Packet.Valid_Total_Length (Buffer);
                  Assert (Valid, "Invalid Total_Length");
                  if Valid then
                     Total_Length := IPv4.Packet.Get_Total_Length (Buffer);
                     Assert (Total_Length'Image, IPv4.Total_Length_Type'Image (44), "Invalid Total_Length");
                     Valid := IPv4.Packet.Valid_Identification (Buffer);
                     Assert (Valid, "Invalid Identification");
                     if Valid then
                        Identification := IPv4.Packet.Get_Identification (Buffer);
                        Assert (Identification'Image, IPv4.Identification_Type'Image (1), "Invalid Identification");
                        Valid := IPv4.Packet.Valid_Flag_R (Buffer);
                        Assert (Valid, "Invalid Flag_R");
                        if Valid then
                           Flag_R := IPv4.Packet.Get_Flag_R (Buffer);
                           Assert (Flag_R'Image, IPv4.Flag_Type'Image (Flag_False), "Invalid Flag_R");
                           Valid := IPv4.Packet.Valid_Flag_DF (Buffer);
                           Assert (Valid, "Invalid Flag_DF");
                           if Valid then
                              Flag_DF := IPv4.Packet.Get_Flag_DF (Buffer);
                              Assert (Flag_DF'Image, IPv4.Flag_Type'Image (Flag_False), "Invalid Flag_DF");
                              Valid := IPv4.Packet.Valid_Flag_MF (Buffer);
                              Assert (Valid, "Invalid Flag_MF");
                              if Valid then
                                 Flag_MF := IPv4.Packet.Get_Flag_MF (Buffer);
                                 Assert (Flag_MF'Image, IPv4.Flag_Type'Image (Flag_False), "Invalid Flag_MF");
                                 Valid := IPv4.Packet.Valid_Fragment_Offset (Buffer);
                                 Assert (Valid, "Invalid Fragment_Offset");
                                 if Valid then
                                    Fragment_Offset := IPv4.Packet.Get_Fragment_Offset (Buffer);
                                    Assert (Fragment_Offset'Image, IPv4.Fragment_Offset_Type'Image (0), "Invalid Fragment_Offset");
                                    Valid := IPv4.Packet.Valid_TTL (Buffer);
                                    Assert (Valid, "Invalid TTL");
                                    if Valid then
                                       TTL := IPv4.Packet.Get_TTL (Buffer);
                                       Assert (TTL'Image, IPv4.TTL_Type'Image (64), "Invalid TTL");
                                       Valid := IPv4.Packet.Valid_Protocol (Buffer);
                                       Assert (Valid, "Invalid Protocol");
                                       if Valid then
                                          Protocol := IPv4.Packet.Get_Protocol (Buffer);
                                          Assert (Protocol'Image, IPv4.Protocol_Type'Image (16#11#), "Invalid Protocol");
                                          Valid := IPv4.Packet.Valid_Header_Checksum (Buffer);
                                          Assert (Valid, "Invalid Header_Checksum");
                                          if Valid then
                                             Header_Checksum := IPv4.Packet.Get_Header_Checksum (Buffer);
                                             Assert (Header_Checksum'Image, IPv4.Header_Checksum_Type'Image (16#7CBE#), "Invalid Header_Checksum");
                                             Valid := IPv4.Packet.Valid_Source (Buffer);
                                             Assert (Valid, "Invalid Source");
                                             if Valid then
                                                Source := IPv4.Packet.Get_Source (Buffer);
                                                Assert (Source'Image, IPv4.Address_Type'Image (16#7f000001#), "Invalid Source");
                                                Valid := IPv4.Packet.Valid_Destination (Buffer);
                                                Assert (Valid, "Invalid Destination");
                                                if Valid then
                                                   Destination := IPv4.Packet.Get_Destination (Buffer);
                                                   Assert (Destination'Image, IPv4.Address_Type'Image (16#7f000001#), "Invalid Destination");
                                                   Valid := IPv4.Packet.Valid_Payload (Buffer);
                                                   Assert (Valid, "Invalid Payload");
                                                   if Valid then
                                                      IPv4.Packet.Get_Payload (Buffer, First, Last);
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

   procedure Test_IPv4_Option (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer        : Types.Bytes := (68, 3, 42);
      Valid         : Boolean;
      First         : Natural;
      Last          : Natural;
      Copied        : IPv4.Flag_Type;
      Option_Class  : IPv4.Option_Class_Type;
      Option_Number : IPv4.Option_Number_Type;
      Option_Length : IPv4.Option_Length_Type;
   begin
      IPv4.Option.Initialize (Buffer);
      Valid := IPv4.Option.Valid_Copied (Buffer);
      Assert (Valid, "Invalid Copied");
      if Valid then
         Copied := IPv4.Option.Get_Copied (Buffer);
         Assert (Copied'Image, Flag_False'Image, "Invalid Copied");
         Valid := IPv4.Option.Valid_Option_Class (Buffer);
         Assert (Valid, "Invalid Option_Class");
         if Valid then
            Option_Class := IPv4.Option.Get_Option_Class (Buffer);
            Assert (Option_Class'Image, Debugging_And_Measurement'Image, "Invalid Option_Class");
            Valid := IPv4.Option.Valid_Option_Number (Buffer);
            Assert (Valid, "Invalid Option_Number");
            if Valid then
               Option_Number := IPv4.Option.Get_Option_Number (Buffer);
               Assert (Option_Number'Image, Natural'Image (4), "Invalid Option_Number");
               Valid := IPv4.Option.Valid_Option_Length (Buffer);
               Assert (Valid, "Invalid Option_Length");
               if Valid then
                  Option_Length := IPv4.Option.Get_Option_Length (Buffer);
                  Assert (Option_Length'Image, Natural'Image (3), "Invalid Option_Length");
                  Valid := IPv4.Option.Valid_Option_Data (Buffer);
                  Assert (Valid, "Invalid Option_Data");
                  if Valid then
                     IPv4.Option.Get_Option_Data (Buffer, First, Last);
                     Assert (First'Image, Natural'Image (3), "Invalid Option_Data'First");
                     Assert (Last'Image, Natural'Image (3), "Invalid Option_Data'Last");
                  end if;
               end if;
            end if;
         end if;
      end if;
      Valid := IPv4.Option.Is_Valid (Buffer);
      Assert (Valid, "Invalid option");
   end Test_IPv4_Option;

   procedure Test_IPv4_With_Options (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer        : Types.Bytes := Read_File ("tests/ipv4-options_udp.raw");
      Valid         : Boolean;
      First         : Natural;
      Last          : Natural;
      Option_First  : Natural;
      Option_Last   : Natural;
      Offset        : IPv4.Options.Offset_Type;
      Option_Length : IPv4.Option_Length_Type;
   begin
      IPv4.Packet.Initialize (Buffer);
      Valid := IPv4.Packet.Valid_Options (Buffer);
      Assert (Valid, "Invalid options");
      if Valid then
         IPv4.Packet.Get_Options (Buffer, First, Last);
         Assert (First'Image, Natural'Image (21), "Invalid Options'First");
         Assert (Last'Image, Natural'Image (36), "Invalid Options'Last");
         Valid := IPv4.Options.Valid_First (Buffer (First .. Last));
         Assert (Valid, "Invalid first");
         if Valid then
            IPv4.Options.First (Buffer (First .. Last), Offset, Option_First, Option_Last);
            Assert (Option_First'Image, Natural'Image (21), "Invalid First of first option");
            Assert (Option_Last'Image, Natural'Image (23), "Invalid Last of first option");
            Valid := IPv4.Option.Is_Valid (Buffer (Option_First .. Option_Last));
            Assert (Valid, "Invalid first option");
            if IPv4.Option.Valid_Option_Length (Buffer (Option_First .. Option_Last)) then
               Option_Length := IPv4.Option.Get_Option_Length (Buffer (Option_First .. Option_Last));
               Assert (Option_Length'Image, Natural'Image (3), "Invalid Length of first option");
            end if;
            Valid := IPv4.Options.Valid_Next (Buffer (First .. Last), Offset);
            Assert (Valid, "Invalid next after first option");
            if Valid then
               IPv4.Options.Next (Buffer (First .. Last), Offset, Option_First, Option_Last);
               Assert (Option_First'Image, Natural'Image (24), "Invalid First of second option");
               Assert (Option_Last'Image, Natural'Image (34), "Invalid Last of second option");
               Valid := IPv4.Option.Is_Valid (Buffer (Option_First .. Option_Last));
               Assert (Valid, "Invalid second option");
               if IPv4.Option.Valid_Option_Length (Buffer (Option_First .. Option_Last)) then
                  Option_Length := IPv4.Option.Get_Option_Length (Buffer (Option_First .. Option_Last));
                  Assert (Option_Length'Image, Natural'Image (11), "Invalid Length of second option");
               end if;
            end if;
            Valid := IPv4.Options.Valid_Next (Buffer (First .. Last), Offset);
            Assert (Valid, "Invalid next after second option");
            if Valid then
               IPv4.Options.Next (Buffer (First .. Last), Offset, Option_First, Option_Last);
               Assert (Option_First'Image, Natural'Image (35), "Invalid First of third option");
               Assert (Option_Last'Image, Natural'Image (35), "Invalid Last of third option");
               Valid := IPv4.Option.Is_Valid (Buffer (Option_First .. Option_Last));
               Assert (Valid, "Invalid third option");
            end if;
            Assert (not IPv4.Options.Valid_Next (Buffer (First .. Last), Offset), "Invalid acceptance of fourth option");
         end if;
      end if;
      Valid := IPv4.Packet.Is_Valid (Buffer);
      Assert (Valid, "Invalid packet");
      end Test_IPv4_With_Options;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_IPv4'Access, "IPv4");
      Register_Routine (T, Test_IPv4_Option'Access, "IPv4 Option");
      Register_Routine (T, Test_IPv4_With_Options'Access, "IPv4 with Options");
   end Register_Tests;

end IPv4.Tests;

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
      First           : Types.Index_Type;
      Last            : Types.Index_Type;
   begin
      IPv4.Packet.Label (Buffer);
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
                                                      Assert (First'Image, Types.Index_Type'Image (21), "Invalid Payload'First");
                                                      Assert (Last'Image, Types.Index_Type'Image (44), "Invalid Payload'Last");
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
      First         : Types.Index_Type;
      Last          : Types.Index_Type;
      Copied        : IPv4.Flag_Type;
      Option_Class  : IPv4.Option_Class_Type;
      Option_Number : IPv4.Option_Number_Type;
      Option_Length : IPv4.Option_Length_Type;
   begin
      IPv4.Option.Label (Buffer);
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
               Assert (Option_Number'Image, Types.Index_Type'Image (4), "Invalid Option_Number");
               Valid := IPv4.Option.Valid_Option_Length (Buffer);
               Assert (Valid, "Invalid Option_Length");
               if Valid then
                  Option_Length := IPv4.Option.Get_Option_Length (Buffer);
                  Assert (Option_Length'Image, Types.Index_Type'Image (3), "Invalid Option_Length");
                  Valid := IPv4.Option.Valid_Option_Data (Buffer);
                  Assert (Valid, "Invalid Option_Data");
                  if Valid then
                     IPv4.Option.Get_Option_Data (Buffer, First, Last);
                     Assert (First'Image, Types.Index_Type'Image (3), "Invalid Option_Data'First");
                     Assert (Last'Image, Types.Index_Type'Image (3), "Invalid Option_Data'Last");
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
      First         : Types.Index_Type;
      Last          : Types.Index_Type;
      Cursor        : IPv4.Options.Cursor_Type;
      Option_Class  : IPv4.Option_Class_Type;
      Option_Length : IPv4.Option_Length_Type;
   begin
      IPv4.Packet.Label (Buffer);
      Valid := IPv4.Packet.Valid_Options (Buffer);
      Assert (Valid, "Invalid options");
      if Valid then
         IPv4.Packet.Get_Options (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (21), "Invalid Options'First");
         Assert (Last'Image, Types.Index_Type'Image (36), "Invalid Options'Last");
         Cursor := IPv4.Options.First (Buffer (First .. Last));
         Assert (Cursor.First'Image, Types.Index_Type'Image (21), "Invalid First of first option");
         Assert (Cursor.Last'Image, Types.Index_Type'Image (36), "Invalid Last of first option");
         Valid := IPv4.Options.Valid_Element (Buffer (First .. Last), Cursor);
         Assert (Valid, "Invalid first");
         if Valid then
            Valid := IPv4.Option.Is_Valid (Buffer (Cursor.First .. Cursor.Last));
            Assert (Valid, "Invalid first option");
            Option_Class := IPv4.Option.Get_Option_Class (Buffer (Cursor.First .. Cursor.Last));
            Assert (Option_Class'Image, Control'Image, "Invalid option class of first option");
            if IPv4.Option.Valid_Option_Length (Buffer (Cursor.First .. Cursor.Last)) then
               Option_Length := IPv4.Option.Get_Option_Length (Buffer (Cursor.First .. Cursor.Last));
               Assert (Option_Length'Image, Types.Index_Type'Image (3), "Invalid Length of first option");
            end if;
            IPv4.Options.Next (Buffer (First .. Last), Cursor);
            Assert (Cursor.First'Image, Types.Index_Type'Image (24), "Invalid First of second option");
            Assert (Cursor.Last'Image, Types.Index_Type'Image (36), "Invalid Last of second option");
            Valid := IPv4.Options.Valid_Element (Buffer (First .. Last), Cursor);
            Assert (Valid, "Invalid element after first option");
            if Valid then
               Valid := IPv4.Option.Is_Valid (Buffer (Cursor.First .. Cursor.Last));
               Assert (Valid, "Invalid second option");
               Option_Class := IPv4.Option.Get_Option_Class (Buffer (Cursor.First .. Cursor.Last));
               Assert (Option_Class'Image, Control'Image, "Invalid option class of second option");
               if IPv4.Option.Valid_Option_Length (Buffer (Cursor.First .. Cursor.Last)) then
                  Option_Length := IPv4.Option.Get_Option_Length (Buffer (Cursor.First .. Cursor.Last));
                  Assert (Option_Length'Image, Types.Index_Type'Image (11), "Invalid Length of second option");
               end if;
               IPv4.Options.Next (Buffer (First .. Last), Cursor);
               Assert (Cursor.First'Image, Types.Index_Type'Image (35), "Invalid First of third option");
               Assert (Cursor.Last'Image, Types.Index_Type'Image (36), "Invalid Last of third option");
               Valid := IPv4.Options.Valid_Element (Buffer (First .. Last), Cursor);
               Assert (Valid, "Invalid element after second option");
               if Valid then
                  Valid := IPv4.Option.Is_Valid (Buffer (Cursor.First .. Cursor.Last));
                  Assert (Valid, "Invalid third option");
                  Option_Class := IPv4.Option.Get_Option_Class (Buffer (Cursor.First .. Cursor.Last));
                  Assert (Option_Class'Image, Control'Image, "Invalid option class of third option");
                  IPv4.Options.Next (Buffer (First .. Last), Cursor);
                  Assert (not IPv4.Options.Valid_Element (Buffer (First .. Last), Cursor), "Invalid acceptance of fourth option");
               end if;
            end if;
         end if;
      end if;
      Valid := IPv4.Packet.Is_Valid (Buffer);
      Assert (Valid, "Invalid packet");
   end Test_IPv4_With_Options;

   procedure Test_IPv4_With_Options_Loop (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer        : Types.Bytes := Read_File ("tests/ipv4-options_udp.raw");
      Valid         : Boolean;
      First         : Types.Index_Type;
      Last          : Types.Index_Type;
      Cursor        : IPv4.Options.Cursor_Type;
      I             : Integer := 0;
   begin
      IPv4.Packet.Label (Buffer);
      Valid := IPv4.Packet.Valid_Options (Buffer);
      Assert (Valid, "Invalid options");
      if Valid then
         IPv4.Packet.Get_Options (Buffer, First, Last);
         Assert (First'Image, Types.Index_Type'Image (21), "Invalid Options'First");
         Assert (Last'Image, Types.Index_Type'Image (36), "Invalid Options'Last");

         Cursor := IPv4.Options.First (Buffer (First .. Last));
         Assert (Cursor.First'Image, Types.Index_Type'Image (21), "Invalid First of first option");
         Assert (Cursor.Last'Image, Types.Index_Type'Image (36), "Invalid Last of first option");

         while IPv4.Options.Valid_Element (Buffer (First .. Last), Cursor) and then I < Integer'Last loop
            declare
               --  workaround for GNATprove version 2018 and prior
               --  needed to be able to proof bounds of slices in loop invariant
               Cf : constant Types.Index_Type := Cursor.First;
               Cl : constant Types.Index_Type := Cursor.Last;
            begin
               pragma Loop_Invariant (Cf >= First and then Cl <= Last);
               pragma Loop_Invariant (Cf = Cursor.First and then Cl = Cursor.Last);
               pragma Loop_Invariant (IPv4.Option.Is_Contained (Buffer (Cf .. Cl)));
               pragma Loop_Invariant (IPv4.Option.Is_Valid (Buffer (Cf .. Cl)));
               pragma Loop_Invariant (I < Integer'Last);
            end;

            I := I + 1;

            IPv4.Options.Next (Buffer (First .. Last), Cursor);
         end loop;

         Assert (I'Image, Integer'Image (3), "Invalid number of options");
      end if;
      Valid := IPv4.Packet.Is_Valid (Buffer);
      Assert (Valid, "Invalid packet");
   end Test_IPv4_With_Options_Loop;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_IPv4'Access, "IPv4");
      Register_Routine (T, Test_IPv4_Option'Access, "IPv4 Option");
      Register_Routine (T, Test_IPv4_With_Options'Access, "IPv4 with Options");
      Register_Routine (T, Test_IPv4_With_Options_Loop'Access, "IPv4 with Options (Loop)");
   end Register_Tests;

end IPv4.Tests;

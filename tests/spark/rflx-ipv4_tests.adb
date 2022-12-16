pragma Style_Checks ("L18");

with SPARK; use SPARK;
with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;

with RFLX.IPv4.Packet;
with RFLX.IPv4.Option;

package body RFLX.IPv4_Tests is

   use type RFLX.RFLX_Builtin_Types.Length, RFLX.RFLX_Builtin_Types.Index;

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("IPv4");
   end Name;

   Payload_Length : RFLX_Builtin_Types.Length;

   procedure Store_Payload_Length (Buffer : RFLX_Builtin_Types.Bytes) is
   begin
      Payload_Length := Buffer'Length;
   end Store_Payload_Length;

   procedure Get_Payload_Length is new IPv4.Packet.Generic_Get_Payload (Store_Payload_Length);

   Option_Data_Length : RFLX_Builtin_Types.Length;

   procedure Store_Option_Data_Length (Buffer : RFLX_Builtin_Types.Bytes) is
   begin
      Option_Data_Length := Buffer'Length;
   end Store_Option_Data_Length;

   procedure Get_Option_Data_Length is new IPv4.Option.Generic_Get_Option_Data (Store_Option_Data_Length);

   Data : RFLX_Builtin_Types.Bytes (RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 23) :=
     (others => 0);

   function Valid_Data_Length (L : RFLX_Builtin_Types.Length) return Boolean is
      (L <= Data'Length)
   with
      SPARK_Mode;

   procedure Write_Data (Buffer : out RFLX_Builtin_Types.Bytes) with
      SPARK_Mode,
      Pre => Valid_Data_Length (Buffer'Length)
   is
   begin
      Buffer := Data (Data'First .. Data'First + Buffer'Length - 1);
   end Write_Data;

   procedure Test_Parsing_IPv4 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer          : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ipv4_udp.raw");
      Context         : IPv4.Packet.Context;
      Valid           : Boolean;
      Version         : IPv4.Version;
      IHL             : IPv4.IHL;
      DSCP            : IPv4.DCSP;
      ECN             : IPv4.ECN;
      Total_Length    : IPv4.Total_Length;
      Identification  : IPv4.Identification;
      Flag_R          : Boolean;
      Flag_DF         : Boolean;
      Flag_MF         : Boolean;
      Fragment_Offset : IPv4.Fragment_Offset;
      TTL             : IPv4.TTL;
      Protocol        : IPv4.Protocol;
      Header_Checksum : IPv4.Header_Checksum;
      Source          : IPv4.Address;
      Destination     : IPv4.Address;
   begin
      IPv4.Packet.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      IPv4.Packet.Verify_Message (Context);

      Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_Version);
      Assert (Valid, "Invalid Version");
      if Valid then
         Version := IPv4.Packet.Get_Version (Context);
         Assert (Version'Image, IPv4.Version'Image (4), "Invalid Version");
         Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_IHL);
         Assert (Valid, "Invalid IHL");
         if Valid then
            IHL := IPv4.Packet.Get_IHL (Context);
            Assert (IHL'Image, IPv4.IHL'Image (5), "Invalid IHL");
            Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_DSCP);
            Assert (Valid, "Invalid DSCP");
            if Valid then
               DSCP := IPv4.Packet.Get_DSCP (Context);
               Assert (DSCP'Image, IPv4.DCSP'Image (0), "Invalid DSCP");
               Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_ECN);
               Assert (Valid, "Invalid ECN");
               if Valid then
                  ECN := IPv4.Packet.Get_ECN (Context);
                  Assert (ECN'Image, IPv4.ECN'Image (0), "Invalid ECN");
                  Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_Total_Length);
                  Assert (Valid, "Invalid Total_Length");
                  if Valid then
                     Total_Length := IPv4.Packet.Get_Total_Length (Context);
                     Assert (Total_Length'Image, IPv4.Total_Length'Image (44), "Invalid Total_Length");
                     Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_Identification);
                     Assert (Valid, "Invalid Identification");
                     if Valid then
                        Identification := IPv4.Packet.Get_Identification (Context);
                        Assert (Identification'Image, IPv4.Identification'Image (1), "Invalid Identification");
                        Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_Flag_R);
                        Assert (Valid, "Invalid Flag_R");
                        if Valid then
                           Flag_R := IPv4.Packet.Get_Flag_R (Context);
                           Assert (Flag_R'Image, Boolean'Image (False), "Invalid Flag_R");
                           Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_Flag_DF);
                           Assert (Valid, "Invalid Flag_DF");
                           if Valid then
                              Flag_DF := IPv4.Packet.Get_Flag_DF (Context);
                              Assert (Flag_DF'Image, Boolean'Image (False), "Invalid Flag_DF");
                              Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_Flag_MF);
                              Assert (Valid, "Invalid Flag_MF");
                              if Valid then
                                 Flag_MF := IPv4.Packet.Get_Flag_MF (Context);
                                 Assert (Flag_MF'Image, Boolean'Image (False), "Invalid Flag_MF");
                                 Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_Fragment_Offset);
                                 Assert (Valid, "Invalid Fragment_Offset");
                                 if Valid then
                                    Fragment_Offset := IPv4.Packet.Get_Fragment_Offset (Context);
                                    Assert (Fragment_Offset'Image, IPv4.Fragment_Offset'Image (0),
                                            "Invalid Fragment_Offset");
                                    Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_TTL);
                                    Assert (Valid, "Invalid TTL");
                                    if Valid then
                                       TTL := IPv4.Packet.Get_TTL (Context);
                                       Assert (TTL'Image, IPv4.TTL'Image (64), "Invalid TTL");
                                       Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_Protocol);
                                       Assert (Valid, "Invalid Protocol");
                                       if Valid then
                                          Protocol := IPv4.Packet.Get_Protocol (Context);
                                          Assert (Protocol.Known, "Unknown Protocol");
                                          if Protocol.Known then
                                             Assert (Protocol.Enum'Image, IPv4.Protocol_Enum'Image (IPv4.P_UDP),
                                                     "Invalid Protocol");
                                          end if;
                                          Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_Header_Checksum);
                                          Assert (Valid, "Invalid Header_Checksum");
                                          if Valid then
                                             Header_Checksum := IPv4.Packet.Get_Header_Checksum (Context);
                                             Assert (Header_Checksum'Image, IPv4.Header_Checksum'Image (16#7CBE#),
                                                     "Invalid Header_Checksum");
                                             Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_Source);
                                             Assert (Valid, "Invalid Source");
                                             if Valid then
                                                Source := IPv4.Packet.Get_Source (Context);
                                                Assert (Source'Image, IPv4.Address'Image (16#7f000001#),
                                                        "Invalid Source");
                                                Valid := IPv4.Packet.Valid (Context, IPv4.Packet.F_Destination);
                                                Assert (Valid, "Invalid Destination");
                                                if Valid then
                                                   Destination := IPv4.Packet.Get_Destination (Context);
                                                   Assert (Destination'Image, IPv4.Address'Image (16#7f000001#),
                                                           "Invalid Destination");
                                                   Valid := IPv4.Packet.Present (Context, IPv4.Packet.F_Payload);
                                                   Assert (Valid, "Invalid Payload");
                                                   if Valid then
                                                      Get_Payload_Length (Context);
                                                      Assert (Payload_Length'Img, Natural'Image (24),
                                                              "Invalid Payload length");
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

      Valid := IPv4.Packet.Well_Formed_Message (Context);
      Assert (Valid, "Invalid packet");

      IPv4.Packet.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (352)'Image, "Invalid Context.Last");
   end Test_Parsing_IPv4;

   procedure Test_Parsing_IPv4_Option (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer        : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(68, 3, 42);
      Context       : IPv4.Option.Context;
      Valid         : Boolean;
      Copied        : Boolean;
      Option_Class  : IPv4.Option_Class;
      Option_Number : IPv4.Option_Number;
      Option_Length : IPv4.Option_Length;
   begin
      IPv4.Option.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      IPv4.Option.Verify_Message (Context);

      Valid := IPv4.Option.Valid (Context, IPv4.Option.F_Copied);
      Assert (Valid, "Invalid Copied");
      if Valid then
         Copied := IPv4.Option.Get_Copied (Context);
         Assert (Copied'Image, False'Image, "Invalid Copied");
         Valid := IPv4.Option.Valid (Context, IPv4.Option.F_Option_Class);
         Assert (Valid, "Invalid Option_Class");
         if Valid then
            Option_Class := IPv4.Option.Get_Option_Class (Context);
            Assert (Option_Class'Image, IPv4.Debugging_And_Measurement'Image, "Invalid Option_Class");
            Valid := IPv4.Option.Valid (Context, IPv4.Option.F_Option_Number);
            Assert (Valid, "Invalid Option_Number");
            if Valid then
               -- https://github.com/Componolit/Workarounds/issues/30
               pragma Warnings (Off, "unreachable code");
               Option_Number := IPv4.Option.Get_Option_Number (Context);
               pragma Warnings (On, "unreachable code");
               Assert (Option_Number'Image, RFLX_Builtin_Types.Index'Image (4), "Invalid Option_Number");
               Valid := IPv4.Option.Valid (Context, IPv4.Option.F_Option_Length);
               Assert (Valid, "Invalid Option_Length");
               if Valid then
                  Option_Length := IPv4.Option.Get_Option_Length (Context);
                  Assert (Option_Length'Image, RFLX_Builtin_Types.Index'Image (3), "Invalid Option_Length");
                  Valid := IPv4.Option.Present (Context, IPv4.Option.F_Option_Data);
                  Assert (Valid, "Invalid Option_Data");
                  if Valid then
                     Get_Option_Data_Length (Context);
                     Assert (Option_Data_Length'Image, Natural'Image (1), "Invalid Option_Data length");
                  end if;
               end if;
            end if;
         end if;
      end if;

      Valid := IPv4.Option.Well_Formed_Message (Context);
      Assert (Valid, "Invalid option");

      IPv4.Option.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (24)'Image, "Invalid Context.Last");
   end Test_Parsing_IPv4_Option;

   -- https://github.com/Componolit/RecordFlux/issues/61

   -- procedure Test_Parsing_IPv4_With_Options (T : in out AUnit.Test_Cases.Test_Case'Class) with
   --   SPARK_Mode, Pre => True
   -- is
   --    pragma Unreferenced (T);
   --    Buffer           : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ipv4-options_udp.raw");
   --    Context          : IPv4.Packet.Context;
   --    Valid            : Boolean;
   --    Sequence_Context : IPv4.Options.Context;
   --    Element_Context  : IPv4.Option.Context;
   --    I                : Integer := 0;
   -- begin
   --    IPv4.Packet.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
   --    IPv4.Packet.Verify_Message (Context);
   --
   --    Valid := IPv4.Packet.Present (Context, IPv4.Packet.F_Options);
   --    Assert (Valid, "Invalid options");
   --    if Valid then
   --       IPv4.Packet.Switch_To_Options (Context, Sequence_Context);
   --
   --       while I <= 10 and then IPv4.Options.Valid_Element (Sequence_Context) loop
   --          pragma Loop_Invariant (IPv4.Options.Has_Buffer (Sequence_Context));
   --          pragma Loop_Invariant (Context.Buffer_First = Sequence_Context.Buffer_First);
   --          pragma Loop_Invariant (Context.Buffer_Last = Sequence_Context.Buffer_Last);
   --
   --          IPv4.Options.Switch (Sequence_Context, Element_Context);
   --          IPv4.Option.Verify_Message (Element_Context);
   --          Assert (IPv4.Option.Well_Formed_Message (Element_Context),
   --                  "Invalid IPv4 Option " & I'Image);
   --          IPv4.Options.Update (Sequence_Context, Element_Context);
   --
   --          I := I + 1;
   --       end loop;
   --
   --       Assert (I'Image, Integer'Image (3), "Invalid number of options");
   --    end if;
   --
   --    Valid := IPv4.Packet.Valid_Message (Context);
   --    Assert (Valid, "Invalid packet");
   -- end Test_Parsing_IPv4_With_Options;

   procedure Test_Generating_IPv4 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Set_Payload is new IPv4.Packet.Generic_Set_Payload (Write_Data, Valid_Data_Length);
      Expected : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ipv4_udp.raw");
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First
                                      .. RFLX_Builtin_Types.Index'First + 2000 => 0);
      Context  : IPv4.Packet.Context;
      Message_Last : RFLX_Builtin_Types.Bit_Length;
   begin
      IPv4.Packet.Initialize (Context, Buffer);
      IPv4.Packet.Set_Version (Context, 4);
      IPv4.Packet.Set_IHL (Context, 5);
      IPv4.Packet.Set_DSCP (Context, 0);
      IPv4.Packet.Set_ECN (Context, 0);
      IPv4.Packet.Set_Total_Length (Context, 44);
      IPv4.Packet.Set_Identification (Context, 1);
      IPv4.Packet.Set_Flag_R (Context, False);
      IPv4.Packet.Set_Flag_DF (Context, False);
      IPv4.Packet.Set_Flag_MF (Context, False);
      IPv4.Packet.Set_Fragment_Offset (Context, 0);
      IPv4.Packet.Set_TTL (Context, 64);
      IPv4.Packet.Set_Protocol (Context, IPv4.P_UDP);
      IPv4.Packet.Set_Header_Checksum (Context, 16#7CBE#);
      IPv4.Packet.Set_Source (Context, 16#7f000001#);
      IPv4.Packet.Set_Destination (Context, 16#7f000001#);
      IPv4.Packet.Set_Options_Empty (Context);
      Data := (0, 53, 0, 53, 0, 24, 1, 82, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Set_Payload (Context, 24);

      Assert (IPv4.Packet.Well_Formed_Message (Context), "Invalid message");
      Assert (not IPv4.Packet.Valid_Message (Context), "Valid message");

      Message_Last := IPv4.Packet.Message_Last (Context);
      IPv4.Packet.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Index'Image (RFLX_Types.To_Index (Message_Last)
              - RFLX_Types.To_Index (Context.First) + 1),
              Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.To_Index (Context.First) .. RFLX_Types.To_Index (Message_Last)),
              Expected.all,
              "Invalid binary representation");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating_IPv4;

   procedure Test_Generating_IPv4_Option (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Set_Option_Data is new IPv4.Option.Generic_Set_Option_Data (Write_Data, Valid_Data_Length);
      Expected : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(68, 3, 42);
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0);
      Context  : IPv4.Option.Context;
      Message_Last : RFLX_Builtin_Types.Bit_Length;
   begin
      IPv4.Option.Initialize (Context, Buffer);
      IPv4.Option.Set_Copied (Context, False);
      IPv4.Option.Set_Option_Class (Context, IPv4.Debugging_And_Measurement);
      IPv4.Option.Set_Option_Number (Context, 4);
      IPv4.Option.Set_Option_Length (Context, 3);
      Data := (42, others => 0);
      Set_Option_Data (Context, 1);

      Assert (IPv4.Option.Well_Formed_Message (Context), "Invalid message");
      Assert (not IPv4.Option.Valid_Message (Context), "Valid message");

      Message_Last := IPv4.Option.Message_Last (Context);
      IPv4.Option.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Index'Image (RFLX_Types.To_Index (Message_Last)
              - RFLX_Types.To_Index (Context.First) + 1),
              Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.To_Index (Context.First) .. RFLX_Types.To_Index (Message_Last)),
              Expected.all,
              "Invalid binary representation");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating_IPv4_Option;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing_IPv4'Access, "Parsing IPv4");
      Register_Routine (T, Test_Parsing_IPv4_Option'Access, "Parsing IPv4 Option");
      -- https://github.com/Componolit/RecordFlux/issues/61
      -- Register_Routine (T, Test_Parsing_IPv4_With_Options'Access, "IPv4 with Options (Loop)");
      Register_Routine (T, Test_Generating_IPv4'Access, "Generating IPv4");
      Register_Routine (T, Test_Generating_IPv4_Option'Access, "Generating IPv4 Option");
   end Register_Tests;

end RFLX.IPv4_Tests;

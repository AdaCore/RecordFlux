with SPARK; use SPARK;
with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;

with RFLX.IPv4.Packet;
with RFLX.Ethernet.Frame;
with RFLX.In_Ethernet.Contains;

package body RFLX.In_Ethernet_Tests is

   use type RFLX.RFLX_Builtin_Types.Length, RFLX.RFLX_Builtin_Types.Index, RFLX.RFLX_Builtin_Types.Bit_Length;

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("In_Ethernet");
   end Name;

   Data : RFLX_Builtin_Types.Bytes (RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 25) :=
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

   procedure Test_Parsing_IPv4_In_Ethernet (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer                 : RFLX_Builtin_Types.Bytes_Ptr :=
         Read_File_Ptr ("tests/data/captured/ethernet_ipv4_udp.raw");
      Ethernet_Frame_Context : Ethernet.Frame.Context;
      IPv4_Packet_Context    : IPv4.Packet.Context;
      Valid                  : Boolean;
   begin
      Ethernet.Frame.Initialize (Ethernet_Frame_Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      Ethernet.Frame.Verify_Message (Ethernet_Frame_Context);
      Valid := Ethernet.Frame.Well_Formed_Message (Ethernet_Frame_Context);
      Assert (Valid, "Invalid Ethernet frame");
      if Valid then
         Valid := In_Ethernet.Contains.IPv4_Packet_In_Ethernet_Frame_Payload (Ethernet_Frame_Context);
         Assert (Valid, "Ethernet frame contains no IPv4 packet");
         if Valid then
            In_Ethernet.Contains.Switch_To_Payload (Ethernet_Frame_Context, IPv4_Packet_Context);
            IPv4.Packet.Verify_Message (IPv4_Packet_Context);
            Valid := IPv4.Packet.Well_Formed_Message (IPv4_Packet_Context);
            Assert (Valid, "Invalid IPv4 packet");
         end if;
      end if;

      IPv4.Packet.Take_Buffer (IPv4_Packet_Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Ethernet_Frame_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image,
              "Invalid Ethernet_Frame_Context.Last");
      Assert (IPv4_Packet_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image,
              "Invalid IPv4_Packet_Context.Last");
   end Test_Parsing_IPv4_In_Ethernet;

   procedure Test_Parsing_IPv4_In_Ethernet_Copy (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer                 : RFLX_Builtin_Types.Bytes_Ptr :=
         Read_File_Ptr ("tests/data/captured/ethernet_ipv4_udp.raw");
      IPv4_Packet_Buffer     : RFLX_Builtin_Types.Bytes_Ptr :=
         new RFLX_Types.Bytes'(RFLX_Types.Index'First .. RFLX_Types.Index'First + 2000 - 1 => 0);
      Ethernet_Frame_Context : Ethernet.Frame.Context;
      IPv4_Packet_Context    : IPv4.Packet.Context;
      Valid                  : Boolean;
   begin
      IPv4.Packet.Initialize (IPv4_Packet_Context, IPv4_Packet_Buffer);
      Ethernet.Frame.Initialize (Ethernet_Frame_Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      Ethernet.Frame.Verify_Message (Ethernet_Frame_Context);
      Valid := Ethernet.Frame.Well_Formed_Message (Ethernet_Frame_Context);
      Assert (Valid, "Invalid Ethernet frame");
      if Valid then
         Valid := In_Ethernet.Contains.IPv4_Packet_In_Ethernet_Frame_Payload (Ethernet_Frame_Context);
         Assert (Valid, "Ethernet frame contains no IPv4 packet");
         if Valid then
            In_Ethernet.Contains.Copy_Payload (Ethernet_Frame_Context, IPv4_Packet_Context);
            IPv4.Packet.Verify_Message (IPv4_Packet_Context);
            Valid := IPv4.Packet.Well_Formed_Message (IPv4_Packet_Context);
            Assert (Valid, "Invalid IPv4 packet");
         end if;
      end if;

      Ethernet.Frame.Take_Buffer (Ethernet_Frame_Context, Buffer);
      IPv4.Packet.Take_Buffer (IPv4_Packet_Context, IPv4_Packet_Buffer);
      RFLX_Types.Free (Buffer);
      RFLX_Types.Free (IPv4_Packet_Buffer);

      Assert (Ethernet_Frame_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image,
              "Invalid Ethernet_Frame_Context.Last");
      Assert (IPv4_Packet_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (368)'Image,
              "Invalid IPv4_Packet_Context.Last");
   end Test_Parsing_IPv4_In_Ethernet_Copy;

   procedure Test_Generating_IPv4_In_Ethernet (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Set_Payload is new IPv4.Packet.Generic_Set_Payload (Write_Data, Valid_Data_Length);
      Expected               : RFLX_Builtin_Types.Bytes_Ptr :=
         Read_File_Ptr ("tests/data/captured/ethernet_ipv4_udp.raw");
      Buffer                 : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 59 => 0);
      Ethernet_Frame_Context : Ethernet.Frame.Context;
      IPv4_Packet_Context    : IPv4.Packet.Context;
      Message_Last           : RFLX_Builtin_Types.Bit_Length;
   begin
      Ethernet.Frame.Initialize (Ethernet_Frame_Context, Buffer);
      Ethernet.Frame.Set_Destination (Ethernet_Frame_Context, 16#FFFFFFFFFFFF#);
      Ethernet.Frame.Set_Source (Ethernet_Frame_Context, 16#000000000000#);
      Ethernet.Frame.Set_Type_Length_TPID (Ethernet_Frame_Context, 16#0800#);
      Ethernet.Frame.Set_Type_Length (Ethernet_Frame_Context, 16#0800#);
      Ethernet.Frame.Initialize_Payload (Ethernet_Frame_Context, 46);
      pragma Assert (Ethernet.Frame.Field_Size (Ethernet_Frame_Context, Ethernet.Frame.F_Payload) = 368);

      Assert (Ethernet.Frame.Well_Formed_Message (Ethernet_Frame_Context), "Invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Ethernet_Frame_Context), "Valid frame");
      Assert (In_Ethernet.Contains.IPv4_Packet_In_Ethernet_Frame_Payload (Ethernet_Frame_Context),
              "Invalid refinement condition");

      In_Ethernet.Contains.Switch_To_Payload (Ethernet_Frame_Context, IPv4_Packet_Context);

      IPv4.Packet.Set_Version (IPv4_Packet_Context, 4);
      IPv4.Packet.Set_IHL (IPv4_Packet_Context, 5);
      IPv4.Packet.Set_DSCP (IPv4_Packet_Context, 0);
      IPv4.Packet.Set_ECN (IPv4_Packet_Context, 0);
      IPv4.Packet.Set_Total_Length (IPv4_Packet_Context, 46);
      IPv4.Packet.Set_Identification (IPv4_Packet_Context, 1);
      IPv4.Packet.Set_Flag_R (IPv4_Packet_Context, False);
      IPv4.Packet.Set_Flag_DF (IPv4_Packet_Context, False);
      IPv4.Packet.Set_Flag_MF (IPv4_Packet_Context, False);
      IPv4.Packet.Set_Fragment_Offset (IPv4_Packet_Context, 0);
      IPv4.Packet.Set_TTL (IPv4_Packet_Context, 64);
      IPv4.Packet.Set_Protocol (IPv4_Packet_Context, IPv4.P_UDP);
      IPv4.Packet.Set_Header_Checksum (IPv4_Packet_Context, 16#7CBC#);
      IPv4.Packet.Set_Source (IPv4_Packet_Context, 16#7f000001#);
      IPv4.Packet.Set_Destination (IPv4_Packet_Context, 16#7f000001#);
      pragma Assert (IPv4.Packet.Field_First (IPv4_Packet_Context, IPv4.Packet.F_Options) = 273);
      pragma Assert (IPv4.Packet.Field_Size (IPv4_Packet_Context, IPv4.Packet.F_Options) = 0);
      pragma Assert (IPv4.Packet.Field_Last (IPv4_Packet_Context, IPv4.Packet.F_Options) = 272);
      IPv4.Packet.Set_Options_Empty (IPv4_Packet_Context);
      Data := (0, 53, 0, 53, 0, 26, 1, 78, others => 0);
      pragma Assert (IPv4.Packet.Field_First (IPv4_Packet_Context, IPv4.Packet.F_Payload) = 273);
      pragma Assert (IPv4.Packet.Field_Size (IPv4_Packet_Context, IPv4.Packet.F_Payload) = 208);
      Set_Payload (IPv4_Packet_Context, Data'Length);

      Assert (IPv4.Packet.Well_Formed_Message (IPv4_Packet_Context), "Invalid message");
      Assert (not IPv4.Packet.Valid_Message (IPv4_Packet_Context), "Valid message");

      Message_Last := IPv4.Packet.Message_Last (IPv4_Packet_Context);
         -- Eng/RecordFlux/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""IPv4_Packet_Context""");
      pragma Warnings (Off, """IPv4_Packet_Context"" is set by ""*"" but not used after the call");
      IPv4.Packet.Take_Buffer (IPv4_Packet_Context, Buffer);
      pragma Warnings (On, """IPv4_Packet_Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""IPv4_Packet_Context""");

      Assert (RFLX_Builtin_Types.Index'Image (RFLX_Types.To_Index (Message_Last)
              - RFLX_Types.To_Index (Ethernet_Frame_Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.To_Index (Ethernet_Frame_Context.First)
              .. RFLX_Types.To_Index (Message_Last)), Expected.all,
              "Invalid binary representation");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);

      Assert (Ethernet_Frame_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image,
              "Invalid Ethernet_Frame_Context.Last");
   end Test_Generating_IPv4_In_Ethernet;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing_IPv4_In_Ethernet'Access, "Parsing IPv4 in Ethernet");
      Register_Routine (T, Test_Parsing_IPv4_In_Ethernet_Copy'Access, "Parsing IPv4 in Ethernet (copy)");
      Register_Routine (T, Test_Generating_IPv4_In_Ethernet'Access, "Generating IPv4 in Ethernet");
   end Register_Tests;

end RFLX.In_Ethernet_Tests;

with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.RFLX_Builtin_Types; use type RFLX.RFLX_Builtin_Types.Length;
with RFLX.RFLX_Types;

with RFLX.IPv4.Packet;
with RFLX.Ethernet.Frame;
with RFLX.In_Ethernet.Contains;

package body RFLX.In_Ethernet.Tests is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("In_Ethernet");
   end Name;

   Data : RFLX_Builtin_Types.Bytes (RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 25) :=
     (others => 0);

   procedure Write_Data (Buffer : out RFLX_Builtin_Types.Bytes) is
   begin
      Buffer := Data (Data'First .. Data'First + Buffer'Length - 1);
   end Write_Data;

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");
   pragma Warnings (Off, "unused assignment to ""Ethernet_Frame_Context""");

   procedure Test_Parsing_IPv4_In_Ethernet (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer                 : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/ethernet_ipv4_udp.raw");
      Ethernet_Frame_Context : Ethernet.Frame.Context := Ethernet.Frame.Create;
      IPv4_Packet_Context    : IPv4.Packet.Context := IPv4.Packet.Create;
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
            In_Ethernet.Contains.Switch_To_Payload (Ethernet_Frame_Context, IPv4_Packet_Context);
            IPv4.Packet.Verify_Message (IPv4_Packet_Context);
            Valid := IPv4.Packet.Structural_Valid_Message (IPv4_Packet_Context);
            Assert (Valid, "Structural invalid IPv4 packet");
         end if;
      end if;
   end Test_Parsing_IPv4_In_Ethernet;

   procedure Test_Generating_IPv4_In_Ethernet (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Set_Payload is new IPv4.Packet.Set_Payload (Write_Data);
      Expected               : constant RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/ethernet_ipv4_udp.raw");
      Buffer                 : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First
                                      .. RFLX_Builtin_Types.Index'First + Expected'Size - 1 => 0);
      Ethernet_Frame_Context : Ethernet.Frame.Context := Ethernet.Frame.Create;
      IPv4_Packet_Context    : IPv4.Packet.Context := IPv4.Packet.Create;
   begin
      Ethernet.Frame.Initialize (Ethernet_Frame_Context, Buffer);
      Ethernet.Frame.Set_Destination (Ethernet_Frame_Context, 16#FFFFFFFFFFFF#);
      Ethernet.Frame.Set_Source (Ethernet_Frame_Context, 16#000000000000#);
      Ethernet.Frame.Set_Type_Length_TPID (Ethernet_Frame_Context, 16#0800#);
      Ethernet.Frame.Set_Type_Length (Ethernet_Frame_Context, 16#0800#);
      Ethernet.Frame.Initialize_Bounded_Payload (Ethernet_Frame_Context, 368);

      Assert (Ethernet.Frame.Structural_Valid_Message (Ethernet_Frame_Context), "Structural invalid frame");
      Assert (not Ethernet.Frame.Valid_Message (Ethernet_Frame_Context), "Valid frame");

      if In_Ethernet.Contains.IPv4_Packet_In_Ethernet_Frame_Payload (Ethernet_Frame_Context) then
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
         IPv4.Packet.Set_Protocol (IPv4_Packet_Context, IPv4.PROTOCOL_UDP);
         IPv4.Packet.Set_Header_Checksum (IPv4_Packet_Context, 16#7CBC#);
         IPv4.Packet.Set_Source (IPv4_Packet_Context, 16#7f000001#);
         IPv4.Packet.Set_Destination (IPv4_Packet_Context, 16#7f000001#);
         Data := (0, 53, 0, 53, 0, 26, 1, 78, others => 0);
         Set_Payload (IPv4_Packet_Context);

         Assert (IPv4.Packet.Structural_Valid_Message (IPv4_Packet_Context), "Structural invalid message");
         Assert (not IPv4.Packet.Valid_Message (IPv4_Packet_Context), "Valid message");

         IPv4.Packet.Take_Buffer (IPv4_Packet_Context, Buffer);

         Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (IPv4_Packet_Context.Last)
                 - RFLX_Types.Byte_Index (Ethernet_Frame_Context.First) + 1), Expected'Length'Img,
                 "Invalid buffer length");
         Assert (Buffer.all (RFLX_Types.Byte_Index (Ethernet_Frame_Context.First)
                 .. RFLX_Types.Byte_Index (Ethernet_Frame_Context.Last)), Expected.all,
                 "Invalid binary representation");
      end if;
   end Test_Generating_IPv4_In_Ethernet;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing_IPv4_In_Ethernet'Access, "Parsing IPv4 in Ethernet");
      Register_Routine (T, Test_Generating_IPv4_In_Ethernet'Access, "Generating IPv4 in Ethernet");
   end Register_Tests;

end RFLX.In_Ethernet.Tests;

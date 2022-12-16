with SPARK; use SPARK;
with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;

with RFLX.UDP.Datagram;
with RFLX.IPv4.Packet;
with RFLX.Ethernet.Frame;
with RFLX.In_Ethernet.Contains;
with RFLX.In_IPv4.Contains;

package body RFLX.In_IPv4_Tests is

   use type RFLX.RFLX_Builtin_Types.Length, RFLX.RFLX_Builtin_Types.Index, RFLX.RFLX_Builtin_Types.Bit_Length;

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("In_IPv4");
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

   procedure Test_Parsing_UDP_In_IPv4 (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer               : RFLX_Builtin_Types.Bytes_Ptr := Read_File_Ptr ("tests/data/captured/ipv4_udp.raw");
      IPv4_Packet_Context  : IPv4.Packet.Context;
      UDP_Datagram_Context : UDP.Datagram.Context;
      Valid                : Boolean;
   begin
      IPv4.Packet.Initialize (IPv4_Packet_Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      IPv4.Packet.Verify_Message (IPv4_Packet_Context);
      Valid := IPv4.Packet.Well_Formed_Message (IPv4_Packet_Context);
      Assert (Valid, "Invalid IPv4 packet");
      if Valid then
         Valid := In_IPv4.Contains.UDP_Datagram_In_IPv4_Packet_Payload (IPv4_Packet_Context);
         Assert (Valid, "Ethernet frame contains no UDP datagram");
         if Valid then
            In_IPv4.Contains.Switch_To_Payload (IPv4_Packet_Context, UDP_Datagram_Context);
            UDP.Datagram.Verify_Message (UDP_Datagram_Context);
            Valid := UDP.Datagram.Well_Formed_Message (UDP_Datagram_Context);
            Assert (Valid, "Invalid UDP datagram");
         end if;
      end if;

      UDP.Datagram.Take_Buffer (UDP_Datagram_Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (IPv4_Packet_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (352)'Image,
              "Invalid IPv4_Packet_Context.Last");
      Assert (UDP_Datagram_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (352)'Image,
              "Invalid UDP_Datagram_Context.Last");
   end Test_Parsing_UDP_In_IPv4;

   procedure Test_Parsing_UDP_In_IPv4_In_Ethernet (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer                 : RFLX_Builtin_Types.Bytes_Ptr :=
         Read_File_Ptr ("tests/data/captured/ethernet_ipv4_udp.raw");
      Ethernet_Frame_Context : Ethernet.Frame.Context;
      IPv4_Packet_Context    : IPv4.Packet.Context;
      UDP_Datagram_Context   : UDP.Datagram.Context;
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
            if Valid then
               Valid := In_IPv4.Contains.UDP_Datagram_In_IPv4_Packet_Payload (IPv4_Packet_Context);
               Assert (Valid, "IPv4 packet contains no UDP datagram");
               if Valid then
                  In_IPv4.Contains.Switch_To_Payload (IPv4_Packet_Context, UDP_Datagram_Context);
                  UDP.Datagram.Verify_Message (UDP_Datagram_Context);
                  Valid := UDP.Datagram.Well_Formed_Message (UDP_Datagram_Context);
                  Assert (Valid, "Invalid UDP datagram");
               end if;
            end if;
         end if;
      end if;

      UDP.Datagram.Take_Buffer (UDP_Datagram_Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Ethernet_Frame_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image,
              "Invalid Ethernet_Frame_Context.Last");
      Assert (IPv4_Packet_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image,
              "Invalid IPv4_Packet_Context.Last");
      Assert (UDP_Datagram_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image,
              "Invalid UDP_Datagram_Context.Last");
   end Test_Parsing_UDP_In_IPv4_In_Ethernet;

   procedure Test_Generating_UDP_In_IPv4_In_Ethernet (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      use type UDP.Length;
      procedure Set_Payload is new UDP.Datagram.Generic_Set_Payload (Write_Data, Valid_Data_Length);
      Expected               : RFLX_Builtin_Types.Bytes_Ptr :=
         Read_File_Ptr ("tests/data/captured/ethernet_ipv4_udp.raw");
      Buffer                 : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First
                                      .. RFLX_Builtin_Types.Index'First + 59 => 0);
      Ethernet_Frame_Context : Ethernet.Frame.Context;
      IPv4_Packet_Context    : IPv4.Packet.Context;
      UDP_Datagram_Context   : UDP.Datagram.Context;
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
         IPv4.Packet.Set_Protocol (IPv4_Packet_Context, IPv4.P_UDP);
         IPv4.Packet.Set_Header_Checksum (IPv4_Packet_Context, 16#7CBC#);
         IPv4.Packet.Set_Source (IPv4_Packet_Context, 16#7f000001#);
         IPv4.Packet.Set_Destination (IPv4_Packet_Context, 16#7f000001#);
         pragma Assert (IPv4.Packet.Field_First (IPv4_Packet_Context, IPv4.Packet.F_Options) = 273);
         pragma Assert (IPv4.Packet.Field_Size (IPv4_Packet_Context, IPv4.Packet.F_Options) = 0);
         pragma Assert (IPv4.Packet.Field_Last (IPv4_Packet_Context, IPv4.Packet.F_Options) = 272);
         IPv4.Packet.Set_Options_Empty (IPv4_Packet_Context);
         pragma Assert (IPv4.Packet.Field_First (IPv4_Packet_Context, IPv4.Packet.F_Payload) = 273);
         pragma Assert (IPv4.Packet.Field_Size (IPv4_Packet_Context, IPv4.Packet.F_Payload) = 208);
         IPv4.Packet.Initialize_Payload (IPv4_Packet_Context);

         Assert (IPv4.Packet.Well_Formed_Message (IPv4_Packet_Context), "Invalid message");
         Assert (not IPv4.Packet.Valid_Message (IPv4_Packet_Context), "Valid message");

         if In_IPv4.Contains.UDP_Datagram_In_IPv4_Packet_Payload (IPv4_Packet_Context) then
            In_IPv4.Contains.Switch_To_Payload (IPv4_Packet_Context, UDP_Datagram_Context);

            UDP.Datagram.Set_Source_Port (UDP_Datagram_Context, 53);
            UDP.Datagram.Set_Destination_Port (UDP_Datagram_Context, 53);
            UDP.Datagram.Set_Length (UDP_Datagram_Context, 26);
            UDP.Datagram.Set_Checksum (UDP_Datagram_Context, 16#014E#);
            Data := (others => 0);
            pragma Assert (UDP.Datagram.Get_Length (UDP_Datagram_Context) = 26);
            pragma Assert (UDP.Datagram.Field_First (UDP_Datagram_Context, UDP.Datagram.F_Payload) = 337);
            pragma Assert (UDP.Datagram.Field_Size (UDP_Datagram_Context, UDP.Datagram.F_Payload) = 144);
            Set_Payload (UDP_Datagram_Context, 18);

            -- https://github.com/Componolit/Workarounds/issues/32
            Message_Last := UDP.Datagram.Message_Last (UDP_Datagram_Context);
            pragma Warnings (Off, "unused assignment to ""UDP_Datagram_Context""");
            pragma Warnings (Off, """UDP_Datagram_Context"" is set by ""*"" but not used after the call");
            UDP.Datagram.Take_Buffer (UDP_Datagram_Context, Buffer);
            pragma Warnings (On, """UDP_Datagram_Context"" is set by ""*"" but not used after the call");
            pragma Warnings (On, "unused assignment to ""UDP_Datagram_Context""");

            Assert (RFLX_Builtin_Types.Index'Image
                    (RFLX_Types.To_Index (Message_Last)
                       - RFLX_Types.To_Index (Ethernet_Frame_Context.First) + 1),
                    Expected'Length'Img,
                    "Invalid buffer length");
            Assert (Buffer.all (RFLX_Types.To_Index (Ethernet_Frame_Context.First)
                    .. RFLX_Types.To_Index (Ethernet_Frame_Context.Last)), Expected.all,
                    "Invalid binary representation");
         end if;
      end if;

      if IPv4.Packet.Has_Buffer (IPv4_Packet_Context) then
         IPv4.Packet.Take_Buffer (IPv4_Packet_Context, Buffer);
      end if;
      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);

      Assert (Ethernet_Frame_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image,
              "Invalid Ethernet_Frame_Context.Last");
      Assert (IPv4_Packet_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (480)'Image,
              "Invalid IPv4_Packet_Context.Last");
   end Test_Generating_UDP_In_IPv4_In_Ethernet;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing_UDP_In_IPv4'Access, "Parsing UDP in IPv4");
      Register_Routine (T, Test_Parsing_UDP_In_IPv4_In_Ethernet'Access, "Parsing UDP in IPv4 in Ethernet");
      Register_Routine (T, Test_Generating_UDP_In_IPv4_In_Ethernet'Access, "Generating UDP in IPv4 in Ethernet");
   end Register_Tests;

end RFLX.In_IPv4_Tests;

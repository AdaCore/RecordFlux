with Ada.Unchecked_Deallocation;
with Basalt.Strings_Generic;
with Interfaces;
with RFLX.IPv4.Packet;
with RFLX.IPv4.Contains;
with RFLX.ICMP;
with RFLX.ICMP.Message;
with RFLX.RFLX_Types;
with Generic_Checksum;
with Generic_Socket;

package body ICMPv4 with
   SPARK_Mode,
   Refined_State => (Ping_State => Sequence)
is

   package Checksum is new Generic_Checksum (RFLX.RFLX_Types);
   function Image is new Basalt.Strings_Generic.Image_Modular (RFLX.ICMP.Sequence_Number);
   function Image is new Basalt.Strings_Generic.Image_Modular (RFLX.IPv4.Total_Length);

   function Image (Addr : RFLX.IPv4.Address) return String with
      Post   => Image'Result'First = 1
                and then Image'Result'Length <= 83,
      Global => null;

   Sequence : RFLX.ICMP.Sequence_Number := 0;

   function Image (Addr : RFLX.IPv4.Address) return String
   is
      use type RFLX.IPv4.Address;
      subtype Octet is RFLX.IPv4.Address range 0 .. 255;
      function Img is new Basalt.Strings_Generic.Image_Modular (Octet);
      type Octet_Address is array (1 .. 4) of Octet;
      Addr_Var : RFLX.IPv4.Address := Addr;
      Address  : Octet_Address;
   begin
      for O of Address loop
         O        := Addr_Var and 16#ff#;
         Addr_Var := Addr_Var / 256;
      end loop;
      return Img (Address (1)) & "."
             & Img (Address (2)) & "."
             & Img (Address (3)) & "."
             & Img (Address (4));
   end Image;

   procedure Free_Bytes_Ptr is new Ada.Unchecked_Deallocation (Object => RFLX.RFLX_Builtin_Types.Bytes,
                                                               Name   => RFLX.RFLX_Builtin_Types.Bytes_Ptr);

   procedure Ping (Addr : String)
   is
      package Socket is new Generic_Socket (RFLX.RFLX_Builtin_Types.Byte,
                                            RFLX.RFLX_Builtin_Types.Index,
                                            RFLX.RFLX_Builtin_Types.Bytes,
                                            RFLX.IPv4.Address);
      Address     : RFLX.IPv4.Address;
      Success     : Boolean;
      Ignore_Last : RFLX.RFLX_Builtin_Types.Index;
      Buffer      : RFLX.RFLX_Builtin_Types.Bytes_Ptr := new RFLX.RFLX_Builtin_Types.Bytes'(1 .. 1024 => 0);
      Last        : RFLX.RFLX_Builtin_Types.Index;
   begin
      Socket.Setup;
      if
         not Socket.Valid
         or else Addr'Length < 1
         or else Addr'Length > 15
         or else Buffer = null
      then
         Free_Bytes_Ptr (Buffer);
         return;
      end if;
      ICMPv4.Get_Address (Addr, Address, Success);
      if not Success then
         Ada.Text_IO.Put_Line ("Failed to parse IP Address: " & Addr);
         Free_Bytes_Ptr (Buffer);
         return;
      end if;
      Ada.Text_IO.Put_Line ("PING " & Addr);
      loop
         pragma Loop_Invariant (Buffer /= null);
         pragma Loop_Invariant (Buffer'First = 1);
         pragma Loop_Invariant (Buffer'Length = 1024);
         ICMPv4.Generate (Buffer, Address, Last);
         Socket.Send (Buffer.all (Buffer'First .. Last), Address, Success);
         if not Success then
            Ada.Text_IO.Put_Line ("Failed to send packet");
            Free_Bytes_Ptr (Buffer);
            return;
         end if;
         Buffer.all := (others => 0);
         Socket.Receive (Buffer.all, Ignore_Last, Success);
         if not Success then
            Ada.Text_IO.Put_Line ("Receive failed");
            Free_Bytes_Ptr (Buffer);
            return;
         end if;
         ICMPv4.Print (Buffer);
         delay 1.0;
      end loop;
   end Ping;

   ----------------
   -- To_Address --
   ----------------

   procedure Get_Address (Str   :     String;
                          Addr  : out RFLX.IPv4.Address;
                          Valid : out Boolean)
   is
      use type RFLX.IPv4.Address;
      package Int renames Interfaces;
      subtype Octet is RFLX.IPv4.Address range 0 .. 255;
      type Octet_Address is array (1 .. 4) of Octet;
      package Val is new Basalt.Strings_Generic.Value_Option_Modular (Octet);
      Address   : Octet_Address := (others => 0);
      Oct_First : Natural;
      Oct_Last  : Natural;
      Oct_Index : Positive      := Address'First;
      V         : Val.Optional;
   begin
      Addr  := 0;
      Valid := False;
      if Str'First not in Positive'Range then
         return;
      end if;
      Oct_First := Str'First;
      Oct_Last  := Str'First - 1;
      for I in Str'Range loop
         pragma Loop_Invariant (Oct_First in Str'Range);
         pragma Loop_Invariant (Oct_Last <= Str'Last);
         pragma Loop_Invariant (Oct_Index in Address'Range);
         case Str (I) is
            when '0' .. '9' =>
               Oct_Last := I;
               if I = Str'Last then
                  V := Val.Value (Str (Oct_First .. Oct_Last));
                  if not V.Valid or Oct_Index /= Address'Last then
                     return;
                  end if;
                  Address (Oct_Index) := V.Value;
               end if;
            when '.' =>
               V := Val.Value (Str (Oct_First .. Oct_Last));
               if not V.Valid then
                  return;
               end if;
               Address (Oct_Index) := V.Value;
               if I = Str'Last then
                  return;
               end if;
               Oct_First := I + 1;
               Oct_Last  := I;
               if Oct_Index = Address'Last then
                  return;
               end if;
               Oct_Index := Oct_Index + 1;
            when others =>
               return;
         end case;
      end loop;
      for I in Address'Range loop
         Addr := Addr + RFLX.IPv4.Address (Int.Shift_Left (Int.Unsigned_32 (Address (I)), (I - 1) * 8));
      end loop;
      Valid := True;
   end Get_Address;

   --------------
   -- Generate --
   --------------

   procedure Generate (Buf  : in out RFLX.RFLX_Builtin_Types.Bytes_Ptr;
                       Addr :        RFLX.IPv4.Address;
                       Last :    out RFLX.RFLX_Builtin_Types.Index)
   is
      use type RFLX.ICMP.Sequence_Number;
      use type RFLX.RFLX_Builtin_Types.Bit_Length;
      IP_Context   : RFLX.IPv4.Packet.Context;
      ICMP_Context : RFLX.ICMP.Message.Context;
      Data         : constant RFLX.RFLX_Builtin_Types.Bytes (1 .. 56) := (others => 65);
      function Valid_Length (L : RFLX.RFLX_Builtin_Types.Length) return Boolean is
         (L = Data'Length);
      procedure Process_Data (Buffer : out RFLX.RFLX_Builtin_Types.Bytes) with
         Pre => Valid_Length (Buffer'Length);
      procedure Process_Data (Buffer : out RFLX.RFLX_Builtin_Types.Bytes)
      is
      begin
         Buffer := Data;
      end Process_Data;
      procedure Set_Data is new RFLX.ICMP.Message.Generic_Set_Data (Process_Data, Valid_Length);
   begin
      pragma Warnings (Off, "unused assignment to ""*_Context""");
      RFLX.IPv4.Packet.Initialize (IP_Context, Buf);
      RFLX.IPv4.Packet.Set_Version (IP_Context, 4);
      RFLX.IPv4.Packet.Set_IHL (IP_Context, 5);
      RFLX.IPv4.Packet.Set_DSCP (IP_Context, 0);
      RFLX.IPv4.Packet.Set_ECN (IP_Context, 0);
      RFLX.IPv4.Packet.Set_Total_Length (IP_Context, 84);
      RFLX.IPv4.Packet.Set_Identification (IP_Context, 1);
      RFLX.IPv4.Packet.Set_Flag_R (IP_Context, False);
      RFLX.IPv4.Packet.Set_Flag_DF (IP_Context, False);
      RFLX.IPv4.Packet.Set_Flag_MF (IP_Context, False);
      RFLX.IPv4.Packet.Set_Fragment_Offset (IP_Context, 0);
      RFLX.IPv4.Packet.Set_TTL (IP_Context, 64);
      RFLX.IPv4.Packet.Set_Protocol (IP_Context, RFLX.IPv4.PROTOCOL_ICMP);
      RFLX.IPv4.Packet.Set_Header_Checksum (IP_Context, 0);
      RFLX.IPv4.Packet.Set_Source (IP_Context, 0);
      RFLX.IPv4.Packet.Set_Destination (IP_Context, Addr);
      RFLX.IPv4.Packet.Set_Options_Empty (IP_Context);
      RFLX.IPv4.Packet.Initialize_Payload (IP_Context);
      if RFLX.IPv4.Contains.ICMP_Message_In_Packet_Payload (IP_Context) then
         RFLX.IPv4.Contains.Switch_To_Payload (IP_Context, ICMP_Context);
         RFLX.ICMP.Message.Set_Tag (ICMP_Context, RFLX.ICMP.Echo_Request);
         RFLX.ICMP.Message.Set_Code_Zero (ICMP_Context, 0);
         RFLX.ICMP.Message.Set_Checksum (ICMP_Context,
                                         Checksum.Echo_Request_Reply_Checksum
                                            (RFLX.ICMP.Echo_Request,
                                             0, 0, Sequence, Data));
         RFLX.ICMP.Message.Set_Identifier (ICMP_Context, 0);
         RFLX.ICMP.Message.Set_Sequence_Number (ICMP_Context, Sequence);
         Set_Data (ICMP_Context);
         RFLX.ICMP.Message.Take_Buffer (ICMP_Context, Buf);
         Sequence := Sequence + 1;
      else
         RFLX.IPv4.Packet.Take_Buffer (IP_Context, Buf);
      end if;
      Last := RFLX.RFLX_Types.Byte_Index (RFLX.IPv4.Packet.Message_Last (IP_Context));
      pragma Warnings (On, "unused assignment to ""*_Context""");
   end Generate;

   -----------
   -- Print --
   -----------

   procedure Print (Buf : in out RFLX.RFLX_Builtin_Types.Bytes_Ptr)
   is
      use type RFLX.ICMP.Tag;
      IP_Context   : RFLX.IPv4.Packet.Context;
      ICMP_Context : RFLX.ICMP.Message.Context;
      Length       : RFLX.IPv4.Total_Length;
      Source       : RFLX.IPv4.Address;
      Seq          : RFLX.ICMP.Sequence_Number;
   begin
      pragma Warnings (Off, "unused assignment to ""*_Context""");
      RFLX.IPv4.Packet.Initialize (IP_Context, Buf);
      RFLX.IPv4.Packet.Verify_Message (IP_Context);
      if
         not RFLX.IPv4.Packet.Structural_Valid_Message (IP_Context)
         or else not RFLX.IPv4.Contains.ICMP_Message_In_Packet_Payload (IP_Context)
      then
         RFLX.IPv4.Packet.Take_Buffer (IP_Context, Buf);
         return;
      end if;
      Length := RFLX.IPv4.Packet.Get_Total_Length (IP_Context);
      Source := RFLX.IPv4.Packet.Get_Source (IP_Context);
      RFLX.IPv4.Contains.Switch_To_Payload (IP_Context, ICMP_Context);
      RFLX.ICMP.Message.Verify_Message (ICMP_Context);
      if
         not RFLX.ICMP.Message.Structural_Valid_Message (ICMP_Context)
         or else RFLX.ICMP.Message.Get_Tag (ICMP_Context) /= RFLX.ICMP.Echo_Reply
      then
         RFLX.ICMP.Message.Take_Buffer (ICMP_Context, Buf);
         return;
      end if;
      Seq := RFLX.ICMP.Message.Get_Sequence_Number (ICMP_Context);
      RFLX.ICMP.Message.Take_Buffer (ICMP_Context, Buf);
      Ada.Text_IO.Put_Line (Image (Length) & " bytes from " & Image (Source) & ": icmp_seq=" & Image (Seq));
      pragma Warnings (On, "unused assignment to ""*_Context""");
   end Print;

end ICMPv4;

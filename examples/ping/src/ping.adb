with Generic_Socket;
with Ada.Command_Line;
with Ada.Text_IO;
with RFLX.IPv4;
with RFLX.RFLX_Builtin_Types;
with ICMPv4;

procedure Ping with
   SPARK_Mode
is
   use type RFLX.RFLX_Builtin_Types.Length;
   use type RFLX.RFLX_Builtin_Types.Bytes_Ptr;
   package Socket is new Generic_Socket (RFLX.RFLX_Builtin_Types.Byte,
                                         RFLX.RFLX_Builtin_Types.Index,
                                         RFLX.RFLX_Builtin_Types.Bytes,
                                         RFLX.IPv4.Address);
   Address : RFLX.IPv4.Address;
   Success : Boolean;
   Buffer  : RFLX.RFLX_Builtin_Types.Bytes_Ptr := new RFLX.RFLX_Builtin_Types.Bytes'(1 .. 1024 => 0);
   Last    : RFLX.RFLX_Builtin_Types.Index;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line (Ada.Command_Line.Command_Name & " <IP address>");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;
   Socket.Setup;
   if not Socket.Valid then
      return;
   end if;
   ICMPv4.Get_Address (Ada.Command_Line.Argument (1), Address, Success);
   if not Success then
      Ada.Text_IO.Put_Line ("Failed to parse IP Address: " & Ada.Command_Line.Argument (1));
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;
   Ada.Text_IO.Put_Line ("PING " & Ada.Command_Line.Argument (1));
   loop
      pragma Loop_Invariant (Buffer /= null);
      pragma Loop_Invariant (Buffer'First = 1);
      pragma Loop_Invariant (Buffer'Length = 1024);
      ICMPv4.Generate (Buffer, Address);
      Socket.Send (Buffer.all (Buffer'First .. Buffer'First + 35), Address, Success);
      if not Success then
         Ada.Text_IO.Put_Line ("Failed to send packet");
         Ada.Command_Line.Set_Exit_Status (1);
         return;
      end if;
      Buffer.all := (others => 0);
      Socket.Receive (Buffer.all, Last, Success);
      if not Success then
         Ada.Text_IO.Put_Line ("Receive failed");
         Ada.Command_Line.Set_Exit_Status (1);
         return;
      end if;
      ICMPv4.Print (Buffer);
      delay 1.0;
   end loop;
end Ping;

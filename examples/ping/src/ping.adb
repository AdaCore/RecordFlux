with Socket;
with Ada.Command_Line;
with Ada.Text_IO;
with RFLX.IPv4;
with ICMPv4;

procedure Ping with
   SPARK_Mode
is
   Address : RFLX.IPv4.Address;
   Success : Boolean;
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
end Ping;

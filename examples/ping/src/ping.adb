with Ada.Command_Line;
with Ada.Text_IO;
with ICMPv4;

procedure Ping
is
begin
   Ada.Command_Line.Set_Exit_Status (1);
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line (Ada.Command_Line.Command_Name & " <IP address>");
      return;
   end if;
   ICMPv4.Ping (Ada.Command_Line.Argument (1));
end Ping;

package body Ethernet.Frame is

   procedure Initialize (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Initialize;

   procedure Get_Payload (Buffer : Types.Bytes; First : out Natural; Last : out Natural) is
   begin
      First := Get_Payload_First (Buffer);
      Last := Get_Payload_Last (Buffer);
   end Get_Payload;

end Ethernet.Frame;

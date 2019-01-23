package body IPv4.Packet is

   procedure Initialize (Buffer : Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Initialize;

   procedure Get_Options (Buffer : Bytes; First : out Natural; Last : out Natural) is
   begin
      First := Get_Options_First (Buffer);
      Last := Get_Options_Last (Buffer);
      pragma Assume (IPv4.Options.Is_Contained (Buffer (First .. Last)));
   end Get_Options;

   procedure Get_Payload (Buffer : Bytes; First : out Natural; Last : out Natural) is
   begin
      First := Get_Payload_First (Buffer);
      Last := Get_Payload_Last (Buffer);
   end Get_Payload;

end IPv4.Packet;

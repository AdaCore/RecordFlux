package body IPv4.Packet is

   procedure Initialize (Buffer : Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Initialize;

   procedure Options (Buffer : Bytes; First : out Natural; Last : out Natural) is
   begin
      First := Options_First (Buffer);
      Last := Options_Last (Buffer);
      pragma Assume (IPv4.Options.Is_Contained (Buffer (First .. Last)));
   end Options;

   procedure Payload (Buffer : Bytes; First : out Natural; Last : out Natural) is
   begin
      First := Payload_First (Buffer);
      Last := Payload_Last (Buffer);
   end Payload;

end IPv4.Packet;

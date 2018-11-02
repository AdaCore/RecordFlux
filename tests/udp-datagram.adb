package body UDP.Datagram is

   procedure Initialize (Buffer : Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Initialize;

   procedure Payload (Buffer : Bytes; First : out Natural; Last : out Natural) is
   begin
      First := Payload_First (Buffer);
      Last := Payload_Last (Buffer);
   end Payload;

end UDP.Datagram;

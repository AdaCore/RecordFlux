package body Ethernet.Frame is

   procedure Payload (Buffer : Bytes; First : out Natural; Last : out Natural) is
   begin
      First := Payload_First (Buffer);
      Last := Payload_Last (Buffer);
   end Payload;

end Ethernet.Frame;

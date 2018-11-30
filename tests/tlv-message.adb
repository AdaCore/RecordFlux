package body TLV.Message is

   procedure Initialize (Buffer : Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Initialize;

   procedure Value (Buffer : Bytes; First : out Natural; Last : out Natural) is
   begin
      First := Value_First (Buffer);
      Last := Value_Last (Buffer);
   end Value;

end TLV.Message;

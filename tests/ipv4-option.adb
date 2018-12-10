package body IPv4.Option is

   procedure Initialize (Buffer : Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Initialize;

   procedure Option_Data (Buffer : Bytes; First : out Natural; Last : out Natural) is
   begin
      First := Option_Data_First (Buffer);
      Last := Option_Data_Last (Buffer);
   end Option_Data;

end IPv4.Option;

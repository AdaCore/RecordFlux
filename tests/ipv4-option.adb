package body IPv4.Option is

   procedure Initialize (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Initialize;

   procedure Get_Option_Data (Buffer : Types.Bytes; First : out Natural; Last : out Natural) is
   begin
      First := Get_Option_Data_First (Buffer);
      Last := Get_Option_Data_Last (Buffer);
   end Get_Option_Data;

end IPv4.Option;

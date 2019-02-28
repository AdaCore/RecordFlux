package body IPv4.Option
  with SPARK_Mode
is

   procedure Label (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Label;

   procedure Get_Option_Data (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) is
   begin
      First := Get_Option_Data_First (Buffer);
      Last := Get_Option_Data_Last (Buffer);
   end Get_Option_Data;

end IPv4.Option;

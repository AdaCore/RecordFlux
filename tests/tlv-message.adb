package body TLV.Message
  with SPARK_Mode
is

   procedure Label (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Label;

   procedure Get_Value (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) is
   begin
      First := Get_Value_First (Buffer);
      Last := Get_Value_Last (Buffer);
   end Get_Value;

end TLV.Message;

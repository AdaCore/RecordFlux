package body Ethernet.Frame
  with SPARK_Mode
is

   procedure Label (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Label;

   procedure Get_Payload (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) is
   begin
      First := Get_Payload_First (Buffer);
      Last := Get_Payload_Last (Buffer);
   end Get_Payload;

end Ethernet.Frame;

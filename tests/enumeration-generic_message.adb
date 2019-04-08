package body Enumeration.Generic_Message
  with SPARK_Mode
is

   procedure Label (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Label;

end Enumeration.Generic_Message;

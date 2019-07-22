package body Arrays with
  SPARK_Mode
is

   function Convert_To_AV_Enumeration (Buffer : Types.Bytes; Offset : Types.Offset_Type) return AV_Enumeration is
      Raw : AV_Enumeration_Base := Convert_To_AV_Enumeration_Base (Buffer, Offset);
   begin
      return (case Raw is when 0 => (True, AV_ZERO), when 1 => (True, AV_ONE), when 2 => (True, AV_TWO), when others => (False, Raw));
   end Convert_To_AV_Enumeration;

end Arrays;

package body IPv4
  with SPARK_Mode
is

   function Convert_To_Protocol_Type (Buffer : Types.Bytes; Offset : Natural) return Protocol_Type is
      Raw : Protocol_Type_Base := Convert_To_Protocol_Type_Base (Buffer, Offset);
   begin
      return (case Raw is when 17 => (True, PROTOCOL_UDP), when others => (False, Raw));
   end Convert_To_Protocol_Type;

end IPv4;

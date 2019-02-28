package body Enumeration
  with SPARK_Mode
is

   function Convert_To_Priority (Buffer : Types.Bytes; Offset : Natural) return Priority is
      Raw : Priority_Base := Convert_To_Priority_Base (Buffer, Offset);
   begin
      return (case Raw is when 1 => (True, LOW), when 4 => (True, MEDIUM), when 7 => (True, HIGH), when others => (False, Raw));
   end Convert_To_Priority;

end Enumeration;

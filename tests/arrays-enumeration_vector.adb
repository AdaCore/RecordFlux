package body Arrays.Enumeration_Vector
  with SPARK_Mode
is

   procedure Label (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Label;

   function First (Buffer : Types.Bytes) return Cursor_Type is
   begin
      if Buffer'Length >= 1 then
         return (Buffer'First, Buffer'First);
      else
         return (Types.Index_Type'Last, Types.Index_Type'First);
      end if;
   end First;

   procedure Next (Buffer : Types.Bytes; Cursor : in out Cursor_Type) is
   begin
      if Cursor.Last <= (Buffer'Last - 1) then
         Cursor := ((Cursor.Last + 1), (Cursor.Last + 1));
      else
         Cursor := (Types.Index_Type'Last, Types.Index_Type'First);
      end if;
   end Next;

end Arrays.Enumeration_Vector;

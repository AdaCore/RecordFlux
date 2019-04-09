package body {prefix}Scalar_Sequence
  with SPARK_Mode
is

   procedure Label (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Label;

   function First (Buffer : Types.Bytes) return Cursor_Type is
   begin
      if Buffer'Length >= Element_Type_Byte_Size then
         return (Buffer'First, Buffer'First + Element_Type_Byte_Size - 1);
      else
         return (Types.Index_Type'Last, Types.Index_Type'First);
      end if;
   end First;

   procedure Next (Buffer : Types.Bytes; Cursor : in out Cursor_Type) is
   begin
      if Cursor.Last <= (Buffer'Last - Element_Type_Byte_Size) then
         Cursor := (Cursor.Last + 1, Cursor.Last + Element_Type_Byte_Size);
      else
         Cursor := (Types.Index_Type'Last, Types.Index_Type'First);
      end if;
   end Next;

end {prefix}Scalar_Sequence;

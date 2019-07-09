package body Message_Sequence
  with SPARK_Mode
is

   procedure Label (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Label;

   function First (Buffer : Types.Bytes) return Cursor_Type is
   begin
      Element_Label (Buffer (Buffer'First .. Buffer'Last));
      return (Buffer'First, Buffer'Last);
   end First;

   procedure Next (Buffer : Types.Bytes; Cursor : in out Cursor_Type) is
   begin
      Cursor := (Cursor.First + Types.Length_Type (Element_Length (Buffer (Cursor.First .. Cursor.Last))),
                 Buffer'Last);
      Element_Label (Buffer (Cursor.First .. Cursor.Last));
   end Next;

end Message_Sequence;

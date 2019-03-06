package body IPv4.Options
  with SPARK_Mode
is

   procedure Label (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Label;

   function First (Buffer : Types.Bytes) return Cursor_Type is
   begin
      IPv4.Option.Label (Buffer (Buffer'First .. Buffer'Last));
      return (Buffer'First, Buffer'Last);
   end First;

   procedure Next (Buffer : Types.Bytes; Cursor : in out Cursor_Type) is
   begin
      Cursor := ((Cursor.First + Types.Length_Type (IPv4.Option.Message_Length (Buffer (Cursor.First .. Cursor.Last)))), Buffer'Last);
      IPv4.Option.Label (Buffer (Cursor.First .. Cursor.Last));
   end Next;

end IPv4.Options;

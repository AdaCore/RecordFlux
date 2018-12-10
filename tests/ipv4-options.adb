package body IPv4.Options is

   procedure Initialize (Buffer : Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Initialize;

   function Valid_First (Buffer : Bytes) return Boolean is
   begin
      return Valid_Next (Buffer, Offset_Type (Buffer'First));
   end Valid_First;

   procedure First (Buffer : Bytes; Offset : out Offset_Type; First : out Natural; Last : out Natural) is
   begin
      Offset := Offset_Type (Buffer'First);
      Next (Buffer, Offset, First, Last);
   end First;

   function Valid_Next (Buffer : Bytes; Offset : Offset_Type) return Boolean is
   begin
      pragma Assume (IPv4.Option.Is_Contained (Buffer (Positive (Offset) .. Buffer'Last)));
      return IPv4.Option.Is_Valid (Buffer (Positive (Offset) .. Buffer'Last));
   end Valid_Next;

   procedure Next (Buffer : Bytes; Offset : in out Offset_Type; First : out Natural; Last : out Natural) is
   begin
      First := Positive (Offset);
      Last := (First + Natural (IPv4.Option.Message_Length (Buffer (First .. Buffer'Last))) + (-1));
      Offset := Offset_Type (Last + 1);
      pragma Assume (IPv4.Option.Is_Contained (Buffer (First .. Last)));
   end Next;

end IPv4.Options;

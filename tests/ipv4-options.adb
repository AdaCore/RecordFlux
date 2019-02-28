package body IPv4.Options
  with SPARK_Mode
is

   procedure Label (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Label;

   function Valid_First (Buffer : Types.Bytes) return Boolean is
   begin
      return Valid_Next (Buffer, Offset_Type (Buffer'First));
   end Valid_First;

   procedure Get_First (Buffer : Types.Bytes; Offset : out Offset_Type; First : out Types.Index_Type; Last : out Types.Index_Type) is
   begin
      Offset := Offset_Type (Buffer'First);
      Get_Next (Buffer, Offset, First, Last);
   end Get_First;

   function Valid_Next (Buffer : Types.Bytes; Offset : Offset_Type) return Boolean is
   begin
      pragma Assume (IPv4.Option.Is_Contained (Buffer (Types.Index_Type (Offset) .. Buffer'Last)));
      return IPv4.Option.Is_Valid (Buffer (Types.Index_Type (Offset) .. Buffer'Last));
   end Valid_Next;

   procedure Get_Next (Buffer : Types.Bytes; Offset : in out Offset_Type; First : out Types.Index_Type; Last : out Types.Index_Type) is
   begin
      First := Types.Index_Type (Offset);
      Last := (First + Types.Length_Type (IPv4.Option.Message_Length (Buffer (First .. Buffer'Last))) + (-1));
      Offset := Offset_Type (Last + 1);
      pragma Assume (IPv4.Option.Is_Contained (Buffer (First .. Last)));
   end Get_Next;

end IPv4.Options;

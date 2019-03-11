package body Arrays.Message
  with SPARK_Mode
is

   procedure Label (Buffer : Types.Bytes) is
   begin
      pragma Assume (Is_Contained (Buffer));
   end Label;

   procedure Get_Modular_Vector (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) is
   begin
      First := Get_Modular_Vector_First (Buffer);
      Last := Get_Modular_Vector_Last (Buffer);
      pragma Assume (Arrays.Modular_Vector.Is_Contained (Buffer (First .. Last)));
   end Get_Modular_Vector;

   procedure Get_Range_Vector (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) is
   begin
      First := Get_Range_Vector_First (Buffer);
      Last := Get_Range_Vector_Last (Buffer);
      pragma Assume (Arrays.Range_Vector.Is_Contained (Buffer (First .. Last)));
   end Get_Range_Vector;

   procedure Get_Enumeration_Vector (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) is
   begin
      First := Get_Enumeration_Vector_First (Buffer);
      Last := Get_Enumeration_Vector_Last (Buffer);
      pragma Assume (Arrays.Enumeration_Vector.Is_Contained (Buffer (First .. Last)));
   end Get_Enumeration_Vector;

   procedure Get_AV_Enumeration_Vector (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) is
   begin
      First := Get_AV_Enumeration_Vector_First (Buffer);
      Last := Get_AV_Enumeration_Vector_Last (Buffer);
      pragma Assume (Arrays.AV_Enumeration_Vector.Is_Contained (Buffer (First .. Last)));
   end Get_AV_Enumeration_Vector;

end Arrays.Message;

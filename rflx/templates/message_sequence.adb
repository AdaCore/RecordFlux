package body {prefix}Message_Sequence with
  SPARK_Mode
is

   function Create return Context_Type is
     (Types.Index_Type'First, Types.Index_Type'First, Types.Bit_Index_Type'First, Types.Bit_Index_Type'First, 0, null, Types.Bit_Index_Type'First, S_Initial);

   procedure Initialize (Context : out Context_Type; Buffer : in out Types.Bytes_Ptr; Buffer_First : Types.Index_Type; Buffer_Last : Types.Index_Type; First : Types.Bit_Index_Type; Last : Types.Bit_Index_Type) is
      Buffer_Address : constant Types.Integer_Address := Types.Bytes_Address (Buffer);
   begin
      Context := (Buffer_First => Buffer_First, Buffer_Last => Buffer_Last, First => First, Last => Last, Buffer_Address => Buffer_Address, Buffer => Buffer, Index => First, State => S_Processing);
      Buffer := null;
   end Initialize;

   procedure Take_Buffer (Context : in out Context_Type; Buffer : out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index_Type; First, Last : Types.Bit_Index_Type) is
   begin
      Buffer := Context.Buffer;
      Context.Buffer := null;
   end Take_Buffer;

   function Valid_Element (Context : Context_Type) return Boolean is
     (Context.State = S_Processing and Context.Index <= Context.Last);

   function Valid (Context : Context_Type) return Boolean is
     (Context.State = S_Valid);

   function Has_Buffer (Context : Context_Type) return Boolean is
     (Context.Buffer /= null);

   procedure Switch (Context : in out Context_Type; Element_Context : out Element_Context_Type) is
      Buffer : Types.Bytes_Ptr := Context.Buffer;
   begin
      Context.Buffer := null;
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Element_Initialize (Element_Context, Buffer, Context.Index, Context.Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch;

   procedure Update (Context : in out Context_Type; Element_Context : in out Element_Context_Type) is
      Buffer        : Types.Bytes_Ptr;
      Valid_Element : Boolean := Element_Valid_Message (Element_Context);
   begin
      Element_Take_Buffer (Element_Context, Buffer);
      Context.Index := Element_Index (Element_Context);
      Context.Buffer := Buffer;
      if not Valid_Element then
         Context.State := S_Invalid;
      elsif Context.Index = Context.Last + 1 then
         Context.State := S_Valid;
      end if;
   end Update;

end {prefix}Message_Sequence;

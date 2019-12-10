package body {prefix}Message_Sequence with
  SPARK_Mode
is

   function Create return Context is
     (Types.Index'First, Types.Index'First, Types.Bit_Index'First, Types.Bit_Index'First, null, Types.Bit_Index'First, S_Initial);

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; Buffer_First : Types.Index; Buffer_Last : Types.Index; First : Types.Bit_Index; Last : Types.Bit_Index) is
   begin
      Ctx := (Buffer_First => Buffer_First, Buffer_Last => Buffer_Last, First => First, Last => Last, Buffer => Buffer, Index => First, State => S_Processing);
      Buffer := null;
   end Initialize;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Valid_Element (Ctx : Context) return Boolean is
     (Ctx.State = S_Processing and Ctx.Index <= Ctx.Last);

   function Valid (Ctx : Context) return Boolean is
     (Ctx.State = S_Valid);

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   procedure Switch (Ctx : in out Context; Element_Ctx : out Element_Context) is
      Buffer : Types.Bytes_Ptr := Ctx.Buffer;
   begin
      Ctx.Buffer := null;
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Element_Initialize (Element_Ctx, Buffer, Ctx.Index, Ctx.Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch;

   procedure Update (Ctx : in out Context; Element_Ctx : in out Element_Context) is
      Buffer        : Types.Bytes_Ptr;
   begin
      Element_Take_Buffer (Element_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Element_Valid_Message (Element_Ctx) then
         Ctx.Index := Element_Last (Element_Ctx) + 1;
         if Ctx.Index = Ctx.Last + 1 then
            Ctx.State := S_Valid;
         end if;
      else
         Ctx.State := S_Invalid;
      end if;
   end Update;

end {prefix}Message_Sequence;

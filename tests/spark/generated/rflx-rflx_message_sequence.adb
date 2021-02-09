pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package body RFLX.RFLX_Message_Sequence with
  SPARK_Mode
is

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, Buffer'First, Buffer'Last, Types.First_Bit_Index (Buffer'First), Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index) is
   begin
      Ctx := (Buffer_First => Buffer_First, Buffer_Last => Buffer_Last, First => First, Last => Last, Buffer => Buffer, Sequence_Last => First - 1, State => S_Valid);
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Ctx.Sequence_Last := Ctx.First - 1;
      Ctx.State := S_Valid;
   end Reset;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   procedure Copy (Ctx : Context; Buffer : out Types.Bytes) is
   begin
      if Buffer'Length > 0 then
         Buffer := Ctx.Buffer.all (Types.Byte_Index (Ctx.First) .. Types.Byte_Index (Ctx.Sequence_Last));
      else
         Buffer := Ctx.Buffer.all (Types.Index'Last .. Types.Index'First);
      end if;
   end Copy;

   procedure Switch (Ctx : in out Context; Element_Ctx : out Element_Context) is
      Buffer : Types.Bytes_Ptr := Ctx.Buffer;
   begin
      Ctx.Buffer := null;
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Element_Initialize (Element_Ctx, Buffer, Ctx.Sequence_Last + 1, Ctx.Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch;

   procedure Update (Ctx : in out Context; Element_Ctx : in out Element_Context) is
      Buffer        : Types.Bytes_Ptr;
      Valid_Message : constant Boolean := Element_Valid_Message (Element_Ctx);
      Last          : Types.Bit_Index := Types.Bit_Index'First;
   begin
      if Valid_Message then
         Last := Element_Last (Element_Ctx);
      end if;
      Element_Take_Buffer (Element_Ctx, Buffer);
      Ctx.Buffer := Buffer;
      if Valid_Message then
         Ctx.Sequence_Last := Last;
      else
         Ctx.State := S_Invalid;
      end if;
   end Update;

end RFLX.RFLX_Message_Sequence;

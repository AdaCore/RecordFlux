pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package body RFLX.RFLX_Scalar_Sequence with
  SPARK_Mode
is

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, Buffer'First, Buffer'Last, Types.First_Bit_Index (Buffer'First), Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index) is
   begin
      Ctx := (Buffer_First => Buffer_First, Buffer_Last => Buffer_Last, First => First, Last => Last, Buffer => Buffer, Sequence_Last => First - 1, State => S_Valid, First_Element => Element_Base_Type'First, Next_Element => Element_Base_Type'First);
      Buffer := null;
   end Initialize;

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

   procedure Next (Ctx : in out Context) is
      Last_Bit     : constant Types.Bit_Index := Ctx.Sequence_Last + Element_Base_Type'Size;
      Buffer_First : constant Types.Index := Types.Byte_Index (Ctx.Sequence_Last + 1);
      Buffer_Last  : constant Types.Index := Types.Byte_Index (Last_Bit);
      Offset       : constant Types.Offset := Types.Offset ((8 - (Last_Bit mod 8)) mod 8);
      function Extract is new Types.Extract (Element_Base_Type);
   begin
      if Buffer_First >= Ctx.Buffer'First and Buffer_Last <= Ctx.Buffer'Last and Buffer_First <= Buffer_Last then
         Ctx.Next_Element := Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         if Valid_Element (Ctx) then
            if Size (Ctx) = 0 then
               Ctx.First_Element := Ctx.Next_Element;
            end if;
         else
            Ctx.State := S_Invalid;
         end if;
      end if;
      Ctx.Sequence_Last := Ctx.Sequence_Last + Element_Base_Type'Size;
   end Next;

   function Get_Element (Ctx : Context) return Element_Type is
     (To_Actual (Ctx.Next_Element));

   function Head (Ctx : Context) return Element_Type is
     (To_Actual (Ctx.First_Element));

   procedure Append_Element (Ctx : in out Context; Value : Element_Type) is
      Last_Bit : Types.Bit_Index;
      First    : Types.Index;
      Last     : Types.Index;
      Offset   : Types.Offset;
      procedure Insert is new Types.Insert (Element_Base_Type);
   begin
      Last_Bit := Ctx.Sequence_Last + Element_Base_Type'Size;
      First := Types.Byte_Index (Ctx.Sequence_Last + 1);
      Last := Types.Byte_Index (Last_Bit);
      Offset := Types.Offset ((8 - (Last_Bit mod 8)) mod 8);
      if First >= Ctx.Buffer'First and Last <= Ctx.Buffer'Last and First <= Last then
         Insert (To_Base (Value), Ctx.Buffer.all (First .. Last), Offset);
      end if;
      if Size (Ctx) = 0 then
         Ctx.First_Element := To_Base (Value);
      end if;
      Ctx.Sequence_Last := Ctx.Sequence_Last + Element_Base_Type'Size;
   end Append_Element;

end RFLX.RFLX_Scalar_Sequence;

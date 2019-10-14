package body RFLX.Scalar_Sequence with
  SPARK_Mode
is

   function Create return Context is
     (Types.Index'First, Types.Index'First, Types.Bit_Index'First, Types.Bit_Index'First, 0, null, Types.Bit_Index'First, S_Initial, Element_Base_Type'First);

   procedure Read_Next_Element (Ctx : in out Context) with
     Pre => Has_Buffer (Ctx),
     Post => Has_Buffer (Ctx) and Ctx.Buffer_First = Ctx.Buffer_First'Old and Ctx.Buffer_Last = Ctx.Buffer_Last'Old and Ctx.Buffer_Address = Ctx.Buffer_Address'Old
   is
      Last_Bit : Types.Bit_Index;
      First    : Types.Index;
      Last     : Types.Index;
      Offset   : Types.Offset;
   begin
      if Ctx.Last - Ctx.Index + 1 >= Element_Base_Type'Size then
         Last_Bit := Ctx.Index + Element_Base_Type'Size - 1;
         First := Types.Byte_Index (Ctx.Index);
         Last := Types.Byte_Index (Last_Bit);
         Offset := Types.Offset ((8 - (Last_Bit mod 8)) mod 8);
         if First >= Ctx.Buffer'First and Last <= Ctx.Buffer'Last and First <= Last then
            Ctx.Next_Element := Extract_Element_Base_Type (Ctx.Buffer.all (First .. Last), Offset);
         end if;
      else
         Ctx.State := S_Invalid;
      end if;
   end Read_Next_Element;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index) is
      Buffer_Address : constant Types.Integer_Address := Types.Bytes_Address (Buffer);
   begin
      Ctx := (Buffer_First => Buffer_First, Buffer_Last => Buffer_Last, First => First, Last => Last, Buffer_Address => Buffer_Address, Buffer => Buffer, Index => First, State => S_Processing, Next_Element => Element_Base_Type'First);
      Buffer := null;
      Read_Next_Element (Ctx);
   end Initialize;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   procedure Next (Ctx : in out Context) is
   begin
      if Ctx.State = S_Valid or Ctx.State = S_Initial then
         return;
      elsif Ctx.Last - Ctx.Index + 1 >= Element_Base_Type'Size then
         Ctx.Index := Ctx.Index + Element_Base_Type'Size;
         if Ctx.Index = Ctx.Last + 1 then
            Ctx.State := S_Valid;
            return;
         end if;
      end if;
      Read_Next_Element (Ctx);
   end Next;

   function Valid_Element (Ctx : Context) return Boolean is
     (Ctx.State = S_Processing and then Valid_Element_Type (Ctx.Next_Element));

   function Get_Element (Ctx : Context) return Element_Type is
     (Convert_To_Element_Type (Ctx.Next_Element));

   function Valid (Ctx : Context) return Boolean is
     (Ctx.State = S_Valid);

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

end RFLX.Scalar_Sequence;

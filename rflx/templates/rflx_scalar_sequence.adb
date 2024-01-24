pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
with {prefix}RFLX_Types.Operations;

package body {prefix}RFLX_Scalar_Sequence with
  SPARK_Mode
is

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, RFLX_Types.To_First_Bit_Index (Buffer'First), RFLX_Types.To_Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length)
   is
      Buffer_First : constant RFLX_Types.Index := Buffer'First;
      Buffer_Last : constant RFLX_Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First => Buffer_First, Buffer_Last => Buffer_Last, First => First, Last => Last, Buffer => Buffer, Sequence_Last => First - 1, State => S_Valid, First_Element => {prefix}RFLX_Types.Base_Integer'First, Next_Element => {prefix}RFLX_Types.Base_Integer'First);
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Ctx.Sequence_Last := Ctx.First - 1;
      Ctx.State := S_Valid;
   end Reset;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX_Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   procedure Copy (Ctx : Context; Buffer : out RFLX_Types.Bytes) is
   begin
      if Buffer'Length > 0 then
         Buffer := Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Sequence_Last));
      else
         Buffer := Ctx.Buffer.all (RFLX_Types.Index'Last .. RFLX_Types.Index'First);
      end if;
   end Copy;

   procedure Next (Ctx : in out Context) is
      Last_Bit     : constant RFLX_Types.Bit_Index := Ctx.Sequence_Last + {prefix}RFLX_Types.Bit_Index (Element_Size);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Sequence_Last + 1);
      Buffer_Last  : constant RFLX_Types.Index := RFLX_Types.To_Index (Last_Bit);
      Offset       : constant RFLX_Types.Offset := RFLX_Types.Offset ((8 - (Last_Bit mod 8)) mod 8);
   begin
      if Buffer_First >= Ctx.Buffer'First and Buffer_Last <= Ctx.Buffer'Last and Buffer_First <= Buffer_Last then
         Ctx.Next_Element := {prefix}RFLX_Types.Operations.Extract (Ctx.Buffer.all, Buffer_First, Buffer_Last, Offset, Element_Size, RFLX_Types.High_Order_First);
         if Valid_Element (Ctx) then
            if Size (Ctx) = 0 then
               Ctx.First_Element := Ctx.Next_Element;
            end if;
         else
            Ctx.State := S_Invalid;
         end if;
      end if;
      Ctx.Sequence_Last := Ctx.Sequence_Last + {prefix}RFLX_Types.Bit_Index (Element_Size);
   end Next;

   function Get_Element (Ctx : Context) return Element_Type is
     (To_Actual (Ctx.Next_Element));

   function Head (Ctx : Context) return Element_Type is
     (To_Actual (Ctx.First_Element));

   procedure Append_Element (Ctx : in out Context; Value : Element_Type) is
      Last_Bit : RFLX_Types.Bit_Index;
      First    : RFLX_Types.Index;
      Last     : RFLX_Types.Index;
      Offset   : RFLX_Types.Offset;
   begin
      Last_Bit := Ctx.Sequence_Last + {prefix}RFLX_Types.Bit_Index (Element_Size);
      First := RFLX_Types.To_Index (Ctx.Sequence_Last + 1);
      Last := RFLX_Types.To_Index (Last_Bit);
      Offset := RFLX_Types.Offset ((8 - (Last_Bit mod 8)) mod 8);
      if First >= Ctx.Buffer'First and Last <= Ctx.Buffer'Last and First <= Last then
         {prefix}RFLX_Types.Operations.Insert (To_Base_Int (Value), Ctx.Buffer.all, First, Last, Offset, Element_Size, RFLX_Types.High_Order_First);
      end if;
      if Size (Ctx) = 0 then
         Ctx.First_Element := To_Base_Int (Value);
      end if;
      Ctx.Sequence_Last := Ctx.Sequence_Last + {prefix}RFLX_Types.Bit_Index (Element_Size);
   end Append_Element;

   procedure Data (Ctx : Context; Data : out RFLX_Types.Bytes) is
   begin
      if Data'Length > 0 then
         Data := Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Sequence_Last));
      else
         Data := Ctx.Buffer.all (1 .. 0);
      end if;
   end Data;

end {prefix}RFLX_Scalar_Sequence;

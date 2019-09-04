package body {prefix}Scalar_Sequence with
  SPARK_Mode
is

   function Create return Context_Type is
     (Types.Index_Type'First, Types.Index_Type'First, Types.Bit_Index_Type'First, Types.Bit_Index_Type'First, 0, null, Types.Bit_Index_Type'First, S_Initial, Element_Base_Type'First);

   procedure Read_Next_Element (Context : in out Context_Type) with
     Pre => Has_Buffer (Context),
     Post => Has_Buffer (Context) and Context.Buffer_First = Context.Buffer_First'Old and Context.Buffer_Last = Context.Buffer_Last'Old and Context.Buffer_Address = Context.Buffer_Address'Old
   is
      Last_Bit : Types.Bit_Index_Type;
      First    : Types.Index_Type;
      Last     : Types.Index_Type;
      Offset   : Types.Offset_Type;
   begin
      if Context.Last - Context.Index + 1 >= Element_Base_Type'Size then
         Last_Bit := Context.Index + Element_Base_Type'Size - 1;
         First := Types.Byte_Index (Context.Index);
         Last := Types.Byte_Index (Last_Bit);
         Offset := Types.Offset_Type ((8 - (Last_Bit mod 8)) mod 8);
         if First >= Context.Buffer'First and Last <= Context.Buffer'Last and First <= Last then
            Context.Next_Element := Convert_To_Element_Base_Type (Context.Buffer.all (First .. Last), Offset);
         end if;
      else
         Context.State := S_Invalid;
      end if;
   end Read_Next_Element;

   procedure Initialize (Context : out Context_Type; Buffer : in out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index_Type; First, Last : Types.Bit_Index_Type) is
      Buffer_Address : constant Types.Integer_Address := Types.Bytes_Address (Buffer);
   begin
      Context := (Buffer_First => Buffer_First, Buffer_Last => Buffer_Last, First => First, Last => Last, Buffer_Address => Buffer_Address, Buffer => Buffer, Index => First, State => S_Processing, Next_Element => Element_Base_Type'First);
      Buffer := null;
      Read_Next_Element (Context);
   end Initialize;

   procedure Take_Buffer (Context : in out Context_Type; Buffer : out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index_Type) is
   begin
      Buffer := Context.Buffer;
      Context.Buffer := null;
   end Take_Buffer;

   procedure Next (Context : in out Context_Type) is
   begin
      if Context.State = S_Valid or Context.State = S_Initial then
         return;
      elsif Context.Last - Context.Index + 1 >= Element_Base_Type'Size then
         Context.Index := Context.Index + Element_Base_Type'Size;
         if Context.Index = Context.Last + 1 then
            Context.State := S_Valid;
            return;
         end if;
      end if;
      Read_Next_Element (Context);
   end Next;

   function Valid_Element (Context : Context_Type) return Boolean is
     (Context.State = S_Processing and then Valid_Element_Type (Context.Next_Element));

   function Get_Element (Context : Context_Type) return Element_Type is
     (Convert_To_Element_Type (Context.Next_Element));

   function Valid (Context : Context_Type) return Boolean is
     (Context.State = S_Valid);

   function Has_Buffer (Context : Context_Type) return Boolean is
     (Context.Buffer /= null);

end {prefix}Scalar_Sequence;

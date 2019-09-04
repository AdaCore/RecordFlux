with {prefix}Types; use type {prefix}Types.Bytes_Ptr, {prefix}Types.Length_Type, {prefix}Types.Bit_Length_Type, {prefix}Types.Integer_Address;

generic
   type Element_Context_Type (Buffer_First, Buffer_Last : Types.Index_Type; First, Last : Types.Bit_Index_Type; Buffer_Address : Types.Integer_Address) is private;
   with procedure Element_Initialize (Context : out Element_Context_Type; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index_Type);
   with procedure Element_Take_Buffer (Context : in out Element_Context_Type; Buffer : out Types.Bytes_Ptr);
   with function Element_Has_Buffer (Element_Context : Element_Context_Type) return Boolean;
   with function Element_Index (Element_Context : Element_Context_Type) return Types.Bit_Index_Type;
   with function Element_Valid_Message (Element_Context : Element_Context_Type) return Boolean;
   with function Element_Valid_Context (Element_Context : Element_Context_Type) return Boolean;
package {prefix}Message_Sequence with
  SPARK_Mode
is

   type Context_Type (Buffer_First, Buffer_Last : Types.Index_Type := Types.Index_Type'First; First, Last : Types.Bit_Index_Type := Types.Bit_Index_Type'First; Buffer_Address : Types.Integer_Address := 0) is private with
     Default_Initial_Condition => False;

   function Create return Context_Type;

   procedure Initialize (Context : out Context_Type; Buffer : in out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index_Type; First, Last : Types.Bit_Index_Type) with
     Pre =>
       (not Context'Constrained
        and then Buffer /= null
        and then Buffer'First = Buffer_First
        and then Buffer'Last = Buffer_Last
        and then Types.Byte_Index (First) >= Buffer'First
        and then Types.Byte_Index (Last) <= Buffer'Last
        and then First <= Last
        and then Last <= Types.Bit_Index_Type'Last / 2),
     Post =>
       (Buffer = null
        and Has_Buffer (Context)
        and Context.Buffer_First = Buffer_First
        and Context.Buffer_Last = Buffer_Last
        and Context.Buffer_Address = Types.Bytes_Address (Buffer)'Old);

   procedure Take_Buffer (Context : in out Context_Type; Buffer : out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index_Type; First, Last : Types.Bit_Index_Type) with
     Pre =>
       (Has_Buffer (Context)
        and then Context.Buffer_First = Buffer_First
        and then Context.Buffer_Last = Buffer_Last
        and then Context.Buffer_First <= Types.Byte_Index (First)
        and then Context.Buffer_Last >= Types.Byte_Index (Last)),
     Post =>
       (not Has_Buffer (Context)
        and Buffer /= null
        and Buffer'First = Buffer_First
        and Buffer'Last = Buffer_Last
        and Buffer'First <= Types.Byte_Index (First)
        and Buffer'Last >= Types.Byte_Index (Last)
        and Context.Buffer_Address = Types.Bytes_Address (Buffer)
        and Context.Buffer_Address = Context.Buffer_Address'Old);

   function Valid_Element (Context : Context_Type) return Boolean with
     Contract_Cases =>
       (Has_Buffer (Context) => (Valid_Element'Result or not Valid_Element'Result) and Has_Buffer (Context),
        not Has_Buffer (Context) => (Valid_Element'Result or not Valid_Element'Result) and not Has_Buffer (Context));

   procedure Switch (Context : in out Context_Type; Element_Context : out Element_Context_Type) with
     Pre =>
       (not Element_Context'Constrained
        and then Has_Buffer (Context)
        and then Valid_Element (Context)),
     Post =>
       (not Has_Buffer (Context)
        and Valid_Element (Context)
        and Element_Valid_Context (Element_Context)
        and Element_Has_Buffer (Element_Context)
        and Context.Buffer_First = Element_Context.Buffer_First
        and Context.Buffer_Last = Element_Context.Buffer_Last
        and Context.Buffer_Address = Element_Context.Buffer_Address
        and Context.First <= Element_Context.First
        and Context.Last >= Element_Context.Last
        and Context.Buffer_First = Context.Buffer_First'Old
        and Context.Buffer_Last = Context.Buffer_Last'Old
        and Context.Buffer_Address = Context.Buffer_Address'Old);

   procedure Update (Context : in out Context_Type; Element_Context : in out Element_Context_Type) with
     Pre =>
       (not Has_Buffer (Context)
        and then Element_Valid_Context (Element_Context)
        and then Element_Has_Buffer (Element_Context)
        and then Valid_Element (Context)
        and then Context.Buffer_First = Element_Context.Buffer_First
        and then Context.Buffer_Last = Element_Context.Buffer_Last
        and then Context.Buffer_Address = Element_Context.Buffer_Address
        and then Context.First <= Element_Context.First
        and then Context.Last >= Element_Context.Last),
     Post =>
       (Has_Buffer (Context)
        and Element_Valid_Context (Element_Context)
        and not Element_Has_Buffer (Element_Context)
        and Context.Buffer_First = Context.Buffer_First'Old
        and Context.Buffer_Last = Context.Buffer_Last'Old
        and Context.Buffer_Address = Context.Buffer_Address'Old);

   function Valid (Context : Context_Type) return Boolean;

   function Has_Buffer (Context : Context_Type) return Boolean;

private

   type State_Type is (S_Initial, S_Processing, S_Valid, S_Invalid);

   use Types;

   type Context_Type (Buffer_First, Buffer_Last : Types.Index_Type := Types.Index_Type'First; First, Last : Types.Bit_Index_Type := Types.Bit_Index_Type'First; Buffer_Address : Types.Integer_Address := 0) is
      record
         Buffer : Types.Bytes_Ptr := null;
         Index  : Types.Bit_Index_Type := Types.Bit_Index_Type'First;
         State  : State_Type := S_Initial;
      end record with
     Dynamic_Predicate =>
       ((if Buffer /= null then
          (Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last
           and Types.Bytes_Address (Buffer) = Buffer_Address))
        and Types.Byte_Index (First) >= Buffer_First
        and Types.Byte_Index (Last) <= Buffer_Last
        and First <= Last
        and Last <= (Types.Bit_Index_Type'Last / 2)
        and Index >= First
        and Index - Last <= 1);

end {prefix}Message_Sequence;

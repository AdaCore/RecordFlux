with RFLX.Types; use type RFLX.Types.Bytes_Ptr, RFLX.Types.Length_Type, RFLX.Types.Bit_Length_Type, RFLX.Types.Integer_Address;

generic
   type Element_Type is private;
   type Element_Base_Type is (<>);
   with function Convert_To_Element_Base_Type (Buffer : Types.Bytes; Offset : Types.Offset_Type) return Element_Base_Type;
   with function Valid_Element_Type (Element : Element_Base_Type) return Boolean;
   with function Convert_To_Element_Type (Element : Element_Base_Type) return Element_Type;
package RFLX.Scalar_Sequence with
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

   procedure Take_Buffer (Context : in out Context_Type; Buffer : out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index_Type) with
     Pre =>
       (Has_Buffer (Context)
        and then Context.Buffer_First = Buffer_First
        and then Context.Buffer_Last = Buffer_Last),
     Post =>
       (not Has_Buffer (Context)
        and Buffer /= null
        and Buffer'First = Buffer_First
        and Buffer'Last = Buffer_Last
        and Context.Buffer_Address = Types.Bytes_Address (Buffer)
        and Context.Buffer_Address = Context.Buffer_Address'Old);

   procedure Next (Context : in out Context_Type) with
     Pre =>
       (Has_Buffer (Context)
        and then Valid_Element (Context)),
     Post =>
       (Has_Buffer (Context)
        and Context.Buffer_First = Context.Buffer_First'Old
        and Context.Buffer_Last = Context.Buffer_Last'Old
        and Context.Buffer_Address = Context.Buffer_Address'Old);

   function Valid_Element (Context : Context_Type) return Boolean with
     Contract_Cases =>
       (Has_Buffer (Context) => (Valid_Element'Result or not Valid_Element'Result)
        and Has_Buffer (Context),
        not Has_Buffer (Context) => (Valid_Element'Result or not Valid_Element'Result)
        and not Has_Buffer (Context));

   function Get_Element (Context : Context_Type) return Element_Type with
     Pre =>
       Valid_Element (Context);

   function Valid (Context : Context_Type) return Boolean;

   function Has_Buffer (Context : Context_Type) return Boolean;

private

   type State_Type is (S_Initial, S_Processing, S_Valid, S_Invalid);

   use Types;

   type Context_Type (Buffer_First, Buffer_Last : Types.Index_Type := Types.Index_Type'First; First, Last : Types.Bit_Index_Type := Types.Bit_Index_Type'First; Buffer_Address : Types.Integer_Address := 0) is
      record
         Buffer       : Types.Bytes_Ptr := null;
         Index        : Types.Bit_Index_Type := Types.Bit_Index_Type'First;
         State        : State_Type := S_Initial;
         Next_Element : Element_Base_Type := Element_Base_Type'First;
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

end RFLX.Scalar_Sequence;

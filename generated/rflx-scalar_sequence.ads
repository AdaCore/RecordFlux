with RFLX.Types; use type RFLX.Types.Bytes_Ptr, RFLX.Types.Length, RFLX.Types.Bit_Length, RFLX.Types.Integer_Address;

generic
   type Element_Type is private;
   type Element_Base_Type is (<>);
   with function Extract_Element_Base_Type (Buffer : Types.Bytes; Offset : Types.Offset) return Element_Base_Type;
   with function Valid_Element_Type (Element : Element_Base_Type) return Boolean;
   with function Convert_To_Element_Type (Element : Element_Base_Type) return Element_Type;
package RFLX.Scalar_Sequence with
  SPARK_Mode
is

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First; Buffer_Address : Types.Integer_Address := 0) is private with
     Default_Initial_Condition => False;

   function Create return Context;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index) with
     Pre =>
       (not Ctx'Constrained
        and then Buffer /= null
        and then Buffer'First = Buffer_First
        and then Buffer'Last = Buffer_Last
        and then Types.Byte_Index (First) >= Buffer'First
        and then Types.Byte_Index (Last) <= Buffer'Last
        and then First <= Last
        and then Last <= Types.Bit_Index'Last / 2),
     Post =>
       (Buffer = null
        and Has_Buffer (Ctx)
        and Ctx.Buffer_First = Buffer_First
        and Ctx.Buffer_Last = Buffer_Last
        and Ctx.Buffer_Address = Types.Bytes_Address (Buffer)'Old);

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index) with
     Pre =>
       (Has_Buffer (Ctx)
        and then Ctx.Buffer_First = Buffer_First
        and then Ctx.Buffer_Last = Buffer_Last),
     Post =>
       (not Has_Buffer (Ctx)
        and Buffer /= null
        and Buffer'First = Buffer_First
        and Buffer'Last = Buffer_Last
        and Ctx.Buffer_Address = Types.Bytes_Address (Buffer)
        and Ctx.Buffer_Address = Ctx.Buffer_Address'Old);

   procedure Next (Ctx : in out Context) with
     Pre =>
       (Has_Buffer (Ctx)
        and then Valid_Element (Ctx)),
     Post =>
       (Has_Buffer (Ctx)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.Buffer_Address = Ctx.Buffer_Address'Old);

   function Valid_Element (Ctx : Context) return Boolean with
     Contract_Cases =>
       (Has_Buffer (Ctx) => (Valid_Element'Result or not Valid_Element'Result)
        and Has_Buffer (Ctx),
        not Has_Buffer (Ctx) => (Valid_Element'Result or not Valid_Element'Result)
        and not Has_Buffer (Ctx));

   function Get_Element (Ctx : Context) return Element_Type with
     Pre =>
       Valid_Element (Ctx);

   function Valid (Ctx : Context) return Boolean;

   function Has_Buffer (Ctx : Context) return Boolean;

private

   type Context_State is (S_Initial, S_Processing, S_Valid, S_Invalid);

   use Types;

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First; Buffer_Address : Types.Integer_Address := 0) is
      record
         Buffer       : Types.Bytes_Ptr := null;
         Index        : Types.Bit_Index := Types.Bit_Index'First;
         State        : Context_State := S_Initial;
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
        and Last <= (Types.Bit_Index'Last / 2)
        and Index >= First
        and Index - Last <= 1);

end RFLX.Scalar_Sequence;

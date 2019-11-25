with {prefix}Types; use type {prefix}Types.Bytes_Ptr, {prefix}Types.Length, {prefix}Types.Bit_Length;

generic
   type Element_Type is private;
   type Element_Base_Type is (<>);
   with function Extract (Buffer : Types.Bytes; Offset : Types.Offset) return Element_Base_Type;
   with procedure Insert (Value : Element_Base_Type; Buffer : in out Types.Bytes; Offset : Types.Offset);
   with function Valid (Element : Element_Base_Type) return Boolean;
   with function Convert (Element : Element_Base_Type) return Element_Type;
   with function Convert (Element : Element_Type) return Element_Base_Type;
package {prefix}Scalar_Sequence with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Scalar_Sequence);

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is private with
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
        and Ctx.First = First
        and Ctx.Last = Last
        and Index (Ctx) = First);

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
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old
        and Index (Ctx) = Index (Ctx)'Old);

   procedure Next (Ctx : in out Context) with
     Pre =>
       (Has_Buffer (Ctx)
        and then Valid_Element (Ctx)),
     Post =>
       (Has_Buffer (Ctx)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old);

   function Valid_Element (Ctx : Context) return Boolean with
     Contract_Cases =>
       (Has_Buffer (Ctx) => (Valid_Element'Result or not Valid_Element'Result)
        and Has_Buffer (Ctx),
        not Has_Buffer (Ctx) => (Valid_Element'Result or not Valid_Element'Result)
        and not Has_Buffer (Ctx));

   function Get_Element (Ctx : Context) return Element_Type with
     Pre =>
       Valid_Element (Ctx);

   procedure Append_Element (Ctx : in out Context; Value : Element_Type) with
     Pre =>
       (Has_Buffer (Ctx)
        and then Valid (Convert (Value))
        and then Available_Space (Ctx) >= Element_Base_Type'Size),
     Post =>
       (Has_Buffer (Ctx)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old
        and Index (Ctx) = Index (Ctx)'Old + Element_Base_Type'Size);

   function Valid (Ctx : Context) return Boolean;

   function Has_Buffer (Ctx : Context) return Boolean;

   function Index (Ctx : Context) return Types.Bit_Index with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Available_Space (Ctx : Context) return Types.Bit_Length with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Context_State is (S_Initial, S_Processing, S_Valid, S_Invalid);

   use Types;

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is
      record
         Buffer       : Types.Bytes_Ptr := null;
         Index        : Types.Bit_Index := Types.Bit_Index'First;
         State        : Context_State := S_Initial;
         Next_Element : Element_Base_Type := Element_Base_Type'First;
      end record with
     Dynamic_Predicate =>
       ((if Buffer /= null then
          (Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last))
        and Types.Byte_Index (First) >= Buffer_First
        and Types.Byte_Index (Last) <= Buffer_Last
        and First <= Last
        and Last <= (Types.Bit_Index'Last / 2)
        and Index >= First
        and Index - Last <= 1);

   function Available_Space (Ctx : Context) return Types.Bit_Length is
      (Ctx.Last - Ctx.Index + 1);

end {prefix}Scalar_Sequence;

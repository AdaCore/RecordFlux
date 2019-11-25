with {prefix}Types; use type {prefix}Types.Bytes_Ptr, {prefix}Types.Length, {prefix}Types.Bit_Length;

generic
   type Element_Context (Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index) is private;
   with procedure Element_Initialize (Ctx : out Element_Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index);
   with procedure Element_Take_Buffer (Ctx : in out Element_Context; Buffer : out Types.Bytes_Ptr);
   with function Element_Has_Buffer (Ctx : Element_Context) return Boolean;
   with function Element_Last (Ctx : Element_Context) return Types.Bit_Index;
   with function Element_Initialized (Ctx : Element_Context) return Boolean;
   with function Element_Valid_Message (Ctx : Element_Context) return Boolean;
   with function Element_Valid_Context (Ctx : Element_Context) return Boolean;
package {prefix}Message_Sequence with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Message_Sequence);

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

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index) with
     Pre =>
       (Has_Buffer (Ctx)
        and then Ctx.Buffer_First = Buffer_First
        and then Ctx.Buffer_Last = Buffer_Last
        and then Ctx.Buffer_First <= Types.Byte_Index (First)
        and then Ctx.Buffer_Last >= Types.Byte_Index (Last)),
     Post =>
       (not Has_Buffer (Ctx)
        and Buffer /= null
        and Buffer'First = Buffer_First
        and Buffer'Last = Buffer_Last
        and Buffer'First <= Types.Byte_Index (First)
        and Buffer'Last >= Types.Byte_Index (Last)
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old
        and Index (Ctx) = Index (Ctx)'Old);

   function Valid_Element (Ctx : Context) return Boolean with
     Contract_Cases =>
       (Has_Buffer (Ctx) => (Valid_Element'Result or not Valid_Element'Result) and Has_Buffer (Ctx),
        not Has_Buffer (Ctx) => (Valid_Element'Result or not Valid_Element'Result) and not Has_Buffer (Ctx));

   procedure Switch (Ctx : in out Context; Element_Ctx : out Element_Context) with
     Pre =>
       (not Element_Ctx'Constrained
        and then Has_Buffer (Ctx)
        and then Valid_Element (Ctx)),
     Post =>
       (not Has_Buffer (Ctx)
        and Valid_Element (Ctx)
        and Element_Valid_Context (Element_Ctx)
        and Element_Has_Buffer (Element_Ctx)
        and Ctx.Buffer_First = Element_Ctx.Buffer_First
        and Ctx.Buffer_Last = Element_Ctx.Buffer_Last
        and Ctx.First <= Element_Ctx.First
        and Ctx.Last >= Element_Ctx.Last
        and Element_Ctx.First = Index (Ctx)
        and Element_Ctx.Last = Ctx.Last
        and Element_Initialized (Element_Ctx)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old
        and Index (Ctx) = Index (Ctx)'Old);

   procedure Update (Ctx : in out Context; Element_Ctx : in out Element_Context) with
     Pre =>
       (not Has_Buffer (Ctx)
        and then Element_Valid_Context (Element_Ctx)
        and then Element_Has_Buffer (Element_Ctx)
        and then Valid_Element (Ctx)
        and then Ctx.Buffer_First = Element_Ctx.Buffer_First
        and then Ctx.Buffer_Last = Element_Ctx.Buffer_Last
        and then Ctx.First <= Element_Ctx.First
        and then Ctx.Last >= Element_Ctx.Last),
     Post =>
       (Has_Buffer (Ctx)
        and Element_Valid_Context (Element_Ctx)
        and not Element_Has_Buffer (Element_Ctx)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old),
     Contract_Cases =>
       (Element_Valid_Message (Element_Ctx) =>
          (Index (Ctx) = Element_Last (Element_Ctx) + 1
           and Element_Last (Element_Ctx) = Element_Last (Element_Ctx)'Old),
        others =>
          True);

   function Valid (Ctx : Context) return Boolean;

   function Has_Buffer (Ctx : Context) return Boolean;

   function Index (Ctx : Context) return Types.Bit_Index with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Context_State is (S_Initial, S_Processing, S_Valid, S_Invalid);

   use Types;

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is
      record
         Buffer : Types.Bytes_Ptr := null;
         Index  : Types.Bit_Index := Types.Bit_Index'First;
         State  : Context_State := S_Initial;
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

   function Index (Ctx : Context) return Types.Bit_Index is
      (Ctx.Index);

end {prefix}Message_Sequence;

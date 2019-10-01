with RFLX.Types;
use type RFLX.Types.Integer_Address;
with RFLX.Scalar_Sequence;

generic
   with package Modular_Vector_Sequence is new Scalar_Sequence (<>);
   with package Range_Vector_Sequence is new Scalar_Sequence (<>);
   with package Enumeration_Vector_Sequence is new Scalar_Sequence (<>);
   with package AV_Enumeration_Vector_Sequence is new Scalar_Sequence (<>);
package RFLX.Arrays.Generic_Message with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   type Virtual_Field is (F_Initial, F_Length, F_Modular_Vector, F_Range_Vector, F_Enumeration_Vector, F_AV_Enumeration_Vector, F_Final);

   subtype Field is Virtual_Field range F_Length .. F_AV_Enumeration_Vector;

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First; Buffer_Address : RFLX.Types.Integer_Address := 0) is private with
     Default_Initial_Condition =>
       False;

   function Create return Context;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then Buffer'Last <= RFLX.Types.Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Ctx.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and then Ctx.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and then Buffer = null;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then RFLX.Types.Byte_Index (First) >= Buffer'First
          and then RFLX.Types.Byte_Index (Last) <= Buffer'Last
          and then First <= Last
          and then Last <= RFLX.Types.Bit_Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and then Buffer = null
          and then Has_Buffer (Ctx)
          and then Ctx.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and then Ctx.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and then Ctx.Buffer_Address = RFLX.Types.Bytes_Address (Buffer)'Old
          and then Ctx.First = First
          and then Ctx.Last = Last;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX.Types.Bytes_Ptr) with
     Pre =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx),
     Post =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Buffer /= null
          and then Ctx.Buffer_First = Buffer'First
          and then Ctx.Buffer_Last = Buffer'Last
          and then Ctx.Buffer_Address = RFLX.Types.Bytes_Address (Buffer)
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.Buffer_Address = Ctx.Buffer_Address'Old
          and then Ctx.First = Ctx.First'Old
          and then Ctx.Last = Ctx.Last'Old
          and then Present (Ctx, F_Length) = Present (Ctx, F_Length)'Old
          and then Present (Ctx, F_Modular_Vector) = Present (Ctx, F_Modular_Vector)'Old
          and then Present (Ctx, F_Range_Vector) = Present (Ctx, F_Range_Vector)'Old
          and then Present (Ctx, F_Enumeration_Vector) = Present (Ctx, F_Enumeration_Vector)'Old
          and then Present (Ctx, F_AV_Enumeration_Vector) = Present (Ctx, F_AV_Enumeration_Vector)'Old;

   function Has_Buffer (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   procedure Field_Range (Ctx : Context; Fld : Field; First : out RFLX.Types.Bit_Index; Last : out RFLX.Types.Bit_Index) with
     Pre =>
       Valid_Context (Ctx)
          and then Present (Ctx, Fld),
     Post =>
       Present (Ctx, Fld)
          and then Ctx.First <= First
          and then Ctx.Last >= Last
          and then First <= Last;

   function Index (Ctx : Context) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Index'Result >= Ctx.First
          and then Index'Result - Ctx.Last <= 1;

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and then (if Fld /= F_Length then (if Valid (Ctx, F_Length)'Old then Valid (Ctx, F_Length)))
          and then (if Fld /= F_Modular_Vector then (if Valid (Ctx, F_Modular_Vector)'Old then Valid (Ctx, F_Modular_Vector)))
          and then (if Fld /= F_Range_Vector then (if Valid (Ctx, F_Range_Vector)'Old then Valid (Ctx, F_Range_Vector)))
          and then (if Fld /= F_Enumeration_Vector then (if Valid (Ctx, F_Enumeration_Vector)'Old then Valid (Ctx, F_Enumeration_Vector)))
          and then (if Fld /= F_AV_Enumeration_Vector then (if Valid (Ctx, F_AV_Enumeration_Vector)'Old then Valid (Ctx, F_AV_Enumeration_Vector)))
          and then Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.Buffer_Address = Ctx.Buffer_Address'Old
          and then Ctx.First = Ctx.First'Old
          and then Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.Buffer_Address = Ctx.Buffer_Address'Old
          and then Ctx.First = Ctx.First'Old
          and then Ctx.Last = Ctx.Last'Old;

   function Present (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       (if Valid'Result then Present (Ctx, Fld)
          and then Structural_Valid (Ctx, Fld));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Structural_Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Incomplete_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Get_Length (Ctx : Context) return Length with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Length);

   generic
      with procedure Process_Modular_Vector (Modular_Vector : RFLX.Types.Bytes);
   procedure Get_Modular_Vector (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Modular_Vector);

   generic
      with procedure Process_Range_Vector (Range_Vector : RFLX.Types.Bytes);
   procedure Get_Range_Vector (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Range_Vector);

   generic
      with procedure Process_Enumeration_Vector (Enumeration_Vector : RFLX.Types.Bytes);
   procedure Get_Enumeration_Vector (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Enumeration_Vector);

   generic
      with procedure Process_AV_Enumeration_Vector (AV_Enumeration_Vector : RFLX.Types.Bytes);
   procedure Get_AV_Enumeration_Vector (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_AV_Enumeration_Vector);

   procedure Switch (Ctx : in out Context; Sequence_Context : out Modular_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then not Sequence_Context'Constrained
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Modular_Vector),
     Post =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Present (Ctx, F_Modular_Vector)
          and then Modular_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Ctx.Buffer_Address = Sequence_Context.Buffer_Address;

   procedure Switch (Ctx : in out Context; Sequence_Context : out Range_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then not Sequence_Context'Constrained
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Range_Vector),
     Post =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Present (Ctx, F_Range_Vector)
          and then Range_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Ctx.Buffer_Address = Sequence_Context.Buffer_Address;

   procedure Switch (Ctx : in out Context; Sequence_Context : out Enumeration_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then not Sequence_Context'Constrained
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Enumeration_Vector),
     Post =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Present (Ctx, F_Enumeration_Vector)
          and then Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Ctx.Buffer_Address = Sequence_Context.Buffer_Address;

   procedure Switch (Ctx : in out Context; Sequence_Context : out AV_Enumeration_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then not Sequence_Context'Constrained
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_AV_Enumeration_Vector),
     Post =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Present (Ctx, F_AV_Enumeration_Vector)
          and then AV_Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Ctx.Buffer_Address = Sequence_Context.Buffer_Address;

   procedure Update (Ctx : in out Context; Sequence_Context : in out Modular_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Present (Ctx, F_Modular_Vector)
          and then Modular_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Ctx.Buffer_Address = Sequence_Context.Buffer_Address,
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then not Modular_Vector_Sequence.Has_Buffer (Sequence_Context);

   procedure Update (Ctx : in out Context; Sequence_Context : in out Range_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Present (Ctx, F_Range_Vector)
          and then Range_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Ctx.Buffer_Address = Sequence_Context.Buffer_Address,
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then not Range_Vector_Sequence.Has_Buffer (Sequence_Context);

   procedure Update (Ctx : in out Context; Sequence_Context : in out Enumeration_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Present (Ctx, F_Enumeration_Vector)
          and then Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Ctx.Buffer_Address = Sequence_Context.Buffer_Address,
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then not Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context);

   procedure Update (Ctx : in out Context; Sequence_Context : in out AV_Enumeration_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Present (Ctx, F_AV_Enumeration_Vector)
          and then AV_Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Ctx.Buffer_Address = Sequence_Context.Buffer_Address,
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then not AV_Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context);

   function Valid_Context (Ctx : Context) return Boolean;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Preliminary, S_Incomplete);

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector | F_Final =>
               null;
            when F_Length =>
               Length_Value : Length;
         end case;
      end record;

   function Valid_Value (Value : Field_Dependent_Value) return Boolean is
     ((case Value.Fld is
         when F_Length =>
            Valid (Value.Length_Value),
         when F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector =>
            True,
         when F_Initial | F_Final =>
            False));

   type Field_Cursor (State : Cursor_State := S_Invalid) is
      record
         case State is
            when S_Valid | S_Structural_Valid | S_Preliminary =>
               First : RFLX.Types.Bit_Index;
               Last : RFLX.Types.Bit_Length;
               Value : Field_Dependent_Value;
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if State = S_Valid
          or State = S_Structural_Valid then Valid_Value (Value));

   type Field_Cursors is array (Field) of Field_Cursor;

   function Valid_Context (Buffer_First, Buffer_Last : RFLX.Types.Index; First, Last : RFLX.Types.Bit_Index; Buffer_Address : RFLX.Types.Integer_Address; Buffer : RFLX.Types.Bytes_Ptr; Index : RFLX.Types.Bit_Index; Fld : Virtual_Field; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then Buffer'First = Buffer_First
        and then Buffer'Last = Buffer_Last
        and then RFLX.Types.Bytes_Address (Buffer) = Buffer_Address)
      and then RFLX.Types.Byte_Index (First) >= Buffer_First
      and then RFLX.Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= RFLX.Types.Bit_Index'Last / 2
      and then Index >= First
      and then Index - Last <= 1
      and then (for all F in Field'First .. Field'Last =>
        (if Cursors (F).State = S_Valid
        or Cursors (F).State = S_Structural_Valid then Cursors (F).First >= First
        and then Cursors (F).Last <= Last
        and then Cursors (F).First <= (Cursors (F).Last + 1)
        and then Cursors (F).Value.Fld = F))
      and then (case Fld is
           when F_Initial =>
              True,
           when F_Length =>
              (Cursors (F_Length).State = S_Valid
                   or Cursors (F_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = Length'Size,
           when F_Modular_Vector =>
              (Cursors (F_Length).State = S_Valid
                   or Cursors (F_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Modular_Vector).State = S_Valid
                   or Cursors (F_Modular_Vector).State = S_Structural_Valid)
                 and then (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = Length'Size,
           when F_Range_Vector =>
              (Cursors (F_Length).State = S_Valid
                   or Cursors (F_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Modular_Vector).State = S_Valid
                   or Cursors (F_Modular_Vector).State = S_Structural_Valid)
                 and then (Cursors (F_Range_Vector).State = S_Valid
                   or Cursors (F_Range_Vector).State = S_Structural_Valid)
                 and then (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = Length'Size,
           when F_Enumeration_Vector =>
              (Cursors (F_Length).State = S_Valid
                   or Cursors (F_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Modular_Vector).State = S_Valid
                   or Cursors (F_Modular_Vector).State = S_Structural_Valid)
                 and then (Cursors (F_Range_Vector).State = S_Valid
                   or Cursors (F_Range_Vector).State = S_Structural_Valid)
                 and then (Cursors (F_Enumeration_Vector).State = S_Valid
                   or Cursors (F_Enumeration_Vector).State = S_Structural_Valid)
                 and then (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = Length'Size,
           when F_AV_Enumeration_Vector | F_Final =>
              (Cursors (F_Length).State = S_Valid
                   or Cursors (F_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Modular_Vector).State = S_Valid
                   or Cursors (F_Modular_Vector).State = S_Structural_Valid)
                 and then (Cursors (F_Range_Vector).State = S_Valid
                   or Cursors (F_Range_Vector).State = S_Structural_Valid)
                 and then (Cursors (F_Enumeration_Vector).State = S_Valid
                   or Cursors (F_Enumeration_Vector).State = S_Structural_Valid)
                 and then (Cursors (F_AV_Enumeration_Vector).State = S_Valid
                   or Cursors (F_AV_Enumeration_Vector).State = S_Structural_Valid)
                 and then (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = Length'Size));

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First; Buffer_Address : RFLX.Types.Integer_Address := 0) is
      record
         Buffer : RFLX.Types.Bytes_Ptr := null;
         Index : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First;
         Fld : Virtual_Field := F_Initial;
         Cursors : Field_Cursors := (others => (State => S_Invalid));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Buffer_First, Buffer_Last, First, Last, Buffer_Address, Buffer, Index, Fld, Cursors);

   function Valid_Context (Ctx : Context) return Boolean is
     (Valid_Context (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last, Ctx.Buffer_Address, Ctx.Buffer, Ctx.Index, Ctx.Fld, Ctx.Cursors));

end RFLX.Arrays.Generic_Message;

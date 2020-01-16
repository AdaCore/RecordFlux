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

   pragma Annotate (GNATprove, Terminating, Generic_Message);

   type Virtual_Field is (F_Initial, F_Length, F_Modular_Vector, F_Range_Vector, F_Enumeration_Vector, F_AV_Enumeration_Vector, F_Final);

   subtype Field is Virtual_Field range F_Length .. F_AV_Enumeration_Vector;

   type Field_Cursor is private with
     Default_Initial_Condition =>
       False;

   type Field_Cursors is array (Virtual_Field) of Field_Cursor;

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       False;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector | F_Final =>
               null;
            when F_Length =>
               Length_Value : Length;
         end case;
      end record;

   function Create return Context;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then Buffer'Last <= RFLX.Types.Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Buffer = null
          and Ctx.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and Ctx.First = RFLX.Types.First_Bit_Index (Ctx.Buffer_First)
          and Initialized (Ctx);

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
          and Buffer = null
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and Ctx.First = First
          and Ctx.Last = Last
          and Initialized (Ctx);

   function Initialized (Ctx : Context) return Boolean with
     Ghost;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX.Types.Bytes_Ptr) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx),
     Post =>
       Valid_Context (Ctx)
          and not Has_Buffer (Ctx)
          and Buffer /= null
          and Ctx.Buffer_First = Buffer'First
          and Ctx.Buffer_Last = Buffer'Last
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old
          and Cursors (Ctx) = Cursors (Ctx)'Old;

   function Has_Buffer (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Message_Last (Ctx : Context) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and Structural_Valid_Message (Ctx);

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Predecessor (Ctx, Fld);

   function Field_Condition (Ctx : Context; Value : Field_Dependent_Value) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Value.Fld in Field'Range
          and Valid_Predecessor (Ctx, Value.Fld);

   function Field_Length (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Available_Space (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old;

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
       (if Valid'Result then
           Structural_Valid (Ctx, Fld)
             and Present (Ctx, Fld));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Invalid (Ctx : Context; Fld : Field) return Boolean with
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
          and Valid (Ctx, F_Length);

   generic
      with procedure Process_Modular_Vector (Modular_Vector : RFLX.Types.Bytes);
   procedure Get_Modular_Vector (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Present (Ctx, F_Modular_Vector);

   generic
      with procedure Process_Range_Vector (Range_Vector : RFLX.Types.Bytes);
   procedure Get_Range_Vector (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Present (Ctx, F_Range_Vector);

   generic
      with procedure Process_Enumeration_Vector (Enumeration_Vector : RFLX.Types.Bytes);
   procedure Get_Enumeration_Vector (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Present (Ctx, F_Enumeration_Vector);

   generic
      with procedure Process_AV_Enumeration_Vector (AV_Enumeration_Vector : RFLX.Types.Bytes);
   procedure Get_AV_Enumeration_Vector (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Present (Ctx, F_AV_Enumeration_Vector);

   procedure Set_Length (Ctx : in out Context; Value : Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Length)
          and then Field_Last (Ctx, F_Length) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Length, Value))
          and then Valid (Value)
          and then Available_Space (Ctx, F_Length) >= Field_Length (Ctx, F_Length),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Length)
          and Get_Length (Ctx) = Value
          and Invalid (Ctx, F_Modular_Vector)
          and Invalid (Ctx, F_Range_Vector)
          and Invalid (Ctx, F_Enumeration_Vector)
          and Invalid (Ctx, F_AV_Enumeration_Vector)
          and (Predecessor (Ctx, F_Modular_Vector) = F_Length
            and Valid_Next (Ctx, F_Modular_Vector))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Length) = Predecessor (Ctx, F_Length)'Old
          and Valid_Next (Ctx, F_Length) = Valid_Next (Ctx, F_Length)'Old;

   procedure Switch_To_Modular_Vector (Ctx : in out Context; Sequence_Context : out Modular_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then not Sequence_Context'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Modular_Vector)
          and then Field_Length (Ctx, F_Modular_Vector) > 0
          and then Field_Last (Ctx, F_Modular_Vector) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Modular_Vector))
          and then Available_Space (Ctx, F_Modular_Vector) >= Field_Length (Ctx, F_Modular_Vector),
     Post =>
       Valid_Context (Ctx)
          and not Has_Buffer (Ctx)
          and Modular_Vector_Sequence.Has_Buffer (Sequence_Context)
          and Ctx.Buffer_First = Sequence_Context.Buffer_First
          and Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and Sequence_Context.First = Field_First (Ctx, F_Modular_Vector)
          and Sequence_Context.Last = Field_Last (Ctx, F_Modular_Vector)
          and Modular_Vector_Sequence.Index (Sequence_Context) = Sequence_Context.First
          and Present (Ctx, F_Modular_Vector)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Modular_Vector) = Predecessor (Ctx, F_Modular_Vector)'Old
          and Path_Condition (Ctx, F_Modular_Vector) = Path_Condition (Ctx, F_Modular_Vector)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Modular_Vector) =>
           Cursor (Ctx, F_Range_Vector) = Cursor (Ctx, F_Range_Vector)'Old
              and Cursor (Ctx, F_Enumeration_Vector) = Cursor (Ctx, F_Enumeration_Vector)'Old
              and Cursor (Ctx, F_AV_Enumeration_Vector) = Cursor (Ctx, F_AV_Enumeration_Vector)'Old,
        others =>
           (Predecessor (Ctx, F_Range_Vector) = F_Modular_Vector
                and Valid_Next (Ctx, F_Range_Vector))
              and Invalid (Ctx, F_Range_Vector)
              and Invalid (Ctx, F_Enumeration_Vector)
              and Invalid (Ctx, F_AV_Enumeration_Vector));

   procedure Switch_To_Range_Vector (Ctx : in out Context; Sequence_Context : out Range_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then not Sequence_Context'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Range_Vector)
          and then Field_Length (Ctx, F_Range_Vector) > 0
          and then Field_Last (Ctx, F_Range_Vector) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Range_Vector))
          and then Available_Space (Ctx, F_Range_Vector) >= Field_Length (Ctx, F_Range_Vector),
     Post =>
       Valid_Context (Ctx)
          and not Has_Buffer (Ctx)
          and Range_Vector_Sequence.Has_Buffer (Sequence_Context)
          and Ctx.Buffer_First = Sequence_Context.Buffer_First
          and Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and Sequence_Context.First = Field_First (Ctx, F_Range_Vector)
          and Sequence_Context.Last = Field_Last (Ctx, F_Range_Vector)
          and Range_Vector_Sequence.Index (Sequence_Context) = Sequence_Context.First
          and Present (Ctx, F_Range_Vector)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Range_Vector) = Predecessor (Ctx, F_Range_Vector)'Old
          and Path_Condition (Ctx, F_Range_Vector) = Path_Condition (Ctx, F_Range_Vector)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old
          and Cursor (Ctx, F_Modular_Vector) = Cursor (Ctx, F_Modular_Vector)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Range_Vector) =>
           Cursor (Ctx, F_Enumeration_Vector) = Cursor (Ctx, F_Enumeration_Vector)'Old
              and Cursor (Ctx, F_AV_Enumeration_Vector) = Cursor (Ctx, F_AV_Enumeration_Vector)'Old,
        others =>
           (Predecessor (Ctx, F_Enumeration_Vector) = F_Range_Vector
                and Valid_Next (Ctx, F_Enumeration_Vector))
              and Invalid (Ctx, F_Enumeration_Vector)
              and Invalid (Ctx, F_AV_Enumeration_Vector));

   procedure Switch_To_Enumeration_Vector (Ctx : in out Context; Sequence_Context : out Enumeration_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then not Sequence_Context'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Enumeration_Vector)
          and then Field_Length (Ctx, F_Enumeration_Vector) > 0
          and then Field_Last (Ctx, F_Enumeration_Vector) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Enumeration_Vector))
          and then Available_Space (Ctx, F_Enumeration_Vector) >= Field_Length (Ctx, F_Enumeration_Vector),
     Post =>
       Valid_Context (Ctx)
          and not Has_Buffer (Ctx)
          and Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context)
          and Ctx.Buffer_First = Sequence_Context.Buffer_First
          and Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and Sequence_Context.First = Field_First (Ctx, F_Enumeration_Vector)
          and Sequence_Context.Last = Field_Last (Ctx, F_Enumeration_Vector)
          and Enumeration_Vector_Sequence.Index (Sequence_Context) = Sequence_Context.First
          and Present (Ctx, F_Enumeration_Vector)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Enumeration_Vector) = Predecessor (Ctx, F_Enumeration_Vector)'Old
          and Path_Condition (Ctx, F_Enumeration_Vector) = Path_Condition (Ctx, F_Enumeration_Vector)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old
          and Cursor (Ctx, F_Modular_Vector) = Cursor (Ctx, F_Modular_Vector)'Old
          and Cursor (Ctx, F_Range_Vector) = Cursor (Ctx, F_Range_Vector)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Enumeration_Vector) =>
           Cursor (Ctx, F_AV_Enumeration_Vector) = Cursor (Ctx, F_AV_Enumeration_Vector)'Old,
        others =>
           (Predecessor (Ctx, F_AV_Enumeration_Vector) = F_Enumeration_Vector
                and Valid_Next (Ctx, F_AV_Enumeration_Vector))
              and Invalid (Ctx, F_AV_Enumeration_Vector));

   procedure Switch_To_AV_Enumeration_Vector (Ctx : in out Context; Sequence_Context : out AV_Enumeration_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then not Sequence_Context'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_AV_Enumeration_Vector)
          and then Field_Length (Ctx, F_AV_Enumeration_Vector) > 0
          and then Field_Last (Ctx, F_AV_Enumeration_Vector) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_AV_Enumeration_Vector))
          and then Available_Space (Ctx, F_AV_Enumeration_Vector) >= Field_Length (Ctx, F_AV_Enumeration_Vector),
     Post =>
       Valid_Context (Ctx)
          and not Has_Buffer (Ctx)
          and AV_Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context)
          and Ctx.Buffer_First = Sequence_Context.Buffer_First
          and Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and Sequence_Context.First = Field_First (Ctx, F_AV_Enumeration_Vector)
          and Sequence_Context.Last = Field_Last (Ctx, F_AV_Enumeration_Vector)
          and AV_Enumeration_Vector_Sequence.Index (Sequence_Context) = Sequence_Context.First
          and Present (Ctx, F_AV_Enumeration_Vector)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_AV_Enumeration_Vector) = Predecessor (Ctx, F_AV_Enumeration_Vector)'Old
          and Path_Condition (Ctx, F_AV_Enumeration_Vector) = Path_Condition (Ctx, F_AV_Enumeration_Vector)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old
          and Cursor (Ctx, F_Modular_Vector) = Cursor (Ctx, F_Modular_Vector)'Old
          and Cursor (Ctx, F_Range_Vector) = Cursor (Ctx, F_Range_Vector)'Old
          and Cursor (Ctx, F_Enumeration_Vector) = Cursor (Ctx, F_Enumeration_Vector)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_AV_Enumeration_Vector) =>
           True,
        others =>
           True);

   procedure Update_Modular_Vector (Ctx : in out Context; Sequence_Context : in out Modular_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Present (Ctx, F_Modular_Vector)
          and then not Has_Buffer (Ctx)
          and then Modular_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Sequence_Context.First = Field_First (Ctx, F_Modular_Vector)
          and then Sequence_Context.Last = Field_Last (Ctx, F_Modular_Vector),
     Post =>
       Valid_Context (Ctx)
          and Present (Ctx, F_Modular_Vector)
          and Has_Buffer (Ctx)
          and not Modular_Vector_Sequence.Has_Buffer (Sequence_Context)
          and Sequence_Context.First = Field_First (Ctx, F_Modular_Vector)
          and Sequence_Context.Last = Field_Last (Ctx, F_Modular_Vector)
          and Sequence_Context.First = Sequence_Context.First'Old
          and Sequence_Context.Last = Sequence_Context.Last'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Field_First (Ctx, F_Modular_Vector) = Field_First (Ctx, F_Modular_Vector)'Old
          and Field_Length (Ctx, F_Modular_Vector) = Field_Length (Ctx, F_Modular_Vector)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old
          and Cursor (Ctx, F_Range_Vector) = Cursor (Ctx, F_Range_Vector)'Old
          and Cursor (Ctx, F_Enumeration_Vector) = Cursor (Ctx, F_Enumeration_Vector)'Old
          and Cursor (Ctx, F_AV_Enumeration_Vector) = Cursor (Ctx, F_AV_Enumeration_Vector)'Old;

   procedure Update_Range_Vector (Ctx : in out Context; Sequence_Context : in out Range_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Present (Ctx, F_Range_Vector)
          and then not Has_Buffer (Ctx)
          and then Range_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Sequence_Context.First = Field_First (Ctx, F_Range_Vector)
          and then Sequence_Context.Last = Field_Last (Ctx, F_Range_Vector),
     Post =>
       Valid_Context (Ctx)
          and Present (Ctx, F_Range_Vector)
          and Has_Buffer (Ctx)
          and not Range_Vector_Sequence.Has_Buffer (Sequence_Context)
          and Sequence_Context.First = Field_First (Ctx, F_Range_Vector)
          and Sequence_Context.Last = Field_Last (Ctx, F_Range_Vector)
          and Sequence_Context.First = Sequence_Context.First'Old
          and Sequence_Context.Last = Sequence_Context.Last'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Field_First (Ctx, F_Range_Vector) = Field_First (Ctx, F_Range_Vector)'Old
          and Field_Length (Ctx, F_Range_Vector) = Field_Length (Ctx, F_Range_Vector)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old
          and Cursor (Ctx, F_Modular_Vector) = Cursor (Ctx, F_Modular_Vector)'Old
          and Cursor (Ctx, F_Enumeration_Vector) = Cursor (Ctx, F_Enumeration_Vector)'Old
          and Cursor (Ctx, F_AV_Enumeration_Vector) = Cursor (Ctx, F_AV_Enumeration_Vector)'Old;

   procedure Update_Enumeration_Vector (Ctx : in out Context; Sequence_Context : in out Enumeration_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Present (Ctx, F_Enumeration_Vector)
          and then not Has_Buffer (Ctx)
          and then Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Sequence_Context.First = Field_First (Ctx, F_Enumeration_Vector)
          and then Sequence_Context.Last = Field_Last (Ctx, F_Enumeration_Vector),
     Post =>
       Valid_Context (Ctx)
          and Present (Ctx, F_Enumeration_Vector)
          and Has_Buffer (Ctx)
          and not Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context)
          and Sequence_Context.First = Field_First (Ctx, F_Enumeration_Vector)
          and Sequence_Context.Last = Field_Last (Ctx, F_Enumeration_Vector)
          and Sequence_Context.First = Sequence_Context.First'Old
          and Sequence_Context.Last = Sequence_Context.Last'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Field_First (Ctx, F_Enumeration_Vector) = Field_First (Ctx, F_Enumeration_Vector)'Old
          and Field_Length (Ctx, F_Enumeration_Vector) = Field_Length (Ctx, F_Enumeration_Vector)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old
          and Cursor (Ctx, F_Modular_Vector) = Cursor (Ctx, F_Modular_Vector)'Old
          and Cursor (Ctx, F_Range_Vector) = Cursor (Ctx, F_Range_Vector)'Old
          and Cursor (Ctx, F_AV_Enumeration_Vector) = Cursor (Ctx, F_AV_Enumeration_Vector)'Old;

   procedure Update_AV_Enumeration_Vector (Ctx : in out Context; Sequence_Context : in out AV_Enumeration_Vector_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Present (Ctx, F_AV_Enumeration_Vector)
          and then not Has_Buffer (Ctx)
          and then AV_Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Sequence_Context.First = Field_First (Ctx, F_AV_Enumeration_Vector)
          and then Sequence_Context.Last = Field_Last (Ctx, F_AV_Enumeration_Vector),
     Post =>
       Valid_Context (Ctx)
          and Present (Ctx, F_AV_Enumeration_Vector)
          and Has_Buffer (Ctx)
          and not AV_Enumeration_Vector_Sequence.Has_Buffer (Sequence_Context)
          and Sequence_Context.First = Field_First (Ctx, F_AV_Enumeration_Vector)
          and Sequence_Context.Last = Field_Last (Ctx, F_AV_Enumeration_Vector)
          and Sequence_Context.First = Sequence_Context.First'Old
          and Sequence_Context.Last = Sequence_Context.Last'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Field_First (Ctx, F_AV_Enumeration_Vector) = Field_First (Ctx, F_AV_Enumeration_Vector)'Old
          and Field_Length (Ctx, F_AV_Enumeration_Vector) = Field_Length (Ctx, F_AV_Enumeration_Vector)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old
          and Cursor (Ctx, F_Modular_Vector) = Cursor (Ctx, F_Modular_Vector)'Old
          and Cursor (Ctx, F_Range_Vector) = Cursor (Ctx, F_Range_Vector)'Old
          and Cursor (Ctx, F_Enumeration_Vector) = Cursor (Ctx, F_Enumeration_Vector)'Old;

   function Valid_Context (Ctx : Context) return Boolean with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Cursor (Ctx : Context; Fld : Field) return Field_Cursor with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Cursors (Ctx : Context) return Field_Cursors with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Incomplete);

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
         Predecessor : Virtual_Field := F_Final;
         case State is
            when S_Valid | S_Structural_Valid =>
               First : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First;
               Last : RFLX.Types.Bit_Length := RFLX.Types.Bit_Length'First;
               Value : Field_Dependent_Value := (Fld => F_Final);
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if State = S_Valid
             or State = S_Structural_Valid then
           Valid_Value (Value));

   function Structural_Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid
      or Cursor.State = S_Structural_Valid);

   function Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid);

   function Invalid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Invalid
      or Cursor.State = S_Incomplete);

   function Valid_Context (Buffer_First, Buffer_Last : RFLX.Types.Index; First, Last : RFLX.Types.Bit_Index; Buffer : access constant RFLX.Types.Bytes; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then
         Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last)
      and then RFLX.Types.Byte_Index (First) >= Buffer_First
      and then RFLX.Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= RFLX.Types.Bit_Index'Last / 2
      and then (for all F in Field'First .. Field'Last =>
        (if Structural_Valid (Cursors (F)) then
         Cursors (F).First >= First
           and Cursors (F).Last <= Last
           and Cursors (F).First <= (Cursors (F).Last + 1)
           and Cursors (F).Value.Fld = F))
      and then ((if Structural_Valid (Cursors (F_Modular_Vector)) then
           (Valid (Cursors (F_Length))
               and then Cursors (F_Modular_Vector).Predecessor = F_Length))
        and then (if Structural_Valid (Cursors (F_Range_Vector)) then
           (Structural_Valid (Cursors (F_Modular_Vector))
               and then Cursors (F_Range_Vector).Predecessor = F_Modular_Vector))
        and then (if Structural_Valid (Cursors (F_Enumeration_Vector)) then
           (Structural_Valid (Cursors (F_Range_Vector))
               and then Cursors (F_Enumeration_Vector).Predecessor = F_Range_Vector))
        and then (if Structural_Valid (Cursors (F_AV_Enumeration_Vector)) then
           (Structural_Valid (Cursors (F_Enumeration_Vector))
               and then Cursors (F_AV_Enumeration_Vector).Predecessor = F_Enumeration_Vector)))
      and then ((if Invalid (Cursors (F_Length)) then
           Invalid (Cursors (F_Modular_Vector)))
        and then (if Invalid (Cursors (F_Modular_Vector)) then
           Invalid (Cursors (F_Range_Vector)))
        and then (if Invalid (Cursors (F_Range_Vector)) then
           Invalid (Cursors (F_Enumeration_Vector)))
        and then (if Invalid (Cursors (F_Enumeration_Vector)) then
           Invalid (Cursors (F_AV_Enumeration_Vector))))
      and then (if Structural_Valid (Cursors (F_Length)) then
         (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = Arrays.Length'Size
           and then Cursors (F_Length).Predecessor = F_Initial
           and then Cursors (F_Length).First = First
           and then (if Structural_Valid (Cursors (F_Modular_Vector)) then
              (Cursors (F_Modular_Vector).Last - Cursors (F_Modular_Vector).First + 1) = RFLX.Types.Bit_Length (Cursors (F_Length).Value.Length_Value) * 8
                and then Cursors (F_Modular_Vector).Predecessor = F_Length
                and then Cursors (F_Modular_Vector).First = (Cursors (F_Length).Last + 1)
                and then (if Structural_Valid (Cursors (F_Range_Vector)) then
                   (Cursors (F_Range_Vector).Last - Cursors (F_Range_Vector).First + 1) = 16
                     and then Cursors (F_Range_Vector).Predecessor = F_Modular_Vector
                     and then Cursors (F_Range_Vector).First = (Cursors (F_Modular_Vector).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Enumeration_Vector)) then
                        (Cursors (F_Enumeration_Vector).Last - Cursors (F_Enumeration_Vector).First + 1) = 16
                          and then Cursors (F_Enumeration_Vector).Predecessor = F_Range_Vector
                          and then Cursors (F_Enumeration_Vector).First = (Cursors (F_Range_Vector).Last + 1)
                          and then (if Structural_Valid (Cursors (F_AV_Enumeration_Vector)) then
                             (Cursors (F_AV_Enumeration_Vector).Last - Cursors (F_AV_Enumeration_Vector).First + 1) = 16
                               and then Cursors (F_AV_Enumeration_Vector).Predecessor = F_Enumeration_Vector
                               and then Cursors (F_AV_Enumeration_Vector).First = (Cursors (F_Enumeration_Vector).Last + 1)))))));

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First) is
      record
         Buffer : RFLX.Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Buffer_First, Buffer_Last, First, Last, Buffer, Cursors);

   function Valid_Context (Ctx : Context) return Boolean is
     (Valid_Context (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last, Ctx.Buffer, Ctx.Cursors));

   function Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.Arrays.Generic_Message;

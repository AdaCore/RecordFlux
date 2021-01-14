pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Generic_Types;
with RFLX.RFLX_Scalar_Sequence;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
   with package Modular_Vector_Sequence is new RFLX.RFLX_Scalar_Sequence (Types, others => <>);
package RFLX.Arrays.Generic_Array_Size_Defined_By_Message_Size with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Warnings (Off, "use clause for type ""U64"" * has no effect");

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Bit_Index, Types.U64;

   pragma Warnings (On, "use clause for type ""U64"" * has no effect");

   type Virtual_Field is (F_Initial, F_Header, F_Vector, F_Final);

   subtype Field is Virtual_Field range F_Header .. F_Vector;

   type Field_Cursor is private with
     Default_Initial_Condition =>
       False;

   type Field_Cursors is private with
     Default_Initial_Condition =>
       False;

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       Types.Byte_Index (First) >= Buffer_First
       and Types.Byte_Index (Last) <= Buffer_Last
       and First <= Last
       and Last < Types.Bit_Index'Last;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Vector | F_Final =>
               null;
            when F_Header =>
               Header_Value : RFLX.Arrays.Enumeration_Base;
         end case;
      end record;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
       and then Buffer /= null
       and then Buffer'Length > 0
       and then Buffer'Last < Types.Index'Last,
     Post =>
       Has_Buffer (Ctx)
       and Buffer = null
       and Ctx.Buffer_First = Buffer'First'Old
       and Ctx.Buffer_Last = Buffer'Last'Old
       and Ctx.First = Types.First_Bit_Index (Ctx.Buffer_First)
       and Ctx.Last = Types.Last_Bit_Index (Ctx.Buffer_Last)
       and Message_Last (Ctx) = Ctx.First
       and Initialized (Ctx),
     Depends =>
       (Ctx => Buffer, Buffer => null);

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
       and then Buffer /= null
       and then Buffer'Length > 0
       and then Types.Byte_Index (First) >= Buffer'First
       and then Types.Byte_Index (Last) <= Buffer'Last
       and then First <= Last
       and then Last < Types.Bit_Index'Last,
     Post =>
       Buffer = null
       and Has_Buffer (Ctx)
       and Ctx.Buffer_First = Buffer'First'Old
       and Ctx.Buffer_Last = Buffer'Last'Old
       and Ctx.First = First
       and Ctx.Last = Last
       and Message_Last (Ctx) = Ctx.First
       and Initialized (Ctx),
     Depends =>
       (Ctx => (Buffer, First, Last), Buffer => null);

   function Initialized (Ctx : Context) return Boolean with
     Ghost;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) with
     Pre =>
       Has_Buffer (Ctx),
     Post =>
       not Has_Buffer (Ctx)
       and Buffer /= null
       and Ctx.Buffer_First = Buffer'First
       and Ctx.Buffer_Last = Buffer'Last
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Context_Cursors (Ctx) = Context_Cursors (Ctx)'Old,
     Depends =>
       (Ctx => Ctx, Buffer => Ctx);

   function Has_Buffer (Ctx : Context) return Boolean;

   function Message_Last (Ctx : Context) return Types.Bit_Index;

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Predecessor (Ctx, Fld);

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean with
     Pre =>
       Has_Buffer (Ctx)
       and Val.Fld in Field'Range
       and Valid_Predecessor (Ctx, Val.Fld);

   function Field_Size (Ctx : Context; Fld : Field) return Types.Bit_Length with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld)
       and then Available_Space (Ctx, Fld) >= Field_Size (Ctx, Fld);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field;

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean;

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean;

   function Available_Space (Ctx : Context; Fld : Field) return Types.Bit_Length with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : Types.Bytes) return Boolean with
     Pre =>
       Has_Buffer (Ctx)
       and Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Post =>
       Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Post =>
       Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old;

   function Present (Ctx : Context; Fld : Field) return Boolean;

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean;

   function Valid (Ctx : Context; Fld : Field) return Boolean with
     Post =>
       (if
           Valid'Result
        then
           Structural_Valid (Ctx, Fld)
           and Present (Ctx, Fld));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean;

   function Invalid (Ctx : Context; Fld : Field) return Boolean;

   function Structural_Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Has_Buffer (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Has_Buffer (Ctx);

   function Incomplete_Message (Ctx : Context) return Boolean;

   function Get_Header (Ctx : Context) return RFLX.Arrays.Enumeration with
     Pre =>
       Valid (Ctx, F_Header);

   generic
      with procedure Process_Vector (Vector : Types.Bytes);
   procedure Get_Vector (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and Present (Ctx, F_Vector);

   procedure Set_Header (Ctx : in out Context; Val : RFLX.Arrays.Enumeration) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Header)
       and then Field_Condition (Ctx, (F_Header, To_Base (Val)))
       and then True
       and then Available_Space (Ctx, F_Header) >= Field_Size (Ctx, F_Header),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Header)
       and Get_Header (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_Header)
       and Invalid (Ctx, F_Vector)
       and (Predecessor (Ctx, F_Vector) = F_Header
            and Valid_Next (Ctx, F_Vector))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Header) = Predecessor (Ctx, F_Header)'Old
       and Valid_Next (Ctx, F_Header) = Valid_Next (Ctx, F_Header)'Old;

   procedure Set_Vector_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Vector)
       and then Field_Condition (Ctx, (Fld => F_Vector))
       and then Available_Space (Ctx, F_Vector) >= Field_Size (Ctx, F_Vector)
       and then Field_First (Ctx, F_Vector) mod Types.Byte'Size = 1
       and then Field_Size (Ctx, F_Vector) mod Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Vector) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Message_Last (Ctx) = Field_Last (Ctx, F_Vector)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Vector) = Predecessor (Ctx, F_Vector)'Old
       and Valid_Next (Ctx, F_Vector) = Valid_Next (Ctx, F_Vector)'Old
       and Get_Header (Ctx) = Get_Header (Ctx)'Old
       and Structural_Valid (Ctx, F_Vector);

   procedure Set_Vector (Ctx : in out Context; Seq_Ctx : Modular_Vector_Sequence.Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Vector)
       and then Field_Condition (Ctx, (Fld => F_Vector))
       and then Available_Space (Ctx, F_Vector) >= Field_Size (Ctx, F_Vector)
       and then Field_First (Ctx, F_Vector) mod Types.Byte'Size = 1
       and then Field_Size (Ctx, F_Vector) mod Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Vector) = Modular_Vector_Sequence.Size (Seq_Ctx)
       and then Modular_Vector_Sequence.Has_Buffer (Seq_Ctx)
       and then Modular_Vector_Sequence.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Message_Last (Ctx) = Field_Last (Ctx, F_Vector)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Vector) = Predecessor (Ctx, F_Vector)'Old
       and Valid_Next (Ctx, F_Vector) = Valid_Next (Ctx, F_Vector)'Old
       and Get_Header (Ctx) = Get_Header (Ctx)'Old
       and Structural_Valid (Ctx, F_Vector)
       and (if
               Field_Size (Ctx, F_Vector) > 0
            then
               Present (Ctx, F_Vector));

   procedure Switch_To_Vector (Ctx : in out Context; Seq_Ctx : out Modular_Vector_Sequence.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Vector)
       and then Field_Size (Ctx, F_Vector) > 0
       and then Field_First (Ctx, F_Vector) mod Types.Byte'Size = 1
       and then Field_Condition (Ctx, (Fld => F_Vector))
       and then Available_Space (Ctx, F_Vector) >= Field_Size (Ctx, F_Vector),
     Post =>
       not Has_Buffer (Ctx)
       and Modular_Vector_Sequence.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_Vector)
       and Seq_Ctx.Last = Field_Last (Ctx, F_Vector)
       and Modular_Vector_Sequence.Valid (Seq_Ctx)
       and Modular_Vector_Sequence.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_Vector)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Vector) = Predecessor (Ctx, F_Vector)'Old
       and Path_Condition (Ctx, F_Vector) = Path_Condition (Ctx, F_Vector)'Old
       and Context_Cursor (Ctx, F_Header) = Context_Cursor (Ctx, F_Header)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Vector) =>
           True,
        others =>
           True);

   function Complete_Vector (Ctx : Context; Seq_Ctx : Modular_Vector_Sequence.Context) return Boolean with
     Pre =>
       Valid_Next (Ctx, F_Vector);

   procedure Update_Vector (Ctx : in out Context; Seq_Ctx : in out Modular_Vector_Sequence.Context) with
     Pre =>
       Present (Ctx, F_Vector)
       and then Complete_Vector (Ctx, Seq_Ctx)
       and then not Has_Buffer (Ctx)
       and then Modular_Vector_Sequence.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_Vector)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_Vector),
     Post =>
       Present (Ctx, F_Vector)
       and Has_Buffer (Ctx)
       and not Modular_Vector_Sequence.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Seq_Ctx.First = Seq_Ctx.First'Old
       and Seq_Ctx.Last = Seq_Ctx.Last'Old
       and Field_First (Ctx, F_Vector) = Field_First (Ctx, F_Vector)'Old
       and Field_Size (Ctx, F_Vector) = Field_Size (Ctx, F_Vector)'Old
       and Context_Cursor (Ctx, F_Header) = Context_Cursor (Ctx, F_Header)'Old,
     Depends =>
       (Ctx => (Ctx, Seq_Ctx), Seq_Ctx => Seq_Ctx);

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Context_Cursors (Ctx : Context) return Field_Cursors with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Incomplete);

   function Valid_Value (Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
          when F_Header =>
             Valid (Val.Header_Value),
          when F_Vector =>
             True,
          when F_Initial | F_Final =>
             False));

   type Field_Cursor (State : Cursor_State := S_Invalid) is
      record
         Predecessor : Virtual_Field := F_Final;
         case State is
            when S_Valid | S_Structural_Valid =>
               First : Types.Bit_Index := Types.Bit_Index'First;
               Last : Types.Bit_Length := Types.Bit_Length'First;
               Value : Field_Dependent_Value := (Fld => F_Final);
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if
           State = S_Valid
           or State = S_Structural_Valid
        then
           Valid_Value (Field_Cursor.Value));

   type Field_Cursors is array (Virtual_Field) of Field_Cursor;

   function Structural_Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid
      or Cursor.State = S_Structural_Valid);

   function Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid);

   function Invalid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Invalid
      or Cursor.State = S_Incomplete);

   function Valid_Context (Buffer_First, Buffer_Last : Types.Index; First, Last, Message_Last : Types.Bit_Index; Buffer : access constant Types.Bytes; Cursors : Field_Cursors) return Boolean is
     ((if
          Buffer /= null
       then
          Buffer'First = Buffer_First
          and Buffer'Last = Buffer_Last)
      and then (Types.Byte_Index (First) >= Buffer_First
                and Types.Byte_Index (Last) <= Buffer_Last
                and First <= Last
                and Last < Types.Bit_Index'Last)
      and then First <= Message_Last
      and then Message_Last <= Last
      and then (for all F in Field'First .. Field'Last =>
                   (if
                       Structural_Valid (Cursors (F))
                    then
                       Cursors (F).First >= First
                       and Cursors (F).Last <= Message_Last
                       and Cursors (F).First <= Cursors (F).Last + 1
                       and Cursors (F).Value.Fld = F))
      and then ((if
                    Structural_Valid (Cursors (F_Vector))
                 then
                    (Valid (Cursors (F_Header))
                     and then Cursors (F_Vector).Predecessor = F_Header)))
      and then ((if
                    Invalid (Cursors (F_Header))
                 then
                    Invalid (Cursors (F_Vector))))
      and then (if
                   Structural_Valid (Cursors (F_Header))
                then
                   Cursors (F_Header).Last - Cursors (F_Header).First + 1 = RFLX.Arrays.Enumeration_Base'Size
                   and then Cursors (F_Header).Predecessor = F_Initial
                   and then Cursors (F_Header).First = First
                   and then (if
                                Structural_Valid (Cursors (F_Vector))
                             then
                                Cursors (F_Vector).Last - Cursors (F_Vector).First + 1 = Types.Bit_Length (Last - First + 1) - Types.Bit_Length (Cursors (F_Header).Last - Cursors (F_Header).First + 1)
                                and then Cursors (F_Vector).Predecessor = F_Header
                                and then Cursors (F_Vector).First = Cursors (F_Header).Last + 1)));

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is
      record
         Message_Last : Types.Bit_Index := First;
         Buffer : Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Message_Last, Context.Buffer, Context.Cursors);

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.Arrays.Generic_Array_Size_Defined_By_Message_Size;

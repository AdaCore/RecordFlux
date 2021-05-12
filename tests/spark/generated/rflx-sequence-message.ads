pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.Sequence.Modular_Vector;
with RFLX.Sequence.Range_Vector;
with RFLX.Sequence.Enumeration_Vector;
with RFLX.Sequence.AV_Enumeration_Vector;

package RFLX.Sequence.Message with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Warnings (Off, "use clause for type ""U64"" * has no effect");

   pragma Warnings (Off, """LENGTH"" is already use-visible through previous use_type_clause");

   use type RFLX_Types.Bytes, RFLX_Types.Bytes_Ptr, RFLX_Types.Length, RFLX_Types.Index, RFLX_Types.Bit_Index, RFLX_Types.U64;

   pragma Warnings (On, """LENGTH"" is already use-visible through previous use_type_clause");

   pragma Warnings (On, "use clause for type ""U64"" * has no effect");

   type Virtual_Field is (F_Initial, F_Length, F_Modular_Vector, F_Range_Vector, F_Enumeration_Vector, F_AV_Enumeration_Vector, F_Final);

   subtype Field is Virtual_Field range F_Length .. F_AV_Enumeration_Vector;

   type Field_Cursor is private with
     Default_Initial_Condition =>
       False;

   type Field_Cursors is private with
     Default_Initial_Condition =>
       False;

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First + 7) is private with
     Default_Initial_Condition =>
       RFLX_Types.Byte_Index (First) >= Buffer_First
       and RFLX_Types.Byte_Index (Last) <= Buffer_Last
       and First <= Last
       and Last < RFLX_Types.Bit_Index'Last
       and First mod RFLX_Types.Byte'Size = 1
       and Last mod RFLX_Types.Byte'Size = 0;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector | F_Final =>
               null;
            when F_Length =>
               Length_Value : RFLX.Sequence.Length;
         end case;
      end record;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
       and then Buffer /= null
       and then Buffer'Length > 0
       and then Buffer'Last < RFLX_Types.Index'Last,
     Post =>
       Has_Buffer (Ctx)
       and Buffer = null
       and Ctx.Buffer_First = Buffer'First'Old
       and Ctx.Buffer_Last = Buffer'Last'Old
       and Ctx.First = RFLX_Types.First_Bit_Index (Ctx.Buffer_First)
       and Ctx.Last = RFLX_Types.Last_Bit_Index (Ctx.Buffer_Last)
       and Initialized (Ctx),
     Depends =>
       (Ctx => Buffer, Buffer => null);

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; First, Last : RFLX_Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
       and then Buffer /= null
       and then Buffer'Length > 0
       and then RFLX_Types.Byte_Index (First) >= Buffer'First
       and then RFLX_Types.Byte_Index (Last) <= Buffer'Last
       and then First <= Last
       and then Last < RFLX_Types.Bit_Index'Last
       and then First mod RFLX_Types.Byte'Size = 1
       and then Last mod RFLX_Types.Byte'Size = 0,
     Post =>
       Buffer = null
       and Has_Buffer (Ctx)
       and Ctx.Buffer_First = Buffer'First'Old
       and Ctx.Buffer_Last = Buffer'Last'Old
       and Ctx.First = First
       and Ctx.Last = Last
       and Initialized (Ctx),
     Depends =>
       (Ctx => (Buffer, First, Last), Buffer => null);

   function Initialized (Ctx : Context) return Boolean with
     Ghost;

   procedure Reset (Ctx : in out Context) with
     Pre =>
       Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Initialized (Ctx);

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX_Types.Bytes_Ptr) with
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

   procedure Copy (Ctx : Context; Buffer : out RFLX_Types.Bytes) with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid_Message (Ctx)
       and then Byte_Size (Ctx) = Buffer'Length;

   generic
      with procedure Read (Buffer : RFLX_Types.Bytes);
   procedure Read (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid_Message (Ctx);

   generic
      with procedure Write (Buffer : out RFLX_Types.Bytes);
   procedure Write (Ctx : in out Context) with
     Pre =>
       Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Initialized (Ctx);

   function Has_Buffer (Ctx : Context) return Boolean;

   function Byte_Size (Ctx : Context) return RFLX_Types.Length;

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid_Message (Ctx);

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Predecessor (Ctx, Fld);

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean with
     Pre =>
       Has_Buffer (Ctx)
       and Val.Fld in Field'Range
       and Valid_Predecessor (Ctx, Val.Fld);

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld)
       and then Available_Space (Ctx, Fld) >= Field_Size (Ctx, Fld);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field;

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean;

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean;

   function Available_Space (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean with
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

   pragma Warnings (Off, "precondition is always False");

   function Get_Length (Ctx : Context) return RFLX.Sequence.Length with
     Pre =>
       Valid (Ctx, F_Length);

   pragma Warnings (On, "precondition is always False");

   generic
      with procedure Process_Modular_Vector (Modular_Vector : RFLX_Types.Bytes);
   procedure Get_Modular_Vector (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and Present (Ctx, F_Modular_Vector);

   generic
      with procedure Process_Range_Vector (Range_Vector : RFLX_Types.Bytes);
   procedure Get_Range_Vector (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and Present (Ctx, F_Range_Vector);

   generic
      with procedure Process_Enumeration_Vector (Enumeration_Vector : RFLX_Types.Bytes);
   procedure Get_Enumeration_Vector (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and Present (Ctx, F_Enumeration_Vector);

   generic
      with procedure Process_AV_Enumeration_Vector (AV_Enumeration_Vector : RFLX_Types.Bytes);
   procedure Get_AV_Enumeration_Vector (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and Present (Ctx, F_AV_Enumeration_Vector);

   procedure Set_Length (Ctx : in out Context; Val : RFLX.Sequence.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Length)
       and then Field_Condition (Ctx, (F_Length, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Length) >= Field_Size (Ctx, F_Length),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Length)
       and Get_Length (Ctx) = Val
       and Invalid (Ctx, F_Modular_Vector)
       and Invalid (Ctx, F_Range_Vector)
       and Invalid (Ctx, F_Enumeration_Vector)
       and Invalid (Ctx, F_AV_Enumeration_Vector)
       and (Predecessor (Ctx, F_Modular_Vector) = F_Length
            and Valid_Next (Ctx, F_Modular_Vector))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Length) = Predecessor (Ctx, F_Length)'Old
       and Valid_Next (Ctx, F_Length) = Valid_Next (Ctx, F_Length)'Old;

   procedure Set_Modular_Vector_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Modular_Vector)
       and then Field_Condition (Ctx, (Fld => F_Modular_Vector))
       and then Available_Space (Ctx, F_Modular_Vector) >= Field_Size (Ctx, F_Modular_Vector)
       and then Field_First (Ctx, F_Modular_Vector) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Modular_Vector) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Modular_Vector) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Modular_Vector) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Modular_Vector)
       and Invalid (Ctx, F_Range_Vector)
       and Invalid (Ctx, F_Enumeration_Vector)
       and Invalid (Ctx, F_AV_Enumeration_Vector)
       and (Predecessor (Ctx, F_Range_Vector) = F_Modular_Vector
            and Valid_Next (Ctx, F_Range_Vector))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Modular_Vector) = Predecessor (Ctx, F_Modular_Vector)'Old
       and Valid_Next (Ctx, F_Modular_Vector) = Valid_Next (Ctx, F_Modular_Vector)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old;

   procedure Set_Modular_Vector (Ctx : in out Context; Seq_Ctx : Sequence.Modular_Vector.Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Modular_Vector)
       and then Field_Condition (Ctx, (Fld => F_Modular_Vector))
       and then Available_Space (Ctx, F_Modular_Vector) >= Field_Size (Ctx, F_Modular_Vector)
       and then Field_First (Ctx, F_Modular_Vector) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Modular_Vector) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Modular_Vector) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Modular_Vector) = Sequence.Modular_Vector.Size (Seq_Ctx)
       and then Sequence.Modular_Vector.Has_Buffer (Seq_Ctx)
       and then Sequence.Modular_Vector.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Modular_Vector)
       and Invalid (Ctx, F_Range_Vector)
       and Invalid (Ctx, F_Enumeration_Vector)
       and Invalid (Ctx, F_AV_Enumeration_Vector)
       and (Predecessor (Ctx, F_Range_Vector) = F_Modular_Vector
            and Valid_Next (Ctx, F_Range_Vector))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Modular_Vector) = Predecessor (Ctx, F_Modular_Vector)'Old
       and Valid_Next (Ctx, F_Modular_Vector) = Valid_Next (Ctx, F_Modular_Vector)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and (if
               Field_Size (Ctx, F_Modular_Vector) > 0
            then
               Present (Ctx, F_Modular_Vector));

   procedure Set_Range_Vector (Ctx : in out Context; Seq_Ctx : Sequence.Range_Vector.Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Range_Vector)
       and then Field_Condition (Ctx, (Fld => F_Range_Vector))
       and then Available_Space (Ctx, F_Range_Vector) >= Field_Size (Ctx, F_Range_Vector)
       and then Field_First (Ctx, F_Range_Vector) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Range_Vector) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Range_Vector) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Range_Vector) = Sequence.Range_Vector.Size (Seq_Ctx)
       and then Sequence.Range_Vector.Has_Buffer (Seq_Ctx)
       and then Sequence.Range_Vector.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Range_Vector)
       and Invalid (Ctx, F_Enumeration_Vector)
       and Invalid (Ctx, F_AV_Enumeration_Vector)
       and (Predecessor (Ctx, F_Enumeration_Vector) = F_Range_Vector
            and Valid_Next (Ctx, F_Enumeration_Vector))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Range_Vector) = Predecessor (Ctx, F_Range_Vector)'Old
       and Valid_Next (Ctx, F_Range_Vector) = Valid_Next (Ctx, F_Range_Vector)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and (if
               Field_Size (Ctx, F_Range_Vector) > 0
            then
               Present (Ctx, F_Range_Vector));

   procedure Set_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : Sequence.Enumeration_Vector.Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Enumeration_Vector)
       and then Field_Condition (Ctx, (Fld => F_Enumeration_Vector))
       and then Available_Space (Ctx, F_Enumeration_Vector) >= Field_Size (Ctx, F_Enumeration_Vector)
       and then Field_First (Ctx, F_Enumeration_Vector) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Enumeration_Vector) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Enumeration_Vector) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Enumeration_Vector) = Sequence.Enumeration_Vector.Size (Seq_Ctx)
       and then Sequence.Enumeration_Vector.Has_Buffer (Seq_Ctx)
       and then Sequence.Enumeration_Vector.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Enumeration_Vector)
       and Invalid (Ctx, F_AV_Enumeration_Vector)
       and (Predecessor (Ctx, F_AV_Enumeration_Vector) = F_Enumeration_Vector
            and Valid_Next (Ctx, F_AV_Enumeration_Vector))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Enumeration_Vector) = Predecessor (Ctx, F_Enumeration_Vector)'Old
       and Valid_Next (Ctx, F_Enumeration_Vector) = Valid_Next (Ctx, F_Enumeration_Vector)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and (if
               Field_Size (Ctx, F_Enumeration_Vector) > 0
            then
               Present (Ctx, F_Enumeration_Vector));

   procedure Set_AV_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : Sequence.AV_Enumeration_Vector.Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_AV_Enumeration_Vector)
       and then Field_Condition (Ctx, (Fld => F_AV_Enumeration_Vector))
       and then Available_Space (Ctx, F_AV_Enumeration_Vector) >= Field_Size (Ctx, F_AV_Enumeration_Vector)
       and then Field_First (Ctx, F_AV_Enumeration_Vector) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_AV_Enumeration_Vector) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_AV_Enumeration_Vector) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_AV_Enumeration_Vector) = Sequence.AV_Enumeration_Vector.Size (Seq_Ctx)
       and then Sequence.AV_Enumeration_Vector.Has_Buffer (Seq_Ctx)
       and then Sequence.AV_Enumeration_Vector.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_AV_Enumeration_Vector)
       and (if
               Structural_Valid_Message (Ctx)
            then
               Message_Last (Ctx) = Field_Last (Ctx, F_AV_Enumeration_Vector))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_AV_Enumeration_Vector) = Predecessor (Ctx, F_AV_Enumeration_Vector)'Old
       and Valid_Next (Ctx, F_AV_Enumeration_Vector) = Valid_Next (Ctx, F_AV_Enumeration_Vector)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and (if
               Field_Size (Ctx, F_AV_Enumeration_Vector) > 0
            then
               Present (Ctx, F_AV_Enumeration_Vector));

   procedure Switch_To_Modular_Vector (Ctx : in out Context; Seq_Ctx : out Sequence.Modular_Vector.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Modular_Vector)
       and then Field_Size (Ctx, F_Modular_Vector) > 0
       and then Field_First (Ctx, F_Modular_Vector) mod RFLX_Types.Byte'Size = 1
       and then Field_Condition (Ctx, (Fld => F_Modular_Vector))
       and then Available_Space (Ctx, F_Modular_Vector) >= Field_Size (Ctx, F_Modular_Vector),
     Post =>
       not Has_Buffer (Ctx)
       and Sequence.Modular_Vector.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_Modular_Vector)
       and Seq_Ctx.Last = Field_Last (Ctx, F_Modular_Vector)
       and Sequence.Modular_Vector.Valid (Seq_Ctx)
       and Sequence.Modular_Vector.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_Modular_Vector)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Modular_Vector) = Predecessor (Ctx, F_Modular_Vector)'Old
       and Path_Condition (Ctx, F_Modular_Vector) = Path_Condition (Ctx, F_Modular_Vector)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Modular_Vector) =>
           Context_Cursor (Ctx, F_Range_Vector) = Context_Cursor (Ctx, F_Range_Vector)'Old
           and Context_Cursor (Ctx, F_Enumeration_Vector) = Context_Cursor (Ctx, F_Enumeration_Vector)'Old
           and Context_Cursor (Ctx, F_AV_Enumeration_Vector) = Context_Cursor (Ctx, F_AV_Enumeration_Vector)'Old,
        others =>
           (Predecessor (Ctx, F_Range_Vector) = F_Modular_Vector
            and Valid_Next (Ctx, F_Range_Vector))
           and Invalid (Ctx, F_Range_Vector)
           and Invalid (Ctx, F_Enumeration_Vector)
           and Invalid (Ctx, F_AV_Enumeration_Vector));

   procedure Switch_To_Range_Vector (Ctx : in out Context; Seq_Ctx : out Sequence.Range_Vector.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Range_Vector)
       and then Field_Size (Ctx, F_Range_Vector) > 0
       and then Field_First (Ctx, F_Range_Vector) mod RFLX_Types.Byte'Size = 1
       and then Field_Condition (Ctx, (Fld => F_Range_Vector))
       and then Available_Space (Ctx, F_Range_Vector) >= Field_Size (Ctx, F_Range_Vector),
     Post =>
       not Has_Buffer (Ctx)
       and Sequence.Range_Vector.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_Range_Vector)
       and Seq_Ctx.Last = Field_Last (Ctx, F_Range_Vector)
       and Sequence.Range_Vector.Valid (Seq_Ctx)
       and Sequence.Range_Vector.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_Range_Vector)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Range_Vector) = Predecessor (Ctx, F_Range_Vector)'Old
       and Path_Condition (Ctx, F_Range_Vector) = Path_Condition (Ctx, F_Range_Vector)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Modular_Vector) = Context_Cursor (Ctx, F_Modular_Vector)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Range_Vector) =>
           Context_Cursor (Ctx, F_Enumeration_Vector) = Context_Cursor (Ctx, F_Enumeration_Vector)'Old
           and Context_Cursor (Ctx, F_AV_Enumeration_Vector) = Context_Cursor (Ctx, F_AV_Enumeration_Vector)'Old,
        others =>
           (Predecessor (Ctx, F_Enumeration_Vector) = F_Range_Vector
            and Valid_Next (Ctx, F_Enumeration_Vector))
           and Invalid (Ctx, F_Enumeration_Vector)
           and Invalid (Ctx, F_AV_Enumeration_Vector));

   procedure Switch_To_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : out Sequence.Enumeration_Vector.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Enumeration_Vector)
       and then Field_Size (Ctx, F_Enumeration_Vector) > 0
       and then Field_First (Ctx, F_Enumeration_Vector) mod RFLX_Types.Byte'Size = 1
       and then Field_Condition (Ctx, (Fld => F_Enumeration_Vector))
       and then Available_Space (Ctx, F_Enumeration_Vector) >= Field_Size (Ctx, F_Enumeration_Vector),
     Post =>
       not Has_Buffer (Ctx)
       and Sequence.Enumeration_Vector.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_Enumeration_Vector)
       and Seq_Ctx.Last = Field_Last (Ctx, F_Enumeration_Vector)
       and Sequence.Enumeration_Vector.Valid (Seq_Ctx)
       and Sequence.Enumeration_Vector.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_Enumeration_Vector)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Enumeration_Vector) = Predecessor (Ctx, F_Enumeration_Vector)'Old
       and Path_Condition (Ctx, F_Enumeration_Vector) = Path_Condition (Ctx, F_Enumeration_Vector)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Modular_Vector) = Context_Cursor (Ctx, F_Modular_Vector)'Old
       and Context_Cursor (Ctx, F_Range_Vector) = Context_Cursor (Ctx, F_Range_Vector)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Enumeration_Vector) =>
           Context_Cursor (Ctx, F_AV_Enumeration_Vector) = Context_Cursor (Ctx, F_AV_Enumeration_Vector)'Old,
        others =>
           (Predecessor (Ctx, F_AV_Enumeration_Vector) = F_Enumeration_Vector
            and Valid_Next (Ctx, F_AV_Enumeration_Vector))
           and Invalid (Ctx, F_AV_Enumeration_Vector));

   procedure Switch_To_AV_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : out Sequence.AV_Enumeration_Vector.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_AV_Enumeration_Vector)
       and then Field_Size (Ctx, F_AV_Enumeration_Vector) > 0
       and then Field_First (Ctx, F_AV_Enumeration_Vector) mod RFLX_Types.Byte'Size = 1
       and then Field_Condition (Ctx, (Fld => F_AV_Enumeration_Vector))
       and then Available_Space (Ctx, F_AV_Enumeration_Vector) >= Field_Size (Ctx, F_AV_Enumeration_Vector),
     Post =>
       not Has_Buffer (Ctx)
       and Sequence.AV_Enumeration_Vector.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_AV_Enumeration_Vector)
       and Seq_Ctx.Last = Field_Last (Ctx, F_AV_Enumeration_Vector)
       and Sequence.AV_Enumeration_Vector.Valid (Seq_Ctx)
       and Sequence.AV_Enumeration_Vector.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_AV_Enumeration_Vector)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_AV_Enumeration_Vector) = Predecessor (Ctx, F_AV_Enumeration_Vector)'Old
       and Path_Condition (Ctx, F_AV_Enumeration_Vector) = Path_Condition (Ctx, F_AV_Enumeration_Vector)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Modular_Vector) = Context_Cursor (Ctx, F_Modular_Vector)'Old
       and Context_Cursor (Ctx, F_Range_Vector) = Context_Cursor (Ctx, F_Range_Vector)'Old
       and Context_Cursor (Ctx, F_Enumeration_Vector) = Context_Cursor (Ctx, F_Enumeration_Vector)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_AV_Enumeration_Vector) =>
           True,
        others =>
           True);

   function Complete_Modular_Vector (Ctx : Context; Seq_Ctx : Sequence.Modular_Vector.Context) return Boolean with
     Pre =>
       Valid_Next (Ctx, F_Modular_Vector);

   function Complete_Range_Vector (Ctx : Context; Seq_Ctx : Sequence.Range_Vector.Context) return Boolean with
     Pre =>
       Valid_Next (Ctx, F_Range_Vector);

   function Complete_Enumeration_Vector (Ctx : Context; Seq_Ctx : Sequence.Enumeration_Vector.Context) return Boolean with
     Pre =>
       Valid_Next (Ctx, F_Enumeration_Vector);

   function Complete_AV_Enumeration_Vector (Ctx : Context; Seq_Ctx : Sequence.AV_Enumeration_Vector.Context) return Boolean with
     Pre =>
       Valid_Next (Ctx, F_AV_Enumeration_Vector);

   procedure Update_Modular_Vector (Ctx : in out Context; Seq_Ctx : in out Sequence.Modular_Vector.Context) with
     Pre =>
       Present (Ctx, F_Modular_Vector)
       and then Complete_Modular_Vector (Ctx, Seq_Ctx)
       and then not Has_Buffer (Ctx)
       and then Sequence.Modular_Vector.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_Modular_Vector)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_Modular_Vector),
     Post =>
       Present (Ctx, F_Modular_Vector)
       and Has_Buffer (Ctx)
       and not Sequence.Modular_Vector.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Seq_Ctx.First = Seq_Ctx.First'Old
       and Seq_Ctx.Last = Seq_Ctx.Last'Old
       and Field_First (Ctx, F_Modular_Vector) = Field_First (Ctx, F_Modular_Vector)'Old
       and Field_Size (Ctx, F_Modular_Vector) = Field_Size (Ctx, F_Modular_Vector)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Range_Vector) = Context_Cursor (Ctx, F_Range_Vector)'Old
       and Context_Cursor (Ctx, F_Enumeration_Vector) = Context_Cursor (Ctx, F_Enumeration_Vector)'Old
       and Context_Cursor (Ctx, F_AV_Enumeration_Vector) = Context_Cursor (Ctx, F_AV_Enumeration_Vector)'Old,
     Depends =>
       (Ctx => (Ctx, Seq_Ctx), Seq_Ctx => Seq_Ctx);

   procedure Update_Range_Vector (Ctx : in out Context; Seq_Ctx : in out Sequence.Range_Vector.Context) with
     Pre =>
       Present (Ctx, F_Range_Vector)
       and then Complete_Range_Vector (Ctx, Seq_Ctx)
       and then not Has_Buffer (Ctx)
       and then Sequence.Range_Vector.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_Range_Vector)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_Range_Vector),
     Post =>
       Present (Ctx, F_Range_Vector)
       and Has_Buffer (Ctx)
       and not Sequence.Range_Vector.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Seq_Ctx.First = Seq_Ctx.First'Old
       and Seq_Ctx.Last = Seq_Ctx.Last'Old
       and Field_First (Ctx, F_Range_Vector) = Field_First (Ctx, F_Range_Vector)'Old
       and Field_Size (Ctx, F_Range_Vector) = Field_Size (Ctx, F_Range_Vector)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Modular_Vector) = Context_Cursor (Ctx, F_Modular_Vector)'Old
       and Context_Cursor (Ctx, F_Enumeration_Vector) = Context_Cursor (Ctx, F_Enumeration_Vector)'Old
       and Context_Cursor (Ctx, F_AV_Enumeration_Vector) = Context_Cursor (Ctx, F_AV_Enumeration_Vector)'Old,
     Depends =>
       (Ctx => (Ctx, Seq_Ctx), Seq_Ctx => Seq_Ctx);

   procedure Update_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : in out Sequence.Enumeration_Vector.Context) with
     Pre =>
       Present (Ctx, F_Enumeration_Vector)
       and then Complete_Enumeration_Vector (Ctx, Seq_Ctx)
       and then not Has_Buffer (Ctx)
       and then Sequence.Enumeration_Vector.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_Enumeration_Vector)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_Enumeration_Vector),
     Post =>
       Present (Ctx, F_Enumeration_Vector)
       and Has_Buffer (Ctx)
       and not Sequence.Enumeration_Vector.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Seq_Ctx.First = Seq_Ctx.First'Old
       and Seq_Ctx.Last = Seq_Ctx.Last'Old
       and Field_First (Ctx, F_Enumeration_Vector) = Field_First (Ctx, F_Enumeration_Vector)'Old
       and Field_Size (Ctx, F_Enumeration_Vector) = Field_Size (Ctx, F_Enumeration_Vector)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Modular_Vector) = Context_Cursor (Ctx, F_Modular_Vector)'Old
       and Context_Cursor (Ctx, F_Range_Vector) = Context_Cursor (Ctx, F_Range_Vector)'Old
       and Context_Cursor (Ctx, F_AV_Enumeration_Vector) = Context_Cursor (Ctx, F_AV_Enumeration_Vector)'Old,
     Depends =>
       (Ctx => (Ctx, Seq_Ctx), Seq_Ctx => Seq_Ctx);

   procedure Update_AV_Enumeration_Vector (Ctx : in out Context; Seq_Ctx : in out Sequence.AV_Enumeration_Vector.Context) with
     Pre =>
       Present (Ctx, F_AV_Enumeration_Vector)
       and then Complete_AV_Enumeration_Vector (Ctx, Seq_Ctx)
       and then not Has_Buffer (Ctx)
       and then Sequence.AV_Enumeration_Vector.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_AV_Enumeration_Vector)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_AV_Enumeration_Vector),
     Post =>
       Present (Ctx, F_AV_Enumeration_Vector)
       and Has_Buffer (Ctx)
       and not Sequence.AV_Enumeration_Vector.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Seq_Ctx.First = Seq_Ctx.First'Old
       and Seq_Ctx.Last = Seq_Ctx.Last'Old
       and Field_First (Ctx, F_AV_Enumeration_Vector) = Field_First (Ctx, F_AV_Enumeration_Vector)'Old
       and Field_Size (Ctx, F_AV_Enumeration_Vector) = Field_Size (Ctx, F_AV_Enumeration_Vector)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Modular_Vector) = Context_Cursor (Ctx, F_Modular_Vector)'Old
       and Context_Cursor (Ctx, F_Range_Vector) = Context_Cursor (Ctx, F_Range_Vector)'Old
       and Context_Cursor (Ctx, F_Enumeration_Vector) = Context_Cursor (Ctx, F_Enumeration_Vector)'Old,
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
          when F_Length =>
             Valid (Val.Length_Value),
          when F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector =>
             True,
          when F_Initial | F_Final =>
             False));

   type Field_Cursor (State : Cursor_State := S_Invalid) is
      record
         Predecessor : Virtual_Field := F_Final;
         case State is
            when S_Valid | S_Structural_Valid =>
               First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First;
               Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First;
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

   pragma Warnings (Off, """Buffer"" is not modified, could be of access constant type");

   function Valid_Context (Buffer_First, Buffer_Last : RFLX_Types.Index; First, Last : RFLX_Types.Bit_Index; Message_Last : RFLX_Types.Bit_Length; Buffer : RFLX_Types.Bytes_Ptr; Cursors : Field_Cursors) return Boolean is
     ((if
          Buffer /= null
       then
          Buffer'First = Buffer_First
          and Buffer'Last = Buffer_Last)
      and then (RFLX_Types.Byte_Index (First) >= Buffer_First
                and RFLX_Types.Byte_Index (Last) <= Buffer_Last
                and First <= Last
                and Last < RFLX_Types.Bit_Index'Last
                and First mod RFLX_Types.Byte'Size = 1
                and Last mod RFLX_Types.Byte'Size = 0)
      and then First - 1 <= Message_Last
      and then Message_Last <= Last
      and then First mod RFLX_Types.Byte'Size = 1
      and then Last mod RFLX_Types.Byte'Size = 0
      and then Message_Last mod RFLX_Types.Byte'Size = 0
      and then (for all F in Field'First .. Field'Last =>
                   (if
                       Structural_Valid (Cursors (F))
                    then
                       Cursors (F).First >= First
                       and Cursors (F).Last <= Message_Last
                       and Cursors (F).First <= Cursors (F).Last + 1
                       and Cursors (F).Value.Fld = F))
      and then ((if
                    Structural_Valid (Cursors (F_Modular_Vector))
                 then
                    (Valid (Cursors (F_Length))
                     and then Cursors (F_Modular_Vector).Predecessor = F_Length))
                and then (if
                             Structural_Valid (Cursors (F_Range_Vector))
                          then
                             (Structural_Valid (Cursors (F_Modular_Vector))
                              and then Cursors (F_Range_Vector).Predecessor = F_Modular_Vector))
                and then (if
                             Structural_Valid (Cursors (F_Enumeration_Vector))
                          then
                             (Structural_Valid (Cursors (F_Range_Vector))
                              and then Cursors (F_Enumeration_Vector).Predecessor = F_Range_Vector))
                and then (if
                             Structural_Valid (Cursors (F_AV_Enumeration_Vector))
                          then
                             (Structural_Valid (Cursors (F_Enumeration_Vector))
                              and then Cursors (F_AV_Enumeration_Vector).Predecessor = F_Enumeration_Vector)))
      and then ((if
                    Invalid (Cursors (F_Length))
                 then
                    Invalid (Cursors (F_Modular_Vector)))
                and then (if
                             Invalid (Cursors (F_Modular_Vector))
                          then
                             Invalid (Cursors (F_Range_Vector)))
                and then (if
                             Invalid (Cursors (F_Range_Vector))
                          then
                             Invalid (Cursors (F_Enumeration_Vector)))
                and then (if
                             Invalid (Cursors (F_Enumeration_Vector))
                          then
                             Invalid (Cursors (F_AV_Enumeration_Vector))))
      and then (if
                   Structural_Valid (Cursors (F_Length))
                then
                   Cursors (F_Length).Last - Cursors (F_Length).First + 1 = RFLX.Sequence.Length'Size
                   and then Cursors (F_Length).Predecessor = F_Initial
                   and then Cursors (F_Length).First = First
                   and then (if
                                Structural_Valid (Cursors (F_Modular_Vector))
                             then
                                Cursors (F_Modular_Vector).Last - Cursors (F_Modular_Vector).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Length).Value.Length_Value) * 8
                                and then Cursors (F_Modular_Vector).Predecessor = F_Length
                                and then Cursors (F_Modular_Vector).First = Cursors (F_Length).Last + 1
                                and then (if
                                             Structural_Valid (Cursors (F_Range_Vector))
                                          then
                                             Cursors (F_Range_Vector).Last - Cursors (F_Range_Vector).First + 1 = 16
                                             and then Cursors (F_Range_Vector).Predecessor = F_Modular_Vector
                                             and then Cursors (F_Range_Vector).First = Cursors (F_Modular_Vector).Last + 1
                                             and then (if
                                                          Structural_Valid (Cursors (F_Enumeration_Vector))
                                                       then
                                                          Cursors (F_Enumeration_Vector).Last - Cursors (F_Enumeration_Vector).First + 1 = 16
                                                          and then Cursors (F_Enumeration_Vector).Predecessor = F_Range_Vector
                                                          and then Cursors (F_Enumeration_Vector).First = Cursors (F_Range_Vector).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_AV_Enumeration_Vector))
                                                                    then
                                                                       Cursors (F_AV_Enumeration_Vector).Last - Cursors (F_AV_Enumeration_Vector).First + 1 = 16
                                                                       and then Cursors (F_AV_Enumeration_Vector).Predecessor = F_Enumeration_Vector
                                                                       and then Cursors (F_AV_Enumeration_Vector).First = Cursors (F_Enumeration_Vector).Last + 1))))));

   pragma Warnings (On, """Buffer"" is not modified, could be of access constant type");

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First + 7) is
      record
         Message_Last : RFLX_Types.Bit_Length := First - 1;
         Buffer : RFLX_Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Message_Last, Context.Buffer, Context.Cursors);

   function Initialized (Ctx : Context) return Boolean is
     (Ctx.Message_Last = Ctx.First - 1
      and then Valid_Next (Ctx, F_Length)
      and then Field_First (Ctx, F_Length) mod RFLX_Types.Byte'Size = 1
      and then Available_Space (Ctx, F_Length) = Ctx.Last - Ctx.First + 1
      and then Invalid (Ctx, F_Length)
      and then Invalid (Ctx, F_Modular_Vector)
      and then Invalid (Ctx, F_Range_Vector)
      and then Invalid (Ctx, F_Enumeration_Vector)
      and then Invalid (Ctx, F_AV_Enumeration_Vector));

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Message_Last);

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Length =>
                    True,
                 when others =>
                    False),
          when F_Length =>
             (case Fld is
                 when F_Modular_Vector =>
                    True,
                 when others =>
                    False),
          when F_Modular_Vector =>
             (case Fld is
                 when F_Range_Vector =>
                    True,
                 when others =>
                    False),
          when F_Range_Vector =>
             (case Fld is
                 when F_Enumeration_Vector =>
                    True,
                 when others =>
                    False),
          when F_Enumeration_Vector =>
             (case Fld is
                 when F_AV_Enumeration_Vector =>
                    True,
                 when others =>
                    False),
          when F_AV_Enumeration_Vector | F_Final =>
             False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
          when F_Initial | F_Length | F_Modular_Vector | F_Range_Vector | F_Enumeration_Vector | F_AV_Enumeration_Vector =>
             True,
          when F_Final =>
             False));

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Length =>
                    RFLX.Sequence.Length'Size,
                 when others =>
                    raise Program_Error),
          when F_Length =>
             (case Fld is
                 when F_Modular_Vector =>
                    RFLX_Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8,
                 when others =>
                    raise Program_Error),
          when F_Modular_Vector =>
             (case Fld is
                 when F_Range_Vector =>
                    16,
                 when others =>
                    raise Program_Error),
          when F_Range_Vector =>
             (case Fld is
                 when F_Enumeration_Vector =>
                    16,
                 when others =>
                    raise Program_Error),
          when F_Enumeration_Vector =>
             (case Fld is
                 when F_AV_Enumeration_Vector =>
                    16,
                 when others =>
                    raise Program_Error),
          when F_AV_Enumeration_Vector | F_Final =>
             0));

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index is
     ((case Fld is
          when F_Length =>
             Ctx.First,
          when F_Modular_Vector =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Length
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Range_Vector =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Modular_Vector
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Enumeration_Vector =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Range_Vector
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_AV_Enumeration_Vector =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Enumeration_Vector
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error)));

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index is
     (Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) - 1);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field is
     ((case Fld is
          when F_Initial =>
             F_Initial,
          when others =>
             Ctx.Cursors (Fld).Predecessor));

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
          when F_Initial =>
             True,
          when F_Length =>
             Ctx.Cursors (Fld).Predecessor = F_Initial,
          when F_Modular_Vector =>
             (Valid (Ctx.Cursors (F_Length))
              and Ctx.Cursors (Fld).Predecessor = F_Length),
          when F_Range_Vector =>
             (Structural_Valid (Ctx.Cursors (F_Modular_Vector))
              and Ctx.Cursors (Fld).Predecessor = F_Modular_Vector),
          when F_Enumeration_Vector =>
             (Structural_Valid (Ctx.Cursors (F_Range_Vector))
              and Ctx.Cursors (Fld).Predecessor = F_Range_Vector),
          when F_AV_Enumeration_Vector =>
             (Structural_Valid (Ctx.Cursors (F_Enumeration_Vector))
              and Ctx.Cursors (Fld).Predecessor = F_Enumeration_Vector),
          when F_Final =>
             (Structural_Valid (Ctx.Cursors (F_AV_Enumeration_Vector))
              and Ctx.Cursors (Fld).Predecessor = F_AV_Enumeration_Vector)));

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean is
     (Valid_Predecessor (Ctx, Fld)
      and then Path_Condition (Ctx, Fld));

   function Available_Space (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     (Ctx.Last - Field_First (Ctx, Fld) + 1);

   function Present (Ctx : Context; Fld : Field) return Boolean is
     (Structural_Valid (Ctx.Cursors (Fld))
      and then Ctx.Cursors (Fld).First < Ctx.Cursors (Fld).Last + 1);

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean is
     ((Ctx.Cursors (Fld).State = S_Valid
       or Ctx.Cursors (Fld).State = S_Structural_Valid));

   function Valid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Valid
      and then Ctx.Cursors (Fld).First < Ctx.Cursors (Fld).Last + 1);

   function Incomplete (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Incomplete);

   function Invalid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Invalid
      or Ctx.Cursors (Fld).State = S_Incomplete);

   function Structural_Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Length)
      and then Structural_Valid (Ctx, F_Modular_Vector)
      and then Structural_Valid (Ctx, F_Range_Vector)
      and then Structural_Valid (Ctx, F_Enumeration_Vector)
      and then Structural_Valid (Ctx, F_AV_Enumeration_Vector));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Length)
      and then Valid (Ctx, F_Modular_Vector)
      and then Valid (Ctx, F_Range_Vector)
      and then Valid (Ctx, F_Enumeration_Vector)
      and then Valid (Ctx, F_AV_Enumeration_Vector));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Length)
      or Incomplete (Ctx, F_Modular_Vector)
      or Incomplete (Ctx, F_Range_Vector)
      or Incomplete (Ctx, F_Enumeration_Vector)
      or Incomplete (Ctx, F_AV_Enumeration_Vector));

   function Get_Length (Ctx : Context) return RFLX.Sequence.Length is
     (To_Actual (Ctx.Cursors (F_Length).Value.Length_Value));

   function Complete_Modular_Vector (Ctx : Context; Seq_Ctx : Sequence.Modular_Vector.Context) return Boolean is
     (Sequence.Modular_Vector.Valid (Seq_Ctx)
      and Sequence.Modular_Vector.Size (Seq_Ctx) = Field_Size (Ctx, F_Modular_Vector));

   function Complete_Range_Vector (Ctx : Context; Seq_Ctx : Sequence.Range_Vector.Context) return Boolean is
     (Sequence.Range_Vector.Valid (Seq_Ctx)
      and Sequence.Range_Vector.Size (Seq_Ctx) = Field_Size (Ctx, F_Range_Vector));

   function Complete_Enumeration_Vector (Ctx : Context; Seq_Ctx : Sequence.Enumeration_Vector.Context) return Boolean is
     (Sequence.Enumeration_Vector.Valid (Seq_Ctx)
      and Sequence.Enumeration_Vector.Size (Seq_Ctx) = Field_Size (Ctx, F_Enumeration_Vector));

   function Complete_AV_Enumeration_Vector (Ctx : Context; Seq_Ctx : Sequence.AV_Enumeration_Vector.Context) return Boolean is
     (Sequence.AV_Enumeration_Vector.Valid (Seq_Ctx)
      and Sequence.AV_Enumeration_Vector.Size (Seq_Ctx) = Field_Size (Ctx, F_AV_Enumeration_Vector));

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.Sequence.Message;

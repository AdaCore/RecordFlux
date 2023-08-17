pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.Sequence.Integer_Vector;

package RFLX.Sequence.Sequence_Size_Defined_By_Message_Size with
  SPARK_Mode,
  Always_Terminates
is

   pragma Warnings (Off, "use clause for type ""Base_Integer"" * has no effect");

   pragma Warnings (Off, "use clause for type ""Bytes"" * has no effect");

   pragma Warnings (Off, """BASE_INTEGER"" is already use-visible through previous use_type_clause");

   pragma Warnings (Off, """LENGTH"" is already use-visible through previous use_type_clause");

   use type RFLX_Types.Bytes;

   use type RFLX_Types.Byte;

   use type RFLX_Types.Bytes_Ptr;

   use type RFLX_Types.Length;

   use type RFLX_Types.Index;

   use type RFLX_Types.Bit_Index;

   use type RFLX_Types.Base_Integer;

   use type RFLX_Types.Offset;

   pragma Warnings (On, """LENGTH"" is already use-visible through previous use_type_clause");

   pragma Warnings (On, """BASE_INTEGER"" is already use-visible through previous use_type_clause");

   pragma Warnings (On, "use clause for type ""Base_Integer"" * has no effect");

   pragma Warnings (On, "use clause for type ""Bytes"" * has no effect");

   pragma Unevaluated_Use_Of_Old (Allow);

   type Virtual_Field is (F_Initial, F_Header, F_Vector, F_Final);

   subtype Field is Virtual_Field range F_Header .. F_Vector;

   type Field_Cursor is private;

   type Field_Cursors is private;

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First) is private with
     Default_Initial_Condition =>
       RFLX_Types.To_Index (First) >= Buffer_First
       and RFLX_Types.To_Index (Last) <= Buffer_Last
       and Buffer_Last < RFLX_Types.Index'Last
       and First <= Last + 1
       and Last < RFLX_Types.Bit_Index'Last
       and First rem RFLX_Types.Byte'Size = 1
       and Last rem RFLX_Types.Byte'Size = 0;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; Written_Last : RFLX_Types.Bit_Length := 0) with
     Pre =>
       not Ctx'Constrained
       and then Buffer /= null
       and then Buffer'Length > 0
       and then Buffer'Last < RFLX_Types.Index'Last
       and then (Written_Last = 0
                 or (Written_Last >= RFLX_Types.To_First_Bit_Index (Buffer'First) - 1
                     and Written_Last <= RFLX_Types.To_Last_Bit_Index (Buffer'Last)))
       and then Written_Last mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Buffer = null
       and Ctx.Buffer_First = Buffer'First'Old
       and Ctx.Buffer_Last = Buffer'Last'Old
       and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
       and Ctx.Last = RFLX_Types.To_Last_Bit_Index (Ctx.Buffer_Last)
       and Initialized (Ctx),
     Depends =>
       (Ctx => (Buffer, Written_Last), Buffer => null);

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length; Written_Last : RFLX_Types.Bit_Length := 0) with
     Pre =>
       not Ctx'Constrained
       and then Buffer /= null
       and then Buffer'Length > 0
       and then Buffer'Last < RFLX_Types.Index'Last
       and then RFLX_Types.To_Index (First) >= Buffer'First
       and then RFLX_Types.To_Index (Last) <= Buffer'Last
       and then First <= Last + 1
       and then Last < RFLX_Types.Bit_Index'Last
       and then First rem RFLX_Types.Byte'Size = 1
       and then Last rem RFLX_Types.Byte'Size = 0
       and then (Written_Last = 0
                 or (Written_Last >= First - 1
                     and Written_Last <= Last))
       and then Written_Last rem RFLX_Types.Byte'Size = 0,
     Post =>
       Buffer = null
       and Has_Buffer (Ctx)
       and Ctx.Buffer_First = Buffer'First'Old
       and Ctx.Buffer_Last = Buffer'Last'Old
       and Ctx.First = First
       and Ctx.Last = Last
       and Initialized (Ctx),
     Depends =>
       (Ctx => (Buffer, First, Last, Written_Last), Buffer => null);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Initialized (Ctx : Context) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   procedure Reset (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
       and Ctx.Last = RFLX_Types.To_Last_Bit_Index (Ctx.Buffer_Last)
       and Initialized (Ctx);

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) with
     Pre =>
       not Ctx'Constrained
       and RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and RFLX_Types.To_Index (First) >= Ctx.Buffer_First
       and RFLX_Types.To_Index (Last) <= Ctx.Buffer_Last
       and First <= Last + 1
       and Last < RFLX_Types.Bit_Length'Last
       and First rem RFLX_Types.Byte'Size = 1
       and Last rem RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = First
       and Ctx.Last = Last
       and Initialized (Ctx);

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX_Types.Bytes_Ptr) with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx),
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
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Well_Formed_Message (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Byte_Size (Ctx) = Buffer'Length;

   function Read (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Well_Formed_Message (Ctx);

   pragma Warnings (Off, "formal parameter ""*"" is not referenced");

   pragma Warnings (Off, "unused variable ""*""");

   function Always_Valid (Buffer : RFLX_Types.Bytes) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""*""");

   pragma Warnings (On, "formal parameter ""*"" is not referenced");

   generic
      with procedure Read (Buffer : RFLX_Types.Bytes);
      with function Pre (Buffer : RFLX_Types.Bytes) return Boolean is Always_Valid;
   procedure Generic_Read (Ctx : Context) with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Well_Formed_Message (Ctx)
       and then Pre (Read (Ctx));

   pragma Warnings (Off, "formal parameter ""*"" is not referenced");

   pragma Warnings (Off, "unused variable ""*""");

   function Always_Valid (Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""*""");

   pragma Warnings (On, "formal parameter ""*"" is not referenced");

   generic
      with procedure Write (Buffer : out RFLX_Types.Bytes; Length : out RFLX_Types.Length; Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length);
      with function Pre (Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) return Boolean is Always_Valid;
   procedure Generic_Write (Ctx : in out Context; Offset : RFLX_Types.Length := 0) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then Offset < RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Buffer_Length (Ctx)
       and then Pre (RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Buffer_Length (Ctx), Offset),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
       and Initialized (Ctx);

   function Has_Buffer (Ctx : Context) return Boolean;

   function Buffer_Length (Ctx : Context) return RFLX_Types.Length with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx);

   function Size (Ctx : Context) return RFLX_Types.Bit_Length with
     Post =>
       Size'Result rem RFLX_Types.Byte'Size = 0;

   function Byte_Size (Ctx : Context) return RFLX_Types.Length;

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Well_Formed_Message (Ctx);

   function Written_Last (Ctx : Context) return RFLX_Types.Bit_Length;

   procedure Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Well_Formed_Message (Ctx)
       and then Data'Length = RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Byte_Size (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Value (Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Predecessor (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Predecessor (Ctx, Fld)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Sufficient_Space (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Vector =>
              Field_Size'Result rem RFLX_Types.Byte'Size = 0,
           when others =>
              True);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Sufficient_Space (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Vector =>
              Field_Last'Result rem RFLX_Types.Byte'Size = 0,
           when others =>
              True);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean;

   function Available_Space (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld);

   function Sufficient_Space (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old;

   function Present (Ctx : Context; Fld : Field) return Boolean;

   function Well_Formed (Ctx : Context; Fld : Field) return Boolean;

   function Valid (Ctx : Context; Fld : Field) return Boolean with
     Post =>
       (if Valid'Result then Well_Formed (Ctx, Fld) and Present (Ctx, Fld));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean;

   function Invalid (Ctx : Context; Fld : Field) return Boolean;

   function Well_Formed_Message (Ctx : Context) return Boolean with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Incomplete_Message (Ctx : Context) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "precondition is always False");

   function Get_Header (Ctx : Context) return RFLX.Sequence.Enumeration with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Header);

   pragma Warnings (On, "precondition is always False");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Set_Header (Ctx : in out Context; Val : RFLX.Sequence.Enumeration) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Header)
       and then RFLX.Sequence.Valid_Enumeration (RFLX.Sequence.To_Base_Integer (Val))
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Available_Space (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Header) >= RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Size (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Header)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Condition (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Header),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Header)
       and Get_Header (Ctx) = Val
       and Invalid (Ctx, F_Vector)
       and (Predecessor (Ctx, F_Vector) = F_Header
            and Valid_Next (Ctx, F_Vector))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Header) = Predecessor (Ctx, F_Header)'Old
       and Valid_Next (Ctx, F_Header) = Valid_Next (Ctx, F_Header)'Old
       and Field_First (Ctx, F_Header) = Field_First (Ctx, F_Header)'Old;

   pragma Warnings (On, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Set_Vector_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Available_Space (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector) >= RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Size (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Condition (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Size (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Vector)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Vector))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Vector) = Predecessor (Ctx, F_Vector)'Old
       and Valid_Next (Ctx, F_Vector) = Valid_Next (Ctx, F_Vector)'Old
       and Get_Header (Ctx) = Get_Header (Ctx)'Old
       and Field_First (Ctx, F_Vector) = Field_First (Ctx, F_Vector)'Old;

   procedure Set_Vector (Ctx : in out Context; Seq_Ctx : RFLX.Sequence.Integer_Vector.Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Available_Space (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector) >= RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Size (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Condition (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Length (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector, RFLX.Sequence.Integer_Vector.Byte_Size (Seq_Ctx))
       and then RFLX.Sequence.Integer_Vector.Has_Buffer (Seq_Ctx)
       and then RFLX.Sequence.Integer_Vector.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Vector)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Vector))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Vector) = Predecessor (Ctx, F_Vector)'Old
       and Valid_Next (Ctx, F_Vector) = Valid_Next (Ctx, F_Vector)'Old
       and Get_Header (Ctx) = Get_Header (Ctx)'Old
       and Field_First (Ctx, F_Vector) = Field_First (Ctx, F_Vector)'Old
       and (if Field_Size (Ctx, F_Vector) > 0 then Present (Ctx, F_Vector));

   procedure Initialize_Vector (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Length (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector, Length)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Available_Space (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector) >= RFLX_Types.To_Bit_Length (Length),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Vector)
       and Field_Size (Ctx, F_Vector) = RFLX_Types.To_Bit_Length (Length)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Vector))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Vector) = Predecessor (Ctx, F_Vector)'Old
       and Valid_Next (Ctx, F_Vector) = Valid_Next (Ctx, F_Vector)'Old
       and Get_Header (Ctx) = Get_Header (Ctx)'Old
       and Field_First (Ctx, F_Vector) = Field_First (Ctx, F_Vector)'Old;

   procedure Switch_To_Vector (Ctx : in out Context; Seq_Ctx : out RFLX.Sequence.Integer_Vector.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Size (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector) > 0
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_First (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector) rem RFLX_Types.Byte'Size = 1
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Available_Space (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector) >= RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Size (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)
       and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_Condition (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector),
     Post =>
       not RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and RFLX.Sequence.Integer_Vector.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_Vector)
       and Seq_Ctx.Last = Field_Last (Ctx, F_Vector)
       and RFLX.Sequence.Integer_Vector.Valid (Seq_Ctx)
       and RFLX.Sequence.Integer_Vector.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_Vector)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Vector) = Predecessor (Ctx, F_Vector)'Old
       and Path_Condition (Ctx, F_Vector) = Path_Condition (Ctx, F_Vector)'Old
       and Field_Last (Ctx, F_Vector) = Field_Last (Ctx, F_Vector)'Old
       and (for all F in Field range F_Header .. F_Header =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F)),
     Contract_Cases =>
       (Well_Formed (Ctx, F_Vector) =>
           True,
        others =>
           True);

   function Complete_Vector (Ctx : Context; Seq_Ctx : RFLX.Sequence.Integer_Vector.Context) return Boolean with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector);

   procedure Update_Vector (Ctx : in out Context; Seq_Ctx : in out RFLX.Sequence.Integer_Vector.Context) with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Present (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Vector)
       and then not RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Has_Buffer (Ctx)
       and then RFLX.Sequence.Integer_Vector.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_Vector)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_Vector),
     Post =>
       (if
           RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Complete_Vector (Ctx, Seq_Ctx)
        then
           Present (Ctx, F_Vector)
        else
           Invalid (Ctx, F_Vector))
       and Has_Buffer (Ctx)
       and not RFLX.Sequence.Integer_Vector.Has_Buffer (Seq_Ctx)
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

   function Context_Cursors_Index (Cursors : Field_Cursors; Fld : Field) return Field_Cursor with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Cursor_State is (S_Valid, S_Well_Formed, S_Invalid, S_Incomplete);

   type Field_Cursor is
      record
         Predecessor : Virtual_Field := F_Final;
         State : Cursor_State := S_Invalid;
         First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First;
         Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First;
         Value : RFLX_Types.Base_Integer := 0;
      end record;

   type Field_Cursors is array (Virtual_Field) of Field_Cursor;

   function Well_Formed (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid
      or Cursor.State = S_Well_Formed);

   function Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid);

   function Invalid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Invalid
      or Cursor.State = S_Incomplete);

   pragma Warnings (Off, """Buffer"" is not modified, could be of access constant type");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Context (Buffer_First, Buffer_Last : RFLX_Types.Index; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length; Verified_Last : RFLX_Types.Bit_Length; Written_Last : RFLX_Types.Bit_Length; Buffer : RFLX_Types.Bytes_Ptr; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then Buffer'First = Buffer_First and Buffer'Last = Buffer_Last)
      and then (RFLX_Types.To_Index (First) >= Buffer_First
                and RFLX_Types.To_Index (Last) <= Buffer_Last
                and Buffer_Last < RFLX_Types.Index'Last
                and First <= Last + 1
                and Last < RFLX_Types.Bit_Index'Last
                and First rem RFLX_Types.Byte'Size = 1
                and Last rem RFLX_Types.Byte'Size = 0)
      and then First - 1 <= Verified_Last
      and then First - 1 <= Written_Last
      and then Verified_Last <= Written_Last
      and then Written_Last <= Last
      and then First rem RFLX_Types.Byte'Size = 1
      and then Last rem RFLX_Types.Byte'Size = 0
      and then Verified_Last rem RFLX_Types.Byte'Size = 0
      and then Written_Last rem RFLX_Types.Byte'Size = 0
      and then (for all F in Field =>
                   (if
                       Well_Formed (Cursors (F))
                    then
                       Cursors (F).First >= First
                       and Cursors (F).Last <= Verified_Last
                       and Cursors (F).First <= Cursors (F).Last + 1
                       and Valid_Value (F, Cursors (F).Value)))
      and then ((if
                    Well_Formed (Cursors (F_Vector))
                 then
                    (Valid (Cursors (F_Header))
                     and then Cursors (F_Vector).Predecessor = F_Header)))
      and then ((if Invalid (Cursors (F_Header)) then Invalid (Cursors (F_Vector))))
      and then ((if
                    Well_Formed (Cursors (F_Header))
                 then
                    (Cursors (F_Header).Last - Cursors (F_Header).First + 1 = 8
                     and then Cursors (F_Header).Predecessor = F_Initial
                     and then Cursors (F_Header).First = First))
                and then (if
                             Well_Formed (Cursors (F_Vector))
                          then
                             (Cursors (F_Vector).Last - Cursors (F_Vector).First + 1 = RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Header).Last)
                              and then Cursors (F_Vector).Predecessor = F_Header
                              and then Cursors (F_Vector).First = Cursors (F_Header).Last + 1))))
    with
     Post =>
       True;

   pragma Warnings (On, """Buffer"" is not modified, could be of access constant type");

   pragma Warnings (On, "postcondition does not mention function result");

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First) is
      record
         Verified_Last : RFLX_Types.Bit_Length := First - 1;
         Written_Last : RFLX_Types.Bit_Length := First - 1;
         Buffer : RFLX_Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => <>);
      end record with
     Dynamic_Predicate =>
       Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Verified_Last, Context.Written_Last, Context.Buffer, Context.Cursors);

   function Initialized (Ctx : Context) return Boolean is
     (Ctx.Verified_Last = Ctx.First - 1
      and then Valid_Next (Ctx, F_Header)
      and then RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Field_First (Ctx, RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.F_Header) rem RFLX_Types.Byte'Size = 1
      and then Available_Space (Ctx, F_Header) = Ctx.Last - Ctx.First + 1
      and then (for all F in Field =>
                   Invalid (Ctx, F)));

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Buffer_Length (Ctx : Context) return RFLX_Types.Length is
     (Ctx.Buffer'Length);

   function Size (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Verified_Last - Ctx.First + 1);

   function Byte_Size (Ctx : Context) return RFLX_Types.Length is
     (RFLX_Types.To_Length (Size (Ctx)));

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Verified_Last);

   function Written_Last (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Written_Last);

   function Valid_Value (Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean is
     ((case Fld is
          when F_Header =>
             RFLX.Sequence.Valid_Enumeration (Val),
          when F_Vector =>
             True));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     (True);

   function Field_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Header | F_Vector =>
             True));

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     ((case Fld is
          when F_Header =>
             8,
          when F_Vector =>
             RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Header).Last)));

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index is
     ((if Fld = F_Header then Ctx.First else Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1));

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
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
          when F_Header =>
             Ctx.Cursors (Fld).Predecessor = F_Initial,
          when F_Vector =>
             (Valid (Ctx.Cursors (F_Header))
              and Ctx.Cursors (Fld).Predecessor = F_Header),
          when F_Final =>
             (Well_Formed (Ctx.Cursors (F_Vector))
              and Ctx.Cursors (Fld).Predecessor = F_Vector)));

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean is
     (Valid_Predecessor (Ctx, Fld)
      and then Path_Condition (Ctx, Fld));

   function Available_Space (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     (Ctx.Last - Field_First (Ctx, Fld) + 1);

   function Sufficient_Space (Ctx : Context; Fld : Field) return Boolean is
     (Available_Space (Ctx, Fld) >= Field_Size (Ctx, Fld));

   function Present (Ctx : Context; Fld : Field) return Boolean is
     (Well_Formed (Ctx.Cursors (Fld))
      and then Ctx.Cursors (Fld).First < Ctx.Cursors (Fld).Last + 1);

   function Well_Formed (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Valid
      or Ctx.Cursors (Fld).State = S_Well_Formed);

   function Valid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Valid
      and then Ctx.Cursors (Fld).First < Ctx.Cursors (Fld).Last + 1);

   function Incomplete (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Incomplete);

   function Invalid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Invalid
      or Ctx.Cursors (Fld).State = S_Incomplete);

   function Well_Formed_Message (Ctx : Context) return Boolean is
     (Well_Formed (Ctx, F_Vector));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Vector));

   function Incomplete_Message (Ctx : Context) return Boolean is
     ((for some F in Field =>
          Incomplete (Ctx, F)));

   function Get_Header (Ctx : Context) return RFLX.Sequence.Enumeration is
     (To_Actual (Ctx.Cursors (F_Header).Value));

   function Valid_Size (Ctx : Context; Fld : Field; Size : RFLX_Types.Bit_Length) return Boolean is
     ((if
          Fld = F_Vector
          and then Ctx.Cursors (Fld).Predecessor = F_Header
       then
          Size <= Available_Space (Ctx, Fld)
       else
          Size = Field_Size (Ctx, Fld)))
    with
     Pre =>
       RFLX.Sequence.Sequence_Size_Defined_By_Message_Size.Valid_Next (Ctx, Fld);

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean is
     (Valid_Size (Ctx, Fld, RFLX_Types.To_Bit_Length (Length)));

   function Complete_Vector (Ctx : Context; Seq_Ctx : RFLX.Sequence.Integer_Vector.Context) return Boolean is
     (RFLX.Sequence.Integer_Vector.Valid (Seq_Ctx)
      and RFLX.Sequence.Integer_Vector.Size (Seq_Ctx) = Field_Size (Ctx, F_Vector));

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

   function Context_Cursors_Index (Cursors : Field_Cursors; Fld : Field) return Field_Cursor is
     (Cursors (Fld));

end RFLX.Sequence.Sequence_Size_Defined_By_Message_Size;

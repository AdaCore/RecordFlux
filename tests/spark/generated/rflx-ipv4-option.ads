pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Builtin_Types.Conversions;
use RFLX.RFLX_Builtin_Types.Conversions;

package RFLX.IPv4.Option with
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

   type Virtual_Field is (F_Initial, F_Copied, F_Option_Class, F_Option_Number, F_Option_Length, F_Option_Data, F_Final);

   subtype Field is Virtual_Field range F_Copied .. F_Option_Data;

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
       and RFLX.IPv4.Option.Has_Buffer (Ctx),
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
       and RFLX.IPv4.Option.Has_Buffer (Ctx)
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
       RFLX.IPv4.Option.Has_Buffer (Ctx),
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
       RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Well_Formed_Message (Ctx)
       and then RFLX.IPv4.Option.Byte_Size (Ctx) = Buffer'Length;

   function Read (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Well_Formed_Message (Ctx);

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
       RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Well_Formed_Message (Ctx)
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
       and then RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then Offset < RFLX.IPv4.Option.Buffer_Length (Ctx)
       and then Pre (RFLX.IPv4.Option.Buffer_Length (Ctx), Offset),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
       and Initialized (Ctx);

   function Has_Buffer (Ctx : Context) return Boolean;

   function Buffer_Length (Ctx : Context) return RFLX_Types.Length with
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx);

   function Size (Ctx : Context) return RFLX_Types.Bit_Length with
     Post =>
       Size'Result rem RFLX_Types.Byte'Size = 0;

   function Byte_Size (Ctx : Context) return RFLX_Types.Length;

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Well_Formed_Message (Ctx);

   function Written_Last (Ctx : Context) return RFLX_Types.Bit_Length;

   procedure Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Well_Formed_Message (Ctx)
       and then Data'Length = RFLX.IPv4.Option.Byte_Size (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Value (Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_Condition (Ctx : Context; Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean with
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Valid_Value (Fld, Val)
       and then RFLX.IPv4.Option.Valid_Next (Ctx, Fld)
       and then RFLX.IPv4.Option.Sufficient_Space (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.IPv4.Option.Valid_Next (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Option_Data =>
              Field_Size'Result rem RFLX_Types.Byte'Size = 0,
           when others =>
              True);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       RFLX.IPv4.Option.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.IPv4.Option.Valid_Next (Ctx, Fld)
       and then RFLX.IPv4.Option.Sufficient_Space (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Option_Data =>
              Field_Last'Result rem RFLX_Types.Byte'Size = 0,
           when others =>
              True);

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean;

   function Available_Space (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.IPv4.Option.Valid_Next (Ctx, Fld);

   function Sufficient_Space (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       RFLX.IPv4.Option.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean with
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx)
       and RFLX.IPv4.Option.Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx),
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
       RFLX.IPv4.Option.Has_Buffer (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Incomplete_Message (Ctx : Context) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "precondition is always False");

   function Get_Copied (Ctx : Context) return Boolean with
     Pre =>
       RFLX.IPv4.Option.Valid (Ctx, RFLX.IPv4.Option.F_Copied);

   function Get_Option_Class (Ctx : Context) return RFLX.IPv4.Option_Class with
     Pre =>
       RFLX.IPv4.Option.Valid (Ctx, RFLX.IPv4.Option.F_Option_Class);

   function Get_Option_Number (Ctx : Context) return RFLX.IPv4.Option_Number with
     Pre =>
       RFLX.IPv4.Option.Valid (Ctx, RFLX.IPv4.Option.F_Option_Number);

   function Get_Option_Length (Ctx : Context) return RFLX.IPv4.Option_Length with
     Pre =>
       RFLX.IPv4.Option.Valid (Ctx, RFLX.IPv4.Option.F_Option_Length);

   pragma Warnings (On, "precondition is always False");

   function Get_Option_Data (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Well_Formed (Ctx, RFLX.IPv4.Option.F_Option_Data)
       and then RFLX.IPv4.Option.Valid_Next (Ctx, RFLX.IPv4.Option.F_Option_Data),
     Post =>
       Get_Option_Data'Result'Length = RFLX_Types.To_Length (Field_Size (Ctx, F_Option_Data));

   procedure Get_Option_Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Well_Formed (Ctx, RFLX.IPv4.Option.F_Option_Data)
       and then RFLX.IPv4.Option.Valid_Next (Ctx, RFLX.IPv4.Option.F_Option_Data)
       and then Data'Length = RFLX_Types.To_Length (RFLX.IPv4.Option.Field_Size (Ctx, RFLX.IPv4.Option.F_Option_Data)),
     Post =>
       Equal (Ctx, F_Option_Data, Data);

   generic
      with procedure Process_Option_Data (Option_Data : RFLX_Types.Bytes);
   procedure Generic_Get_Option_Data (Ctx : Context) with
     Pre =>
       RFLX.IPv4.Option.Has_Buffer (Ctx)
       and RFLX.IPv4.Option.Present (Ctx, RFLX.IPv4.Option.F_Option_Data);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean with
     Pre =>
       RFLX.IPv4.Option.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Set_Copied (Ctx : in out Context; Val : Boolean) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Valid_Next (Ctx, RFLX.IPv4.Option.F_Copied)
       and then Valid_Boolean (To_Base_Integer (Val))
       and then RFLX.IPv4.Option.Available_Space (Ctx, RFLX.IPv4.Option.F_Copied) >= RFLX.IPv4.Option.Field_Size (Ctx, RFLX.IPv4.Option.F_Copied)
       and then RFLX.IPv4.Option.Field_Condition (Ctx, RFLX.IPv4.Option.F_Copied, To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Copied)
       and Get_Copied (Ctx) = Val
       and Invalid (Ctx, F_Option_Class)
       and Invalid (Ctx, F_Option_Number)
       and Invalid (Ctx, F_Option_Length)
       and Invalid (Ctx, F_Option_Data)
       and Valid_Next (Ctx, F_Option_Class)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Valid_Next (Ctx, F_Copied) = Valid_Next (Ctx, F_Copied)'Old
       and Field_First (Ctx, F_Copied) = Field_First (Ctx, F_Copied)'Old;

   procedure Set_Option_Class (Ctx : in out Context; Val : RFLX.IPv4.Option_Class) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Valid_Next (Ctx, RFLX.IPv4.Option.F_Option_Class)
       and then RFLX.IPv4.Valid_Option_Class (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Option.Available_Space (Ctx, RFLX.IPv4.Option.F_Option_Class) >= RFLX.IPv4.Option.Field_Size (Ctx, RFLX.IPv4.Option.F_Option_Class)
       and then RFLX.IPv4.Option.Field_Condition (Ctx, RFLX.IPv4.Option.F_Option_Class, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Option_Class)
       and Get_Option_Class (Ctx) = Val
       and Invalid (Ctx, F_Option_Number)
       and Invalid (Ctx, F_Option_Length)
       and Invalid (Ctx, F_Option_Data)
       and Valid_Next (Ctx, F_Option_Number)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Valid_Next (Ctx, F_Option_Class) = Valid_Next (Ctx, F_Option_Class)'Old
       and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
       and Field_First (Ctx, F_Option_Class) = Field_First (Ctx, F_Option_Class)'Old
       and (for all F in Field range F_Copied .. F_Copied =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Option_Number (Ctx : in out Context; Val : RFLX.IPv4.Option_Number) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Valid_Next (Ctx, RFLX.IPv4.Option.F_Option_Number)
       and then RFLX.IPv4.Valid_Option_Number (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Option.Available_Space (Ctx, RFLX.IPv4.Option.F_Option_Number) >= RFLX.IPv4.Option.Field_Size (Ctx, RFLX.IPv4.Option.F_Option_Number)
       and then RFLX.IPv4.Option.Field_Condition (Ctx, RFLX.IPv4.Option.F_Option_Number, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Option_Number)
       and Get_Option_Number (Ctx) = Val
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Number))
       and Invalid (Ctx, F_Option_Length)
       and Invalid (Ctx, F_Option_Data)
       and (if Get_Option_Number (Ctx) > 1 then Valid_Next (Ctx, F_Option_Length))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Valid_Next (Ctx, F_Option_Number) = Valid_Next (Ctx, F_Option_Number)'Old
       and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
       and Get_Option_Class (Ctx) = Get_Option_Class (Ctx)'Old
       and Field_First (Ctx, F_Option_Number) = Field_First (Ctx, F_Option_Number)'Old
       and (for all F in Field range F_Copied .. F_Option_Class =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Option_Length (Ctx : in out Context; Val : RFLX.IPv4.Option_Length) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Valid_Next (Ctx, RFLX.IPv4.Option.F_Option_Length)
       and then RFLX.IPv4.Valid_Option_Length (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Option.Available_Space (Ctx, RFLX.IPv4.Option.F_Option_Length) >= RFLX.IPv4.Option.Field_Size (Ctx, RFLX.IPv4.Option.F_Option_Length)
       and then RFLX.IPv4.Option.Field_Condition (Ctx, RFLX.IPv4.Option.F_Option_Length, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Option_Length)
       and Get_Option_Length (Ctx) = Val
       and Invalid (Ctx, F_Option_Data)
       and (if
               (RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Class (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Debugging_And_Measurement))
                and then Get_Option_Number (Ctx) = 4)
               or else (RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Class (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                        and then (Get_Option_Number (Ctx) = 9
                                  or else Get_Option_Number (Ctx) = 3
                                  or else Get_Option_Number (Ctx) = 7))
               or else (Get_Option_Length (Ctx) = 11
                        and then RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Class (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                        and then Get_Option_Number (Ctx) = 2)
               or else (Get_Option_Length (Ctx) = 4
                        and then RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Class (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                        and then Get_Option_Number (Ctx) = 8)
            then
               Valid_Next (Ctx, F_Option_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Valid_Next (Ctx, F_Option_Length) = Valid_Next (Ctx, F_Option_Length)'Old
       and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
       and Get_Option_Class (Ctx) = Get_Option_Class (Ctx)'Old
       and Get_Option_Number (Ctx) = Get_Option_Number (Ctx)'Old
       and Field_First (Ctx, F_Option_Length) = Field_First (Ctx, F_Option_Length)'Old
       and (for all F in Field range F_Copied .. F_Option_Number =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   pragma Warnings (On, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Set_Option_Data_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Valid_Next (Ctx, RFLX.IPv4.Option.F_Option_Data)
       and then RFLX.IPv4.Option.Available_Space (Ctx, RFLX.IPv4.Option.F_Option_Data) >= RFLX.IPv4.Option.Field_Size (Ctx, RFLX.IPv4.Option.F_Option_Data)
       and then RFLX.IPv4.Option.Field_Condition (Ctx, RFLX.IPv4.Option.F_Option_Data, 0)
       and then RFLX.IPv4.Option.Field_Size (Ctx, RFLX.IPv4.Option.F_Option_Data) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Option_Data)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Valid_Next (Ctx, F_Option_Data) = Valid_Next (Ctx, F_Option_Data)'Old
       and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
       and Get_Option_Class (Ctx) = Get_Option_Class (Ctx)'Old
       and Get_Option_Number (Ctx) = Get_Option_Number (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Option_Data) = Field_First (Ctx, F_Option_Data)'Old;

   procedure Initialize_Option_Data (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Valid_Next (Ctx, RFLX.IPv4.Option.F_Option_Data)
       and then RFLX.IPv4.Option.Available_Space (Ctx, RFLX.IPv4.Option.F_Option_Data) >= RFLX.IPv4.Option.Field_Size (Ctx, RFLX.IPv4.Option.F_Option_Data),
     Post =>
       Has_Buffer (Ctx)
       and then Well_Formed (Ctx, F_Option_Data)
       and then (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Data))
       and then Ctx.Buffer_First = Ctx.Buffer_First'Old
       and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and then Ctx.First = Ctx.First'Old
       and then Ctx.Last = Ctx.Last'Old
       and then Valid_Next (Ctx, F_Option_Data) = Valid_Next (Ctx, F_Option_Data)'Old
       and then Get_Copied (Ctx) = Get_Copied (Ctx)'Old
       and then Get_Option_Class (Ctx) = Get_Option_Class (Ctx)'Old
       and then Get_Option_Number (Ctx) = Get_Option_Number (Ctx)'Old
       and then Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and then Field_First (Ctx, F_Option_Data) = Field_First (Ctx, F_Option_Data)'Old;

   procedure Set_Option_Data (Ctx : in out Context; Data : RFLX_Types.Bytes) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Valid_Next (Ctx, RFLX.IPv4.Option.F_Option_Data)
       and then RFLX.IPv4.Option.Available_Space (Ctx, RFLX.IPv4.Option.F_Option_Data) >= RFLX.IPv4.Option.Field_Size (Ctx, RFLX.IPv4.Option.F_Option_Data)
       and then RFLX.IPv4.Option.Valid_Length (Ctx, RFLX.IPv4.Option.F_Option_Data, Data'Length)
       and then RFLX.IPv4.Option.Available_Space (Ctx, RFLX.IPv4.Option.F_Option_Data) >= Data'Length * RFLX_Types.Byte'Size
       and then RFLX.IPv4.Option.Field_Condition (Ctx, RFLX.IPv4.Option.F_Option_Data, 0),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Option_Data)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Valid_Next (Ctx, F_Option_Data) = Valid_Next (Ctx, F_Option_Data)'Old
       and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
       and Get_Option_Class (Ctx) = Get_Option_Class (Ctx)'Old
       and Get_Option_Number (Ctx) = Get_Option_Number (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Option_Data) = Field_First (Ctx, F_Option_Data)'Old
       and Equal (Ctx, F_Option_Data, Data);

   generic
      with procedure Process_Option_Data (Option_Data : out RFLX_Types.Bytes);
      with function Process_Data_Pre (Length : RFLX_Types.Length) return Boolean;
   procedure Generic_Set_Option_Data (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Option.Has_Buffer (Ctx)
       and then RFLX.IPv4.Option.Valid_Next (Ctx, RFLX.IPv4.Option.F_Option_Data)
       and then RFLX.IPv4.Option.Available_Space (Ctx, RFLX.IPv4.Option.F_Option_Data) >= RFLX.IPv4.Option.Field_Size (Ctx, RFLX.IPv4.Option.F_Option_Data)
       and then RFLX.IPv4.Option.Valid_Length (Ctx, RFLX.IPv4.Option.F_Option_Data, Length)
       and then RFLX_Types.To_Length (RFLX.IPv4.Option.Available_Space (Ctx, RFLX.IPv4.Option.F_Option_Data)) >= Length
       and then Process_Data_Pre (Length),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Option_Data)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Valid_Next (Ctx, F_Option_Data) = Valid_Next (Ctx, F_Option_Data)'Old
       and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
       and Get_Option_Class (Ctx) = Get_Option_Class (Ctx)'Old
       and Get_Option_Number (Ctx) = Get_Option_Number (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Option_Data) = Field_First (Ctx, F_Option_Data)'Old;

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

   pragma Warnings (Off, "postcondition does not mention function result");

   function Cursors_Invariant (Cursors : Field_Cursors; First : RFLX_Types.Bit_Index; Verified_Last : RFLX_Types.Bit_Length) return Boolean is
     ((for all F in Field =>
          (if
              Well_Formed (Cursors (F))
           then
              Cursors (F).First >= First
              and Cursors (F).Last <= Verified_Last
              and Cursors (F).First <= Cursors (F).Last + 1
              and Valid_Value (F, Cursors (F).Value))))
    with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "formal parameter ""*"" is not referenced");

   pragma Warnings (Off, "postcondition does not mention function result");

   pragma Warnings (Off, "unused variable ""*""");

   function Valid_Predecessors_Invariant (Cursors : Field_Cursors; First : RFLX_Types.Bit_Index; Verified_Last : RFLX_Types.Bit_Length; Written_Last : RFLX_Types.Bit_Length; Buffer : RFLX_Types.Bytes_Ptr) return Boolean is
     ((if Well_Formed (Cursors (F_Copied)) then True)
      and then (if Well_Formed (Cursors (F_Option_Class)) then Valid (Cursors (F_Copied)))
      and then (if Well_Formed (Cursors (F_Option_Number)) then Valid (Cursors (F_Option_Class)))
      and then (if
                   Well_Formed (Cursors (F_Option_Length))
                then
                   (Valid (Cursors (F_Option_Number))
                    and then Cursors (F_Option_Number).Value > 1))
      and then (if
                   Well_Formed (Cursors (F_Option_Data))
                then
                   (Valid (Cursors (F_Option_Length))
                    and then ((RFLX_Types.Base_Integer (Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Debugging_And_Measurement))
                               and then Cursors (F_Option_Number).Value = 4)
                              or else (RFLX_Types.Base_Integer (Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                                       and then (Cursors (F_Option_Number).Value = 9
                                                 or else Cursors (F_Option_Number).Value = 3
                                                 or else Cursors (F_Option_Number).Value = 7))
                              or else (Cursors (F_Option_Length).Value = 11
                                       and then RFLX_Types.Base_Integer (Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                                       and then Cursors (F_Option_Number).Value = 2)
                              or else (Cursors (F_Option_Length).Value = 4
                                       and then RFLX_Types.Base_Integer (Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                                       and then Cursors (F_Option_Number).Value = 8)))))
    with
     Pre =>
       Cursors_Invariant (Cursors, First, Verified_Last),
     Post =>
       True;

   pragma Warnings (On, "formal parameter ""*"" is not referenced");

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (On, "unused variable ""*""");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Next_Internal (Cursors : Field_Cursors; First : RFLX_Types.Bit_Index; Verified_Last : RFLX_Types.Bit_Length; Written_Last : RFLX_Types.Bit_Length; Buffer : RFLX_Types.Bytes_Ptr; Fld : Field) return Boolean is
     ((case Fld is
          when F_Copied =>
             True,
          when F_Option_Class =>
             (Valid (Cursors (F_Copied))
              and then True),
          when F_Option_Number =>
             (Valid (Cursors (F_Option_Class))
              and then True),
          when F_Option_Length =>
             (Valid (Cursors (F_Option_Number))
              and then Cursors (F_Option_Number).Value > 1),
          when F_Option_Data =>
             (Valid (Cursors (F_Option_Length))
              and then ((RFLX_Types.Base_Integer (Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Debugging_And_Measurement))
                         and then Cursors (F_Option_Number).Value = 4)
                        or else (RFLX_Types.Base_Integer (Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                                 and then (Cursors (F_Option_Number).Value = 9
                                           or else Cursors (F_Option_Number).Value = 3
                                           or else Cursors (F_Option_Number).Value = 7))
                        or else (Cursors (F_Option_Length).Value = 11
                                 and then RFLX_Types.Base_Integer (Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                                 and then Cursors (F_Option_Number).Value = 2)
                        or else (Cursors (F_Option_Length).Value = 4
                                 and then RFLX_Types.Base_Integer (Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                                 and then Cursors (F_Option_Number).Value = 8)))))
    with
     Pre =>
       Cursors_Invariant (Cursors, First, Verified_Last)
       and then Valid_Predecessors_Invariant (Cursors, First, Verified_Last, Written_Last, Buffer),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "unused variable ""*""");

   pragma Warnings (Off, "formal parameter ""*"" is not referenced");

   function Field_Size_Internal (Cursors : Field_Cursors; First : RFLX_Types.Bit_Index; Verified_Last : RFLX_Types.Bit_Length; Written_Last : RFLX_Types.Bit_Length; Buffer : RFLX_Types.Bytes_Ptr; Fld : Field) return RFLX_Types.Bit_Length'Base is
     ((case Fld is
          when F_Copied =>
             1,
          when F_Option_Class =>
             2,
          when F_Option_Number =>
             5,
          when F_Option_Length =>
             8,
          when F_Option_Data =>
             (RFLX_Types.Bit_Length (Cursors (F_Option_Length).Value) - 2) * 8))
    with
     Pre =>
       Cursors_Invariant (Cursors, First, Verified_Last)
       and then Valid_Predecessors_Invariant (Cursors, First, Verified_Last, Written_Last, Buffer)
       and then Valid_Next_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, Fld);

   pragma Warnings (On, "unused variable ""*""");

   pragma Warnings (On, "formal parameter ""*"" is not referenced");

   pragma Warnings (Off, "postcondition does not mention function result");

   pragma Warnings (Off, "unused variable ""*""");

   pragma Warnings (Off, "no recursive call visible");

   pragma Warnings (Off, "formal parameter ""*"" is not referenced");

   function Field_First_Internal (Cursors : Field_Cursors; First : RFLX_Types.Bit_Index; Verified_Last : RFLX_Types.Bit_Length; Written_Last : RFLX_Types.Bit_Length; Buffer : RFLX_Types.Bytes_Ptr; Fld : Field) return RFLX_Types.Bit_Index'Base is
     ((case Fld is
          when F_Copied =>
             First,
          when F_Option_Class =>
             First + 1,
          when F_Option_Number =>
             First + 3,
          when F_Option_Length =>
             First + 8,
          when F_Option_Data =>
             First + 16))
    with
     Pre =>
       Cursors_Invariant (Cursors, First, Verified_Last)
       and then Valid_Predecessors_Invariant (Cursors, First, Verified_Last, Written_Last, Buffer)
       and then Valid_Next_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, Fld),
     Post =>
       True,
     Subprogram_Variant =>
       (Decreases =>
         Fld);

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (On, "unused variable ""*""");

   pragma Warnings (On, "no recursive call visible");

   pragma Warnings (On, "formal parameter ""*"" is not referenced");

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
      and then Cursors_Invariant (Cursors, First, Verified_Last)
      and then Valid_Predecessors_Invariant (Cursors, First, Verified_Last, Written_Last, Buffer)
      and then ((if Invalid (Cursors (F_Copied)) then Invalid (Cursors (F_Option_Class)))
                and then (if Invalid (Cursors (F_Option_Class)) then Invalid (Cursors (F_Option_Number)))
                and then (if Invalid (Cursors (F_Option_Number)) then Invalid (Cursors (F_Option_Length)))
                and then (if Invalid (Cursors (F_Option_Length)) then Invalid (Cursors (F_Option_Data))))
      and then ((if
                    Well_Formed (Cursors (F_Copied))
                 then
                    (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1 = 1
                     and then Cursors (F_Copied).First = First))
                and then (if
                             Well_Formed (Cursors (F_Option_Class))
                          then
                             (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1 = 2
                              and then Cursors (F_Option_Class).First = Cursors (F_Copied).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Option_Number))
                          then
                             (Cursors (F_Option_Number).Last - Cursors (F_Option_Number).First + 1 = 5
                              and then Cursors (F_Option_Number).First = Cursors (F_Option_Class).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Option_Length))
                          then
                             (Cursors (F_Option_Length).Last - Cursors (F_Option_Length).First + 1 = 8
                              and then Cursors (F_Option_Length).First = Cursors (F_Option_Number).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Option_Data))
                          then
                             (Cursors (F_Option_Data).Last - Cursors (F_Option_Data).First + 1 = (RFLX_Types.Bit_Length (Cursors (F_Option_Length).Value) - 2) * 8
                              and then Cursors (F_Option_Data).First = Cursors (F_Option_Length).Last + 1))))
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
      and then Valid_Next (Ctx, F_Copied)
      and then RFLX.IPv4.Option.Field_First (Ctx, RFLX.IPv4.Option.F_Copied) rem RFLX_Types.Byte'Size = 1
      and then Available_Space (Ctx, F_Copied) = Ctx.Last - Ctx.First + 1
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
          when F_Copied =>
             Valid_Boolean (Val),
          when F_Option_Class =>
             RFLX.IPv4.Valid_Option_Class (Val),
          when F_Option_Number =>
             RFLX.IPv4.Valid_Option_Number (Val),
          when F_Option_Length =>
             RFLX.IPv4.Valid_Option_Length (Val),
          when F_Option_Data =>
             True));

   function Field_Condition (Ctx : Context; Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean is
     ((case Fld is
          when F_Copied | F_Option_Class =>
             True,
          when F_Option_Number =>
             (RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
              and then Val = 1)
             or else Val > 1,
          when F_Option_Length =>
             (RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Debugging_And_Measurement))
              and then Ctx.Cursors (F_Option_Number).Value = 4)
             or else (RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                      and then (Ctx.Cursors (F_Option_Number).Value = 9
                                or else Ctx.Cursors (F_Option_Number).Value = 3
                                or else Ctx.Cursors (F_Option_Number).Value = 7))
             or else (Val = 11
                      and then RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                      and then Ctx.Cursors (F_Option_Number).Value = 2)
             or else (Val = 4
                      and then RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
                      and then Ctx.Cursors (F_Option_Number).Value = 8),
          when F_Option_Data =>
             True));

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     (Field_Size_Internal (Ctx.Cursors, Ctx.First, Ctx.Verified_Last, Ctx.Written_Last, Ctx.Buffer, Fld));

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index is
     (Field_First_Internal (Ctx.Cursors, Ctx.First, Ctx.Verified_Last, Ctx.Written_Last, Ctx.Buffer, Fld));

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     (Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) - 1);

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean is
     (Valid_Next_Internal (Ctx.Cursors, Ctx.First, Ctx.Verified_Last, Ctx.Written_Last, Ctx.Buffer, Fld));

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
     (Well_Formed (Ctx, F_Option_Data)
      or else (Valid (Ctx, F_Option_Number)
               and then RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
               and then Ctx.Cursors (F_Option_Number).Value = 1));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Option_Data)
      or else (Valid (Ctx, F_Option_Number)
               and then RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Class).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.IPv4.Control))
               and then Ctx.Cursors (F_Option_Number).Value = 1));

   function Incomplete_Message (Ctx : Context) return Boolean is
     ((for some F in Field =>
          Incomplete (Ctx, F)));

   function Get_Copied (Ctx : Context) return Boolean is
     (To_Actual (Ctx.Cursors (F_Copied).Value));

   function Get_Option_Class (Ctx : Context) return RFLX.IPv4.Option_Class is
     (To_Actual (Ctx.Cursors (F_Option_Class).Value));

   function Get_Option_Number (Ctx : Context) return RFLX.IPv4.Option_Number is
     (To_Actual (Ctx.Cursors (F_Option_Number).Value));

   function Get_Option_Length (Ctx : Context) return RFLX.IPv4.Option_Length is
     (To_Actual (Ctx.Cursors (F_Option_Length).Value));

   function Valid_Size (Ctx : Context; Fld : Field; Size : RFLX_Types.Bit_Length) return Boolean is
     (Size = Field_Size (Ctx, Fld))
    with
     Pre =>
       RFLX.IPv4.Option.Valid_Next (Ctx, Fld);

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean is
     (Valid_Size (Ctx, Fld, RFLX_Types.To_Bit_Length (Length)));

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

   function Context_Cursors_Index (Cursors : Field_Cursors; Fld : Field) return Field_Cursor is
     (Cursors (Fld));

end RFLX.IPv4.Option;

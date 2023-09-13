pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.ICMP.Message with
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

   type Virtual_Field is (F_Initial, F_Tag, F_Code_Destination_Unreachable, F_Code_Redirect, F_Code_Time_Exceeded, F_Code_Zero, F_Checksum, F_Gateway_Internet_Address, F_Identifier, F_Pointer, F_Unused_32, F_Sequence_Number, F_Unused_24, F_Originate_Timestamp, F_Data, F_Receive_Timestamp, F_Transmit_Timestamp, F_Final);

   subtype Field is Virtual_Field range F_Tag .. F_Transmit_Timestamp;

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
       and RFLX.ICMP.Message.Has_Buffer (Ctx),
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
       and RFLX.ICMP.Message.Has_Buffer (Ctx)
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
       RFLX.ICMP.Message.Has_Buffer (Ctx),
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
       RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Well_Formed_Message (Ctx)
       and then RFLX.ICMP.Message.Byte_Size (Ctx) = Buffer'Length;

   function Read (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Well_Formed_Message (Ctx);

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
       RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Well_Formed_Message (Ctx)
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
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then Offset < RFLX.ICMP.Message.Buffer_Length (Ctx)
       and then Pre (RFLX.ICMP.Message.Buffer_Length (Ctx), Offset),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
       and Initialized (Ctx);

   function Has_Buffer (Ctx : Context) return Boolean;

   function Buffer_Length (Ctx : Context) return RFLX_Types.Length with
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx);

   function Size (Ctx : Context) return RFLX_Types.Bit_Length with
     Post =>
       Size'Result rem RFLX_Types.Byte'Size = 0;

   function Byte_Size (Ctx : Context) return RFLX_Types.Length;

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Well_Formed_Message (Ctx);

   function Written_Last (Ctx : Context) return RFLX_Types.Bit_Length;

   procedure Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Well_Formed_Message (Ctx)
       and then Data'Length = RFLX.ICMP.Message.Byte_Size (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Value (Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_Condition (Ctx : Context; Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean with
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Value (Fld, Val)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, Fld)
       and then RFLX.ICMP.Message.Sufficient_Space (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.ICMP.Message.Valid_Next (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Data =>
              Field_Size'Result rem RFLX_Types.Byte'Size = 0,
           when others =>
              True);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       RFLX.ICMP.Message.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.ICMP.Message.Valid_Next (Ctx, Fld)
       and then RFLX.ICMP.Message.Sufficient_Space (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Data =>
              Field_Last'Result rem RFLX_Types.Byte'Size = 0,
           when others =>
              True);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean;

   function Available_Space (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.ICMP.Message.Valid_Next (Ctx, Fld);

   function Sufficient_Space (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       RFLX.ICMP.Message.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean with
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx)
       and RFLX.ICMP.Message.Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx),
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
       RFLX.ICMP.Message.Has_Buffer (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Incomplete_Message (Ctx : Context) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "precondition is always False");

   function Get_Tag (Ctx : Context) return RFLX.ICMP.Tag with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Tag);

   function Get_Code_Destination_Unreachable (Ctx : Context) return RFLX.ICMP.Code_Destination_Unreachable with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Code_Destination_Unreachable);

   function Get_Code_Redirect (Ctx : Context) return RFLX.ICMP.Code_Redirect with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Code_Redirect);

   function Get_Code_Time_Exceeded (Ctx : Context) return RFLX.ICMP.Code_Time_Exceeded with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Code_Time_Exceeded);

   function Get_Code_Zero (Ctx : Context) return RFLX.ICMP.Code_Zero with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Code_Zero);

   function Get_Checksum (Ctx : Context) return RFLX.ICMP.Checksum with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Checksum);

   function Get_Gateway_Internet_Address (Ctx : Context) return RFLX.ICMP.Gateway_Internet_Address with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Gateway_Internet_Address);

   function Get_Identifier (Ctx : Context) return RFLX.ICMP.Identifier with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Identifier);

   function Get_Pointer (Ctx : Context) return RFLX.ICMP.Pointer with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Pointer);

   function Get_Unused_32 (Ctx : Context) return RFLX.ICMP.Unused_32 with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Unused_32);

   function Get_Sequence_Number (Ctx : Context) return RFLX.ICMP.Sequence_Number with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Sequence_Number);

   function Get_Unused_24 (Ctx : Context) return RFLX.ICMP.Unused_24 with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Unused_24);

   function Get_Originate_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Originate_Timestamp);

   function Get_Receive_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Receive_Timestamp);

   function Get_Transmit_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp with
     Pre =>
       RFLX.ICMP.Message.Valid (Ctx, RFLX.ICMP.Message.F_Transmit_Timestamp);

   pragma Warnings (On, "precondition is always False");

   function Get_Data (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Well_Formed (Ctx, RFLX.ICMP.Message.F_Data)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Data),
     Post =>
       Get_Data'Result'Length = RFLX_Types.To_Length (Field_Size (Ctx, F_Data));

   procedure Get_Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Well_Formed (Ctx, RFLX.ICMP.Message.F_Data)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Data)
       and then Data'Length = RFLX_Types.To_Length (RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Data)),
     Post =>
       Equal (Ctx, F_Data, Data);

   generic
      with procedure Process_Data (Data : RFLX_Types.Bytes);
   procedure Generic_Get_Data (Ctx : Context) with
     Pre =>
       RFLX.ICMP.Message.Has_Buffer (Ctx)
       and RFLX.ICMP.Message.Present (Ctx, RFLX.ICMP.Message.F_Data);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean with
     Pre =>
       RFLX.ICMP.Message.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Set_Tag (Ctx : in out Context; Val : RFLX.ICMP.Tag) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Tag)
       and then RFLX.ICMP.Valid_Tag (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Tag) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Tag)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Tag, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Tag)
       and Get_Tag (Ctx) = Val
       and Invalid (Ctx, F_Code_Destination_Unreachable)
       and Invalid (Ctx, F_Code_Redirect)
       and Invalid (Ctx, F_Code_Time_Exceeded)
       and Invalid (Ctx, F_Code_Zero)
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Gateway_Internet_Address)
       and Invalid (Ctx, F_Identifier)
       and Invalid (Ctx, F_Pointer)
       and Invalid (Ctx, F_Unused_32)
       and Invalid (Ctx, F_Sequence_Number)
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Destination_Unreachable))
            then
               Predecessor (Ctx, F_Code_Destination_Unreachable) = F_Tag
               and Valid_Next (Ctx, F_Code_Destination_Unreachable))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Redirect))
            then
               Predecessor (Ctx, F_Code_Redirect) = F_Tag
               and Valid_Next (Ctx, F_Code_Redirect))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Time_Exceeded))
            then
               Predecessor (Ctx, F_Code_Time_Exceeded) = F_Tag
               and Valid_Next (Ctx, F_Code_Time_Exceeded))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Reply))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Request))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Parameter_Problem))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Source_Quench))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request))
            then
               Predecessor (Ctx, F_Code_Zero) = F_Tag
               and Valid_Next (Ctx, F_Code_Zero))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Tag) = Predecessor (Ctx, F_Tag)'Old
       and Valid_Next (Ctx, F_Tag) = Valid_Next (Ctx, F_Tag)'Old
       and Field_First (Ctx, F_Tag) = Field_First (Ctx, F_Tag)'Old;

   procedure Set_Code_Destination_Unreachable (Ctx : in out Context; Val : RFLX.ICMP.Code_Destination_Unreachable) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Code_Destination_Unreachable)
       and then RFLX.ICMP.Valid_Code_Destination_Unreachable (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Code_Destination_Unreachable) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Code_Destination_Unreachable)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Code_Destination_Unreachable, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Code_Destination_Unreachable)
       and Get_Code_Destination_Unreachable (Ctx) = Val
       and Invalid (Ctx, F_Code_Redirect)
       and Invalid (Ctx, F_Code_Time_Exceeded)
       and Invalid (Ctx, F_Code_Zero)
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Gateway_Internet_Address)
       and Invalid (Ctx, F_Identifier)
       and Invalid (Ctx, F_Pointer)
       and Invalid (Ctx, F_Unused_32)
       and Invalid (Ctx, F_Sequence_Number)
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (Predecessor (Ctx, F_Checksum) = F_Code_Destination_Unreachable
            and Valid_Next (Ctx, F_Checksum))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Code_Destination_Unreachable) = Predecessor (Ctx, F_Code_Destination_Unreachable)'Old
       and Valid_Next (Ctx, F_Code_Destination_Unreachable) = Valid_Next (Ctx, F_Code_Destination_Unreachable)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Field_First (Ctx, F_Code_Destination_Unreachable) = Field_First (Ctx, F_Code_Destination_Unreachable)'Old
       and (for all F in Field range F_Tag .. F_Tag =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Code_Redirect (Ctx : in out Context; Val : RFLX.ICMP.Code_Redirect) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Code_Redirect)
       and then RFLX.ICMP.Valid_Code_Redirect (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Code_Redirect) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Code_Redirect)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Code_Redirect, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Code_Redirect)
       and Get_Code_Redirect (Ctx) = Val
       and Invalid (Ctx, F_Code_Time_Exceeded)
       and Invalid (Ctx, F_Code_Zero)
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Gateway_Internet_Address)
       and Invalid (Ctx, F_Identifier)
       and Invalid (Ctx, F_Pointer)
       and Invalid (Ctx, F_Unused_32)
       and Invalid (Ctx, F_Sequence_Number)
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (Predecessor (Ctx, F_Checksum) = F_Code_Redirect
            and Valid_Next (Ctx, F_Checksum))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Code_Redirect) = Predecessor (Ctx, F_Code_Redirect)'Old
       and Valid_Next (Ctx, F_Code_Redirect) = Valid_Next (Ctx, F_Code_Redirect)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Field_First (Ctx, F_Code_Redirect) = Field_First (Ctx, F_Code_Redirect)'Old
       and (for all F in Field range F_Tag .. F_Code_Destination_Unreachable =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Code_Time_Exceeded (Ctx : in out Context; Val : RFLX.ICMP.Code_Time_Exceeded) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Code_Time_Exceeded)
       and then RFLX.ICMP.Valid_Code_Time_Exceeded (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Code_Time_Exceeded) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Code_Time_Exceeded)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Code_Time_Exceeded, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Code_Time_Exceeded)
       and Get_Code_Time_Exceeded (Ctx) = Val
       and Invalid (Ctx, F_Code_Zero)
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Gateway_Internet_Address)
       and Invalid (Ctx, F_Identifier)
       and Invalid (Ctx, F_Pointer)
       and Invalid (Ctx, F_Unused_32)
       and Invalid (Ctx, F_Sequence_Number)
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (Predecessor (Ctx, F_Checksum) = F_Code_Time_Exceeded
            and Valid_Next (Ctx, F_Checksum))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Code_Time_Exceeded) = Predecessor (Ctx, F_Code_Time_Exceeded)'Old
       and Valid_Next (Ctx, F_Code_Time_Exceeded) = Valid_Next (Ctx, F_Code_Time_Exceeded)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Field_First (Ctx, F_Code_Time_Exceeded) = Field_First (Ctx, F_Code_Time_Exceeded)'Old
       and (for all F in Field range F_Tag .. F_Code_Redirect =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Code_Zero (Ctx : in out Context; Val : RFLX.ICMP.Code_Zero) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Code_Zero)
       and then RFLX.ICMP.Valid_Code_Zero (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Code_Zero) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Code_Zero)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Code_Zero, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Code_Zero)
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Gateway_Internet_Address)
       and Invalid (Ctx, F_Identifier)
       and Invalid (Ctx, F_Pointer)
       and Invalid (Ctx, F_Unused_32)
       and Invalid (Ctx, F_Sequence_Number)
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (Predecessor (Ctx, F_Checksum) = F_Code_Zero
            and Valid_Next (Ctx, F_Checksum))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Code_Zero) = Predecessor (Ctx, F_Code_Zero)'Old
       and Valid_Next (Ctx, F_Code_Zero) = Valid_Next (Ctx, F_Code_Zero)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Field_First (Ctx, F_Code_Zero) = Field_First (Ctx, F_Code_Zero)'Old
       and (for all F in Field range F_Tag .. F_Code_Time_Exceeded =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Checksum (Ctx : in out Context; Val : RFLX.ICMP.Checksum) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Checksum)
       and then RFLX.ICMP.Valid_Checksum (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Checksum) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Checksum)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Checksum, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Checksum)
       and Get_Checksum (Ctx) = Val
       and Invalid (Ctx, F_Gateway_Internet_Address)
       and Invalid (Ctx, F_Identifier)
       and Invalid (Ctx, F_Pointer)
       and Invalid (Ctx, F_Unused_32)
       and Invalid (Ctx, F_Sequence_Number)
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Redirect))
            then
               Predecessor (Ctx, F_Gateway_Internet_Address) = F_Checksum
               and Valid_Next (Ctx, F_Gateway_Internet_Address))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Reply))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Request))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
            then
               Predecessor (Ctx, F_Identifier) = F_Checksum
               and Valid_Next (Ctx, F_Identifier))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Parameter_Problem))
            then
               Predecessor (Ctx, F_Pointer) = F_Checksum
               and Valid_Next (Ctx, F_Pointer))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Time_Exceeded))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Destination_Unreachable))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Source_Quench))
            then
               Predecessor (Ctx, F_Unused_32) = F_Checksum
               and Valid_Next (Ctx, F_Unused_32))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Checksum) = Predecessor (Ctx, F_Checksum)'Old
       and Valid_Next (Ctx, F_Checksum) = Valid_Next (Ctx, F_Checksum)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Field_First (Ctx, F_Checksum) = Field_First (Ctx, F_Checksum)'Old
       and (for all F in Field range F_Tag .. F_Code_Zero =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Gateway_Internet_Address (Ctx : in out Context; Val : RFLX.ICMP.Gateway_Internet_Address) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Gateway_Internet_Address)
       and then RFLX.ICMP.Valid_Gateway_Internet_Address (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Gateway_Internet_Address) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Gateway_Internet_Address)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Gateway_Internet_Address, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Gateway_Internet_Address)
       and Get_Gateway_Internet_Address (Ctx) = Val
       and Invalid (Ctx, F_Identifier)
       and Invalid (Ctx, F_Pointer)
       and Invalid (Ctx, F_Unused_32)
       and Invalid (Ctx, F_Sequence_Number)
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (Predecessor (Ctx, F_Data) = F_Gateway_Internet_Address
            and Valid_Next (Ctx, F_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Gateway_Internet_Address) = Predecessor (Ctx, F_Gateway_Internet_Address)'Old
       and Valid_Next (Ctx, F_Gateway_Internet_Address) = Valid_Next (Ctx, F_Gateway_Internet_Address)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Field_First (Ctx, F_Gateway_Internet_Address) = Field_First (Ctx, F_Gateway_Internet_Address)'Old
       and (for all F in Field range F_Tag .. F_Checksum =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Identifier (Ctx : in out Context; Val : RFLX.ICMP.Identifier) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Identifier)
       and then RFLX.ICMP.Valid_Identifier (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Identifier) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Identifier)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Identifier, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Identifier)
       and Get_Identifier (Ctx) = Val
       and Invalid (Ctx, F_Pointer)
       and Invalid (Ctx, F_Unused_32)
       and Invalid (Ctx, F_Sequence_Number)
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (Predecessor (Ctx, F_Sequence_Number) = F_Identifier
            and Valid_Next (Ctx, F_Sequence_Number))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Identifier) = Predecessor (Ctx, F_Identifier)'Old
       and Valid_Next (Ctx, F_Identifier) = Valid_Next (Ctx, F_Identifier)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Field_First (Ctx, F_Identifier) = Field_First (Ctx, F_Identifier)'Old
       and (for all F in Field range F_Tag .. F_Gateway_Internet_Address =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Pointer (Ctx : in out Context; Val : RFLX.ICMP.Pointer) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Pointer)
       and then RFLX.ICMP.Valid_Pointer (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Pointer) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Pointer)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Pointer, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Pointer)
       and Get_Pointer (Ctx) = Val
       and Invalid (Ctx, F_Unused_32)
       and Invalid (Ctx, F_Sequence_Number)
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (Predecessor (Ctx, F_Unused_24) = F_Pointer
            and Valid_Next (Ctx, F_Unused_24))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Pointer) = Predecessor (Ctx, F_Pointer)'Old
       and Valid_Next (Ctx, F_Pointer) = Valid_Next (Ctx, F_Pointer)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Field_First (Ctx, F_Pointer) = Field_First (Ctx, F_Pointer)'Old
       and (for all F in Field range F_Tag .. F_Identifier =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Unused_32 (Ctx : in out Context; Val : RFLX.ICMP.Unused_32) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Unused_32)
       and then RFLX.ICMP.Valid_Unused_32 (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Unused_32) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Unused_32)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Unused_32, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Unused_32)
       and Invalid (Ctx, F_Sequence_Number)
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (Predecessor (Ctx, F_Data) = F_Unused_32
            and Valid_Next (Ctx, F_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Unused_32) = Predecessor (Ctx, F_Unused_32)'Old
       and Valid_Next (Ctx, F_Unused_32) = Valid_Next (Ctx, F_Unused_32)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Field_First (Ctx, F_Unused_32) = Field_First (Ctx, F_Unused_32)'Old
       and (for all F in Field range F_Tag .. F_Pointer =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Sequence_Number (Ctx : in out Context; Val : RFLX.ICMP.Sequence_Number) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Sequence_Number)
       and then RFLX.ICMP.Valid_Sequence_Number (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Sequence_Number) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Sequence_Number)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Sequence_Number, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Sequence_Number)
       and Get_Sequence_Number (Ctx) = Val
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Sequence_Number))
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request))
            then
               Predecessor (Ctx, F_Data) = F_Sequence_Number
               and Valid_Next (Ctx, F_Data))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Tag (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply))
            then
               Predecessor (Ctx, F_Originate_Timestamp) = F_Sequence_Number
               and Valid_Next (Ctx, F_Originate_Timestamp))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Sequence_Number) = Predecessor (Ctx, F_Sequence_Number)'Old
       and Valid_Next (Ctx, F_Sequence_Number) = Valid_Next (Ctx, F_Sequence_Number)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Identifier (Ctx) = Get_Identifier (Ctx)'Old
       and Field_First (Ctx, F_Sequence_Number) = Field_First (Ctx, F_Sequence_Number)'Old
       and (for all F in Field range F_Tag .. F_Unused_32 =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Unused_24 (Ctx : in out Context; Val : RFLX.ICMP.Unused_24) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Unused_24)
       and then RFLX.ICMP.Valid_Unused_24 (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Unused_24) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Unused_24)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Unused_24, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (Predecessor (Ctx, F_Data) = F_Unused_24
            and Valid_Next (Ctx, F_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Unused_24) = Predecessor (Ctx, F_Unused_24)'Old
       and Valid_Next (Ctx, F_Unused_24) = Valid_Next (Ctx, F_Unused_24)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Pointer (Ctx) = Get_Pointer (Ctx)'Old
       and Field_First (Ctx, F_Unused_24) = Field_First (Ctx, F_Unused_24)'Old
       and (for all F in Field range F_Tag .. F_Sequence_Number =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Originate_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Originate_Timestamp)
       and then RFLX.ICMP.Valid_Timestamp (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Originate_Timestamp) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Originate_Timestamp)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Originate_Timestamp, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Originate_Timestamp)
       and Get_Originate_Timestamp (Ctx) = Val
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (Predecessor (Ctx, F_Receive_Timestamp) = F_Originate_Timestamp
            and Valid_Next (Ctx, F_Receive_Timestamp))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Originate_Timestamp) = Predecessor (Ctx, F_Originate_Timestamp)'Old
       and Valid_Next (Ctx, F_Originate_Timestamp) = Valid_Next (Ctx, F_Originate_Timestamp)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Identifier (Ctx) = Get_Identifier (Ctx)'Old
       and Get_Sequence_Number (Ctx) = Get_Sequence_Number (Ctx)'Old
       and Field_First (Ctx, F_Originate_Timestamp) = Field_First (Ctx, F_Originate_Timestamp)'Old
       and (for all F in Field range F_Tag .. F_Unused_24 =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Receive_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Receive_Timestamp)
       and then RFLX.ICMP.Valid_Timestamp (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Receive_Timestamp) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Receive_Timestamp)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Receive_Timestamp, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Receive_Timestamp)
       and Get_Receive_Timestamp (Ctx) = Val
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (Predecessor (Ctx, F_Transmit_Timestamp) = F_Receive_Timestamp
            and Valid_Next (Ctx, F_Transmit_Timestamp))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Receive_Timestamp) = Predecessor (Ctx, F_Receive_Timestamp)'Old
       and Valid_Next (Ctx, F_Receive_Timestamp) = Valid_Next (Ctx, F_Receive_Timestamp)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Identifier (Ctx) = Get_Identifier (Ctx)'Old
       and Get_Sequence_Number (Ctx) = Get_Sequence_Number (Ctx)'Old
       and Get_Originate_Timestamp (Ctx) = Get_Originate_Timestamp (Ctx)'Old
       and Field_First (Ctx, F_Receive_Timestamp) = Field_First (Ctx, F_Receive_Timestamp)'Old
       and (for all F in Field range F_Tag .. F_Data =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Transmit_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Transmit_Timestamp)
       and then RFLX.ICMP.Valid_Timestamp (RFLX.ICMP.To_Base_Integer (Val))
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Transmit_Timestamp) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Transmit_Timestamp)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Transmit_Timestamp, RFLX.ICMP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Transmit_Timestamp)
       and Get_Transmit_Timestamp (Ctx) = Val
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Transmit_Timestamp))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Transmit_Timestamp) = Predecessor (Ctx, F_Transmit_Timestamp)'Old
       and Valid_Next (Ctx, F_Transmit_Timestamp) = Valid_Next (Ctx, F_Transmit_Timestamp)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Identifier (Ctx) = Get_Identifier (Ctx)'Old
       and Get_Sequence_Number (Ctx) = Get_Sequence_Number (Ctx)'Old
       and Get_Originate_Timestamp (Ctx) = Get_Originate_Timestamp (Ctx)'Old
       and Get_Receive_Timestamp (Ctx) = Get_Receive_Timestamp (Ctx)'Old
       and Field_First (Ctx, F_Transmit_Timestamp) = Field_First (Ctx, F_Transmit_Timestamp)'Old
       and (for all F in Field range F_Tag .. F_Receive_Timestamp =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   pragma Warnings (On, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Set_Data_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Data)
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Data) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Data)
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Data, 0)
       and then RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Data) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Data)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Field_First (Ctx, F_Data) = Field_First (Ctx, F_Data)'Old;

   procedure Initialize_Data (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Data)
       and then RFLX.ICMP.Message.Valid_Length (Ctx, RFLX.ICMP.Message.F_Data, Length)
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Data) >= RFLX_Types.To_Bit_Length (Length),
     Post =>
       Has_Buffer (Ctx)
       and then Well_Formed (Ctx, F_Data)
       and then Field_Size (Ctx, F_Data) = RFLX_Types.To_Bit_Length (Length)
       and then (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and then Invalid (Ctx, F_Receive_Timestamp)
       and then Invalid (Ctx, F_Transmit_Timestamp)
       and then Ctx.Buffer_First = Ctx.Buffer_First'Old
       and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and then Ctx.First = Ctx.First'Old
       and then Ctx.Last = Ctx.Last'Old
       and then Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and then Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and then Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and then Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and then Field_First (Ctx, F_Data) = Field_First (Ctx, F_Data)'Old;

   procedure Set_Data (Ctx : in out Context; Data : RFLX_Types.Bytes) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Data)
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Data) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Data)
       and then RFLX.ICMP.Message.Valid_Length (Ctx, RFLX.ICMP.Message.F_Data, Data'Length)
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Data) >= Data'Length * RFLX_Types.Byte'Size
       and then RFLX.ICMP.Message.Field_Condition (Ctx, RFLX.ICMP.Message.F_Data, 0),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Data)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Field_First (Ctx, F_Data) = Field_First (Ctx, F_Data)'Old
       and Equal (Ctx, F_Data, Data);

   generic
      with procedure Process_Data (Data : out RFLX_Types.Bytes);
      with function Process_Data_Pre (Length : RFLX_Types.Length) return Boolean;
   procedure Generic_Set_Data (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.ICMP.Message.Has_Buffer (Ctx)
       and then RFLX.ICMP.Message.Valid_Next (Ctx, RFLX.ICMP.Message.F_Data)
       and then RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Data) >= RFLX.ICMP.Message.Field_Size (Ctx, RFLX.ICMP.Message.F_Data)
       and then RFLX.ICMP.Message.Valid_Length (Ctx, RFLX.ICMP.Message.F_Data, Length)
       and then RFLX_Types.To_Length (RFLX.ICMP.Message.Available_Space (Ctx, RFLX.ICMP.Message.F_Data)) >= Length
       and then Process_Data_Pre (Length),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Data)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Field_First (Ctx, F_Data) = Field_First (Ctx, F_Data)'Old;

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
     ((if Well_Formed (Cursors (F_Tag)) then Cursors (F_Tag).Predecessor = F_Initial)
      and then (if
                   Well_Formed (Cursors (F_Code_Destination_Unreachable))
                then
                   (Valid (Cursors (F_Tag))
                    and then Cursors (F_Code_Destination_Unreachable).Predecessor = F_Tag
                    and then RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Destination_Unreachable))))
      and then (if
                   Well_Formed (Cursors (F_Code_Redirect))
                then
                   (Valid (Cursors (F_Tag))
                    and then Cursors (F_Code_Redirect).Predecessor = F_Tag
                    and then RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Redirect))))
      and then (if
                   Well_Formed (Cursors (F_Code_Time_Exceeded))
                then
                   (Valid (Cursors (F_Tag))
                    and then Cursors (F_Code_Time_Exceeded).Predecessor = F_Tag
                    and then RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Time_Exceeded))))
      and then (if
                   Well_Formed (Cursors (F_Code_Zero))
                then
                   (Valid (Cursors (F_Tag))
                    and then Cursors (F_Code_Zero).Predecessor = F_Tag
                    and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Reply))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Request))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Parameter_Problem))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Source_Quench))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request)))))
      and then (if
                   Well_Formed (Cursors (F_Checksum))
                then
                   (Valid (Cursors (F_Code_Destination_Unreachable))
                    and then Cursors (F_Checksum).Predecessor = F_Code_Destination_Unreachable)
                   or (Valid (Cursors (F_Code_Redirect))
                       and then Cursors (F_Checksum).Predecessor = F_Code_Redirect)
                   or (Valid (Cursors (F_Code_Time_Exceeded))
                       and then Cursors (F_Checksum).Predecessor = F_Code_Time_Exceeded)
                   or (Valid (Cursors (F_Code_Zero))
                       and then Cursors (F_Checksum).Predecessor = F_Code_Zero))
      and then (if
                   Well_Formed (Cursors (F_Gateway_Internet_Address))
                then
                   (Valid (Cursors (F_Checksum))
                    and then Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                    and then RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Redirect))))
      and then (if
                   Well_Formed (Cursors (F_Identifier))
                then
                   (Valid (Cursors (F_Checksum))
                    and then Cursors (F_Identifier).Predecessor = F_Checksum
                    and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Reply))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Request))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply)))))
      and then (if
                   Well_Formed (Cursors (F_Pointer))
                then
                   (Valid (Cursors (F_Checksum))
                    and then Cursors (F_Pointer).Predecessor = F_Checksum
                    and then RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Parameter_Problem))))
      and then (if
                   Well_Formed (Cursors (F_Unused_32))
                then
                   (Valid (Cursors (F_Checksum))
                    and then Cursors (F_Unused_32).Predecessor = F_Checksum
                    and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Time_Exceeded))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Destination_Unreachable))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Source_Quench)))))
      and then (if
                   Well_Formed (Cursors (F_Sequence_Number))
                then
                   (Valid (Cursors (F_Identifier))
                    and then Cursors (F_Sequence_Number).Predecessor = F_Identifier))
      and then (if
                   Well_Formed (Cursors (F_Unused_24))
                then
                   (Valid (Cursors (F_Pointer))
                    and then Cursors (F_Unused_24).Predecessor = F_Pointer))
      and then (if
                   Well_Formed (Cursors (F_Originate_Timestamp))
                then
                   (Valid (Cursors (F_Sequence_Number))
                    and then Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                    and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
                              or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply)))))
      and then (if
                   Well_Formed (Cursors (F_Data))
                then
                   (Valid (Cursors (F_Gateway_Internet_Address))
                    and then Cursors (F_Data).Predecessor = F_Gateway_Internet_Address)
                   or (Valid (Cursors (F_Sequence_Number))
                       and then Cursors (F_Data).Predecessor = F_Sequence_Number
                       and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
                                 or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request))))
                   or (Valid (Cursors (F_Unused_24))
                       and then Cursors (F_Data).Predecessor = F_Unused_24)
                   or (Valid (Cursors (F_Unused_32))
                       and then Cursors (F_Data).Predecessor = F_Unused_32))
      and then (if
                   Well_Formed (Cursors (F_Receive_Timestamp))
                then
                   (Valid (Cursors (F_Originate_Timestamp))
                    and then Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp))
      and then (if
                   Well_Formed (Cursors (F_Transmit_Timestamp))
                then
                   (Valid (Cursors (F_Receive_Timestamp))
                    and then Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp)))
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
          when F_Tag =>
             Cursors (F_Tag).Predecessor = F_Initial,
          when F_Code_Destination_Unreachable =>
             (Valid (Cursors (F_Tag))
              and then RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Destination_Unreachable))
              and then Cursors (F_Code_Destination_Unreachable).Predecessor = F_Tag),
          when F_Code_Redirect =>
             (Valid (Cursors (F_Tag))
              and then RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Redirect))
              and then Cursors (F_Code_Redirect).Predecessor = F_Tag),
          when F_Code_Time_Exceeded =>
             (Valid (Cursors (F_Tag))
              and then RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Time_Exceeded))
              and then Cursors (F_Code_Time_Exceeded).Predecessor = F_Tag),
          when F_Code_Zero =>
             (Valid (Cursors (F_Tag))
              and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Reply))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Request))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Parameter_Problem))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Source_Quench))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request)))
              and then Cursors (F_Code_Zero).Predecessor = F_Tag),
          when F_Checksum =>
             (Valid (Cursors (F_Code_Destination_Unreachable))
              and then True
              and then Cursors (F_Checksum).Predecessor = F_Code_Destination_Unreachable)
             or (Valid (Cursors (F_Code_Redirect))
                 and then True
                 and then Cursors (F_Checksum).Predecessor = F_Code_Redirect)
             or (Valid (Cursors (F_Code_Time_Exceeded))
                 and then True
                 and then Cursors (F_Checksum).Predecessor = F_Code_Time_Exceeded)
             or (Valid (Cursors (F_Code_Zero))
                 and then True
                 and then Cursors (F_Checksum).Predecessor = F_Code_Zero),
          when F_Gateway_Internet_Address =>
             (Valid (Cursors (F_Checksum))
              and then RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Redirect))
              and then Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum),
          when F_Identifier =>
             (Valid (Cursors (F_Checksum))
              and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Reply))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Request))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply)))
              and then Cursors (F_Identifier).Predecessor = F_Checksum),
          when F_Pointer =>
             (Valid (Cursors (F_Checksum))
              and then RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Parameter_Problem))
              and then Cursors (F_Pointer).Predecessor = F_Checksum),
          when F_Unused_32 =>
             (Valid (Cursors (F_Checksum))
              and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Time_Exceeded))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Destination_Unreachable))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Source_Quench)))
              and then Cursors (F_Unused_32).Predecessor = F_Checksum),
          when F_Sequence_Number =>
             (Valid (Cursors (F_Identifier))
              and then True
              and then Cursors (F_Sequence_Number).Predecessor = F_Identifier),
          when F_Unused_24 =>
             (Valid (Cursors (F_Pointer))
              and then True
              and then Cursors (F_Unused_24).Predecessor = F_Pointer),
          when F_Originate_Timestamp =>
             (Valid (Cursors (F_Sequence_Number))
              and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
                        or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply)))
              and then Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number),
          when F_Data =>
             (Valid (Cursors (F_Gateway_Internet_Address))
              and then True
              and then Cursors (F_Data).Predecessor = F_Gateway_Internet_Address)
             or (Valid (Cursors (F_Sequence_Number))
                 and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
                           or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request)))
                 and then Cursors (F_Data).Predecessor = F_Sequence_Number)
             or (Valid (Cursors (F_Unused_24))
                 and then True
                 and then Cursors (F_Data).Predecessor = F_Unused_24)
             or (Valid (Cursors (F_Unused_32))
                 and then True
                 and then Cursors (F_Data).Predecessor = F_Unused_32),
          when F_Receive_Timestamp =>
             (Valid (Cursors (F_Originate_Timestamp))
              and then True
              and then Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp),
          when F_Transmit_Timestamp =>
             (Valid (Cursors (F_Receive_Timestamp))
              and then True
              and then Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp)))
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
          when F_Tag | F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             8,
          when F_Checksum =>
             16,
          when F_Gateway_Internet_Address =>
             32,
          when F_Identifier =>
             16,
          when F_Pointer =>
             8,
          when F_Unused_32 =>
             32,
          when F_Sequence_Number =>
             16,
          when F_Unused_24 =>
             24,
          when F_Originate_Timestamp =>
             32,
          when F_Data =>
             (if
                 Well_Formed (Cursors (F_Gateway_Internet_Address))
              then
                 224
              elsif
                 Well_Formed (Cursors (F_Sequence_Number))
                 and then (RFLX_Types.Bit_Length (Cursors (F_Tag).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.ICMP.Echo_Reply))
                           or RFLX_Types.Bit_Length (Cursors (F_Tag).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.ICMP.Echo_Request)))
              then
                 RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Sequence_Number).Last)
              elsif
                 Well_Formed (Cursors (F_Unused_24))
              then
                 224
              elsif
                 Well_Formed (Cursors (F_Unused_32))
              then
                 224
              else
                 RFLX_Types.Unreachable),
          when F_Receive_Timestamp | F_Transmit_Timestamp =>
             32))
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
          when F_Tag =>
             First,
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             First + 8,
          when F_Checksum =>
             (if
                 Well_Formed (Cursors (F_Code_Destination_Unreachable))
                 and then True
              then
                 First + 16
              elsif
                 Well_Formed (Cursors (F_Code_Redirect))
                 and then True
              then
                 First + 16
              elsif
                 Well_Formed (Cursors (F_Code_Time_Exceeded))
                 and then True
              then
                 First + 16
              elsif
                 Well_Formed (Cursors (F_Code_Zero))
                 and then True
              then
                 First + 16
              else
                 RFLX_Types.Unreachable),
          when F_Gateway_Internet_Address | F_Identifier | F_Pointer | F_Unused_32 =>
             Field_First_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, F_Checksum) + 16,
          when F_Sequence_Number =>
             Field_First_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, F_Checksum) + 32,
          when F_Unused_24 =>
             Field_First_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, F_Checksum) + 24,
          when F_Originate_Timestamp =>
             Field_First_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, F_Checksum) + 48,
          when F_Data =>
             (if
                 Well_Formed (Cursors (F_Gateway_Internet_Address))
                 and then True
              then
                 Field_First_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, F_Checksum) + 48
              elsif
                 Well_Formed (Cursors (F_Sequence_Number))
                 and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
                           or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request)))
              then
                 Field_First_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, F_Checksum) + 48
              elsif
                 Well_Formed (Cursors (F_Unused_24))
                 and then True
              then
                 Field_First_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, F_Checksum) + 48
              elsif
                 Well_Formed (Cursors (F_Unused_32))
                 and then True
              then
                 Field_First_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, F_Checksum) + 48
              else
                 RFLX_Types.Unreachable),
          when F_Receive_Timestamp =>
             Field_First_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, F_Checksum) + 80,
          when F_Transmit_Timestamp =>
             Field_First_Internal (Cursors, First, Verified_Last, Written_Last, Buffer, F_Checksum) + 112))
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
      and then ((if Invalid (Cursors (F_Tag)) then Invalid (Cursors (F_Code_Destination_Unreachable)))
                and then (if Invalid (Cursors (F_Tag)) then Invalid (Cursors (F_Code_Redirect)))
                and then (if Invalid (Cursors (F_Tag)) then Invalid (Cursors (F_Code_Time_Exceeded)))
                and then (if Invalid (Cursors (F_Tag)) then Invalid (Cursors (F_Code_Zero)))
                and then (if
                             Invalid (Cursors (F_Code_Destination_Unreachable))
                             and then Invalid (Cursors (F_Code_Redirect))
                             and then Invalid (Cursors (F_Code_Time_Exceeded))
                             and then Invalid (Cursors (F_Code_Zero))
                          then
                             Invalid (Cursors (F_Checksum)))
                and then (if Invalid (Cursors (F_Checksum)) then Invalid (Cursors (F_Gateway_Internet_Address)))
                and then (if Invalid (Cursors (F_Checksum)) then Invalid (Cursors (F_Identifier)))
                and then (if Invalid (Cursors (F_Checksum)) then Invalid (Cursors (F_Pointer)))
                and then (if Invalid (Cursors (F_Checksum)) then Invalid (Cursors (F_Unused_32)))
                and then (if Invalid (Cursors (F_Identifier)) then Invalid (Cursors (F_Sequence_Number)))
                and then (if Invalid (Cursors (F_Pointer)) then Invalid (Cursors (F_Unused_24)))
                and then (if Invalid (Cursors (F_Sequence_Number)) then Invalid (Cursors (F_Originate_Timestamp)))
                and then (if
                             Invalid (Cursors (F_Gateway_Internet_Address))
                             and then Invalid (Cursors (F_Sequence_Number))
                             and then Invalid (Cursors (F_Unused_24))
                             and then Invalid (Cursors (F_Unused_32))
                          then
                             Invalid (Cursors (F_Data)))
                and then (if Invalid (Cursors (F_Originate_Timestamp)) then Invalid (Cursors (F_Receive_Timestamp)))
                and then (if Invalid (Cursors (F_Receive_Timestamp)) then Invalid (Cursors (F_Transmit_Timestamp))))
      and then ((if
                    Well_Formed (Cursors (F_Tag))
                 then
                    (Cursors (F_Tag).Last - Cursors (F_Tag).First + 1 = 8
                     and then Cursors (F_Tag).Predecessor = F_Initial
                     and then Cursors (F_Tag).First = First))
                and then (if
                             Well_Formed (Cursors (F_Code_Destination_Unreachable))
                          then
                             (Cursors (F_Code_Destination_Unreachable).Last - Cursors (F_Code_Destination_Unreachable).First + 1 = 8
                              and then Cursors (F_Code_Destination_Unreachable).Predecessor = F_Tag
                              and then Cursors (F_Code_Destination_Unreachable).First = Cursors (F_Tag).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Code_Redirect))
                          then
                             (Cursors (F_Code_Redirect).Last - Cursors (F_Code_Redirect).First + 1 = 8
                              and then Cursors (F_Code_Redirect).Predecessor = F_Tag
                              and then Cursors (F_Code_Redirect).First = Cursors (F_Tag).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Code_Time_Exceeded))
                          then
                             (Cursors (F_Code_Time_Exceeded).Last - Cursors (F_Code_Time_Exceeded).First + 1 = 8
                              and then Cursors (F_Code_Time_Exceeded).Predecessor = F_Tag
                              and then Cursors (F_Code_Time_Exceeded).First = Cursors (F_Tag).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Code_Zero))
                          then
                             (Cursors (F_Code_Zero).Last - Cursors (F_Code_Zero).First + 1 = 8
                              and then Cursors (F_Code_Zero).Predecessor = F_Tag
                              and then Cursors (F_Code_Zero).First = Cursors (F_Tag).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Checksum))
                          then
                             (if
                                 Well_Formed (Cursors (F_Code_Destination_Unreachable))
                                 and then True
                              then
                                 Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1 = 16
                                 and then Cursors (F_Checksum).Predecessor = F_Code_Destination_Unreachable
                                 and then Cursors (F_Checksum).First = Cursors (F_Code_Destination_Unreachable).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Code_Redirect))
                                          and then True
                                       then
                                          Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1 = 16
                                          and then Cursors (F_Checksum).Predecessor = F_Code_Redirect
                                          and then Cursors (F_Checksum).First = Cursors (F_Code_Redirect).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Code_Time_Exceeded))
                                          and then True
                                       then
                                          Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1 = 16
                                          and then Cursors (F_Checksum).Predecessor = F_Code_Time_Exceeded
                                          and then Cursors (F_Checksum).First = Cursors (F_Code_Time_Exceeded).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Code_Zero))
                                          and then True
                                       then
                                          Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1 = 16
                                          and then Cursors (F_Checksum).Predecessor = F_Code_Zero
                                          and then Cursors (F_Checksum).First = Cursors (F_Code_Zero).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Gateway_Internet_Address))
                          then
                             (Cursors (F_Gateway_Internet_Address).Last - Cursors (F_Gateway_Internet_Address).First + 1 = 32
                              and then Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                              and then Cursors (F_Gateway_Internet_Address).First = Cursors (F_Checksum).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Identifier))
                          then
                             (Cursors (F_Identifier).Last - Cursors (F_Identifier).First + 1 = 16
                              and then Cursors (F_Identifier).Predecessor = F_Checksum
                              and then Cursors (F_Identifier).First = Cursors (F_Checksum).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Pointer))
                          then
                             (Cursors (F_Pointer).Last - Cursors (F_Pointer).First + 1 = 8
                              and then Cursors (F_Pointer).Predecessor = F_Checksum
                              and then Cursors (F_Pointer).First = Cursors (F_Checksum).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Unused_32))
                          then
                             (Cursors (F_Unused_32).Last - Cursors (F_Unused_32).First + 1 = 32
                              and then Cursors (F_Unused_32).Predecessor = F_Checksum
                              and then Cursors (F_Unused_32).First = Cursors (F_Checksum).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Sequence_Number))
                          then
                             (Cursors (F_Sequence_Number).Last - Cursors (F_Sequence_Number).First + 1 = 16
                              and then Cursors (F_Sequence_Number).Predecessor = F_Identifier
                              and then Cursors (F_Sequence_Number).First = Cursors (F_Identifier).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Unused_24))
                          then
                             (Cursors (F_Unused_24).Last - Cursors (F_Unused_24).First + 1 = 24
                              and then Cursors (F_Unused_24).Predecessor = F_Pointer
                              and then Cursors (F_Unused_24).First = Cursors (F_Pointer).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Originate_Timestamp))
                          then
                             (Cursors (F_Originate_Timestamp).Last - Cursors (F_Originate_Timestamp).First + 1 = 32
                              and then Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                              and then Cursors (F_Originate_Timestamp).First = Cursors (F_Sequence_Number).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Data))
                          then
                             (if
                                 Well_Formed (Cursors (F_Gateway_Internet_Address))
                                 and then True
                              then
                                 Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                 and then Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                 and then Cursors (F_Data).First = Cursors (F_Gateway_Internet_Address).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Sequence_Number))
                                          and then (RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
                                                    or RFLX_Types.Base_Integer (Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request)))
                                       then
                                          Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Sequence_Number).Last)
                                          and then Cursors (F_Data).Predecessor = F_Sequence_Number
                                          and then Cursors (F_Data).First = Cursors (F_Sequence_Number).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Unused_24))
                                          and then True
                                       then
                                          Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                          and then Cursors (F_Data).Predecessor = F_Unused_24
                                          and then Cursors (F_Data).First = Cursors (F_Unused_24).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Unused_32))
                                          and then True
                                       then
                                          Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                          and then Cursors (F_Data).Predecessor = F_Unused_32
                                          and then Cursors (F_Data).First = Cursors (F_Unused_32).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Receive_Timestamp))
                          then
                             (Cursors (F_Receive_Timestamp).Last - Cursors (F_Receive_Timestamp).First + 1 = 32
                              and then Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                              and then Cursors (F_Receive_Timestamp).First = Cursors (F_Originate_Timestamp).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Transmit_Timestamp))
                          then
                             (Cursors (F_Transmit_Timestamp).Last - Cursors (F_Transmit_Timestamp).First + 1 = 32
                              and then Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                              and then Cursors (F_Transmit_Timestamp).First = Cursors (F_Receive_Timestamp).Last + 1))))
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
      and then Valid_Next (Ctx, F_Tag)
      and then RFLX.ICMP.Message.Field_First (Ctx, RFLX.ICMP.Message.F_Tag) rem RFLX_Types.Byte'Size = 1
      and then Available_Space (Ctx, F_Tag) = Ctx.Last - Ctx.First + 1
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
          when F_Tag =>
             RFLX.ICMP.Valid_Tag (Val),
          when F_Code_Destination_Unreachable =>
             RFLX.ICMP.Valid_Code_Destination_Unreachable (Val),
          when F_Code_Redirect =>
             RFLX.ICMP.Valid_Code_Redirect (Val),
          when F_Code_Time_Exceeded =>
             RFLX.ICMP.Valid_Code_Time_Exceeded (Val),
          when F_Code_Zero =>
             RFLX.ICMP.Valid_Code_Zero (Val),
          when F_Checksum =>
             RFLX.ICMP.Valid_Checksum (Val),
          when F_Gateway_Internet_Address =>
             RFLX.ICMP.Valid_Gateway_Internet_Address (Val),
          when F_Identifier =>
             RFLX.ICMP.Valid_Identifier (Val),
          when F_Pointer =>
             RFLX.ICMP.Valid_Pointer (Val),
          when F_Unused_32 =>
             RFLX.ICMP.Valid_Unused_32 (Val),
          when F_Sequence_Number =>
             RFLX.ICMP.Valid_Sequence_Number (Val),
          when F_Unused_24 =>
             RFLX.ICMP.Valid_Unused_24 (Val),
          when F_Originate_Timestamp =>
             RFLX.ICMP.Valid_Timestamp (Val),
          when F_Data =>
             True,
          when F_Receive_Timestamp | F_Transmit_Timestamp =>
             RFLX.ICMP.Valid_Timestamp (Val)));

   function Field_Condition (Ctx : Context; Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean is
     ((case Fld is
          when F_Tag =>
             Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Destination_Unreachable))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Redirect))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Time_Exceeded))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Reply))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Request))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Parameter_Problem))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Source_Quench))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request)),
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             True,
          when F_Checksum =>
             RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Redirect))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Parameter_Problem))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Reply))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Request))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Time_Exceeded))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Destination_Unreachable))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Source_Quench)),
          when F_Gateway_Internet_Address | F_Identifier | F_Pointer | F_Unused_32 =>
             True,
          when F_Sequence_Number =>
             RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Reply))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Echo_Request))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Request))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Reply))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Msg))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Timestamp_Reply)),
          when F_Unused_24 | F_Originate_Timestamp | F_Data | F_Receive_Timestamp | F_Transmit_Timestamp =>
             True));

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     (Field_Size_Internal (Ctx.Cursors, Ctx.First, Ctx.Verified_Last, Ctx.Written_Last, Ctx.Buffer, Fld));

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index is
     (Field_First_Internal (Ctx.Cursors, Ctx.First, Ctx.Verified_Last, Ctx.Written_Last, Ctx.Buffer, Fld));

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     (Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) - 1);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field is
     ((case Fld is
          when F_Initial =>
             F_Initial,
          when others =>
             Ctx.Cursors (Fld).Predecessor));

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
     (Well_Formed (Ctx, F_Data)
      or (Valid (Ctx, F_Sequence_Number)
          and then (RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Request))
                    or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Reply))))
      or Valid (Ctx, F_Transmit_Timestamp));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Data)
      or (Valid (Ctx, F_Sequence_Number)
          and then (RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Request))
                    or RFLX_Types.Base_Integer (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.ICMP.Information_Reply))))
      or Valid (Ctx, F_Transmit_Timestamp));

   function Incomplete_Message (Ctx : Context) return Boolean is
     ((for some F in Field =>
          Incomplete (Ctx, F)));

   function Get_Tag (Ctx : Context) return RFLX.ICMP.Tag is
     (To_Actual (Ctx.Cursors (F_Tag).Value));

   function Get_Code_Destination_Unreachable (Ctx : Context) return RFLX.ICMP.Code_Destination_Unreachable is
     (To_Actual (Ctx.Cursors (F_Code_Destination_Unreachable).Value));

   function Get_Code_Redirect (Ctx : Context) return RFLX.ICMP.Code_Redirect is
     (To_Actual (Ctx.Cursors (F_Code_Redirect).Value));

   function Get_Code_Time_Exceeded (Ctx : Context) return RFLX.ICMP.Code_Time_Exceeded is
     (To_Actual (Ctx.Cursors (F_Code_Time_Exceeded).Value));

   function Get_Code_Zero (Ctx : Context) return RFLX.ICMP.Code_Zero is
     (To_Actual (Ctx.Cursors (F_Code_Zero).Value));

   function Get_Checksum (Ctx : Context) return RFLX.ICMP.Checksum is
     (To_Actual (Ctx.Cursors (F_Checksum).Value));

   function Get_Gateway_Internet_Address (Ctx : Context) return RFLX.ICMP.Gateway_Internet_Address is
     (To_Actual (Ctx.Cursors (F_Gateway_Internet_Address).Value));

   function Get_Identifier (Ctx : Context) return RFLX.ICMP.Identifier is
     (To_Actual (Ctx.Cursors (F_Identifier).Value));

   function Get_Pointer (Ctx : Context) return RFLX.ICMP.Pointer is
     (To_Actual (Ctx.Cursors (F_Pointer).Value));

   function Get_Unused_32 (Ctx : Context) return RFLX.ICMP.Unused_32 is
     (To_Actual (Ctx.Cursors (F_Unused_32).Value));

   function Get_Sequence_Number (Ctx : Context) return RFLX.ICMP.Sequence_Number is
     (To_Actual (Ctx.Cursors (F_Sequence_Number).Value));

   function Get_Unused_24 (Ctx : Context) return RFLX.ICMP.Unused_24 is
     (To_Actual (Ctx.Cursors (F_Unused_24).Value));

   function Get_Originate_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp is
     (To_Actual (Ctx.Cursors (F_Originate_Timestamp).Value));

   function Get_Receive_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp is
     (To_Actual (Ctx.Cursors (F_Receive_Timestamp).Value));

   function Get_Transmit_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp is
     (To_Actual (Ctx.Cursors (F_Transmit_Timestamp).Value));

   function Valid_Size (Ctx : Context; Fld : Field; Size : RFLX_Types.Bit_Length) return Boolean is
     ((if
          Fld = F_Data
          and then Ctx.Cursors (Fld).Predecessor = F_Sequence_Number
          and then (RFLX_Types.Bit_Length (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.ICMP.Echo_Reply))
                    or RFLX_Types.Bit_Length (Ctx.Cursors (F_Tag).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.ICMP.Echo_Request)))
       then
          Size <= Available_Space (Ctx, Fld)
       else
          Size = Field_Size (Ctx, Fld)))
    with
     Pre =>
       RFLX.ICMP.Message.Valid_Next (Ctx, Fld);

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean is
     (Valid_Size (Ctx, Fld, RFLX_Types.To_Bit_Length (Length)));

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

   function Context_Cursors_Index (Cursors : Field_Cursors; Fld : Field) return Field_Cursor is
     (Cursors (Fld));

end RFLX.ICMP.Message;

pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.ICMP.Message with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Warnings (Off, "use clause for type ""U64"" * has no effect");

   pragma Warnings (Off, """LENGTH"" is already use-visible through previous use_type_clause");

   use type RFLX_Types.Bytes, RFLX_Types.Bytes_Ptr, RFLX_Types.Length, RFLX_Types.Index, RFLX_Types.Bit_Index, RFLX_Types.U64;

   pragma Warnings (On, """LENGTH"" is already use-visible through previous use_type_clause");

   pragma Warnings (On, "use clause for type ""U64"" * has no effect");

   type Virtual_Field is (F_Initial, F_Tag, F_Code_Destination_Unreachable, F_Code_Redirect, F_Code_Time_Exceeded, F_Code_Zero, F_Checksum, F_Gateway_Internet_Address, F_Identifier, F_Pointer, F_Unused_32, F_Sequence_Number, F_Unused_24, F_Originate_Timestamp, F_Data, F_Receive_Timestamp, F_Transmit_Timestamp, F_Final);

   subtype Field is Virtual_Field range F_Tag .. F_Transmit_Timestamp;

   type Field_Cursor is private with
     Default_Initial_Condition =>
       False;

   type Field_Cursors is private with
     Default_Initial_Condition =>
       False;

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First) is private with
     Default_Initial_Condition =>
       RFLX_Types.Byte_Index (First) >= Buffer_First
       and RFLX_Types.Byte_Index (Last) <= Buffer_Last
       and First <= Last + 1
       and Last < RFLX_Types.Bit_Index'Last
       and First mod RFLX_Types.Byte'Size = 1
       and Last mod RFLX_Types.Byte'Size = 0;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Data | F_Final =>
               null;
            when F_Tag =>
               Tag_Value : RFLX.ICMP.Tag_Base;
            when F_Code_Destination_Unreachable =>
               Code_Destination_Unreachable_Value : RFLX.ICMP.Code_Destination_Unreachable_Base;
            when F_Code_Redirect =>
               Code_Redirect_Value : RFLX.ICMP.Code_Redirect_Base;
            when F_Code_Time_Exceeded =>
               Code_Time_Exceeded_Value : RFLX.ICMP.Code_Time_Exceeded_Base;
            when F_Code_Zero =>
               Code_Zero_Value : RFLX.ICMP.Code_Zero_Base;
            when F_Checksum =>
               Checksum_Value : RFLX.ICMP.Checksum;
            when F_Gateway_Internet_Address =>
               Gateway_Internet_Address_Value : RFLX.ICMP.Gateway_Internet_Address;
            when F_Identifier =>
               Identifier_Value : RFLX.ICMP.Identifier;
            when F_Pointer =>
               Pointer_Value : RFLX.ICMP.Pointer;
            when F_Unused_32 =>
               Unused_32_Value : RFLX.ICMP.Unused_32_Base;
            when F_Sequence_Number =>
               Sequence_Number_Value : RFLX.ICMP.Sequence_Number;
            when F_Unused_24 =>
               Unused_24_Value : RFLX.ICMP.Unused_24_Base;
            when F_Originate_Timestamp =>
               Originate_Timestamp_Value : RFLX.ICMP.Timestamp;
            when F_Receive_Timestamp =>
               Receive_Timestamp_Value : RFLX.ICMP.Timestamp;
            when F_Transmit_Timestamp =>
               Transmit_Timestamp_Value : RFLX.ICMP.Timestamp;
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

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) with
     Pre =>
       not Ctx'Constrained
       and then Buffer /= null
       and then Buffer'Length > 0
       and then RFLX_Types.Byte_Index (First) >= Buffer'First
       and then RFLX_Types.Byte_Index (Last) <= Buffer'Last
       and then First <= Last + 1
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

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) with
     Pre =>
       not Ctx'Constrained
       and Has_Buffer (Ctx)
       and RFLX_Types.Byte_Index (First) >= Ctx.Buffer_First
       and RFLX_Types.Byte_Index (Last) <= Ctx.Buffer_Last
       and First <= Last + 1
       and Last < RFLX_Types.Bit_Length'Last
       and First mod RFLX_Types.Byte'Size = 1
       and Last mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = First
       and Ctx.Last = Last
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
      with procedure Write (Buffer : out RFLX_Types.Bytes; Length : out RFLX_Types.Length);
   procedure Write (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
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

   function Get_Tag (Ctx : Context) return RFLX.ICMP.Tag with
     Pre =>
       Valid (Ctx, F_Tag);

   function Get_Code_Destination_Unreachable (Ctx : Context) return RFLX.ICMP.Code_Destination_Unreachable with
     Pre =>
       Valid (Ctx, F_Code_Destination_Unreachable);

   function Get_Code_Redirect (Ctx : Context) return RFLX.ICMP.Code_Redirect with
     Pre =>
       Valid (Ctx, F_Code_Redirect);

   function Get_Code_Time_Exceeded (Ctx : Context) return RFLX.ICMP.Code_Time_Exceeded with
     Pre =>
       Valid (Ctx, F_Code_Time_Exceeded);

   function Get_Code_Zero (Ctx : Context) return RFLX.ICMP.Code_Zero with
     Pre =>
       Valid (Ctx, F_Code_Zero);

   function Get_Checksum (Ctx : Context) return RFLX.ICMP.Checksum with
     Pre =>
       Valid (Ctx, F_Checksum);

   function Get_Gateway_Internet_Address (Ctx : Context) return RFLX.ICMP.Gateway_Internet_Address with
     Pre =>
       Valid (Ctx, F_Gateway_Internet_Address);

   function Get_Identifier (Ctx : Context) return RFLX.ICMP.Identifier with
     Pre =>
       Valid (Ctx, F_Identifier);

   function Get_Pointer (Ctx : Context) return RFLX.ICMP.Pointer with
     Pre =>
       Valid (Ctx, F_Pointer);

   function Get_Unused_32 (Ctx : Context) return RFLX.ICMP.Unused_32 with
     Pre =>
       Valid (Ctx, F_Unused_32);

   function Get_Sequence_Number (Ctx : Context) return RFLX.ICMP.Sequence_Number with
     Pre =>
       Valid (Ctx, F_Sequence_Number);

   function Get_Unused_24 (Ctx : Context) return RFLX.ICMP.Unused_24 with
     Pre =>
       Valid (Ctx, F_Unused_24);

   function Get_Originate_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp with
     Pre =>
       Valid (Ctx, F_Originate_Timestamp);

   function Get_Receive_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp with
     Pre =>
       Valid (Ctx, F_Receive_Timestamp);

   function Get_Transmit_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp with
     Pre =>
       Valid (Ctx, F_Transmit_Timestamp);

   pragma Warnings (On, "precondition is always False");

   procedure Get_Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       Has_Buffer (Ctx)
       and then Present (Ctx, F_Data)
       and then Valid_Next (Ctx, F_Data)
       and then Data'Length = RFLX_Types.Byte_Length (Field_Size (Ctx, F_Data));

   generic
      with procedure Process_Data (Data : RFLX_Types.Bytes);
   procedure Generic_Get_Data (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and Present (Ctx, F_Data);

   procedure Set_Tag (Ctx : in out Context; Val : RFLX.ICMP.Tag) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Tag)
       and then Field_Condition (Ctx, (F_Tag, To_Base (Val)))
       and then True
       and then Available_Space (Ctx, F_Tag) >= Field_Size (Ctx, F_Tag),
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
               RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
            then
               Predecessor (Ctx, F_Code_Destination_Unreachable) = F_Tag
               and Valid_Next (Ctx, F_Code_Destination_Unreachable))
       and (if
               RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Redirect))
            then
               Predecessor (Ctx, F_Code_Redirect) = F_Tag
               and Valid_Next (Ctx, F_Code_Redirect))
       and (if
               RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Time_Exceeded))
            then
               Predecessor (Ctx, F_Code_Time_Exceeded) = F_Tag
               and Valid_Next (Ctx, F_Code_Time_Exceeded))
       and (if
               RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Information_Reply))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Information_Request))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Parameter_Problem))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Source_Quench))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Echo_Reply))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Echo_Request))
            then
               Predecessor (Ctx, F_Code_Zero) = F_Tag
               and Valid_Next (Ctx, F_Code_Zero))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Tag) = Predecessor (Ctx, F_Tag)'Old
       and Valid_Next (Ctx, F_Tag) = Valid_Next (Ctx, F_Tag)'Old;

   procedure Set_Code_Destination_Unreachable (Ctx : in out Context; Val : RFLX.ICMP.Code_Destination_Unreachable) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Code_Destination_Unreachable)
       and then Field_Condition (Ctx, (F_Code_Destination_Unreachable, To_Base (Val)))
       and then True
       and then Available_Space (Ctx, F_Code_Destination_Unreachable) >= Field_Size (Ctx, F_Code_Destination_Unreachable),
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old;

   procedure Set_Code_Redirect (Ctx : in out Context; Val : RFLX.ICMP.Code_Redirect) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Code_Redirect)
       and then Field_Condition (Ctx, (F_Code_Redirect, To_Base (Val)))
       and then True
       and then Available_Space (Ctx, F_Code_Redirect) >= Field_Size (Ctx, F_Code_Redirect),
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old;

   procedure Set_Code_Time_Exceeded (Ctx : in out Context; Val : RFLX.ICMP.Code_Time_Exceeded) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Code_Time_Exceeded)
       and then Field_Condition (Ctx, (F_Code_Time_Exceeded, To_Base (Val)))
       and then True
       and then Available_Space (Ctx, F_Code_Time_Exceeded) >= Field_Size (Ctx, F_Code_Time_Exceeded),
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old;

   procedure Set_Code_Zero (Ctx : in out Context; Val : RFLX.ICMP.Code_Zero) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Code_Zero)
       and then Field_Condition (Ctx, (F_Code_Zero, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Code_Zero) >= Field_Size (Ctx, F_Code_Zero),
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old
       and Context_Cursor (Ctx, F_Code_Time_Exceeded) = Context_Cursor (Ctx, F_Code_Time_Exceeded)'Old;

   procedure Set_Checksum (Ctx : in out Context; Val : RFLX.ICMP.Checksum) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Checksum)
       and then Field_Condition (Ctx, (F_Checksum, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Checksum) >= Field_Size (Ctx, F_Checksum),
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
               RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Redirect))
            then
               Predecessor (Ctx, F_Gateway_Internet_Address) = F_Checksum
               and Valid_Next (Ctx, F_Gateway_Internet_Address))
       and (if
               RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Information_Reply))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Information_Request))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Echo_Request))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Echo_Reply))
            then
               Predecessor (Ctx, F_Identifier) = F_Checksum
               and Valid_Next (Ctx, F_Identifier))
       and (if
               RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Parameter_Problem))
            then
               Predecessor (Ctx, F_Pointer) = F_Checksum
               and Valid_Next (Ctx, F_Pointer))
       and (if
               RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Time_Exceeded))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Source_Quench))
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old
       and Context_Cursor (Ctx, F_Code_Time_Exceeded) = Context_Cursor (Ctx, F_Code_Time_Exceeded)'Old
       and Context_Cursor (Ctx, F_Code_Zero) = Context_Cursor (Ctx, F_Code_Zero)'Old;

   procedure Set_Gateway_Internet_Address (Ctx : in out Context; Val : RFLX.ICMP.Gateway_Internet_Address) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Gateway_Internet_Address)
       and then Field_Condition (Ctx, (F_Gateway_Internet_Address, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Gateway_Internet_Address) >= Field_Size (Ctx, F_Gateway_Internet_Address),
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old
       and Context_Cursor (Ctx, F_Code_Time_Exceeded) = Context_Cursor (Ctx, F_Code_Time_Exceeded)'Old
       and Context_Cursor (Ctx, F_Code_Zero) = Context_Cursor (Ctx, F_Code_Zero)'Old
       and Context_Cursor (Ctx, F_Checksum) = Context_Cursor (Ctx, F_Checksum)'Old;

   procedure Set_Identifier (Ctx : in out Context; Val : RFLX.ICMP.Identifier) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Identifier)
       and then Field_Condition (Ctx, (F_Identifier, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Identifier) >= Field_Size (Ctx, F_Identifier),
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old
       and Context_Cursor (Ctx, F_Code_Time_Exceeded) = Context_Cursor (Ctx, F_Code_Time_Exceeded)'Old
       and Context_Cursor (Ctx, F_Code_Zero) = Context_Cursor (Ctx, F_Code_Zero)'Old
       and Context_Cursor (Ctx, F_Checksum) = Context_Cursor (Ctx, F_Checksum)'Old
       and Context_Cursor (Ctx, F_Gateway_Internet_Address) = Context_Cursor (Ctx, F_Gateway_Internet_Address)'Old;

   procedure Set_Pointer (Ctx : in out Context; Val : RFLX.ICMP.Pointer) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Pointer)
       and then Field_Condition (Ctx, (F_Pointer, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Pointer) >= Field_Size (Ctx, F_Pointer),
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old
       and Context_Cursor (Ctx, F_Code_Time_Exceeded) = Context_Cursor (Ctx, F_Code_Time_Exceeded)'Old
       and Context_Cursor (Ctx, F_Code_Zero) = Context_Cursor (Ctx, F_Code_Zero)'Old
       and Context_Cursor (Ctx, F_Checksum) = Context_Cursor (Ctx, F_Checksum)'Old
       and Context_Cursor (Ctx, F_Gateway_Internet_Address) = Context_Cursor (Ctx, F_Gateway_Internet_Address)'Old
       and Context_Cursor (Ctx, F_Identifier) = Context_Cursor (Ctx, F_Identifier)'Old;

   procedure Set_Unused_32 (Ctx : in out Context; Val : RFLX.ICMP.Unused_32) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Unused_32)
       and then Field_Condition (Ctx, (F_Unused_32, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Unused_32) >= Field_Size (Ctx, F_Unused_32),
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old
       and Context_Cursor (Ctx, F_Code_Time_Exceeded) = Context_Cursor (Ctx, F_Code_Time_Exceeded)'Old
       and Context_Cursor (Ctx, F_Code_Zero) = Context_Cursor (Ctx, F_Code_Zero)'Old
       and Context_Cursor (Ctx, F_Checksum) = Context_Cursor (Ctx, F_Checksum)'Old
       and Context_Cursor (Ctx, F_Gateway_Internet_Address) = Context_Cursor (Ctx, F_Gateway_Internet_Address)'Old
       and Context_Cursor (Ctx, F_Identifier) = Context_Cursor (Ctx, F_Identifier)'Old
       and Context_Cursor (Ctx, F_Pointer) = Context_Cursor (Ctx, F_Pointer)'Old;

   procedure Set_Sequence_Number (Ctx : in out Context; Val : RFLX.ICMP.Sequence_Number) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Sequence_Number)
       and then Field_Condition (Ctx, (F_Sequence_Number, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Sequence_Number) >= Field_Size (Ctx, F_Sequence_Number),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Sequence_Number)
       and Get_Sequence_Number (Ctx) = Val
       and (if
               Structural_Valid_Message (Ctx)
            then
               Message_Last (Ctx) = Field_Last (Ctx, F_Sequence_Number))
       and Invalid (Ctx, F_Unused_24)
       and Invalid (Ctx, F_Originate_Timestamp)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and (if
               RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Echo_Reply))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Echo_Request))
            then
               Predecessor (Ctx, F_Data) = F_Sequence_Number
               and Valid_Next (Ctx, F_Data))
       and (if
               RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
               or RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old
       and Context_Cursor (Ctx, F_Code_Time_Exceeded) = Context_Cursor (Ctx, F_Code_Time_Exceeded)'Old
       and Context_Cursor (Ctx, F_Code_Zero) = Context_Cursor (Ctx, F_Code_Zero)'Old
       and Context_Cursor (Ctx, F_Checksum) = Context_Cursor (Ctx, F_Checksum)'Old
       and Context_Cursor (Ctx, F_Gateway_Internet_Address) = Context_Cursor (Ctx, F_Gateway_Internet_Address)'Old
       and Context_Cursor (Ctx, F_Identifier) = Context_Cursor (Ctx, F_Identifier)'Old
       and Context_Cursor (Ctx, F_Pointer) = Context_Cursor (Ctx, F_Pointer)'Old
       and Context_Cursor (Ctx, F_Unused_32) = Context_Cursor (Ctx, F_Unused_32)'Old;

   procedure Set_Unused_24 (Ctx : in out Context; Val : RFLX.ICMP.Unused_24) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Unused_24)
       and then Field_Condition (Ctx, (F_Unused_24, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Unused_24) >= Field_Size (Ctx, F_Unused_24),
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old
       and Context_Cursor (Ctx, F_Code_Time_Exceeded) = Context_Cursor (Ctx, F_Code_Time_Exceeded)'Old
       and Context_Cursor (Ctx, F_Code_Zero) = Context_Cursor (Ctx, F_Code_Zero)'Old
       and Context_Cursor (Ctx, F_Checksum) = Context_Cursor (Ctx, F_Checksum)'Old
       and Context_Cursor (Ctx, F_Gateway_Internet_Address) = Context_Cursor (Ctx, F_Gateway_Internet_Address)'Old
       and Context_Cursor (Ctx, F_Identifier) = Context_Cursor (Ctx, F_Identifier)'Old
       and Context_Cursor (Ctx, F_Pointer) = Context_Cursor (Ctx, F_Pointer)'Old
       and Context_Cursor (Ctx, F_Unused_32) = Context_Cursor (Ctx, F_Unused_32)'Old
       and Context_Cursor (Ctx, F_Sequence_Number) = Context_Cursor (Ctx, F_Sequence_Number)'Old;

   procedure Set_Originate_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Originate_Timestamp)
       and then Field_Condition (Ctx, (F_Originate_Timestamp, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Originate_Timestamp) >= Field_Size (Ctx, F_Originate_Timestamp),
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old
       and Context_Cursor (Ctx, F_Code_Time_Exceeded) = Context_Cursor (Ctx, F_Code_Time_Exceeded)'Old
       and Context_Cursor (Ctx, F_Code_Zero) = Context_Cursor (Ctx, F_Code_Zero)'Old
       and Context_Cursor (Ctx, F_Checksum) = Context_Cursor (Ctx, F_Checksum)'Old
       and Context_Cursor (Ctx, F_Gateway_Internet_Address) = Context_Cursor (Ctx, F_Gateway_Internet_Address)'Old
       and Context_Cursor (Ctx, F_Identifier) = Context_Cursor (Ctx, F_Identifier)'Old
       and Context_Cursor (Ctx, F_Pointer) = Context_Cursor (Ctx, F_Pointer)'Old
       and Context_Cursor (Ctx, F_Unused_32) = Context_Cursor (Ctx, F_Unused_32)'Old
       and Context_Cursor (Ctx, F_Sequence_Number) = Context_Cursor (Ctx, F_Sequence_Number)'Old
       and Context_Cursor (Ctx, F_Unused_24) = Context_Cursor (Ctx, F_Unused_24)'Old;

   procedure Set_Receive_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Receive_Timestamp)
       and then Field_Condition (Ctx, (F_Receive_Timestamp, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Receive_Timestamp) >= Field_Size (Ctx, F_Receive_Timestamp),
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old
       and Context_Cursor (Ctx, F_Code_Time_Exceeded) = Context_Cursor (Ctx, F_Code_Time_Exceeded)'Old
       and Context_Cursor (Ctx, F_Code_Zero) = Context_Cursor (Ctx, F_Code_Zero)'Old
       and Context_Cursor (Ctx, F_Checksum) = Context_Cursor (Ctx, F_Checksum)'Old
       and Context_Cursor (Ctx, F_Gateway_Internet_Address) = Context_Cursor (Ctx, F_Gateway_Internet_Address)'Old
       and Context_Cursor (Ctx, F_Identifier) = Context_Cursor (Ctx, F_Identifier)'Old
       and Context_Cursor (Ctx, F_Pointer) = Context_Cursor (Ctx, F_Pointer)'Old
       and Context_Cursor (Ctx, F_Unused_32) = Context_Cursor (Ctx, F_Unused_32)'Old
       and Context_Cursor (Ctx, F_Sequence_Number) = Context_Cursor (Ctx, F_Sequence_Number)'Old
       and Context_Cursor (Ctx, F_Unused_24) = Context_Cursor (Ctx, F_Unused_24)'Old
       and Context_Cursor (Ctx, F_Originate_Timestamp) = Context_Cursor (Ctx, F_Originate_Timestamp)'Old
       and Context_Cursor (Ctx, F_Data) = Context_Cursor (Ctx, F_Data)'Old;

   procedure Set_Transmit_Timestamp (Ctx : in out Context; Val : RFLX.ICMP.Timestamp) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Transmit_Timestamp)
       and then Field_Condition (Ctx, (F_Transmit_Timestamp, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Transmit_Timestamp) >= Field_Size (Ctx, F_Transmit_Timestamp),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Transmit_Timestamp)
       and Get_Transmit_Timestamp (Ctx) = Val
       and (if
               Structural_Valid_Message (Ctx)
            then
               Message_Last (Ctx) = Field_Last (Ctx, F_Transmit_Timestamp))
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
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old
       and Context_Cursor (Ctx, F_Code_Destination_Unreachable) = Context_Cursor (Ctx, F_Code_Destination_Unreachable)'Old
       and Context_Cursor (Ctx, F_Code_Redirect) = Context_Cursor (Ctx, F_Code_Redirect)'Old
       and Context_Cursor (Ctx, F_Code_Time_Exceeded) = Context_Cursor (Ctx, F_Code_Time_Exceeded)'Old
       and Context_Cursor (Ctx, F_Code_Zero) = Context_Cursor (Ctx, F_Code_Zero)'Old
       and Context_Cursor (Ctx, F_Checksum) = Context_Cursor (Ctx, F_Checksum)'Old
       and Context_Cursor (Ctx, F_Gateway_Internet_Address) = Context_Cursor (Ctx, F_Gateway_Internet_Address)'Old
       and Context_Cursor (Ctx, F_Identifier) = Context_Cursor (Ctx, F_Identifier)'Old
       and Context_Cursor (Ctx, F_Pointer) = Context_Cursor (Ctx, F_Pointer)'Old
       and Context_Cursor (Ctx, F_Unused_32) = Context_Cursor (Ctx, F_Unused_32)'Old
       and Context_Cursor (Ctx, F_Sequence_Number) = Context_Cursor (Ctx, F_Sequence_Number)'Old
       and Context_Cursor (Ctx, F_Unused_24) = Context_Cursor (Ctx, F_Unused_24)'Old
       and Context_Cursor (Ctx, F_Originate_Timestamp) = Context_Cursor (Ctx, F_Originate_Timestamp)'Old
       and Context_Cursor (Ctx, F_Data) = Context_Cursor (Ctx, F_Data)'Old
       and Context_Cursor (Ctx, F_Receive_Timestamp) = Context_Cursor (Ctx, F_Receive_Timestamp)'Old;

   procedure Set_Data_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Data)
       and then Field_Condition (Ctx, (Fld => F_Data))
       and then Available_Space (Ctx, F_Data) >= Field_Size (Ctx, F_Data)
       and then Field_First (Ctx, F_Data) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Data) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Data)
       and (if
               Structural_Valid_Message (Ctx)
            then
               Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old;

   procedure Initialize_Data (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Data)
       and then Field_Condition (Ctx, (Fld => F_Data))
       and then Available_Space (Ctx, F_Data) >= Field_Size (Ctx, F_Data)
       and then Field_First (Ctx, F_Data) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Data)
       and (if
               Structural_Valid_Message (Ctx)
            then
               Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old;

   procedure Set_Data (Ctx : in out Context; Data : RFLX_Types.Bytes) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Data)
       and then Field_Condition (Ctx, (Fld => F_Data))
       and then Available_Space (Ctx, F_Data) >= Field_Size (Ctx, F_Data)
       and then Field_First (Ctx, F_Data) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0
       and then Data'Length = RFLX_Types.Byte_Length (Field_Size (Ctx, F_Data)),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Data)
       and (if
               Structural_Valid_Message (Ctx)
            then
               Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old;

   generic
      with procedure Process_Data (Data : out RFLX_Types.Bytes);
      with function Valid_Length (Length : RFLX_Types.Length) return Boolean;
   procedure Generic_Set_Data (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Data)
       and then Field_Condition (Ctx, (Fld => F_Data))
       and then Available_Space (Ctx, F_Data) >= Field_Size (Ctx, F_Data)
       and then Field_First (Ctx, F_Data) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0
       and then Valid_Length (RFLX_Types.Length (Field_Size (Ctx, F_Data) / RFLX_Types.Byte'Size)),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Data)
       and (if
               Structural_Valid_Message (Ctx)
            then
               Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Invalid (Ctx, F_Receive_Timestamp)
       and Invalid (Ctx, F_Transmit_Timestamp)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old;

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
          when F_Tag =>
             Valid (Val.Tag_Value),
          when F_Code_Destination_Unreachable =>
             Valid (Val.Code_Destination_Unreachable_Value),
          when F_Code_Redirect =>
             Valid (Val.Code_Redirect_Value),
          when F_Code_Time_Exceeded =>
             Valid (Val.Code_Time_Exceeded_Value),
          when F_Code_Zero =>
             Valid (Val.Code_Zero_Value),
          when F_Checksum =>
             Valid (Val.Checksum_Value),
          when F_Gateway_Internet_Address =>
             Valid (Val.Gateway_Internet_Address_Value),
          when F_Identifier =>
             Valid (Val.Identifier_Value),
          when F_Pointer =>
             Valid (Val.Pointer_Value),
          when F_Unused_32 =>
             Valid (Val.Unused_32_Value),
          when F_Sequence_Number =>
             Valid (Val.Sequence_Number_Value),
          when F_Unused_24 =>
             Valid (Val.Unused_24_Value),
          when F_Originate_Timestamp =>
             Valid (Val.Originate_Timestamp_Value),
          when F_Data =>
             True,
          when F_Receive_Timestamp =>
             Valid (Val.Receive_Timestamp_Value),
          when F_Transmit_Timestamp =>
             Valid (Val.Transmit_Timestamp_Value),
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

   function Valid_Context (Buffer_First, Buffer_Last : RFLX_Types.Index; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length; Message_Last : RFLX_Types.Bit_Length; Buffer : RFLX_Types.Bytes_Ptr; Cursors : Field_Cursors) return Boolean is
     ((if
          Buffer /= null
       then
          Buffer'First = Buffer_First
          and Buffer'Last = Buffer_Last)
      and then (RFLX_Types.Byte_Index (First) >= Buffer_First
                and RFLX_Types.Byte_Index (Last) <= Buffer_Last
                and First <= Last + 1
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
                    Structural_Valid (Cursors (F_Code_Destination_Unreachable))
                 then
                    (Valid (Cursors (F_Tag))
                     and then Cursors (F_Code_Destination_Unreachable).Predecessor = F_Tag
                     and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))))
                and then (if
                             Structural_Valid (Cursors (F_Code_Redirect))
                          then
                             (Valid (Cursors (F_Tag))
                              and then Cursors (F_Code_Redirect).Predecessor = F_Tag
                              and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))))
                and then (if
                             Structural_Valid (Cursors (F_Code_Time_Exceeded))
                          then
                             (Valid (Cursors (F_Tag))
                              and then Cursors (F_Code_Time_Exceeded).Predecessor = F_Tag
                              and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))))
                and then (if
                             Structural_Valid (Cursors (F_Code_Zero))
                          then
                             (Valid (Cursors (F_Tag))
                              and then Cursors (F_Code_Zero).Predecessor = F_Tag
                              and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)))))
                and then (if
                             Structural_Valid (Cursors (F_Checksum))
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
                             Structural_Valid (Cursors (F_Gateway_Internet_Address))
                          then
                             (Valid (Cursors (F_Checksum))
                              and then Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                              and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))))
                and then (if
                             Structural_Valid (Cursors (F_Identifier))
                          then
                             (Valid (Cursors (F_Checksum))
                              and then Cursors (F_Identifier).Predecessor = F_Checksum
                              and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))))
                and then (if
                             Structural_Valid (Cursors (F_Pointer))
                          then
                             (Valid (Cursors (F_Checksum))
                              and then Cursors (F_Pointer).Predecessor = F_Checksum
                              and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))))
                and then (if
                             Structural_Valid (Cursors (F_Unused_32))
                          then
                             (Valid (Cursors (F_Checksum))
                              and then Cursors (F_Unused_32).Predecessor = F_Checksum
                              and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))))
                and then (if
                             Structural_Valid (Cursors (F_Sequence_Number))
                          then
                             (Valid (Cursors (F_Identifier))
                              and then Cursors (F_Sequence_Number).Predecessor = F_Identifier))
                and then (if
                             Structural_Valid (Cursors (F_Unused_24))
                          then
                             (Valid (Cursors (F_Pointer))
                              and then Cursors (F_Unused_24).Predecessor = F_Pointer))
                and then (if
                             Structural_Valid (Cursors (F_Originate_Timestamp))
                          then
                             (Valid (Cursors (F_Sequence_Number))
                              and then Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                              and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                        or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))))
                and then (if
                             Structural_Valid (Cursors (F_Data))
                          then
                             (Valid (Cursors (F_Gateway_Internet_Address))
                              and then Cursors (F_Data).Predecessor = F_Gateway_Internet_Address)
                             or (Valid (Cursors (F_Sequence_Number))
                                 and then Cursors (F_Data).Predecessor = F_Sequence_Number
                                 and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                           or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))))
                             or (Valid (Cursors (F_Unused_24))
                                 and then Cursors (F_Data).Predecessor = F_Unused_24)
                             or (Valid (Cursors (F_Unused_32))
                                 and then Cursors (F_Data).Predecessor = F_Unused_32))
                and then (if
                             Structural_Valid (Cursors (F_Receive_Timestamp))
                          then
                             (Valid (Cursors (F_Originate_Timestamp))
                              and then Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp))
                and then (if
                             Structural_Valid (Cursors (F_Transmit_Timestamp))
                          then
                             (Valid (Cursors (F_Receive_Timestamp))
                              and then Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp)))
      and then ((if
                    Invalid (Cursors (F_Tag))
                 then
                    Invalid (Cursors (F_Code_Destination_Unreachable)))
                and then (if
                             Invalid (Cursors (F_Tag))
                          then
                             Invalid (Cursors (F_Code_Redirect)))
                and then (if
                             Invalid (Cursors (F_Tag))
                          then
                             Invalid (Cursors (F_Code_Time_Exceeded)))
                and then (if
                             Invalid (Cursors (F_Tag))
                          then
                             Invalid (Cursors (F_Code_Zero)))
                and then (if
                             Invalid (Cursors (F_Code_Destination_Unreachable))
                             and then Invalid (Cursors (F_Code_Redirect))
                             and then Invalid (Cursors (F_Code_Time_Exceeded))
                             and then Invalid (Cursors (F_Code_Zero))
                          then
                             Invalid (Cursors (F_Checksum)))
                and then (if
                             Invalid (Cursors (F_Checksum))
                          then
                             Invalid (Cursors (F_Gateway_Internet_Address)))
                and then (if
                             Invalid (Cursors (F_Checksum))
                          then
                             Invalid (Cursors (F_Identifier)))
                and then (if
                             Invalid (Cursors (F_Checksum))
                          then
                             Invalid (Cursors (F_Pointer)))
                and then (if
                             Invalid (Cursors (F_Checksum))
                          then
                             Invalid (Cursors (F_Unused_32)))
                and then (if
                             Invalid (Cursors (F_Identifier))
                          then
                             Invalid (Cursors (F_Sequence_Number)))
                and then (if
                             Invalid (Cursors (F_Pointer))
                          then
                             Invalid (Cursors (F_Unused_24)))
                and then (if
                             Invalid (Cursors (F_Sequence_Number))
                          then
                             Invalid (Cursors (F_Originate_Timestamp)))
                and then (if
                             Invalid (Cursors (F_Gateway_Internet_Address))
                             and then Invalid (Cursors (F_Sequence_Number))
                             and then Invalid (Cursors (F_Unused_24))
                             and then Invalid (Cursors (F_Unused_32))
                          then
                             Invalid (Cursors (F_Data)))
                and then (if
                             Invalid (Cursors (F_Originate_Timestamp))
                          then
                             Invalid (Cursors (F_Receive_Timestamp)))
                and then (if
                             Invalid (Cursors (F_Receive_Timestamp))
                          then
                             Invalid (Cursors (F_Transmit_Timestamp))))
      and then (if
                   Structural_Valid (Cursors (F_Tag))
                then
                   Cursors (F_Tag).Last - Cursors (F_Tag).First + 1 = RFLX.ICMP.Tag_Base'Size
                   and then Cursors (F_Tag).Predecessor = F_Initial
                   and then Cursors (F_Tag).First = First
                   and then (if
                                Structural_Valid (Cursors (F_Code_Destination_Unreachable))
                                and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                             then
                                Cursors (F_Code_Destination_Unreachable).Last - Cursors (F_Code_Destination_Unreachable).First + 1 = RFLX.ICMP.Code_Destination_Unreachable_Base'Size
                                and then Cursors (F_Code_Destination_Unreachable).Predecessor = F_Tag
                                and then Cursors (F_Code_Destination_Unreachable).First = Cursors (F_Tag).Last + 1
                                and then (if
                                             Structural_Valid (Cursors (F_Checksum))
                                          then
                                             Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                             and then Cursors (F_Checksum).Predecessor = F_Code_Destination_Unreachable
                                             and then Cursors (F_Checksum).First = Cursors (F_Code_Destination_Unreachable).Last + 1
                                             and then (if
                                                          Structural_Valid (Cursors (F_Gateway_Internet_Address))
                                                          and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                                                       then
                                                          Cursors (F_Gateway_Internet_Address).Last - Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                          and then Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                          and then Cursors (F_Gateway_Internet_Address).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Data))
                                                                    then
                                                                       Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                       and then Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                       and then Cursors (F_Data).First = Cursors (F_Gateway_Internet_Address).Last + 1))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Identifier))
                                                          and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                                                       then
                                                          Cursors (F_Identifier).Last - Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                          and then Cursors (F_Identifier).Predecessor = F_Checksum
                                                          and then Cursors (F_Identifier).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Sequence_Number))
                                                                    then
                                                                       Cursors (F_Sequence_Number).Last - Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                       and then Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                       and then Cursors (F_Sequence_Number).First = Cursors (F_Identifier).Last + 1
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Data))
                                                                                    and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                                                              or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)))
                                                                                 then
                                                                                    Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Last) - RFLX_Types.Bit_Length (Cursors (F_Sequence_Number).Last)
                                                                                    and then Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                    and then Cursors (F_Data).First = Cursors (F_Sequence_Number).Last + 1)
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Originate_Timestamp))
                                                                                    and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                                                              or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                                                                 then
                                                                                    Cursors (F_Originate_Timestamp).Last - Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                    and then Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                    and then Cursors (F_Originate_Timestamp).First = Cursors (F_Sequence_Number).Last + 1
                                                                                    and then (if
                                                                                                 Structural_Valid (Cursors (F_Receive_Timestamp))
                                                                                              then
                                                                                                 Cursors (F_Receive_Timestamp).Last - Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                 and then Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                 and then Cursors (F_Receive_Timestamp).First = Cursors (F_Originate_Timestamp).Last + 1
                                                                                                 and then (if
                                                                                                              Structural_Valid (Cursors (F_Transmit_Timestamp))
                                                                                                           then
                                                                                                              Cursors (F_Transmit_Timestamp).Last - Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                              and then Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                              and then Cursors (F_Transmit_Timestamp).First = Cursors (F_Receive_Timestamp).Last + 1)))))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Pointer))
                                                          and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                                       then
                                                          Cursors (F_Pointer).Last - Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                          and then Cursors (F_Pointer).Predecessor = F_Checksum
                                                          and then Cursors (F_Pointer).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Unused_24))
                                                                    then
                                                                       Cursors (F_Unused_24).Last - Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                       and then Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                       and then Cursors (F_Unused_24).First = Cursors (F_Pointer).Last + 1
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Data))
                                                                                 then
                                                                                    Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                                    and then Cursors (F_Data).Predecessor = F_Unused_24
                                                                                    and then Cursors (F_Data).First = Cursors (F_Unused_24).Last + 1)))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Unused_32))
                                                          and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                                                       then
                                                          Cursors (F_Unused_32).Last - Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                          and then Cursors (F_Unused_32).Predecessor = F_Checksum
                                                          and then Cursors (F_Unused_32).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Data))
                                                                    then
                                                                       Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                       and then Cursors (F_Data).Predecessor = F_Unused_32
                                                                       and then Cursors (F_Data).First = Cursors (F_Unused_32).Last + 1))))
                   and then (if
                                Structural_Valid (Cursors (F_Code_Redirect))
                                and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                             then
                                Cursors (F_Code_Redirect).Last - Cursors (F_Code_Redirect).First + 1 = RFLX.ICMP.Code_Redirect_Base'Size
                                and then Cursors (F_Code_Redirect).Predecessor = F_Tag
                                and then Cursors (F_Code_Redirect).First = Cursors (F_Tag).Last + 1
                                and then (if
                                             Structural_Valid (Cursors (F_Checksum))
                                          then
                                             Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                             and then Cursors (F_Checksum).Predecessor = F_Code_Redirect
                                             and then Cursors (F_Checksum).First = Cursors (F_Code_Redirect).Last + 1
                                             and then (if
                                                          Structural_Valid (Cursors (F_Gateway_Internet_Address))
                                                          and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                                                       then
                                                          Cursors (F_Gateway_Internet_Address).Last - Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                          and then Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                          and then Cursors (F_Gateway_Internet_Address).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Data))
                                                                    then
                                                                       Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                       and then Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                       and then Cursors (F_Data).First = Cursors (F_Gateway_Internet_Address).Last + 1))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Identifier))
                                                          and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                                                       then
                                                          Cursors (F_Identifier).Last - Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                          and then Cursors (F_Identifier).Predecessor = F_Checksum
                                                          and then Cursors (F_Identifier).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Sequence_Number))
                                                                    then
                                                                       Cursors (F_Sequence_Number).Last - Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                       and then Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                       and then Cursors (F_Sequence_Number).First = Cursors (F_Identifier).Last + 1
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Data))
                                                                                    and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                                                              or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)))
                                                                                 then
                                                                                    Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Last) - RFLX_Types.Bit_Length (Cursors (F_Sequence_Number).Last)
                                                                                    and then Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                    and then Cursors (F_Data).First = Cursors (F_Sequence_Number).Last + 1)
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Originate_Timestamp))
                                                                                    and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                                                              or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                                                                 then
                                                                                    Cursors (F_Originate_Timestamp).Last - Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                    and then Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                    and then Cursors (F_Originate_Timestamp).First = Cursors (F_Sequence_Number).Last + 1
                                                                                    and then (if
                                                                                                 Structural_Valid (Cursors (F_Receive_Timestamp))
                                                                                              then
                                                                                                 Cursors (F_Receive_Timestamp).Last - Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                 and then Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                 and then Cursors (F_Receive_Timestamp).First = Cursors (F_Originate_Timestamp).Last + 1
                                                                                                 and then (if
                                                                                                              Structural_Valid (Cursors (F_Transmit_Timestamp))
                                                                                                           then
                                                                                                              Cursors (F_Transmit_Timestamp).Last - Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                              and then Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                              and then Cursors (F_Transmit_Timestamp).First = Cursors (F_Receive_Timestamp).Last + 1)))))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Pointer))
                                                          and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                                       then
                                                          Cursors (F_Pointer).Last - Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                          and then Cursors (F_Pointer).Predecessor = F_Checksum
                                                          and then Cursors (F_Pointer).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Unused_24))
                                                                    then
                                                                       Cursors (F_Unused_24).Last - Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                       and then Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                       and then Cursors (F_Unused_24).First = Cursors (F_Pointer).Last + 1
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Data))
                                                                                 then
                                                                                    Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                                    and then Cursors (F_Data).Predecessor = F_Unused_24
                                                                                    and then Cursors (F_Data).First = Cursors (F_Unused_24).Last + 1)))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Unused_32))
                                                          and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                                                       then
                                                          Cursors (F_Unused_32).Last - Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                          and then Cursors (F_Unused_32).Predecessor = F_Checksum
                                                          and then Cursors (F_Unused_32).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Data))
                                                                    then
                                                                       Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                       and then Cursors (F_Data).Predecessor = F_Unused_32
                                                                       and then Cursors (F_Data).First = Cursors (F_Unused_32).Last + 1))))
                   and then (if
                                Structural_Valid (Cursors (F_Code_Time_Exceeded))
                                and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                             then
                                Cursors (F_Code_Time_Exceeded).Last - Cursors (F_Code_Time_Exceeded).First + 1 = RFLX.ICMP.Code_Time_Exceeded_Base'Size
                                and then Cursors (F_Code_Time_Exceeded).Predecessor = F_Tag
                                and then Cursors (F_Code_Time_Exceeded).First = Cursors (F_Tag).Last + 1
                                and then (if
                                             Structural_Valid (Cursors (F_Checksum))
                                          then
                                             Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                             and then Cursors (F_Checksum).Predecessor = F_Code_Time_Exceeded
                                             and then Cursors (F_Checksum).First = Cursors (F_Code_Time_Exceeded).Last + 1
                                             and then (if
                                                          Structural_Valid (Cursors (F_Gateway_Internet_Address))
                                                          and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                                                       then
                                                          Cursors (F_Gateway_Internet_Address).Last - Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                          and then Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                          and then Cursors (F_Gateway_Internet_Address).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Data))
                                                                    then
                                                                       Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                       and then Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                       and then Cursors (F_Data).First = Cursors (F_Gateway_Internet_Address).Last + 1))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Identifier))
                                                          and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                                                       then
                                                          Cursors (F_Identifier).Last - Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                          and then Cursors (F_Identifier).Predecessor = F_Checksum
                                                          and then Cursors (F_Identifier).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Sequence_Number))
                                                                    then
                                                                       Cursors (F_Sequence_Number).Last - Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                       and then Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                       and then Cursors (F_Sequence_Number).First = Cursors (F_Identifier).Last + 1
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Data))
                                                                                    and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                                                              or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)))
                                                                                 then
                                                                                    Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Last) - RFLX_Types.Bit_Length (Cursors (F_Sequence_Number).Last)
                                                                                    and then Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                    and then Cursors (F_Data).First = Cursors (F_Sequence_Number).Last + 1)
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Originate_Timestamp))
                                                                                    and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                                                              or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                                                                 then
                                                                                    Cursors (F_Originate_Timestamp).Last - Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                    and then Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                    and then Cursors (F_Originate_Timestamp).First = Cursors (F_Sequence_Number).Last + 1
                                                                                    and then (if
                                                                                                 Structural_Valid (Cursors (F_Receive_Timestamp))
                                                                                              then
                                                                                                 Cursors (F_Receive_Timestamp).Last - Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                 and then Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                 and then Cursors (F_Receive_Timestamp).First = Cursors (F_Originate_Timestamp).Last + 1
                                                                                                 and then (if
                                                                                                              Structural_Valid (Cursors (F_Transmit_Timestamp))
                                                                                                           then
                                                                                                              Cursors (F_Transmit_Timestamp).Last - Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                              and then Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                              and then Cursors (F_Transmit_Timestamp).First = Cursors (F_Receive_Timestamp).Last + 1)))))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Pointer))
                                                          and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                                       then
                                                          Cursors (F_Pointer).Last - Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                          and then Cursors (F_Pointer).Predecessor = F_Checksum
                                                          and then Cursors (F_Pointer).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Unused_24))
                                                                    then
                                                                       Cursors (F_Unused_24).Last - Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                       and then Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                       and then Cursors (F_Unused_24).First = Cursors (F_Pointer).Last + 1
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Data))
                                                                                 then
                                                                                    Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                                    and then Cursors (F_Data).Predecessor = F_Unused_24
                                                                                    and then Cursors (F_Data).First = Cursors (F_Unused_24).Last + 1)))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Unused_32))
                                                          and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                                                       then
                                                          Cursors (F_Unused_32).Last - Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                          and then Cursors (F_Unused_32).Predecessor = F_Checksum
                                                          and then Cursors (F_Unused_32).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Data))
                                                                    then
                                                                       Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                       and then Cursors (F_Data).Predecessor = F_Unused_32
                                                                       and then Cursors (F_Data).First = Cursors (F_Unused_32).Last + 1))))
                   and then (if
                                Structural_Valid (Cursors (F_Code_Zero))
                                and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                          or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                          or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                          or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                          or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                          or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench))
                                          or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                          or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)))
                             then
                                Cursors (F_Code_Zero).Last - Cursors (F_Code_Zero).First + 1 = RFLX.ICMP.Code_Zero_Base'Size
                                and then Cursors (F_Code_Zero).Predecessor = F_Tag
                                and then Cursors (F_Code_Zero).First = Cursors (F_Tag).Last + 1
                                and then (if
                                             Structural_Valid (Cursors (F_Checksum))
                                          then
                                             Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1 = RFLX.ICMP.Checksum'Size
                                             and then Cursors (F_Checksum).Predecessor = F_Code_Zero
                                             and then Cursors (F_Checksum).First = Cursors (F_Code_Zero).Last + 1
                                             and then (if
                                                          Structural_Valid (Cursors (F_Gateway_Internet_Address))
                                                          and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                                                       then
                                                          Cursors (F_Gateway_Internet_Address).Last - Cursors (F_Gateway_Internet_Address).First + 1 = RFLX.ICMP.Gateway_Internet_Address'Size
                                                          and then Cursors (F_Gateway_Internet_Address).Predecessor = F_Checksum
                                                          and then Cursors (F_Gateway_Internet_Address).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Data))
                                                                    then
                                                                       Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                       and then Cursors (F_Data).Predecessor = F_Gateway_Internet_Address
                                                                       and then Cursors (F_Data).First = Cursors (F_Gateway_Internet_Address).Last + 1))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Identifier))
                                                          and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                                                       then
                                                          Cursors (F_Identifier).Last - Cursors (F_Identifier).First + 1 = RFLX.ICMP.Identifier'Size
                                                          and then Cursors (F_Identifier).Predecessor = F_Checksum
                                                          and then Cursors (F_Identifier).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Sequence_Number))
                                                                    then
                                                                       Cursors (F_Sequence_Number).Last - Cursors (F_Sequence_Number).First + 1 = RFLX.ICMP.Sequence_Number'Size
                                                                       and then Cursors (F_Sequence_Number).Predecessor = F_Identifier
                                                                       and then Cursors (F_Sequence_Number).First = Cursors (F_Identifier).Last + 1
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Data))
                                                                                    and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                                                              or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)))
                                                                                 then
                                                                                    Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Last) - RFLX_Types.Bit_Length (Cursors (F_Sequence_Number).Last)
                                                                                    and then Cursors (F_Data).Predecessor = F_Sequence_Number
                                                                                    and then Cursors (F_Data).First = Cursors (F_Sequence_Number).Last + 1)
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Originate_Timestamp))
                                                                                    and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                                                              or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                                                                 then
                                                                                    Cursors (F_Originate_Timestamp).Last - Cursors (F_Originate_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                    and then Cursors (F_Originate_Timestamp).Predecessor = F_Sequence_Number
                                                                                    and then Cursors (F_Originate_Timestamp).First = Cursors (F_Sequence_Number).Last + 1
                                                                                    and then (if
                                                                                                 Structural_Valid (Cursors (F_Receive_Timestamp))
                                                                                              then
                                                                                                 Cursors (F_Receive_Timestamp).Last - Cursors (F_Receive_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                 and then Cursors (F_Receive_Timestamp).Predecessor = F_Originate_Timestamp
                                                                                                 and then Cursors (F_Receive_Timestamp).First = Cursors (F_Originate_Timestamp).Last + 1
                                                                                                 and then (if
                                                                                                              Structural_Valid (Cursors (F_Transmit_Timestamp))
                                                                                                           then
                                                                                                              Cursors (F_Transmit_Timestamp).Last - Cursors (F_Transmit_Timestamp).First + 1 = RFLX.ICMP.Timestamp'Size
                                                                                                              and then Cursors (F_Transmit_Timestamp).Predecessor = F_Receive_Timestamp
                                                                                                              and then Cursors (F_Transmit_Timestamp).First = Cursors (F_Receive_Timestamp).Last + 1)))))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Pointer))
                                                          and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                                       then
                                                          Cursors (F_Pointer).Last - Cursors (F_Pointer).First + 1 = RFLX.ICMP.Pointer'Size
                                                          and then Cursors (F_Pointer).Predecessor = F_Checksum
                                                          and then Cursors (F_Pointer).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Unused_24))
                                                                    then
                                                                       Cursors (F_Unused_24).Last - Cursors (F_Unused_24).First + 1 = RFLX.ICMP.Unused_24_Base'Size
                                                                       and then Cursors (F_Unused_24).Predecessor = F_Pointer
                                                                       and then Cursors (F_Unused_24).First = Cursors (F_Pointer).Last + 1
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Data))
                                                                                 then
                                                                                    Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                                    and then Cursors (F_Data).Predecessor = F_Unused_24
                                                                                    and then Cursors (F_Data).First = Cursors (F_Unused_24).Last + 1)))
                                             and then (if
                                                          Structural_Valid (Cursors (F_Unused_32))
                                                          and then (RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                                                    or RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                                                       then
                                                          Cursors (F_Unused_32).Last - Cursors (F_Unused_32).First + 1 = RFLX.ICMP.Unused_32_Base'Size
                                                          and then Cursors (F_Unused_32).Predecessor = F_Checksum
                                                          and then Cursors (F_Unused_32).First = Cursors (F_Checksum).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Data))
                                                                    then
                                                                       Cursors (F_Data).Last - Cursors (F_Data).First + 1 = 224
                                                                       and then Cursors (F_Data).Predecessor = F_Unused_32
                                                                       and then Cursors (F_Data).First = Cursors (F_Unused_32).Last + 1))))));

   pragma Warnings (On, """Buffer"" is not modified, could be of access constant type");

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First) is
      record
         Message_Last : RFLX_Types.Bit_Length := First - 1;
         Buffer : RFLX_Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Message_Last, Context.Buffer, Context.Cursors);

   function Initialized (Ctx : Context) return Boolean is
     (Ctx.Message_Last = Ctx.First - 1
      and then Valid_Next (Ctx, F_Tag)
      and then Field_First (Ctx, F_Tag) mod RFLX_Types.Byte'Size = 1
      and then Available_Space (Ctx, F_Tag) = Ctx.Last - Ctx.First + 1
      and then Invalid (Ctx, F_Tag)
      and then Invalid (Ctx, F_Code_Destination_Unreachable)
      and then Invalid (Ctx, F_Code_Redirect)
      and then Invalid (Ctx, F_Code_Time_Exceeded)
      and then Invalid (Ctx, F_Code_Zero)
      and then Invalid (Ctx, F_Checksum)
      and then Invalid (Ctx, F_Gateway_Internet_Address)
      and then Invalid (Ctx, F_Identifier)
      and then Invalid (Ctx, F_Pointer)
      and then Invalid (Ctx, F_Unused_32)
      and then Invalid (Ctx, F_Sequence_Number)
      and then Invalid (Ctx, F_Unused_24)
      and then Invalid (Ctx, F_Originate_Timestamp)
      and then Invalid (Ctx, F_Data)
      and then Invalid (Ctx, F_Receive_Timestamp)
      and then Invalid (Ctx, F_Transmit_Timestamp));

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Message_Last);

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Tag =>
                    True,
                 when others =>
                    False),
          when F_Tag =>
             (case Fld is
                 when F_Code_Destination_Unreachable =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable)),
                 when F_Code_Redirect =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect)),
                 when F_Code_Time_Exceeded =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded)),
                 when F_Code_Zero =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)),
                 when others =>
                    False),
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             (case Fld is
                 when F_Checksum =>
                    True,
                 when others =>
                    False),
          when F_Checksum =>
             (case Fld is
                 when F_Gateway_Internet_Address =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect)),
                 when F_Identifier =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)),
                 when F_Pointer =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem)),
                 when F_Unused_32 =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)),
                 when others =>
                    False),
          when F_Gateway_Internet_Address =>
             (case Fld is
                 when F_Data =>
                    True,
                 when others =>
                    False),
          when F_Identifier =>
             (case Fld is
                 when F_Sequence_Number =>
                    True,
                 when others =>
                    False),
          when F_Pointer =>
             (case Fld is
                 when F_Unused_24 =>
                    True,
                 when others =>
                    False),
          when F_Unused_32 =>
             (case Fld is
                 when F_Data =>
                    True,
                 when others =>
                    False),
          when F_Sequence_Number =>
             (case Fld is
                 when F_Data =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)),
                 when F_Originate_Timestamp =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)),
                 when others =>
                    False),
          when F_Unused_24 =>
             (case Fld is
                 when F_Data =>
                    True,
                 when others =>
                    False),
          when F_Originate_Timestamp =>
             (case Fld is
                 when F_Receive_Timestamp =>
                    True,
                 when others =>
                    False),
          when F_Data =>
             False,
          when F_Receive_Timestamp =>
             (case Fld is
                 when F_Transmit_Timestamp =>
                    True,
                 when others =>
                    False),
          when F_Transmit_Timestamp | F_Final =>
             False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
          when F_Initial =>
             True,
          when F_Tag =>
             RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
             or RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
             or RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
             or RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
             or RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
             or RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
             or RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
             or RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
             or RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench))
             or RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
             or RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)),
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             True,
          when F_Checksum =>
             RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)),
          when F_Gateway_Internet_Address | F_Identifier | F_Pointer | F_Unused_32 =>
             True,
          when F_Sequence_Number =>
             RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
             or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)),
          when F_Unused_24 | F_Originate_Timestamp | F_Data | F_Receive_Timestamp | F_Transmit_Timestamp =>
             True,
          when F_Final =>
             False));

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Tag =>
                    RFLX.ICMP.Tag_Base'Size,
                 when others =>
                    raise Program_Error),
          when F_Tag =>
             (case Fld is
                 when F_Code_Destination_Unreachable =>
                    RFLX.ICMP.Code_Destination_Unreachable_Base'Size,
                 when F_Code_Redirect =>
                    RFLX.ICMP.Code_Redirect_Base'Size,
                 when F_Code_Time_Exceeded =>
                    RFLX.ICMP.Code_Time_Exceeded_Base'Size,
                 when F_Code_Zero =>
                    RFLX.ICMP.Code_Zero_Base'Size,
                 when others =>
                    raise Program_Error),
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             (case Fld is
                 when F_Checksum =>
                    RFLX.ICMP.Checksum'Size,
                 when others =>
                    raise Program_Error),
          when F_Checksum =>
             (case Fld is
                 when F_Gateway_Internet_Address =>
                    RFLX.ICMP.Gateway_Internet_Address'Size,
                 when F_Identifier =>
                    RFLX.ICMP.Identifier'Size,
                 when F_Pointer =>
                    RFLX.ICMP.Pointer'Size,
                 when F_Unused_32 =>
                    RFLX.ICMP.Unused_32_Base'Size,
                 when others =>
                    raise Program_Error),
          when F_Gateway_Internet_Address =>
             (case Fld is
                 when F_Data =>
                    224,
                 when others =>
                    raise Program_Error),
          when F_Identifier =>
             (case Fld is
                 when F_Sequence_Number =>
                    RFLX.ICMP.Sequence_Number'Size,
                 when others =>
                    raise Program_Error),
          when F_Pointer =>
             (case Fld is
                 when F_Unused_24 =>
                    RFLX.ICMP.Unused_24_Base'Size,
                 when others =>
                    raise Program_Error),
          when F_Unused_32 =>
             (case Fld is
                 when F_Data =>
                    224,
                 when others =>
                    raise Program_Error),
          when F_Sequence_Number =>
             (case Fld is
                 when F_Data =>
                    RFLX_Types.Bit_Length (Ctx.Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number).Last),
                 when F_Originate_Timestamp =>
                    RFLX.ICMP.Timestamp'Size,
                 when others =>
                    raise Program_Error),
          when F_Unused_24 =>
             (case Fld is
                 when F_Data =>
                    224,
                 when others =>
                    raise Program_Error),
          when F_Originate_Timestamp =>
             (case Fld is
                 when F_Receive_Timestamp =>
                    RFLX.ICMP.Timestamp'Size,
                 when others =>
                    raise Program_Error),
          when F_Data =>
             0,
          when F_Receive_Timestamp =>
             (case Fld is
                 when F_Transmit_Timestamp =>
                    RFLX.ICMP.Timestamp'Size,
                 when others =>
                    raise Program_Error),
          when F_Transmit_Timestamp | F_Final =>
             0));

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index is
     ((case Fld is
          when F_Tag =>
             Ctx.First,
          when F_Code_Destination_Unreachable =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Tag
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Code_Redirect =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Tag
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Code_Time_Exceeded =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Tag
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Code_Zero =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Tag
                 and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Checksum =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Code_Destination_Unreachable
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Code_Redirect
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Code_Time_Exceeded
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Code_Zero
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Gateway_Internet_Address =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Checksum
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Identifier =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Checksum
                 and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Pointer =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Checksum
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Unused_32 =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Checksum
                 and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Sequence_Number =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Identifier
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Unused_24 =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Pointer
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Originate_Timestamp =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Sequence_Number
                 and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Data =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Gateway_Internet_Address
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Sequence_Number
                 and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                           or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Unused_24
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Unused_32
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Receive_Timestamp =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Originate_Timestamp
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Transmit_Timestamp =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Receive_Timestamp
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
          when F_Tag =>
             Ctx.Cursors (Fld).Predecessor = F_Initial,
          when F_Code_Destination_Unreachable | F_Code_Redirect | F_Code_Time_Exceeded | F_Code_Zero =>
             (Valid (Ctx.Cursors (F_Tag))
              and Ctx.Cursors (Fld).Predecessor = F_Tag),
          when F_Checksum =>
             (Valid (Ctx.Cursors (F_Code_Destination_Unreachable))
              and Ctx.Cursors (Fld).Predecessor = F_Code_Destination_Unreachable)
             or (Valid (Ctx.Cursors (F_Code_Redirect))
                 and Ctx.Cursors (Fld).Predecessor = F_Code_Redirect)
             or (Valid (Ctx.Cursors (F_Code_Time_Exceeded))
                 and Ctx.Cursors (Fld).Predecessor = F_Code_Time_Exceeded)
             or (Valid (Ctx.Cursors (F_Code_Zero))
                 and Ctx.Cursors (Fld).Predecessor = F_Code_Zero),
          when F_Gateway_Internet_Address | F_Identifier | F_Pointer | F_Unused_32 =>
             (Valid (Ctx.Cursors (F_Checksum))
              and Ctx.Cursors (Fld).Predecessor = F_Checksum),
          when F_Sequence_Number =>
             (Valid (Ctx.Cursors (F_Identifier))
              and Ctx.Cursors (Fld).Predecessor = F_Identifier),
          when F_Unused_24 =>
             (Valid (Ctx.Cursors (F_Pointer))
              and Ctx.Cursors (Fld).Predecessor = F_Pointer),
          when F_Originate_Timestamp =>
             (Valid (Ctx.Cursors (F_Sequence_Number))
              and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number),
          when F_Data =>
             (Valid (Ctx.Cursors (F_Gateway_Internet_Address))
              and Ctx.Cursors (Fld).Predecessor = F_Gateway_Internet_Address)
             or (Valid (Ctx.Cursors (F_Sequence_Number))
                 and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number)
             or (Valid (Ctx.Cursors (F_Unused_24))
                 and Ctx.Cursors (Fld).Predecessor = F_Unused_24)
             or (Valid (Ctx.Cursors (F_Unused_32))
                 and Ctx.Cursors (Fld).Predecessor = F_Unused_32),
          when F_Receive_Timestamp =>
             (Valid (Ctx.Cursors (F_Originate_Timestamp))
              and Ctx.Cursors (Fld).Predecessor = F_Originate_Timestamp),
          when F_Transmit_Timestamp =>
             (Valid (Ctx.Cursors (F_Receive_Timestamp))
              and Ctx.Cursors (Fld).Predecessor = F_Receive_Timestamp),
          when F_Final =>
             (Structural_Valid (Ctx.Cursors (F_Data))
              and Ctx.Cursors (Fld).Predecessor = F_Data)
             or (Valid (Ctx.Cursors (F_Sequence_Number))
                 and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number)
             or (Valid (Ctx.Cursors (F_Transmit_Timestamp))
                 and Ctx.Cursors (Fld).Predecessor = F_Transmit_Timestamp)));

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
     (Valid (Ctx, F_Tag)
      and then ((Valid (Ctx, F_Code_Destination_Unreachable)
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                 and then Valid (Ctx, F_Checksum)
                 and then ((Valid (Ctx, F_Gateway_Internet_Address)
                            and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                            and then Structural_Valid (Ctx, F_Data))
                           or (Valid (Ctx, F_Identifier)
                               and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                               and then Valid (Ctx, F_Sequence_Number)
                               and then ((Structural_Valid (Ctx, F_Data)
                                          and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))))
                                         or (Valid (Ctx, F_Originate_Timestamp)
                                             and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                       or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                             and then Valid (Ctx, F_Receive_Timestamp)
                                             and then Valid (Ctx, F_Transmit_Timestamp))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))))
                           or (Valid (Ctx, F_Pointer)
                               and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                               and then Valid (Ctx, F_Unused_24)
                               and then Structural_Valid (Ctx, F_Data))
                           or (Valid (Ctx, F_Unused_32)
                               and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                               and then Structural_Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Redirect)
                    and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                               and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Structural_Valid (Ctx, F_Data)
                                             and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                       or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                          or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                                  and then Structural_Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Time_Exceeded)
                    and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                               and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Structural_Valid (Ctx, F_Data)
                                             and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                       or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                          or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                                  and then Structural_Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Zero)
                    and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                               and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Structural_Valid (Ctx, F_Data)
                                             and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                       or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                          or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Structural_Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                                  and then Structural_Valid (Ctx, F_Data))))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Tag)
      and then ((Valid (Ctx, F_Code_Destination_Unreachable)
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                 and then Valid (Ctx, F_Checksum)
                 and then ((Valid (Ctx, F_Gateway_Internet_Address)
                            and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                            and then Valid (Ctx, F_Data))
                           or (Valid (Ctx, F_Identifier)
                               and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                               and then Valid (Ctx, F_Sequence_Number)
                               and then ((Valid (Ctx, F_Data)
                                          and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                    or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))))
                                         or (Valid (Ctx, F_Originate_Timestamp)
                                             and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                       or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                             and then Valid (Ctx, F_Receive_Timestamp)
                                             and then Valid (Ctx, F_Transmit_Timestamp))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))))
                           or (Valid (Ctx, F_Pointer)
                               and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                               and then Valid (Ctx, F_Unused_24)
                               and then Valid (Ctx, F_Data))
                           or (Valid (Ctx, F_Unused_32)
                               and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                         or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                               and then Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Redirect)
                    and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                               and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Valid (Ctx, F_Data)
                                             and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                       or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                          or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                                  and then Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Time_Exceeded)
                    and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                               and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Valid (Ctx, F_Data)
                                             and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                       or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                          or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                                  and then Valid (Ctx, F_Data))))
                or (Valid (Ctx, F_Code_Zero)
                    and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                              or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request)))
                    and then Valid (Ctx, F_Checksum)
                    and then ((Valid (Ctx, F_Gateway_Internet_Address)
                               and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Redirect))
                               and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Identifier)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply)))
                                  and then Valid (Ctx, F_Sequence_Number)
                                  and then ((Valid (Ctx, F_Data)
                                             and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Reply))
                                                       or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Echo_Request))))
                                            or (Valid (Ctx, F_Originate_Timestamp)
                                                and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Msg))
                                                          or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Timestamp_Reply)))
                                                and then Valid (Ctx, F_Receive_Timestamp)
                                                and then Valid (Ctx, F_Transmit_Timestamp))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Request))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Information_Reply))))
                              or (Valid (Ctx, F_Pointer)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Parameter_Problem))
                                  and then Valid (Ctx, F_Unused_24)
                                  and then Valid (Ctx, F_Data))
                              or (Valid (Ctx, F_Unused_32)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Time_Exceeded))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Destination_Unreachable))
                                            or RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (Source_Quench)))
                                  and then Valid (Ctx, F_Data))))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Tag)
      or Incomplete (Ctx, F_Code_Destination_Unreachable)
      or Incomplete (Ctx, F_Code_Redirect)
      or Incomplete (Ctx, F_Code_Time_Exceeded)
      or Incomplete (Ctx, F_Code_Zero)
      or Incomplete (Ctx, F_Checksum)
      or Incomplete (Ctx, F_Gateway_Internet_Address)
      or Incomplete (Ctx, F_Identifier)
      or Incomplete (Ctx, F_Pointer)
      or Incomplete (Ctx, F_Unused_32)
      or Incomplete (Ctx, F_Sequence_Number)
      or Incomplete (Ctx, F_Unused_24)
      or Incomplete (Ctx, F_Originate_Timestamp)
      or Incomplete (Ctx, F_Data)
      or Incomplete (Ctx, F_Receive_Timestamp)
      or Incomplete (Ctx, F_Transmit_Timestamp));

   function Get_Tag (Ctx : Context) return RFLX.ICMP.Tag is
     (To_Actual (Ctx.Cursors (F_Tag).Value.Tag_Value));

   function Get_Code_Destination_Unreachable (Ctx : Context) return RFLX.ICMP.Code_Destination_Unreachable is
     (To_Actual (Ctx.Cursors (F_Code_Destination_Unreachable).Value.Code_Destination_Unreachable_Value));

   function Get_Code_Redirect (Ctx : Context) return RFLX.ICMP.Code_Redirect is
     (To_Actual (Ctx.Cursors (F_Code_Redirect).Value.Code_Redirect_Value));

   function Get_Code_Time_Exceeded (Ctx : Context) return RFLX.ICMP.Code_Time_Exceeded is
     (To_Actual (Ctx.Cursors (F_Code_Time_Exceeded).Value.Code_Time_Exceeded_Value));

   function Get_Code_Zero (Ctx : Context) return RFLX.ICMP.Code_Zero is
     (To_Actual (Ctx.Cursors (F_Code_Zero).Value.Code_Zero_Value));

   function Get_Checksum (Ctx : Context) return RFLX.ICMP.Checksum is
     (To_Actual (Ctx.Cursors (F_Checksum).Value.Checksum_Value));

   function Get_Gateway_Internet_Address (Ctx : Context) return RFLX.ICMP.Gateway_Internet_Address is
     (To_Actual (Ctx.Cursors (F_Gateway_Internet_Address).Value.Gateway_Internet_Address_Value));

   function Get_Identifier (Ctx : Context) return RFLX.ICMP.Identifier is
     (To_Actual (Ctx.Cursors (F_Identifier).Value.Identifier_Value));

   function Get_Pointer (Ctx : Context) return RFLX.ICMP.Pointer is
     (To_Actual (Ctx.Cursors (F_Pointer).Value.Pointer_Value));

   function Get_Unused_32 (Ctx : Context) return RFLX.ICMP.Unused_32 is
     (To_Actual (Ctx.Cursors (F_Unused_32).Value.Unused_32_Value));

   function Get_Sequence_Number (Ctx : Context) return RFLX.ICMP.Sequence_Number is
     (To_Actual (Ctx.Cursors (F_Sequence_Number).Value.Sequence_Number_Value));

   function Get_Unused_24 (Ctx : Context) return RFLX.ICMP.Unused_24 is
     (To_Actual (Ctx.Cursors (F_Unused_24).Value.Unused_24_Value));

   function Get_Originate_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp is
     (To_Actual (Ctx.Cursors (F_Originate_Timestamp).Value.Originate_Timestamp_Value));

   function Get_Receive_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp is
     (To_Actual (Ctx.Cursors (F_Receive_Timestamp).Value.Receive_Timestamp_Value));

   function Get_Transmit_Timestamp (Ctx : Context) return RFLX.ICMP.Timestamp is
     (To_Actual (Ctx.Cursors (F_Transmit_Timestamp).Value.Transmit_Timestamp_Value));

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.ICMP.Message;

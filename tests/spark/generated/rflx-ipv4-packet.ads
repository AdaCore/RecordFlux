pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Builtin_Types.Conversions;
use RFLX.RFLX_Builtin_Types.Conversions;
with RFLX.IPv4.Options;

package RFLX.IPv4.Packet with
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

   type Virtual_Field is (F_Initial, F_Version, F_IHL, F_DSCP, F_ECN, F_Total_Length, F_Identification, F_Flag_R, F_Flag_DF, F_Flag_MF, F_Fragment_Offset, F_TTL, F_Protocol, F_Header_Checksum, F_Source, F_Destination, F_Options, F_Payload, F_Final);

   subtype Field is Virtual_Field range F_Version .. F_Payload;

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
       and RFLX.IPv4.Packet.Has_Buffer (Ctx),
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
       and RFLX.IPv4.Packet.Has_Buffer (Ctx)
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
       RFLX.IPv4.Packet.Has_Buffer (Ctx),
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
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Well_Formed_Message (Ctx)
       and then RFLX.IPv4.Packet.Byte_Size (Ctx) = Buffer'Length;

   function Read (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Well_Formed_Message (Ctx);

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
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Well_Formed_Message (Ctx)
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
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then Offset < RFLX.IPv4.Packet.Buffer_Length (Ctx)
       and then Pre (RFLX.IPv4.Packet.Buffer_Length (Ctx), Offset),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
       and Initialized (Ctx);

   function Has_Buffer (Ctx : Context) return Boolean;

   function Buffer_Length (Ctx : Context) return RFLX_Types.Length with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx);

   function Size (Ctx : Context) return RFLX_Types.Bit_Length with
     Post =>
       Size'Result rem RFLX_Types.Byte'Size = 0;

   function Byte_Size (Ctx : Context) return RFLX_Types.Length;

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Well_Formed_Message (Ctx);

   function Written_Last (Ctx : Context) return RFLX_Types.Bit_Length;

   procedure Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Well_Formed_Message (Ctx)
       and then Data'Length = RFLX.IPv4.Packet.Byte_Size (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Value (Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       RFLX.IPv4.Packet.Valid_Predecessor (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_Condition (Ctx : Context; Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Predecessor (Ctx, Fld)
       and then RFLX.IPv4.Packet.Valid_Value (Fld, Val)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, Fld)
       and then RFLX.IPv4.Packet.Sufficient_Space (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.IPv4.Packet.Valid_Next (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Options | F_Payload =>
              Field_Size'Result rem RFLX_Types.Byte'Size = 0,
           when others =>
              True);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       RFLX.IPv4.Packet.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.IPv4.Packet.Valid_Next (Ctx, Fld)
       and then RFLX.IPv4.Packet.Sufficient_Space (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Options | F_Payload =>
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
       RFLX.IPv4.Packet.Valid_Next (Ctx, Fld);

   function Sufficient_Space (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       RFLX.IPv4.Packet.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and RFLX.IPv4.Packet.Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx),
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
       RFLX.IPv4.Packet.Has_Buffer (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Incomplete_Message (Ctx : Context) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "precondition is always False");

   function Get_Version (Ctx : Context) return RFLX.IPv4.Version with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Version);

   function Get_IHL (Ctx : Context) return RFLX.IPv4.IHL with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_IHL);

   function Get_DSCP (Ctx : Context) return RFLX.IPv4.DCSP with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_DSCP);

   function Get_ECN (Ctx : Context) return RFLX.IPv4.ECN with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_ECN);

   function Get_Total_Length (Ctx : Context) return RFLX.IPv4.Total_Length with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Total_Length);

   function Get_Identification (Ctx : Context) return RFLX.IPv4.Identification with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Identification);

   function Get_Flag_R (Ctx : Context) return Boolean with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Flag_R);

   function Get_Flag_DF (Ctx : Context) return Boolean with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Flag_DF);

   function Get_Flag_MF (Ctx : Context) return Boolean with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Flag_MF);

   function Get_Fragment_Offset (Ctx : Context) return RFLX.IPv4.Fragment_Offset with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Fragment_Offset);

   function Get_TTL (Ctx : Context) return RFLX.IPv4.TTL with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_TTL);

   function Get_Protocol (Ctx : Context) return RFLX.IPv4.Protocol with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Protocol);

   function Get_Header_Checksum (Ctx : Context) return RFLX.IPv4.Header_Checksum with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Header_Checksum);

   function Get_Source (Ctx : Context) return RFLX.IPv4.Address with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Source);

   function Get_Destination (Ctx : Context) return RFLX.IPv4.Address with
     Pre =>
       RFLX.IPv4.Packet.Valid (Ctx, RFLX.IPv4.Packet.F_Destination);

   pragma Warnings (On, "precondition is always False");

   function Get_Payload (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Well_Formed (Ctx, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Payload),
     Post =>
       Get_Payload'Result'Length = RFLX_Types.To_Length (Field_Size (Ctx, F_Payload));

   procedure Get_Payload (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Well_Formed (Ctx, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Payload)
       and then Data'Length = RFLX_Types.To_Length (RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Payload)),
     Post =>
       Equal (Ctx, F_Payload, Data);

   generic
      with procedure Process_Payload (Payload : RFLX_Types.Bytes);
   procedure Generic_Get_Payload (Ctx : Context) with
     Pre =>
       RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and RFLX.IPv4.Packet.Present (Ctx, RFLX.IPv4.Packet.F_Payload);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean with
     Pre =>
       RFLX.IPv4.Packet.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Set_Version (Ctx : in out Context; Val : RFLX.IPv4.Version) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Version)
       and then RFLX.IPv4.Valid_Version (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Version) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Version)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Version, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Version)
       and Invalid (Ctx, F_IHL)
       and Invalid (Ctx, F_DSCP)
       and Invalid (Ctx, F_ECN)
       and Invalid (Ctx, F_Total_Length)
       and Invalid (Ctx, F_Identification)
       and Invalid (Ctx, F_Flag_R)
       and Invalid (Ctx, F_Flag_DF)
       and Invalid (Ctx, F_Flag_MF)
       and Invalid (Ctx, F_Fragment_Offset)
       and Invalid (Ctx, F_TTL)
       and Invalid (Ctx, F_Protocol)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_IHL) = F_Version
            and Valid_Next (Ctx, F_IHL))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Version) = Predecessor (Ctx, F_Version)'Old
       and Valid_Next (Ctx, F_Version) = Valid_Next (Ctx, F_Version)'Old
       and Field_First (Ctx, F_Version) = Field_First (Ctx, F_Version)'Old;

   procedure Set_IHL (Ctx : in out Context; Val : RFLX.IPv4.IHL) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_IHL)
       and then RFLX.IPv4.Valid_IHL (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_IHL) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_IHL)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_IHL, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_IHL)
       and Get_IHL (Ctx) = Val
       and Invalid (Ctx, F_DSCP)
       and Invalid (Ctx, F_ECN)
       and Invalid (Ctx, F_Total_Length)
       and Invalid (Ctx, F_Identification)
       and Invalid (Ctx, F_Flag_R)
       and Invalid (Ctx, F_Flag_DF)
       and Invalid (Ctx, F_Flag_MF)
       and Invalid (Ctx, F_Fragment_Offset)
       and Invalid (Ctx, F_TTL)
       and Invalid (Ctx, F_Protocol)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_DSCP) = F_IHL
            and Valid_Next (Ctx, F_DSCP))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_IHL) = Predecessor (Ctx, F_IHL)'Old
       and Valid_Next (Ctx, F_IHL) = Valid_Next (Ctx, F_IHL)'Old
       and Field_First (Ctx, F_IHL) = Field_First (Ctx, F_IHL)'Old
       and (for all F in Field range F_Version .. F_Version =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_DSCP (Ctx : in out Context; Val : RFLX.IPv4.DCSP) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_DSCP)
       and then RFLX.IPv4.Valid_DCSP (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_DSCP) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_DSCP)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_DSCP, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_DSCP)
       and Get_DSCP (Ctx) = Val
       and Invalid (Ctx, F_ECN)
       and Invalid (Ctx, F_Total_Length)
       and Invalid (Ctx, F_Identification)
       and Invalid (Ctx, F_Flag_R)
       and Invalid (Ctx, F_Flag_DF)
       and Invalid (Ctx, F_Flag_MF)
       and Invalid (Ctx, F_Fragment_Offset)
       and Invalid (Ctx, F_TTL)
       and Invalid (Ctx, F_Protocol)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_ECN) = F_DSCP
            and Valid_Next (Ctx, F_ECN))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_DSCP) = Predecessor (Ctx, F_DSCP)'Old
       and Valid_Next (Ctx, F_DSCP) = Valid_Next (Ctx, F_DSCP)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Field_First (Ctx, F_DSCP) = Field_First (Ctx, F_DSCP)'Old
       and (for all F in Field range F_Version .. F_IHL =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_ECN (Ctx : in out Context; Val : RFLX.IPv4.ECN) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_ECN)
       and then RFLX.IPv4.Valid_ECN (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_ECN) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_ECN)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_ECN, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_ECN)
       and Get_ECN (Ctx) = Val
       and Invalid (Ctx, F_Total_Length)
       and Invalid (Ctx, F_Identification)
       and Invalid (Ctx, F_Flag_R)
       and Invalid (Ctx, F_Flag_DF)
       and Invalid (Ctx, F_Flag_MF)
       and Invalid (Ctx, F_Fragment_Offset)
       and Invalid (Ctx, F_TTL)
       and Invalid (Ctx, F_Protocol)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Total_Length) = F_ECN
            and Valid_Next (Ctx, F_Total_Length))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_ECN) = Predecessor (Ctx, F_ECN)'Old
       and Valid_Next (Ctx, F_ECN) = Valid_Next (Ctx, F_ECN)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Field_First (Ctx, F_ECN) = Field_First (Ctx, F_ECN)'Old
       and (for all F in Field range F_Version .. F_DSCP =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Total_Length (Ctx : in out Context; Val : RFLX.IPv4.Total_Length) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Total_Length)
       and then RFLX.IPv4.Valid_Total_Length (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Total_Length) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Total_Length)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Total_Length, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Total_Length)
       and Get_Total_Length (Ctx) = Val
       and Invalid (Ctx, F_Identification)
       and Invalid (Ctx, F_Flag_R)
       and Invalid (Ctx, F_Flag_DF)
       and Invalid (Ctx, F_Flag_MF)
       and Invalid (Ctx, F_Fragment_Offset)
       and Invalid (Ctx, F_TTL)
       and Invalid (Ctx, F_Protocol)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (if
               RFLX_Types.Base_Integer (Get_Total_Length (Ctx)) >= RFLX_Types.Base_Integer (Get_IHL (Ctx)) * 4
            then
               Predecessor (Ctx, F_Identification) = F_Total_Length
               and Valid_Next (Ctx, F_Identification))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Total_Length) = Predecessor (Ctx, F_Total_Length)'Old
       and Valid_Next (Ctx, F_Total_Length) = Valid_Next (Ctx, F_Total_Length)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Field_First (Ctx, F_Total_Length) = Field_First (Ctx, F_Total_Length)'Old
       and (for all F in Field range F_Version .. F_ECN =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Identification (Ctx : in out Context; Val : RFLX.IPv4.Identification) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Identification)
       and then RFLX.IPv4.Valid_Identification (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Identification) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Identification)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Identification, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Identification)
       and Get_Identification (Ctx) = Val
       and Invalid (Ctx, F_Flag_R)
       and Invalid (Ctx, F_Flag_DF)
       and Invalid (Ctx, F_Flag_MF)
       and Invalid (Ctx, F_Fragment_Offset)
       and Invalid (Ctx, F_TTL)
       and Invalid (Ctx, F_Protocol)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Flag_R) = F_Identification
            and Valid_Next (Ctx, F_Flag_R))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Identification) = Predecessor (Ctx, F_Identification)'Old
       and Valid_Next (Ctx, F_Identification) = Valid_Next (Ctx, F_Identification)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Field_First (Ctx, F_Identification) = Field_First (Ctx, F_Identification)'Old
       and (for all F in Field range F_Version .. F_Total_Length =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Flag_R (Ctx : in out Context; Val : Boolean) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Flag_R)
       and then Valid_Boolean (To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Flag_R) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Flag_R)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Flag_R, To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Flag_R)
       and Get_Flag_R (Ctx) = Val
       and Invalid (Ctx, F_Flag_DF)
       and Invalid (Ctx, F_Flag_MF)
       and Invalid (Ctx, F_Fragment_Offset)
       and Invalid (Ctx, F_TTL)
       and Invalid (Ctx, F_Protocol)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Flag_R (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (False))
            then
               Predecessor (Ctx, F_Flag_DF) = F_Flag_R
               and Valid_Next (Ctx, F_Flag_DF))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Flag_R) = Predecessor (Ctx, F_Flag_R)'Old
       and Valid_Next (Ctx, F_Flag_R) = Valid_Next (Ctx, F_Flag_R)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Field_First (Ctx, F_Flag_R) = Field_First (Ctx, F_Flag_R)'Old
       and (for all F in Field range F_Version .. F_Identification =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Flag_DF (Ctx : in out Context; Val : Boolean) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Flag_DF)
       and then Valid_Boolean (To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Flag_DF) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Flag_DF)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Flag_DF, To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Flag_DF)
       and Get_Flag_DF (Ctx) = Val
       and Invalid (Ctx, F_Flag_MF)
       and Invalid (Ctx, F_Fragment_Offset)
       and Invalid (Ctx, F_TTL)
       and Invalid (Ctx, F_Protocol)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Flag_MF) = F_Flag_DF
            and Valid_Next (Ctx, F_Flag_MF))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Flag_DF) = Predecessor (Ctx, F_Flag_DF)'Old
       and Valid_Next (Ctx, F_Flag_DF) = Valid_Next (Ctx, F_Flag_DF)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Field_First (Ctx, F_Flag_DF) = Field_First (Ctx, F_Flag_DF)'Old
       and (for all F in Field range F_Version .. F_Flag_R =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Flag_MF (Ctx : in out Context; Val : Boolean) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Flag_MF)
       and then Valid_Boolean (To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Flag_MF) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Flag_MF)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Flag_MF, To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Flag_MF)
       and Get_Flag_MF (Ctx) = Val
       and Invalid (Ctx, F_Fragment_Offset)
       and Invalid (Ctx, F_TTL)
       and Invalid (Ctx, F_Protocol)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Fragment_Offset) = F_Flag_MF
            and Valid_Next (Ctx, F_Fragment_Offset))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Flag_MF) = Predecessor (Ctx, F_Flag_MF)'Old
       and Valid_Next (Ctx, F_Flag_MF) = Valid_Next (Ctx, F_Flag_MF)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Field_First (Ctx, F_Flag_MF) = Field_First (Ctx, F_Flag_MF)'Old
       and (for all F in Field range F_Version .. F_Flag_DF =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Fragment_Offset (Ctx : in out Context; Val : RFLX.IPv4.Fragment_Offset) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Fragment_Offset)
       and then RFLX.IPv4.Valid_Fragment_Offset (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Fragment_Offset) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Fragment_Offset)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Fragment_Offset, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Fragment_Offset)
       and Get_Fragment_Offset (Ctx) = Val
       and Invalid (Ctx, F_TTL)
       and Invalid (Ctx, F_Protocol)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_TTL) = F_Fragment_Offset
            and Valid_Next (Ctx, F_TTL))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Fragment_Offset) = Predecessor (Ctx, F_Fragment_Offset)'Old
       and Valid_Next (Ctx, F_Fragment_Offset) = Valid_Next (Ctx, F_Fragment_Offset)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Field_First (Ctx, F_Fragment_Offset) = Field_First (Ctx, F_Fragment_Offset)'Old
       and (for all F in Field range F_Version .. F_Flag_MF =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_TTL (Ctx : in out Context; Val : RFLX.IPv4.TTL) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_TTL)
       and then RFLX.IPv4.Valid_TTL (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_TTL) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_TTL)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_TTL, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_TTL)
       and Get_TTL (Ctx) = Val
       and Invalid (Ctx, F_Protocol)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Protocol) = F_TTL
            and Valid_Next (Ctx, F_Protocol))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_TTL) = Predecessor (Ctx, F_TTL)'Old
       and Valid_Next (Ctx, F_TTL) = Valid_Next (Ctx, F_TTL)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Field_First (Ctx, F_TTL) = Field_First (Ctx, F_TTL)'Old
       and (for all F in Field range F_Version .. F_Fragment_Offset =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Protocol (Ctx : in out Context; Val : RFLX.IPv4.Protocol_Enum) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Protocol)
       and then RFLX.IPv4.Valid_Protocol (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Protocol) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Protocol)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Protocol, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Protocol)
       and Get_Protocol (Ctx) = (True, Val)
       and Invalid (Ctx, F_Header_Checksum)
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Header_Checksum) = F_Protocol
            and Valid_Next (Ctx, F_Header_Checksum))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Protocol) = Predecessor (Ctx, F_Protocol)'Old
       and Valid_Next (Ctx, F_Protocol) = Valid_Next (Ctx, F_Protocol)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Field_First (Ctx, F_Protocol) = Field_First (Ctx, F_Protocol)'Old
       and (for all F in Field range F_Version .. F_TTL =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Header_Checksum (Ctx : in out Context; Val : RFLX.IPv4.Header_Checksum) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Header_Checksum)
       and then RFLX.IPv4.Valid_Header_Checksum (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Header_Checksum) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Header_Checksum)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Header_Checksum, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Header_Checksum)
       and Get_Header_Checksum (Ctx) = Val
       and Invalid (Ctx, F_Source)
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Source) = F_Header_Checksum
            and Valid_Next (Ctx, F_Source))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Header_Checksum) = Predecessor (Ctx, F_Header_Checksum)'Old
       and Valid_Next (Ctx, F_Header_Checksum) = Valid_Next (Ctx, F_Header_Checksum)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Field_First (Ctx, F_Header_Checksum) = Field_First (Ctx, F_Header_Checksum)'Old
       and (for all F in Field range F_Version .. F_Protocol =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Source (Ctx : in out Context; Val : RFLX.IPv4.Address) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Source)
       and then RFLX.IPv4.Valid_Address (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Source) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Source)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Source, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Source)
       and Get_Source (Ctx) = Val
       and Invalid (Ctx, F_Destination)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Destination) = F_Source
            and Valid_Next (Ctx, F_Destination))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Source) = Predecessor (Ctx, F_Source)'Old
       and Valid_Next (Ctx, F_Source) = Valid_Next (Ctx, F_Source)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Get_Header_Checksum (Ctx) = Get_Header_Checksum (Ctx)'Old
       and Field_First (Ctx, F_Source) = Field_First (Ctx, F_Source)'Old
       and (for all F in Field range F_Version .. F_Header_Checksum =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Destination (Ctx : in out Context; Val : RFLX.IPv4.Address) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Destination)
       and then RFLX.IPv4.Valid_Address (RFLX.IPv4.To_Base_Integer (Val))
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Destination) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Destination)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Destination, RFLX.IPv4.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Destination)
       and Get_Destination (Ctx) = Val
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Options) = F_Destination
            and Valid_Next (Ctx, F_Options))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Destination) = Predecessor (Ctx, F_Destination)'Old
       and Valid_Next (Ctx, F_Destination) = Valid_Next (Ctx, F_Destination)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Get_Header_Checksum (Ctx) = Get_Header_Checksum (Ctx)'Old
       and Get_Source (Ctx) = Get_Source (Ctx)'Old
       and Field_First (Ctx, F_Destination) = Field_First (Ctx, F_Destination)'Old
       and (for all F in Field range F_Version .. F_Source =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   pragma Warnings (On, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Set_Options_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Options)
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Options) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Options)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Options, 0)
       and then RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Options) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Payload) = F_Options
            and Valid_Next (Ctx, F_Payload))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Get_Header_Checksum (Ctx) = Get_Header_Checksum (Ctx)'Old
       and Get_Source (Ctx) = Get_Source (Ctx)'Old
       and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
       and Field_First (Ctx, F_Options) = Field_First (Ctx, F_Options)'Old;

   procedure Set_Payload_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Payload) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Payload, 0)
       and then RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Payload) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Payload)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Payload))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Get_Header_Checksum (Ctx) = Get_Header_Checksum (Ctx)'Old
       and Get_Source (Ctx) = Get_Source (Ctx)'Old
       and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
       and Field_First (Ctx, F_Payload) = Field_First (Ctx, F_Payload)'Old;

   procedure Set_Options (Ctx : in out Context; Seq_Ctx : RFLX.IPv4.Options.Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Options)
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Options) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Options)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Options, 0)
       and then RFLX.IPv4.Packet.Valid_Length (Ctx, RFLX.IPv4.Packet.F_Options, RFLX.IPv4.Options.Byte_Size (Seq_Ctx))
       and then RFLX.IPv4.Options.Has_Buffer (Seq_Ctx)
       and then RFLX.IPv4.Options.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Payload) = F_Options
            and Valid_Next (Ctx, F_Payload))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Get_Header_Checksum (Ctx) = Get_Header_Checksum (Ctx)'Old
       and Get_Source (Ctx) = Get_Source (Ctx)'Old
       and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
       and Field_First (Ctx, F_Options) = Field_First (Ctx, F_Options)'Old
       and (if Field_Size (Ctx, F_Options) > 0 then Present (Ctx, F_Options));

   procedure Initialize_Options (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Options)
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Options) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Options),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Payload) = F_Options
            and Valid_Next (Ctx, F_Payload))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Get_Header_Checksum (Ctx) = Get_Header_Checksum (Ctx)'Old
       and Get_Source (Ctx) = Get_Source (Ctx)'Old
       and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
       and Field_First (Ctx, F_Options) = Field_First (Ctx, F_Options)'Old;

   procedure Initialize_Payload (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Payload) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Payload),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Payload)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Payload))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Get_Header_Checksum (Ctx) = Get_Header_Checksum (Ctx)'Old
       and Get_Source (Ctx) = Get_Source (Ctx)'Old
       and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
       and Field_First (Ctx, F_Payload) = Field_First (Ctx, F_Payload)'Old;

   procedure Set_Payload (Ctx : in out Context; Data : RFLX_Types.Bytes) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Payload) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Valid_Length (Ctx, RFLX.IPv4.Packet.F_Payload, Data'Length)
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Payload) >= Data'Length * RFLX_Types.Byte'Size
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Payload, 0),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Payload)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Payload))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Get_Header_Checksum (Ctx) = Get_Header_Checksum (Ctx)'Old
       and Get_Source (Ctx) = Get_Source (Ctx)'Old
       and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
       and Field_First (Ctx, F_Payload) = Field_First (Ctx, F_Payload)'Old
       and Equal (Ctx, F_Payload, Data);

   generic
      with procedure Process_Payload (Payload : out RFLX_Types.Bytes);
      with function Process_Data_Pre (Length : RFLX_Types.Length) return Boolean;
   procedure Generic_Set_Payload (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Payload) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Payload)
       and then RFLX.IPv4.Packet.Valid_Length (Ctx, RFLX.IPv4.Packet.F_Payload, Length)
       and then RFLX_Types.To_Length (RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Payload)) >= Length
       and then Process_Data_Pre (Length),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Payload)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Payload))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Get_TTL (Ctx) = Get_TTL (Ctx)'Old
       and Get_Protocol (Ctx) = Get_Protocol (Ctx)'Old
       and Get_Header_Checksum (Ctx) = Get_Header_Checksum (Ctx)'Old
       and Get_Source (Ctx) = Get_Source (Ctx)'Old
       and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
       and Field_First (Ctx, F_Payload) = Field_First (Ctx, F_Payload)'Old;

   procedure Switch_To_Options (Ctx : in out Context; Seq_Ctx : out RFLX.IPv4.Options.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Options)
       and then RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Options) > 0
       and then RFLX.IPv4.Packet.Field_First (Ctx, RFLX.IPv4.Packet.F_Options) rem RFLX_Types.Byte'Size = 1
       and then RFLX.IPv4.Packet.Available_Space (Ctx, RFLX.IPv4.Packet.F_Options) >= RFLX.IPv4.Packet.Field_Size (Ctx, RFLX.IPv4.Packet.F_Options)
       and then RFLX.IPv4.Packet.Field_Condition (Ctx, RFLX.IPv4.Packet.F_Options, 0),
     Post =>
       not RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and RFLX.IPv4.Options.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_Options)
       and Seq_Ctx.Last = Field_Last (Ctx, F_Options)
       and RFLX.IPv4.Options.Valid (Seq_Ctx)
       and RFLX.IPv4.Options.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_Options)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Path_Condition (Ctx, F_Options) = Path_Condition (Ctx, F_Options)'Old
       and Field_Last (Ctx, F_Options) = Field_Last (Ctx, F_Options)'Old
       and (for all F in Field range F_Version .. F_Destination =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F)),
     Contract_Cases =>
       (Well_Formed (Ctx, F_Options) =>
           (for all F in Field range F_Payload .. F_Payload =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F)),
        others =>
           (Predecessor (Ctx, F_Payload) = F_Options
            and Valid_Next (Ctx, F_Payload))
           and Invalid (Ctx, F_Payload));

   function Complete_Options (Ctx : Context; Seq_Ctx : RFLX.IPv4.Options.Context) return Boolean with
     Pre =>
       RFLX.IPv4.Packet.Valid_Next (Ctx, RFLX.IPv4.Packet.F_Options);

   procedure Update_Options (Ctx : in out Context; Seq_Ctx : in out RFLX.IPv4.Options.Context) with
     Pre =>
       RFLX.IPv4.Packet.Present (Ctx, RFLX.IPv4.Packet.F_Options)
       and then not RFLX.IPv4.Packet.Has_Buffer (Ctx)
       and then RFLX.IPv4.Options.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_Options)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_Options),
     Post =>
       (if
           RFLX.IPv4.Packet.Complete_Options (Ctx, Seq_Ctx)
        then
           Present (Ctx, F_Options)
           and Context_Cursor (Ctx, F_Payload) = Context_Cursor (Ctx, F_Payload)'Old
        else
           Invalid (Ctx, F_Options)
           and Invalid (Ctx, F_Payload))
       and Has_Buffer (Ctx)
       and not RFLX.IPv4.Options.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Seq_Ctx.First = Seq_Ctx.First'Old
       and Seq_Ctx.Last = Seq_Ctx.Last'Old
       and Field_First (Ctx, F_Options) = Field_First (Ctx, F_Options)'Old
       and Field_Size (Ctx, F_Options) = Field_Size (Ctx, F_Options)'Old
       and Context_Cursor (Ctx, F_Version) = Context_Cursor (Ctx, F_Version)'Old
       and Context_Cursor (Ctx, F_IHL) = Context_Cursor (Ctx, F_IHL)'Old
       and Context_Cursor (Ctx, F_DSCP) = Context_Cursor (Ctx, F_DSCP)'Old
       and Context_Cursor (Ctx, F_ECN) = Context_Cursor (Ctx, F_ECN)'Old
       and Context_Cursor (Ctx, F_Total_Length) = Context_Cursor (Ctx, F_Total_Length)'Old
       and Context_Cursor (Ctx, F_Identification) = Context_Cursor (Ctx, F_Identification)'Old
       and Context_Cursor (Ctx, F_Flag_R) = Context_Cursor (Ctx, F_Flag_R)'Old
       and Context_Cursor (Ctx, F_Flag_DF) = Context_Cursor (Ctx, F_Flag_DF)'Old
       and Context_Cursor (Ctx, F_Flag_MF) = Context_Cursor (Ctx, F_Flag_MF)'Old
       and Context_Cursor (Ctx, F_Fragment_Offset) = Context_Cursor (Ctx, F_Fragment_Offset)'Old
       and Context_Cursor (Ctx, F_TTL) = Context_Cursor (Ctx, F_TTL)'Old
       and Context_Cursor (Ctx, F_Protocol) = Context_Cursor (Ctx, F_Protocol)'Old
       and Context_Cursor (Ctx, F_Header_Checksum) = Context_Cursor (Ctx, F_Header_Checksum)'Old
       and Context_Cursor (Ctx, F_Source) = Context_Cursor (Ctx, F_Source)'Old
       and Context_Cursor (Ctx, F_Destination) = Context_Cursor (Ctx, F_Destination)'Old,
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
                    Well_Formed (Cursors (F_IHL))
                 then
                    (Valid (Cursors (F_Version))
                     and then Cursors (F_IHL).Predecessor = F_Version))
                and then (if
                             Well_Formed (Cursors (F_DSCP))
                          then
                             (Valid (Cursors (F_IHL))
                              and then Cursors (F_DSCP).Predecessor = F_IHL))
                and then (if
                             Well_Formed (Cursors (F_ECN))
                          then
                             (Valid (Cursors (F_DSCP))
                              and then Cursors (F_ECN).Predecessor = F_DSCP))
                and then (if
                             Well_Formed (Cursors (F_Total_Length))
                          then
                             (Valid (Cursors (F_ECN))
                              and then Cursors (F_Total_Length).Predecessor = F_ECN))
                and then (if
                             Well_Formed (Cursors (F_Identification))
                          then
                             (Valid (Cursors (F_Total_Length))
                              and then Cursors (F_Identification).Predecessor = F_Total_Length
                              and then RFLX_Types.Base_Integer (Cursors (F_Total_Length).Value) >= RFLX_Types.Base_Integer (Cursors (F_IHL).Value) * 4))
                and then (if
                             Well_Formed (Cursors (F_Flag_R))
                          then
                             (Valid (Cursors (F_Identification))
                              and then Cursors (F_Flag_R).Predecessor = F_Identification))
                and then (if
                             Well_Formed (Cursors (F_Flag_DF))
                          then
                             (Valid (Cursors (F_Flag_R))
                              and then Cursors (F_Flag_DF).Predecessor = F_Flag_R
                              and then RFLX_Types.Base_Integer (Cursors (F_Flag_R).Value) = RFLX_Types.Base_Integer (To_Base_Integer (False))))
                and then (if
                             Well_Formed (Cursors (F_Flag_MF))
                          then
                             (Valid (Cursors (F_Flag_DF))
                              and then Cursors (F_Flag_MF).Predecessor = F_Flag_DF))
                and then (if
                             Well_Formed (Cursors (F_Fragment_Offset))
                          then
                             (Valid (Cursors (F_Flag_MF))
                              and then Cursors (F_Fragment_Offset).Predecessor = F_Flag_MF))
                and then (if
                             Well_Formed (Cursors (F_TTL))
                          then
                             (Valid (Cursors (F_Fragment_Offset))
                              and then Cursors (F_TTL).Predecessor = F_Fragment_Offset))
                and then (if
                             Well_Formed (Cursors (F_Protocol))
                          then
                             (Valid (Cursors (F_TTL))
                              and then Cursors (F_Protocol).Predecessor = F_TTL))
                and then (if
                             Well_Formed (Cursors (F_Header_Checksum))
                          then
                             (Valid (Cursors (F_Protocol))
                              and then Cursors (F_Header_Checksum).Predecessor = F_Protocol))
                and then (if
                             Well_Formed (Cursors (F_Source))
                          then
                             (Valid (Cursors (F_Header_Checksum))
                              and then Cursors (F_Source).Predecessor = F_Header_Checksum))
                and then (if
                             Well_Formed (Cursors (F_Destination))
                          then
                             (Valid (Cursors (F_Source))
                              and then Cursors (F_Destination).Predecessor = F_Source))
                and then (if
                             Well_Formed (Cursors (F_Options))
                          then
                             (Valid (Cursors (F_Destination))
                              and then Cursors (F_Options).Predecessor = F_Destination))
                and then (if
                             Well_Formed (Cursors (F_Payload))
                          then
                             (Well_Formed (Cursors (F_Options))
                              and then Cursors (F_Payload).Predecessor = F_Options)))
      and then ((if Invalid (Cursors (F_Version)) then Invalid (Cursors (F_IHL)))
                and then (if Invalid (Cursors (F_IHL)) then Invalid (Cursors (F_DSCP)))
                and then (if Invalid (Cursors (F_DSCP)) then Invalid (Cursors (F_ECN)))
                and then (if Invalid (Cursors (F_ECN)) then Invalid (Cursors (F_Total_Length)))
                and then (if Invalid (Cursors (F_Total_Length)) then Invalid (Cursors (F_Identification)))
                and then (if Invalid (Cursors (F_Identification)) then Invalid (Cursors (F_Flag_R)))
                and then (if Invalid (Cursors (F_Flag_R)) then Invalid (Cursors (F_Flag_DF)))
                and then (if Invalid (Cursors (F_Flag_DF)) then Invalid (Cursors (F_Flag_MF)))
                and then (if Invalid (Cursors (F_Flag_MF)) then Invalid (Cursors (F_Fragment_Offset)))
                and then (if Invalid (Cursors (F_Fragment_Offset)) then Invalid (Cursors (F_TTL)))
                and then (if Invalid (Cursors (F_TTL)) then Invalid (Cursors (F_Protocol)))
                and then (if Invalid (Cursors (F_Protocol)) then Invalid (Cursors (F_Header_Checksum)))
                and then (if Invalid (Cursors (F_Header_Checksum)) then Invalid (Cursors (F_Source)))
                and then (if Invalid (Cursors (F_Source)) then Invalid (Cursors (F_Destination)))
                and then (if Invalid (Cursors (F_Destination)) then Invalid (Cursors (F_Options)))
                and then (if Invalid (Cursors (F_Options)) then Invalid (Cursors (F_Payload))))
      and then ((if
                    Well_Formed (Cursors (F_Version))
                 then
                    (Cursors (F_Version).Last - Cursors (F_Version).First + 1 = 4
                     and then Cursors (F_Version).Predecessor = F_Initial
                     and then Cursors (F_Version).First = First))
                and then (if
                             Well_Formed (Cursors (F_IHL))
                          then
                             (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1 = 4
                              and then Cursors (F_IHL).Predecessor = F_Version
                              and then Cursors (F_IHL).First = Cursors (F_Version).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_DSCP))
                          then
                             (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1 = 6
                              and then Cursors (F_DSCP).Predecessor = F_IHL
                              and then Cursors (F_DSCP).First = Cursors (F_IHL).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_ECN))
                          then
                             (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1 = 2
                              and then Cursors (F_ECN).Predecessor = F_DSCP
                              and then Cursors (F_ECN).First = Cursors (F_DSCP).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Total_Length))
                          then
                             (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1 = 16
                              and then Cursors (F_Total_Length).Predecessor = F_ECN
                              and then Cursors (F_Total_Length).First = Cursors (F_ECN).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Identification))
                          then
                             (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1 = 16
                              and then Cursors (F_Identification).Predecessor = F_Total_Length
                              and then Cursors (F_Identification).First = Cursors (F_Total_Length).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Flag_R))
                          then
                             (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1 = 1
                              and then Cursors (F_Flag_R).Predecessor = F_Identification
                              and then Cursors (F_Flag_R).First = Cursors (F_Identification).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Flag_DF))
                          then
                             (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1 = 1
                              and then Cursors (F_Flag_DF).Predecessor = F_Flag_R
                              and then Cursors (F_Flag_DF).First = Cursors (F_Flag_R).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Flag_MF))
                          then
                             (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1 = 1
                              and then Cursors (F_Flag_MF).Predecessor = F_Flag_DF
                              and then Cursors (F_Flag_MF).First = Cursors (F_Flag_DF).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Fragment_Offset))
                          then
                             (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1 = 13
                              and then Cursors (F_Fragment_Offset).Predecessor = F_Flag_MF
                              and then Cursors (F_Fragment_Offset).First = Cursors (F_Flag_MF).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_TTL))
                          then
                             (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1 = 8
                              and then Cursors (F_TTL).Predecessor = F_Fragment_Offset
                              and then Cursors (F_TTL).First = Cursors (F_Fragment_Offset).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Protocol))
                          then
                             (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1 = 8
                              and then Cursors (F_Protocol).Predecessor = F_TTL
                              and then Cursors (F_Protocol).First = Cursors (F_TTL).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Header_Checksum))
                          then
                             (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1 = 16
                              and then Cursors (F_Header_Checksum).Predecessor = F_Protocol
                              and then Cursors (F_Header_Checksum).First = Cursors (F_Protocol).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Source))
                          then
                             (Cursors (F_Source).Last - Cursors (F_Source).First + 1 = 32
                              and then Cursors (F_Source).Predecessor = F_Header_Checksum
                              and then Cursors (F_Source).First = Cursors (F_Header_Checksum).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Destination))
                          then
                             (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1 = 32
                              and then Cursors (F_Destination).Predecessor = F_Source
                              and then Cursors (F_Destination).First = Cursors (F_Source).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Options))
                          then
                             (Cursors (F_Options).Last - Cursors (F_Options).First + 1 = (RFLX_Types.Bit_Length (Cursors (F_IHL).Value) - 5) * 32
                              and then Cursors (F_Options).Predecessor = F_Destination
                              and then Cursors (F_Options).First = Cursors (F_Destination).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Payload))
                          then
                             (Cursors (F_Payload).Last - Cursors (F_Payload).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Total_Length).Value) * 8 + RFLX_Types.Bit_Length (Cursors (F_IHL).Value) * (-32)
                              and then Cursors (F_Payload).Predecessor = F_Options
                              and then Cursors (F_Payload).First = Cursors (F_Options).Last + 1))))
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
      and then Valid_Next (Ctx, F_Version)
      and then RFLX.IPv4.Packet.Field_First (Ctx, RFLX.IPv4.Packet.F_Version) rem RFLX_Types.Byte'Size = 1
      and then Available_Space (Ctx, F_Version) = Ctx.Last - Ctx.First + 1
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
          when F_Version =>
             RFLX.IPv4.Valid_Version (Val),
          when F_IHL =>
             RFLX.IPv4.Valid_IHL (Val),
          when F_DSCP =>
             RFLX.IPv4.Valid_DCSP (Val),
          when F_ECN =>
             RFLX.IPv4.Valid_ECN (Val),
          when F_Total_Length =>
             RFLX.IPv4.Valid_Total_Length (Val),
          when F_Identification =>
             RFLX.IPv4.Valid_Identification (Val),
          when F_Flag_R | F_Flag_DF | F_Flag_MF =>
             Valid_Boolean (Val),
          when F_Fragment_Offset =>
             RFLX.IPv4.Valid_Fragment_Offset (Val),
          when F_TTL =>
             RFLX.IPv4.Valid_TTL (Val),
          when F_Protocol =>
             RFLX.IPv4.Valid_Protocol (Val),
          when F_Header_Checksum =>
             RFLX.IPv4.Valid_Header_Checksum (Val),
          when F_Source | F_Destination =>
             RFLX.IPv4.Valid_Address (Val),
          when F_Options | F_Payload =>
             True));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial | F_Version | F_IHL | F_DSCP | F_ECN | F_Identification | F_Flag_DF | F_Flag_MF | F_Fragment_Offset | F_TTL | F_Protocol | F_Header_Checksum | F_Source | F_Destination | F_Options | F_Payload | F_Final =>
             True,
          when F_Total_Length =>
             RFLX_Types.Base_Integer (Ctx.Cursors (F_Total_Length).Value) >= RFLX_Types.Base_Integer (Ctx.Cursors (F_IHL).Value) * 4,
          when F_Flag_R =>
             RFLX_Types.Base_Integer (Ctx.Cursors (F_Flag_R).Value) = RFLX_Types.Base_Integer (To_Base_Integer (False))));

   function Field_Condition (Ctx : Context; Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean is
     ((case Fld is
          when F_Version | F_IHL | F_DSCP | F_ECN =>
             True,
          when F_Total_Length =>
             Val >= RFLX_Types.Base_Integer (Ctx.Cursors (F_IHL).Value) * 4,
          when F_Identification =>
             True,
          when F_Flag_R =>
             Val = RFLX_Types.Base_Integer (To_Base_Integer (False)),
          when F_Flag_DF | F_Flag_MF | F_Fragment_Offset | F_TTL | F_Protocol | F_Header_Checksum | F_Source | F_Destination | F_Options | F_Payload =>
             True));

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     ((case Fld is
          when F_Version | F_IHL =>
             4,
          when F_DSCP =>
             6,
          when F_ECN =>
             2,
          when F_Total_Length | F_Identification =>
             16,
          when F_Flag_R | F_Flag_DF | F_Flag_MF =>
             1,
          when F_Fragment_Offset =>
             13,
          when F_TTL | F_Protocol =>
             8,
          when F_Header_Checksum =>
             16,
          when F_Source | F_Destination =>
             32,
          when F_Options =>
             (RFLX_Types.Bit_Length (Ctx.Cursors (F_IHL).Value) - 5) * 32,
          when F_Payload =>
             RFLX_Types.Bit_Length (Ctx.Cursors (F_Total_Length).Value) * 8 + RFLX_Types.Bit_Length (Ctx.Cursors (F_IHL).Value) * (-32)));

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index is
     ((if Fld = F_Version then Ctx.First else Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1));

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
          when F_Version =>
             Ctx.Cursors (Fld).Predecessor = F_Initial,
          when F_IHL =>
             (Valid (Ctx.Cursors (F_Version))
              and Ctx.Cursors (Fld).Predecessor = F_Version),
          when F_DSCP =>
             (Valid (Ctx.Cursors (F_IHL))
              and Ctx.Cursors (Fld).Predecessor = F_IHL),
          when F_ECN =>
             (Valid (Ctx.Cursors (F_DSCP))
              and Ctx.Cursors (Fld).Predecessor = F_DSCP),
          when F_Total_Length =>
             (Valid (Ctx.Cursors (F_ECN))
              and Ctx.Cursors (Fld).Predecessor = F_ECN),
          when F_Identification =>
             (Valid (Ctx.Cursors (F_Total_Length))
              and Ctx.Cursors (Fld).Predecessor = F_Total_Length),
          when F_Flag_R =>
             (Valid (Ctx.Cursors (F_Identification))
              and Ctx.Cursors (Fld).Predecessor = F_Identification),
          when F_Flag_DF =>
             (Valid (Ctx.Cursors (F_Flag_R))
              and Ctx.Cursors (Fld).Predecessor = F_Flag_R),
          when F_Flag_MF =>
             (Valid (Ctx.Cursors (F_Flag_DF))
              and Ctx.Cursors (Fld).Predecessor = F_Flag_DF),
          when F_Fragment_Offset =>
             (Valid (Ctx.Cursors (F_Flag_MF))
              and Ctx.Cursors (Fld).Predecessor = F_Flag_MF),
          when F_TTL =>
             (Valid (Ctx.Cursors (F_Fragment_Offset))
              and Ctx.Cursors (Fld).Predecessor = F_Fragment_Offset),
          when F_Protocol =>
             (Valid (Ctx.Cursors (F_TTL))
              and Ctx.Cursors (Fld).Predecessor = F_TTL),
          when F_Header_Checksum =>
             (Valid (Ctx.Cursors (F_Protocol))
              and Ctx.Cursors (Fld).Predecessor = F_Protocol),
          when F_Source =>
             (Valid (Ctx.Cursors (F_Header_Checksum))
              and Ctx.Cursors (Fld).Predecessor = F_Header_Checksum),
          when F_Destination =>
             (Valid (Ctx.Cursors (F_Source))
              and Ctx.Cursors (Fld).Predecessor = F_Source),
          when F_Options =>
             (Valid (Ctx.Cursors (F_Destination))
              and Ctx.Cursors (Fld).Predecessor = F_Destination),
          when F_Payload =>
             (Well_Formed (Ctx.Cursors (F_Options))
              and Ctx.Cursors (Fld).Predecessor = F_Options),
          when F_Final =>
             (Well_Formed (Ctx.Cursors (F_Payload))
              and Ctx.Cursors (Fld).Predecessor = F_Payload)));

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
     (Well_Formed (Ctx, F_Payload));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Payload));

   function Incomplete_Message (Ctx : Context) return Boolean is
     ((for some F in Field =>
          Incomplete (Ctx, F)));

   function Get_Version (Ctx : Context) return RFLX.IPv4.Version is
     (To_Actual (Ctx.Cursors (F_Version).Value));

   function Get_IHL (Ctx : Context) return RFLX.IPv4.IHL is
     (To_Actual (Ctx.Cursors (F_IHL).Value));

   function Get_DSCP (Ctx : Context) return RFLX.IPv4.DCSP is
     (To_Actual (Ctx.Cursors (F_DSCP).Value));

   function Get_ECN (Ctx : Context) return RFLX.IPv4.ECN is
     (To_Actual (Ctx.Cursors (F_ECN).Value));

   function Get_Total_Length (Ctx : Context) return RFLX.IPv4.Total_Length is
     (To_Actual (Ctx.Cursors (F_Total_Length).Value));

   function Get_Identification (Ctx : Context) return RFLX.IPv4.Identification is
     (To_Actual (Ctx.Cursors (F_Identification).Value));

   function Get_Flag_R (Ctx : Context) return Boolean is
     (To_Actual (Ctx.Cursors (F_Flag_R).Value));

   function Get_Flag_DF (Ctx : Context) return Boolean is
     (To_Actual (Ctx.Cursors (F_Flag_DF).Value));

   function Get_Flag_MF (Ctx : Context) return Boolean is
     (To_Actual (Ctx.Cursors (F_Flag_MF).Value));

   function Get_Fragment_Offset (Ctx : Context) return RFLX.IPv4.Fragment_Offset is
     (To_Actual (Ctx.Cursors (F_Fragment_Offset).Value));

   function Get_TTL (Ctx : Context) return RFLX.IPv4.TTL is
     (To_Actual (Ctx.Cursors (F_TTL).Value));

   function Get_Protocol (Ctx : Context) return RFLX.IPv4.Protocol is
     (To_Actual (Ctx.Cursors (F_Protocol).Value));

   function Get_Header_Checksum (Ctx : Context) return RFLX.IPv4.Header_Checksum is
     (To_Actual (Ctx.Cursors (F_Header_Checksum).Value));

   function Get_Source (Ctx : Context) return RFLX.IPv4.Address is
     (To_Actual (Ctx.Cursors (F_Source).Value));

   function Get_Destination (Ctx : Context) return RFLX.IPv4.Address is
     (To_Actual (Ctx.Cursors (F_Destination).Value));

   function Valid_Size (Ctx : Context; Fld : Field; Size : RFLX_Types.Bit_Length) return Boolean is
     (Size = Field_Size (Ctx, Fld))
    with
     Pre =>
       RFLX.IPv4.Packet.Valid_Next (Ctx, Fld);

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean is
     (Valid_Size (Ctx, Fld, RFLX_Types.To_Bit_Length (Length)));

   function Complete_Options (Ctx : Context; Seq_Ctx : RFLX.IPv4.Options.Context) return Boolean is
     (RFLX.IPv4.Options.Valid (Seq_Ctx)
      and RFLX.IPv4.Options.Size (Seq_Ctx) = Field_Size (Ctx, F_Options));

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

   function Context_Cursors_Index (Cursors : Field_Cursors; Fld : Field) return Field_Cursor is
     (Cursors (Fld));

end RFLX.IPv4.Packet;

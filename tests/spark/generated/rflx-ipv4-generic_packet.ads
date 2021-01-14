pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Builtin_Types.Conversions;
use RFLX.RFLX_Builtin_Types.Conversions;
with RFLX.RFLX_Generic_Types;
with RFLX.RFLX_Message_Sequence;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
   with package Options_Sequence is new RFLX.RFLX_Message_Sequence (Types, others => <>);
package RFLX.IPv4.Generic_Packet with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Warnings (Off, "use clause for type ""U64"" * has no effect");

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Bit_Index, Types.U64;

   pragma Warnings (On, "use clause for type ""U64"" * has no effect");

   type Virtual_Field is (F_Initial, F_Version, F_IHL, F_DSCP, F_ECN, F_Total_Length, F_Identification, F_Flag_R, F_Flag_DF, F_Flag_MF, F_Fragment_Offset, F_TTL, F_Protocol, F_Header_Checksum, F_Source, F_Destination, F_Options, F_Payload, F_Final);

   subtype Field is Virtual_Field range F_Version .. F_Payload;

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
            when F_Initial | F_Options | F_Payload | F_Final =>
               null;
            when F_Version =>
               Version_Value : RFLX.IPv4.Version_Base;
            when F_IHL =>
               IHL_Value : RFLX.IPv4.IHL_Base;
            when F_DSCP =>
               DSCP_Value : RFLX.IPv4.DCSP;
            when F_ECN =>
               ECN_Value : RFLX.IPv4.ECN;
            when F_Total_Length =>
               Total_Length_Value : RFLX.IPv4.Total_Length;
            when F_Identification =>
               Identification_Value : RFLX.IPv4.Identification;
            when F_Flag_R =>
               Flag_R_Value : RFLX.RFLX_Builtin_Types.Boolean_Base;
            when F_Flag_DF =>
               Flag_DF_Value : RFLX.RFLX_Builtin_Types.Boolean_Base;
            when F_Flag_MF =>
               Flag_MF_Value : RFLX.RFLX_Builtin_Types.Boolean_Base;
            when F_Fragment_Offset =>
               Fragment_Offset_Value : RFLX.IPv4.Fragment_Offset;
            when F_TTL =>
               TTL_Value : RFLX.IPv4.TTL;
            when F_Protocol =>
               Protocol_Value : RFLX.IPv4.Protocol_Base;
            when F_Header_Checksum =>
               Header_Checksum_Value : RFLX.IPv4.Header_Checksum;
            when F_Source =>
               Source_Value : RFLX.IPv4.Address;
            when F_Destination =>
               Destination_Value : RFLX.IPv4.Address;
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

   function Get_Version (Ctx : Context) return RFLX.IPv4.Version with
     Pre =>
       Valid (Ctx, F_Version);

   function Get_IHL (Ctx : Context) return RFLX.IPv4.IHL with
     Pre =>
       Valid (Ctx, F_IHL);

   function Get_DSCP (Ctx : Context) return RFLX.IPv4.DCSP with
     Pre =>
       Valid (Ctx, F_DSCP);

   function Get_ECN (Ctx : Context) return RFLX.IPv4.ECN with
     Pre =>
       Valid (Ctx, F_ECN);

   function Get_Total_Length (Ctx : Context) return RFLX.IPv4.Total_Length with
     Pre =>
       Valid (Ctx, F_Total_Length);

   function Get_Identification (Ctx : Context) return RFLX.IPv4.Identification with
     Pre =>
       Valid (Ctx, F_Identification);

   function Get_Flag_R (Ctx : Context) return Boolean with
     Pre =>
       Valid (Ctx, F_Flag_R);

   function Get_Flag_DF (Ctx : Context) return Boolean with
     Pre =>
       Valid (Ctx, F_Flag_DF);

   function Get_Flag_MF (Ctx : Context) return Boolean with
     Pre =>
       Valid (Ctx, F_Flag_MF);

   function Get_Fragment_Offset (Ctx : Context) return RFLX.IPv4.Fragment_Offset with
     Pre =>
       Valid (Ctx, F_Fragment_Offset);

   function Get_TTL (Ctx : Context) return RFLX.IPv4.TTL with
     Pre =>
       Valid (Ctx, F_TTL);

   function Get_Protocol (Ctx : Context) return RFLX.IPv4.Protocol with
     Pre =>
       Valid (Ctx, F_Protocol);

   function Get_Header_Checksum (Ctx : Context) return RFLX.IPv4.Header_Checksum with
     Pre =>
       Valid (Ctx, F_Header_Checksum);

   function Get_Source (Ctx : Context) return RFLX.IPv4.Address with
     Pre =>
       Valid (Ctx, F_Source);

   function Get_Destination (Ctx : Context) return RFLX.IPv4.Address with
     Pre =>
       Valid (Ctx, F_Destination);

   generic
      with procedure Process_Options (Options : Types.Bytes);
   procedure Get_Options (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and Present (Ctx, F_Options);

   generic
      with procedure Process_Payload (Payload : Types.Bytes);
   procedure Get_Payload (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and Present (Ctx, F_Payload);

   procedure Set_Version (Ctx : in out Context; Val : RFLX.IPv4.Version) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Version)
       and then Field_Condition (Ctx, (F_Version, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Version) >= Field_Size (Ctx, F_Version),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Version)
       and Get_Version (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_Version)
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
       and Valid_Next (Ctx, F_Version) = Valid_Next (Ctx, F_Version)'Old;

   procedure Set_IHL (Ctx : in out Context; Val : RFLX.IPv4.IHL) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_IHL)
       and then Field_Condition (Ctx, (F_IHL, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_IHL) >= Field_Size (Ctx, F_IHL),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_IHL)
       and Get_IHL (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_IHL)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
       and Context_Cursor (Ctx, F_Version) = Context_Cursor (Ctx, F_Version)'Old;

   procedure Set_DSCP (Ctx : in out Context; Val : RFLX.IPv4.DCSP) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_DSCP)
       and then Field_Condition (Ctx, (F_DSCP, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_DSCP) >= Field_Size (Ctx, F_DSCP),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_DSCP)
       and Get_DSCP (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_DSCP)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Context_Cursor (Ctx, F_Version) = Context_Cursor (Ctx, F_Version)'Old
       and Context_Cursor (Ctx, F_IHL) = Context_Cursor (Ctx, F_IHL)'Old;

   procedure Set_ECN (Ctx : in out Context; Val : RFLX.IPv4.ECN) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_ECN)
       and then Field_Condition (Ctx, (F_ECN, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_ECN) >= Field_Size (Ctx, F_ECN),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_ECN)
       and Get_ECN (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_ECN)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Context_Cursor (Ctx, F_Version) = Context_Cursor (Ctx, F_Version)'Old
       and Context_Cursor (Ctx, F_IHL) = Context_Cursor (Ctx, F_IHL)'Old
       and Context_Cursor (Ctx, F_DSCP) = Context_Cursor (Ctx, F_DSCP)'Old;

   procedure Set_Total_Length (Ctx : in out Context; Val : RFLX.IPv4.Total_Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Total_Length)
       and then Field_Condition (Ctx, (F_Total_Length, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Total_Length) >= Field_Size (Ctx, F_Total_Length),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Total_Length)
       and Get_Total_Length (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_Total_Length)
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
               Types.U64 (Get_Total_Length (Ctx)) >= Types.U64 (Get_IHL (Ctx)) * 4
            then
               Predecessor (Ctx, F_Identification) = F_Total_Length
               and Valid_Next (Ctx, F_Identification))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Total_Length) = Predecessor (Ctx, F_Total_Length)'Old
       and Valid_Next (Ctx, F_Total_Length) = Valid_Next (Ctx, F_Total_Length)'Old
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Context_Cursor (Ctx, F_Version) = Context_Cursor (Ctx, F_Version)'Old
       and Context_Cursor (Ctx, F_IHL) = Context_Cursor (Ctx, F_IHL)'Old
       and Context_Cursor (Ctx, F_DSCP) = Context_Cursor (Ctx, F_DSCP)'Old
       and Context_Cursor (Ctx, F_ECN) = Context_Cursor (Ctx, F_ECN)'Old;

   procedure Set_Identification (Ctx : in out Context; Val : RFLX.IPv4.Identification) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Identification)
       and then Field_Condition (Ctx, (F_Identification, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Identification) >= Field_Size (Ctx, F_Identification),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Identification)
       and Get_Identification (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_Identification)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Context_Cursor (Ctx, F_Version) = Context_Cursor (Ctx, F_Version)'Old
       and Context_Cursor (Ctx, F_IHL) = Context_Cursor (Ctx, F_IHL)'Old
       and Context_Cursor (Ctx, F_DSCP) = Context_Cursor (Ctx, F_DSCP)'Old
       and Context_Cursor (Ctx, F_ECN) = Context_Cursor (Ctx, F_ECN)'Old
       and Context_Cursor (Ctx, F_Total_Length) = Context_Cursor (Ctx, F_Total_Length)'Old;

   procedure Set_Flag_R (Ctx : in out Context; Val : Boolean) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Flag_R)
       and then Field_Condition (Ctx, (F_Flag_R, To_Base (Val)))
       and then True
       and then Available_Space (Ctx, F_Flag_R) >= Field_Size (Ctx, F_Flag_R),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Flag_R)
       and Get_Flag_R (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_Flag_R)
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
               Types.U64 (To_Base (Get_Flag_R (Ctx))) = Types.U64 (To_Base (False))
            then
               Predecessor (Ctx, F_Flag_DF) = F_Flag_R
               and Valid_Next (Ctx, F_Flag_DF))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Flag_R) = Predecessor (Ctx, F_Flag_R)'Old
       and Valid_Next (Ctx, F_Flag_R) = Valid_Next (Ctx, F_Flag_R)'Old
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Context_Cursor (Ctx, F_Version) = Context_Cursor (Ctx, F_Version)'Old
       and Context_Cursor (Ctx, F_IHL) = Context_Cursor (Ctx, F_IHL)'Old
       and Context_Cursor (Ctx, F_DSCP) = Context_Cursor (Ctx, F_DSCP)'Old
       and Context_Cursor (Ctx, F_ECN) = Context_Cursor (Ctx, F_ECN)'Old
       and Context_Cursor (Ctx, F_Total_Length) = Context_Cursor (Ctx, F_Total_Length)'Old
       and Context_Cursor (Ctx, F_Identification) = Context_Cursor (Ctx, F_Identification)'Old;

   procedure Set_Flag_DF (Ctx : in out Context; Val : Boolean) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Flag_DF)
       and then Field_Condition (Ctx, (F_Flag_DF, To_Base (Val)))
       and then True
       and then Available_Space (Ctx, F_Flag_DF) >= Field_Size (Ctx, F_Flag_DF),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Flag_DF)
       and Get_Flag_DF (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_Flag_DF)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Context_Cursor (Ctx, F_Version) = Context_Cursor (Ctx, F_Version)'Old
       and Context_Cursor (Ctx, F_IHL) = Context_Cursor (Ctx, F_IHL)'Old
       and Context_Cursor (Ctx, F_DSCP) = Context_Cursor (Ctx, F_DSCP)'Old
       and Context_Cursor (Ctx, F_ECN) = Context_Cursor (Ctx, F_ECN)'Old
       and Context_Cursor (Ctx, F_Total_Length) = Context_Cursor (Ctx, F_Total_Length)'Old
       and Context_Cursor (Ctx, F_Identification) = Context_Cursor (Ctx, F_Identification)'Old
       and Context_Cursor (Ctx, F_Flag_R) = Context_Cursor (Ctx, F_Flag_R)'Old;

   procedure Set_Flag_MF (Ctx : in out Context; Val : Boolean) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Flag_MF)
       and then Field_Condition (Ctx, (F_Flag_MF, To_Base (Val)))
       and then True
       and then Available_Space (Ctx, F_Flag_MF) >= Field_Size (Ctx, F_Flag_MF),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Flag_MF)
       and Get_Flag_MF (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_Flag_MF)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Context_Cursor (Ctx, F_Version) = Context_Cursor (Ctx, F_Version)'Old
       and Context_Cursor (Ctx, F_IHL) = Context_Cursor (Ctx, F_IHL)'Old
       and Context_Cursor (Ctx, F_DSCP) = Context_Cursor (Ctx, F_DSCP)'Old
       and Context_Cursor (Ctx, F_ECN) = Context_Cursor (Ctx, F_ECN)'Old
       and Context_Cursor (Ctx, F_Total_Length) = Context_Cursor (Ctx, F_Total_Length)'Old
       and Context_Cursor (Ctx, F_Identification) = Context_Cursor (Ctx, F_Identification)'Old
       and Context_Cursor (Ctx, F_Flag_R) = Context_Cursor (Ctx, F_Flag_R)'Old
       and Context_Cursor (Ctx, F_Flag_DF) = Context_Cursor (Ctx, F_Flag_DF)'Old;

   procedure Set_Fragment_Offset (Ctx : in out Context; Val : RFLX.IPv4.Fragment_Offset) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Fragment_Offset)
       and then Field_Condition (Ctx, (F_Fragment_Offset, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Fragment_Offset) >= Field_Size (Ctx, F_Fragment_Offset),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Fragment_Offset)
       and Get_Fragment_Offset (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_Fragment_Offset)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Context_Cursor (Ctx, F_Version) = Context_Cursor (Ctx, F_Version)'Old
       and Context_Cursor (Ctx, F_IHL) = Context_Cursor (Ctx, F_IHL)'Old
       and Context_Cursor (Ctx, F_DSCP) = Context_Cursor (Ctx, F_DSCP)'Old
       and Context_Cursor (Ctx, F_ECN) = Context_Cursor (Ctx, F_ECN)'Old
       and Context_Cursor (Ctx, F_Total_Length) = Context_Cursor (Ctx, F_Total_Length)'Old
       and Context_Cursor (Ctx, F_Identification) = Context_Cursor (Ctx, F_Identification)'Old
       and Context_Cursor (Ctx, F_Flag_R) = Context_Cursor (Ctx, F_Flag_R)'Old
       and Context_Cursor (Ctx, F_Flag_DF) = Context_Cursor (Ctx, F_Flag_DF)'Old
       and Context_Cursor (Ctx, F_Flag_MF) = Context_Cursor (Ctx, F_Flag_MF)'Old;

   procedure Set_TTL (Ctx : in out Context; Val : RFLX.IPv4.TTL) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_TTL)
       and then Field_Condition (Ctx, (F_TTL, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_TTL) >= Field_Size (Ctx, F_TTL),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_TTL)
       and Get_TTL (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_TTL)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
       and Get_IHL (Ctx) = Get_IHL (Ctx)'Old
       and Get_DSCP (Ctx) = Get_DSCP (Ctx)'Old
       and Get_ECN (Ctx) = Get_ECN (Ctx)'Old
       and Get_Total_Length (Ctx) = Get_Total_Length (Ctx)'Old
       and Get_Identification (Ctx) = Get_Identification (Ctx)'Old
       and Get_Flag_R (Ctx) = Get_Flag_R (Ctx)'Old
       and Get_Flag_DF (Ctx) = Get_Flag_DF (Ctx)'Old
       and Get_Flag_MF (Ctx) = Get_Flag_MF (Ctx)'Old
       and Get_Fragment_Offset (Ctx) = Get_Fragment_Offset (Ctx)'Old
       and Context_Cursor (Ctx, F_Version) = Context_Cursor (Ctx, F_Version)'Old
       and Context_Cursor (Ctx, F_IHL) = Context_Cursor (Ctx, F_IHL)'Old
       and Context_Cursor (Ctx, F_DSCP) = Context_Cursor (Ctx, F_DSCP)'Old
       and Context_Cursor (Ctx, F_ECN) = Context_Cursor (Ctx, F_ECN)'Old
       and Context_Cursor (Ctx, F_Total_Length) = Context_Cursor (Ctx, F_Total_Length)'Old
       and Context_Cursor (Ctx, F_Identification) = Context_Cursor (Ctx, F_Identification)'Old
       and Context_Cursor (Ctx, F_Flag_R) = Context_Cursor (Ctx, F_Flag_R)'Old
       and Context_Cursor (Ctx, F_Flag_DF) = Context_Cursor (Ctx, F_Flag_DF)'Old
       and Context_Cursor (Ctx, F_Flag_MF) = Context_Cursor (Ctx, F_Flag_MF)'Old
       and Context_Cursor (Ctx, F_Fragment_Offset) = Context_Cursor (Ctx, F_Fragment_Offset)'Old;

   procedure Set_Protocol (Ctx : in out Context; Val : RFLX.IPv4.Protocol_Enum) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Protocol)
       and then Field_Condition (Ctx, (F_Protocol, To_Base (Val)))
       and then True
       and then Available_Space (Ctx, F_Protocol) >= Field_Size (Ctx, F_Protocol),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Protocol)
       and Get_Protocol (Ctx) = (True, Val)
       and Message_Last (Ctx) = Field_Last (Ctx, F_Protocol)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
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
       and Context_Cursor (Ctx, F_TTL) = Context_Cursor (Ctx, F_TTL)'Old;

   procedure Set_Header_Checksum (Ctx : in out Context; Val : RFLX.IPv4.Header_Checksum) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Header_Checksum)
       and then Field_Condition (Ctx, (F_Header_Checksum, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Header_Checksum) >= Field_Size (Ctx, F_Header_Checksum),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Header_Checksum)
       and Get_Header_Checksum (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_Header_Checksum)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
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
       and Context_Cursor (Ctx, F_Protocol) = Context_Cursor (Ctx, F_Protocol)'Old;

   procedure Set_Source (Ctx : in out Context; Val : RFLX.IPv4.Address) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Source)
       and then Field_Condition (Ctx, (F_Source, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Source) >= Field_Size (Ctx, F_Source),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Source)
       and Get_Source (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_Source)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
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
       and Context_Cursor (Ctx, F_Header_Checksum) = Context_Cursor (Ctx, F_Header_Checksum)'Old;

   procedure Set_Destination (Ctx : in out Context; Val : RFLX.IPv4.Address) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Destination)
       and then Field_Condition (Ctx, (F_Destination, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Destination) >= Field_Size (Ctx, F_Destination),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Destination)
       and Get_Destination (Ctx) = Val
       and Message_Last (Ctx) = Field_Last (Ctx, F_Destination)
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
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
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
       and Context_Cursor (Ctx, F_Source) = Context_Cursor (Ctx, F_Source)'Old;

   procedure Set_Options_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Options)
       and then Field_Condition (Ctx, (Fld => F_Options))
       and then Available_Space (Ctx, F_Options) >= Field_Size (Ctx, F_Options)
       and then Field_First (Ctx, F_Options) mod Types.Byte'Size = 1
       and then Field_Size (Ctx, F_Options) mod Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Options) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Message_Last (Ctx) = Field_Last (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Payload) = F_Options
            and Valid_Next (Ctx, F_Payload))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
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
       and Structural_Valid (Ctx, F_Options);

   procedure Set_Payload_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Payload)
       and then Field_Condition (Ctx, (Fld => F_Payload))
       and then Available_Space (Ctx, F_Payload) >= Field_Size (Ctx, F_Payload)
       and then Field_First (Ctx, F_Payload) mod Types.Byte'Size = 1
       and then Field_Size (Ctx, F_Payload) mod Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Payload) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Message_Last (Ctx) = Field_Last (Ctx, F_Payload)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
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
       and Structural_Valid (Ctx, F_Payload);

   procedure Set_Options (Ctx : in out Context; Seq_Ctx : Options_Sequence.Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Options)
       and then Field_Condition (Ctx, (Fld => F_Options))
       and then Available_Space (Ctx, F_Options) >= Field_Size (Ctx, F_Options)
       and then Field_First (Ctx, F_Options) mod Types.Byte'Size = 1
       and then Field_Size (Ctx, F_Options) mod Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Options) = Options_Sequence.Size (Seq_Ctx)
       and then Options_Sequence.Has_Buffer (Seq_Ctx)
       and then Options_Sequence.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Message_Last (Ctx) = Field_Last (Ctx, F_Options)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Payload) = F_Options
            and Valid_Next (Ctx, F_Payload))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
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
       and Structural_Valid (Ctx, F_Options)
       and (if
               Field_Size (Ctx, F_Options) > 0
            then
               Present (Ctx, F_Options));

   generic
      with procedure Process_Payload (Payload : out Types.Bytes);
      with function Valid_Length (Length : Types.Length) return Boolean;
   procedure Set_Payload (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Payload)
       and then Field_Condition (Ctx, (Fld => F_Payload))
       and then Available_Space (Ctx, F_Payload) >= Field_Size (Ctx, F_Payload)
       and then Field_First (Ctx, F_Payload) mod Types.Byte'Size = 1
       and then Field_Size (Ctx, F_Payload) mod Types.Byte'Size = 0
       and then Valid_Length (Types.Length (Field_Size (Ctx, F_Payload) / Types.Byte'Size)),
     Post =>
       Has_Buffer (Ctx)
       and Message_Last (Ctx) = Field_Last (Ctx, F_Payload)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
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
       and Structural_Valid (Ctx, F_Payload);

   procedure Initialize_Payload (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Payload)
       and then Field_Condition (Ctx, (Fld => F_Payload))
       and then Available_Space (Ctx, F_Payload) >= Field_Size (Ctx, F_Payload)
       and then Field_First (Ctx, F_Payload) mod Types.Byte'Size = 1
       and then Field_Size (Ctx, F_Payload) mod Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Message_Last (Ctx) = Field_Last (Ctx, F_Payload)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_Version (Ctx) = Get_Version (Ctx)'Old
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
       and Structural_Valid (Ctx, F_Payload);

   procedure Switch_To_Options (Ctx : in out Context; Seq_Ctx : out Options_Sequence.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Options)
       and then Field_Size (Ctx, F_Options) > 0
       and then Field_First (Ctx, F_Options) mod Types.Byte'Size = 1
       and then Field_Condition (Ctx, (Fld => F_Options))
       and then Available_Space (Ctx, F_Options) >= Field_Size (Ctx, F_Options),
     Post =>
       not Has_Buffer (Ctx)
       and Options_Sequence.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_Options)
       and Seq_Ctx.Last = Field_Last (Ctx, F_Options)
       and Options_Sequence.Valid (Seq_Ctx)
       and Options_Sequence.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_Options)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Path_Condition (Ctx, F_Options) = Path_Condition (Ctx, F_Options)'Old
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
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Options) =>
           Context_Cursor (Ctx, F_Payload) = Context_Cursor (Ctx, F_Payload)'Old,
        others =>
           (Predecessor (Ctx, F_Payload) = F_Options
            and Valid_Next (Ctx, F_Payload))
           and Invalid (Ctx, F_Payload));

   function Complete_Options (Ctx : Context; Seq_Ctx : Options_Sequence.Context) return Boolean with
     Pre =>
       Valid_Next (Ctx, F_Options);

   procedure Update_Options (Ctx : in out Context; Seq_Ctx : in out Options_Sequence.Context) with
     Pre =>
       Present (Ctx, F_Options)
       and then Complete_Options (Ctx, Seq_Ctx)
       and then not Has_Buffer (Ctx)
       and then Options_Sequence.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_Options)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_Options),
     Post =>
       Present (Ctx, F_Options)
       and Has_Buffer (Ctx)
       and not Options_Sequence.Has_Buffer (Seq_Ctx)
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
       and Context_Cursor (Ctx, F_Destination) = Context_Cursor (Ctx, F_Destination)'Old
       and Context_Cursor (Ctx, F_Payload) = Context_Cursor (Ctx, F_Payload)'Old,
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
          when F_Version =>
             Valid (Val.Version_Value),
          when F_IHL =>
             Valid (Val.IHL_Value),
          when F_DSCP =>
             Valid (Val.DSCP_Value),
          when F_ECN =>
             Valid (Val.ECN_Value),
          when F_Total_Length =>
             Valid (Val.Total_Length_Value),
          when F_Identification =>
             Valid (Val.Identification_Value),
          when F_Flag_R =>
             Valid (Val.Flag_R_Value),
          when F_Flag_DF =>
             Valid (Val.Flag_DF_Value),
          when F_Flag_MF =>
             Valid (Val.Flag_MF_Value),
          when F_Fragment_Offset =>
             Valid (Val.Fragment_Offset_Value),
          when F_TTL =>
             Valid (Val.TTL_Value),
          when F_Protocol =>
             Valid (Val.Protocol_Value),
          when F_Header_Checksum =>
             Valid (Val.Header_Checksum_Value),
          when F_Source =>
             Valid (Val.Source_Value),
          when F_Destination =>
             Valid (Val.Destination_Value),
          when F_Options | F_Payload =>
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
                    Structural_Valid (Cursors (F_IHL))
                 then
                    (Valid (Cursors (F_Version))
                     and then Cursors (F_IHL).Predecessor = F_Version))
                and then (if
                             Structural_Valid (Cursors (F_DSCP))
                          then
                             (Valid (Cursors (F_IHL))
                              and then Cursors (F_DSCP).Predecessor = F_IHL))
                and then (if
                             Structural_Valid (Cursors (F_ECN))
                          then
                             (Valid (Cursors (F_DSCP))
                              and then Cursors (F_ECN).Predecessor = F_DSCP))
                and then (if
                             Structural_Valid (Cursors (F_Total_Length))
                          then
                             (Valid (Cursors (F_ECN))
                              and then Cursors (F_Total_Length).Predecessor = F_ECN))
                and then (if
                             Structural_Valid (Cursors (F_Identification))
                          then
                             (Valid (Cursors (F_Total_Length))
                              and then Cursors (F_Identification).Predecessor = F_Total_Length
                              and then Types.U64 (Cursors (F_Total_Length).Value.Total_Length_Value) >= Types.U64 (Cursors (F_IHL).Value.IHL_Value) * 4))
                and then (if
                             Structural_Valid (Cursors (F_Flag_R))
                          then
                             (Valid (Cursors (F_Identification))
                              and then Cursors (F_Flag_R).Predecessor = F_Identification))
                and then (if
                             Structural_Valid (Cursors (F_Flag_DF))
                          then
                             (Valid (Cursors (F_Flag_R))
                              and then Cursors (F_Flag_DF).Predecessor = F_Flag_R
                              and then Types.U64 (Cursors (F_Flag_R).Value.Flag_R_Value) = Types.U64 (To_Base (False))))
                and then (if
                             Structural_Valid (Cursors (F_Flag_MF))
                          then
                             (Valid (Cursors (F_Flag_DF))
                              and then Cursors (F_Flag_MF).Predecessor = F_Flag_DF))
                and then (if
                             Structural_Valid (Cursors (F_Fragment_Offset))
                          then
                             (Valid (Cursors (F_Flag_MF))
                              and then Cursors (F_Fragment_Offset).Predecessor = F_Flag_MF))
                and then (if
                             Structural_Valid (Cursors (F_TTL))
                          then
                             (Valid (Cursors (F_Fragment_Offset))
                              and then Cursors (F_TTL).Predecessor = F_Fragment_Offset))
                and then (if
                             Structural_Valid (Cursors (F_Protocol))
                          then
                             (Valid (Cursors (F_TTL))
                              and then Cursors (F_Protocol).Predecessor = F_TTL))
                and then (if
                             Structural_Valid (Cursors (F_Header_Checksum))
                          then
                             (Valid (Cursors (F_Protocol))
                              and then Cursors (F_Header_Checksum).Predecessor = F_Protocol))
                and then (if
                             Structural_Valid (Cursors (F_Source))
                          then
                             (Valid (Cursors (F_Header_Checksum))
                              and then Cursors (F_Source).Predecessor = F_Header_Checksum))
                and then (if
                             Structural_Valid (Cursors (F_Destination))
                          then
                             (Valid (Cursors (F_Source))
                              and then Cursors (F_Destination).Predecessor = F_Source))
                and then (if
                             Structural_Valid (Cursors (F_Options))
                          then
                             (Valid (Cursors (F_Destination))
                              and then Cursors (F_Options).Predecessor = F_Destination))
                and then (if
                             Structural_Valid (Cursors (F_Payload))
                          then
                             (Structural_Valid (Cursors (F_Options))
                              and then Cursors (F_Payload).Predecessor = F_Options)))
      and then ((if
                    Invalid (Cursors (F_Version))
                 then
                    Invalid (Cursors (F_IHL)))
                and then (if
                             Invalid (Cursors (F_IHL))
                          then
                             Invalid (Cursors (F_DSCP)))
                and then (if
                             Invalid (Cursors (F_DSCP))
                          then
                             Invalid (Cursors (F_ECN)))
                and then (if
                             Invalid (Cursors (F_ECN))
                          then
                             Invalid (Cursors (F_Total_Length)))
                and then (if
                             Invalid (Cursors (F_Total_Length))
                          then
                             Invalid (Cursors (F_Identification)))
                and then (if
                             Invalid (Cursors (F_Identification))
                          then
                             Invalid (Cursors (F_Flag_R)))
                and then (if
                             Invalid (Cursors (F_Flag_R))
                          then
                             Invalid (Cursors (F_Flag_DF)))
                and then (if
                             Invalid (Cursors (F_Flag_DF))
                          then
                             Invalid (Cursors (F_Flag_MF)))
                and then (if
                             Invalid (Cursors (F_Flag_MF))
                          then
                             Invalid (Cursors (F_Fragment_Offset)))
                and then (if
                             Invalid (Cursors (F_Fragment_Offset))
                          then
                             Invalid (Cursors (F_TTL)))
                and then (if
                             Invalid (Cursors (F_TTL))
                          then
                             Invalid (Cursors (F_Protocol)))
                and then (if
                             Invalid (Cursors (F_Protocol))
                          then
                             Invalid (Cursors (F_Header_Checksum)))
                and then (if
                             Invalid (Cursors (F_Header_Checksum))
                          then
                             Invalid (Cursors (F_Source)))
                and then (if
                             Invalid (Cursors (F_Source))
                          then
                             Invalid (Cursors (F_Destination)))
                and then (if
                             Invalid (Cursors (F_Destination))
                          then
                             Invalid (Cursors (F_Options)))
                and then (if
                             Invalid (Cursors (F_Options))
                          then
                             Invalid (Cursors (F_Payload))))
      and then (if
                   Structural_Valid (Cursors (F_Version))
                then
                   Cursors (F_Version).Last - Cursors (F_Version).First + 1 = RFLX.IPv4.Version_Base'Size
                   and then Cursors (F_Version).Predecessor = F_Initial
                   and then Cursors (F_Version).First = First
                   and then (if
                                Structural_Valid (Cursors (F_IHL))
                             then
                                Cursors (F_IHL).Last - Cursors (F_IHL).First + 1 = RFLX.IPv4.IHL_Base'Size
                                and then Cursors (F_IHL).Predecessor = F_Version
                                and then Cursors (F_IHL).First = Cursors (F_Version).Last + 1
                                and then (if
                                             Structural_Valid (Cursors (F_DSCP))
                                          then
                                             Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1 = RFLX.IPv4.DCSP'Size
                                             and then Cursors (F_DSCP).Predecessor = F_IHL
                                             and then Cursors (F_DSCP).First = Cursors (F_IHL).Last + 1
                                             and then (if
                                                          Structural_Valid (Cursors (F_ECN))
                                                       then
                                                          Cursors (F_ECN).Last - Cursors (F_ECN).First + 1 = RFLX.IPv4.ECN'Size
                                                          and then Cursors (F_ECN).Predecessor = F_DSCP
                                                          and then Cursors (F_ECN).First = Cursors (F_DSCP).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Total_Length))
                                                                    then
                                                                       Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1 = RFLX.IPv4.Total_Length'Size
                                                                       and then Cursors (F_Total_Length).Predecessor = F_ECN
                                                                       and then Cursors (F_Total_Length).First = Cursors (F_ECN).Last + 1
                                                                       and then (if
                                                                                    Structural_Valid (Cursors (F_Identification))
                                                                                    and then Types.U64 (Cursors (F_Total_Length).Value.Total_Length_Value) >= Types.U64 (Cursors (F_IHL).Value.IHL_Value) * 4
                                                                                 then
                                                                                    Cursors (F_Identification).Last - Cursors (F_Identification).First + 1 = RFLX.IPv4.Identification'Size
                                                                                    and then Cursors (F_Identification).Predecessor = F_Total_Length
                                                                                    and then Cursors (F_Identification).First = Cursors (F_Total_Length).Last + 1
                                                                                    and then (if
                                                                                                 Structural_Valid (Cursors (F_Flag_R))
                                                                                              then
                                                                                                 Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1 = RFLX.RFLX_Builtin_Types.Boolean_Base'Size
                                                                                                 and then Cursors (F_Flag_R).Predecessor = F_Identification
                                                                                                 and then Cursors (F_Flag_R).First = Cursors (F_Identification).Last + 1
                                                                                                 and then (if
                                                                                                              Structural_Valid (Cursors (F_Flag_DF))
                                                                                                              and then Types.U64 (Cursors (F_Flag_R).Value.Flag_R_Value) = Types.U64 (To_Base (False))
                                                                                                           then
                                                                                                              Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1 = RFLX.RFLX_Builtin_Types.Boolean_Base'Size
                                                                                                              and then Cursors (F_Flag_DF).Predecessor = F_Flag_R
                                                                                                              and then Cursors (F_Flag_DF).First = Cursors (F_Flag_R).Last + 1
                                                                                                              and then (if
                                                                                                                           Structural_Valid (Cursors (F_Flag_MF))
                                                                                                                        then
                                                                                                                           Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1 = RFLX.RFLX_Builtin_Types.Boolean_Base'Size
                                                                                                                           and then Cursors (F_Flag_MF).Predecessor = F_Flag_DF
                                                                                                                           and then Cursors (F_Flag_MF).First = Cursors (F_Flag_DF).Last + 1
                                                                                                                           and then (if
                                                                                                                                        Structural_Valid (Cursors (F_Fragment_Offset))
                                                                                                                                     then
                                                                                                                                        Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1 = RFLX.IPv4.Fragment_Offset'Size
                                                                                                                                        and then Cursors (F_Fragment_Offset).Predecessor = F_Flag_MF
                                                                                                                                        and then Cursors (F_Fragment_Offset).First = Cursors (F_Flag_MF).Last + 1
                                                                                                                                        and then (if
                                                                                                                                                     Structural_Valid (Cursors (F_TTL))
                                                                                                                                                  then
                                                                                                                                                     Cursors (F_TTL).Last - Cursors (F_TTL).First + 1 = RFLX.IPv4.TTL'Size
                                                                                                                                                     and then Cursors (F_TTL).Predecessor = F_Fragment_Offset
                                                                                                                                                     and then Cursors (F_TTL).First = Cursors (F_Fragment_Offset).Last + 1
                                                                                                                                                     and then (if
                                                                                                                                                                  Structural_Valid (Cursors (F_Protocol))
                                                                                                                                                               then
                                                                                                                                                                  Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1 = RFLX.IPv4.Protocol_Base'Size
                                                                                                                                                                  and then Cursors (F_Protocol).Predecessor = F_TTL
                                                                                                                                                                  and then Cursors (F_Protocol).First = Cursors (F_TTL).Last + 1
                                                                                                                                                                  and then (if
                                                                                                                                                                               Structural_Valid (Cursors (F_Header_Checksum))
                                                                                                                                                                            then
                                                                                                                                                                               Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1 = RFLX.IPv4.Header_Checksum'Size
                                                                                                                                                                               and then Cursors (F_Header_Checksum).Predecessor = F_Protocol
                                                                                                                                                                               and then Cursors (F_Header_Checksum).First = Cursors (F_Protocol).Last + 1
                                                                                                                                                                               and then (if
                                                                                                                                                                                            Structural_Valid (Cursors (F_Source))
                                                                                                                                                                                         then
                                                                                                                                                                                            Cursors (F_Source).Last - Cursors (F_Source).First + 1 = RFLX.IPv4.Address'Size
                                                                                                                                                                                            and then Cursors (F_Source).Predecessor = F_Header_Checksum
                                                                                                                                                                                            and then Cursors (F_Source).First = Cursors (F_Header_Checksum).Last + 1
                                                                                                                                                                                            and then (if
                                                                                                                                                                                                         Structural_Valid (Cursors (F_Destination))
                                                                                                                                                                                                      then
                                                                                                                                                                                                         Cursors (F_Destination).Last - Cursors (F_Destination).First + 1 = RFLX.IPv4.Address'Size
                                                                                                                                                                                                         and then Cursors (F_Destination).Predecessor = F_Source
                                                                                                                                                                                                         and then Cursors (F_Destination).First = Cursors (F_Source).Last + 1
                                                                                                                                                                                                         and then (if
                                                                                                                                                                                                                      Structural_Valid (Cursors (F_Options))
                                                                                                                                                                                                                   then
                                                                                                                                                                                                                      Cursors (F_Options).Last - Cursors (F_Options).First + 1 = (Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) - 5) * 32
                                                                                                                                                                                                                      and then Cursors (F_Options).Predecessor = F_Destination
                                                                                                                                                                                                                      and then Cursors (F_Options).First = Cursors (F_Destination).Last + 1
                                                                                                                                                                                                                      and then (if
                                                                                                                                                                                                                                   Structural_Valid (Cursors (F_Payload))
                                                                                                                                                                                                                                then
                                                                                                                                                                                                                                   Cursors (F_Payload).Last - Cursors (F_Payload).First + 1 = Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) * 8 + Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * (-32)
                                                                                                                                                                                                                                   and then Cursors (F_Payload).Predecessor = F_Options
                                                                                                                                                                                                                                   and then Cursors (F_Payload).First = Cursors (F_Options).Last + 1))))))))))))))))));

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

end RFLX.IPv4.Generic_Packet;

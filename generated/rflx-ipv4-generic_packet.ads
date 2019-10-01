with RFLX.Types;
use type RFLX.Types.Integer_Address;
with RFLX.Message_Sequence;

generic
   with package Options_Sequence is new Message_Sequence (<>);
package RFLX.IPv4.Generic_Packet with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   type Virtual_Field is (F_Initial, F_Version, F_IHL, F_DSCP, F_ECN, F_Total_Length, F_Identification, F_Flag_R, F_Flag_DF, F_Flag_MF, F_Fragment_Offset, F_TTL, F_Protocol, F_Header_Checksum, F_Source, F_Destination, F_Options, F_Payload, F_Final);

   subtype Field is Virtual_Field range F_Version .. F_Payload;

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
          and then Present (Ctx, F_Version) = Present (Ctx, F_Version)'Old
          and then Present (Ctx, F_IHL) = Present (Ctx, F_IHL)'Old
          and then Present (Ctx, F_DSCP) = Present (Ctx, F_DSCP)'Old
          and then Present (Ctx, F_ECN) = Present (Ctx, F_ECN)'Old
          and then Present (Ctx, F_Total_Length) = Present (Ctx, F_Total_Length)'Old
          and then Present (Ctx, F_Identification) = Present (Ctx, F_Identification)'Old
          and then Present (Ctx, F_Flag_R) = Present (Ctx, F_Flag_R)'Old
          and then Present (Ctx, F_Flag_DF) = Present (Ctx, F_Flag_DF)'Old
          and then Present (Ctx, F_Flag_MF) = Present (Ctx, F_Flag_MF)'Old
          and then Present (Ctx, F_Fragment_Offset) = Present (Ctx, F_Fragment_Offset)'Old
          and then Present (Ctx, F_TTL) = Present (Ctx, F_TTL)'Old
          and then Present (Ctx, F_Protocol) = Present (Ctx, F_Protocol)'Old
          and then Present (Ctx, F_Header_Checksum) = Present (Ctx, F_Header_Checksum)'Old
          and then Present (Ctx, F_Source) = Present (Ctx, F_Source)'Old
          and then Present (Ctx, F_Destination) = Present (Ctx, F_Destination)'Old
          and then Present (Ctx, F_Options) = Present (Ctx, F_Options)'Old
          and then Present (Ctx, F_Payload) = Present (Ctx, F_Payload)'Old;

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
          and then (if Fld /= F_Version then (if Valid (Ctx, F_Version)'Old then Valid (Ctx, F_Version)))
          and then (if Fld /= F_IHL then (if Valid (Ctx, F_IHL)'Old then Valid (Ctx, F_IHL)))
          and then (if Fld /= F_DSCP then (if Valid (Ctx, F_DSCP)'Old then Valid (Ctx, F_DSCP)))
          and then (if Fld /= F_ECN then (if Valid (Ctx, F_ECN)'Old then Valid (Ctx, F_ECN)))
          and then (if Fld /= F_Total_Length then (if Valid (Ctx, F_Total_Length)'Old then Valid (Ctx, F_Total_Length)))
          and then (if Fld /= F_Identification then (if Valid (Ctx, F_Identification)'Old then Valid (Ctx, F_Identification)))
          and then (if Fld /= F_Flag_R then (if Valid (Ctx, F_Flag_R)'Old then Valid (Ctx, F_Flag_R)))
          and then (if Fld /= F_Flag_DF then (if Valid (Ctx, F_Flag_DF)'Old then Valid (Ctx, F_Flag_DF)))
          and then (if Fld /= F_Flag_MF then (if Valid (Ctx, F_Flag_MF)'Old then Valid (Ctx, F_Flag_MF)))
          and then (if Fld /= F_Fragment_Offset then (if Valid (Ctx, F_Fragment_Offset)'Old then Valid (Ctx, F_Fragment_Offset)))
          and then (if Fld /= F_TTL then (if Valid (Ctx, F_TTL)'Old then Valid (Ctx, F_TTL)))
          and then (if Fld /= F_Protocol then (if Valid (Ctx, F_Protocol)'Old then Valid (Ctx, F_Protocol)))
          and then (if Fld /= F_Header_Checksum then (if Valid (Ctx, F_Header_Checksum)'Old then Valid (Ctx, F_Header_Checksum)))
          and then (if Fld /= F_Source then (if Valid (Ctx, F_Source)'Old then Valid (Ctx, F_Source)))
          and then (if Fld /= F_Destination then (if Valid (Ctx, F_Destination)'Old then Valid (Ctx, F_Destination)))
          and then (if Fld /= F_Options then (if Valid (Ctx, F_Options)'Old then Valid (Ctx, F_Options)))
          and then (if Fld /= F_Payload then (if Valid (Ctx, F_Payload)'Old then Valid (Ctx, F_Payload)))
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

   function Get_Version (Ctx : Context) return Version with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Version);

   function Get_IHL (Ctx : Context) return IHL with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_IHL);

   function Get_DSCP (Ctx : Context) return DCSP with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_DSCP);

   function Get_ECN (Ctx : Context) return ECN with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_ECN);

   function Get_Total_Length (Ctx : Context) return Total_Length with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Total_Length);

   function Get_Identification (Ctx : Context) return Identification with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Identification);

   function Get_Flag_R (Ctx : Context) return Flag with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Flag_R);

   function Get_Flag_DF (Ctx : Context) return Flag with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Flag_DF);

   function Get_Flag_MF (Ctx : Context) return Flag with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Flag_MF);

   function Get_Fragment_Offset (Ctx : Context) return Fragment_Offset with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Fragment_Offset);

   function Get_TTL (Ctx : Context) return TTL with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_TTL);

   function Get_Protocol (Ctx : Context) return Protocol with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Protocol);

   function Get_Header_Checksum (Ctx : Context) return Header_Checksum with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Header_Checksum);

   function Get_Source (Ctx : Context) return Address with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Source);

   function Get_Destination (Ctx : Context) return Address with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Destination);

   generic
      with procedure Process_Options (Options : RFLX.Types.Bytes);
   procedure Get_Options (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Options);

   generic
      with procedure Process_Payload (Payload : RFLX.Types.Bytes);
   procedure Get_Payload (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Payload);

   procedure Switch (Ctx : in out Context; Sequence_Context : out Options_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then not Sequence_Context'Constrained
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Options),
     Post =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Present (Ctx, F_Options)
          and then Options_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Ctx.Buffer_Address = Sequence_Context.Buffer_Address;

   procedure Update (Ctx : in out Context; Sequence_Context : in out Options_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Present (Ctx, F_Options)
          and then Options_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Ctx.Buffer_Address = Sequence_Context.Buffer_Address,
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then not Options_Sequence.Has_Buffer (Sequence_Context);

   function Valid_Context (Ctx : Context) return Boolean;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Preliminary, S_Incomplete);

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Options | F_Payload | F_Final =>
               null;
            when F_Version =>
               Version_Value : Version_Base;
            when F_IHL =>
               IHL_Value : IHL_Base;
            when F_DSCP =>
               DSCP_Value : DCSP;
            when F_ECN =>
               ECN_Value : ECN;
            when F_Total_Length =>
               Total_Length_Value : Total_Length_Base;
            when F_Identification =>
               Identification_Value : Identification;
            when F_Flag_R =>
               Flag_R_Value : Flag_Base;
            when F_Flag_DF =>
               Flag_DF_Value : Flag_Base;
            when F_Flag_MF =>
               Flag_MF_Value : Flag_Base;
            when F_Fragment_Offset =>
               Fragment_Offset_Value : Fragment_Offset;
            when F_TTL =>
               TTL_Value : TTL;
            when F_Protocol =>
               Protocol_Value : Protocol_Base;
            when F_Header_Checksum =>
               Header_Checksum_Value : Header_Checksum;
            when F_Source =>
               Source_Value : Address;
            when F_Destination =>
               Destination_Value : Address;
         end case;
      end record;

   function Valid_Value (Value : Field_Dependent_Value) return Boolean is
     ((case Value.Fld is
         when F_Version =>
            Valid (Value.Version_Value),
         when F_IHL =>
            Valid (Value.IHL_Value),
         when F_DSCP =>
            Valid (Value.DSCP_Value),
         when F_ECN =>
            Valid (Value.ECN_Value),
         when F_Total_Length =>
            Valid (Value.Total_Length_Value),
         when F_Identification =>
            Valid (Value.Identification_Value),
         when F_Flag_R =>
            Valid (Value.Flag_R_Value),
         when F_Flag_DF =>
            Valid (Value.Flag_DF_Value),
         when F_Flag_MF =>
            Valid (Value.Flag_MF_Value),
         when F_Fragment_Offset =>
            Valid (Value.Fragment_Offset_Value),
         when F_TTL =>
            Valid (Value.TTL_Value),
         when F_Protocol =>
            Valid (Value.Protocol_Value),
         when F_Header_Checksum =>
            Valid (Value.Header_Checksum_Value),
         when F_Source =>
            Valid (Value.Source_Value),
         when F_Destination =>
            Valid (Value.Destination_Value),
         when F_Options | F_Payload =>
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
           when F_Version =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size,
           when F_IHL =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size,
           when F_DSCP =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size,
           when F_ECN =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size,
           when F_Total_Length =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size,
           when F_Identification =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size,
           when F_Flag_R =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_R).State = S_Valid
                   or Cursors (F_Flag_R).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size,
           when F_Flag_DF =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_R).State = S_Valid
                   or Cursors (F_Flag_R).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_DF).State = S_Valid
                   or Cursors (F_Flag_DF).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Base'Size,
           when F_Flag_MF =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_R).State = S_Valid
                   or Cursors (F_Flag_R).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_DF).State = S_Valid
                   or Cursors (F_Flag_DF).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_MF).State = S_Valid
                   or Cursors (F_Flag_MF).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Base'Size,
           when F_Fragment_Offset =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_R).State = S_Valid
                   or Cursors (F_Flag_R).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_DF).State = S_Valid
                   or Cursors (F_Flag_DF).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_MF).State = S_Valid
                   or Cursors (F_Flag_MF).State = S_Structural_Valid)
                 and then (Cursors (F_Fragment_Offset).State = S_Valid
                   or Cursors (F_Fragment_Offset).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset'Size,
           when F_TTL =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_R).State = S_Valid
                   or Cursors (F_Flag_R).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_DF).State = S_Valid
                   or Cursors (F_Flag_DF).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_MF).State = S_Valid
                   or Cursors (F_Flag_MF).State = S_Structural_Valid)
                 and then (Cursors (F_Fragment_Offset).State = S_Valid
                   or Cursors (F_Fragment_Offset).State = S_Structural_Valid)
                 and then (Cursors (F_TTL).State = S_Valid
                   or Cursors (F_TTL).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL'Size,
           when F_Protocol =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_R).State = S_Valid
                   or Cursors (F_Flag_R).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_DF).State = S_Valid
                   or Cursors (F_Flag_DF).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_MF).State = S_Valid
                   or Cursors (F_Flag_MF).State = S_Structural_Valid)
                 and then (Cursors (F_Fragment_Offset).State = S_Valid
                   or Cursors (F_Fragment_Offset).State = S_Structural_Valid)
                 and then (Cursors (F_TTL).State = S_Valid
                   or Cursors (F_TTL).State = S_Structural_Valid)
                 and then (Cursors (F_Protocol).State = S_Valid
                   or Cursors (F_Protocol).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Base'Size,
           when F_Header_Checksum =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_R).State = S_Valid
                   or Cursors (F_Flag_R).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_DF).State = S_Valid
                   or Cursors (F_Flag_DF).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_MF).State = S_Valid
                   or Cursors (F_Flag_MF).State = S_Structural_Valid)
                 and then (Cursors (F_Fragment_Offset).State = S_Valid
                   or Cursors (F_Fragment_Offset).State = S_Structural_Valid)
                 and then (Cursors (F_TTL).State = S_Valid
                   or Cursors (F_TTL).State = S_Structural_Valid)
                 and then (Cursors (F_Protocol).State = S_Valid
                   or Cursors (F_Protocol).State = S_Structural_Valid)
                 and then (Cursors (F_Header_Checksum).State = S_Valid
                   or Cursors (F_Header_Checksum).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Base'Size
                 and then (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1) = Header_Checksum'Size,
           when F_Source =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_R).State = S_Valid
                   or Cursors (F_Flag_R).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_DF).State = S_Valid
                   or Cursors (F_Flag_DF).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_MF).State = S_Valid
                   or Cursors (F_Flag_MF).State = S_Structural_Valid)
                 and then (Cursors (F_Fragment_Offset).State = S_Valid
                   or Cursors (F_Fragment_Offset).State = S_Structural_Valid)
                 and then (Cursors (F_TTL).State = S_Valid
                   or Cursors (F_TTL).State = S_Structural_Valid)
                 and then (Cursors (F_Protocol).State = S_Valid
                   or Cursors (F_Protocol).State = S_Structural_Valid)
                 and then (Cursors (F_Header_Checksum).State = S_Valid
                   or Cursors (F_Header_Checksum).State = S_Structural_Valid)
                 and then (Cursors (F_Source).State = S_Valid
                   or Cursors (F_Source).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Base'Size
                 and then (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1) = Header_Checksum'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address'Size,
           when F_Destination =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_R).State = S_Valid
                   or Cursors (F_Flag_R).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_DF).State = S_Valid
                   or Cursors (F_Flag_DF).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_MF).State = S_Valid
                   or Cursors (F_Flag_MF).State = S_Structural_Valid)
                 and then (Cursors (F_Fragment_Offset).State = S_Valid
                   or Cursors (F_Fragment_Offset).State = S_Structural_Valid)
                 and then (Cursors (F_TTL).State = S_Valid
                   or Cursors (F_TTL).State = S_Structural_Valid)
                 and then (Cursors (F_Protocol).State = S_Valid
                   or Cursors (F_Protocol).State = S_Structural_Valid)
                 and then (Cursors (F_Header_Checksum).State = S_Valid
                   or Cursors (F_Header_Checksum).State = S_Structural_Valid)
                 and then (Cursors (F_Source).State = S_Valid
                   or Cursors (F_Source).State = S_Structural_Valid)
                 and then (Cursors (F_Destination).State = S_Valid
                   or Cursors (F_Destination).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) = 5
                   or RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) > 5)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Base'Size
                 and then (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1) = Header_Checksum'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address'Size
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address'Size,
           when F_Options =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_R).State = S_Valid
                   or Cursors (F_Flag_R).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_DF).State = S_Valid
                   or Cursors (F_Flag_DF).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_MF).State = S_Valid
                   or Cursors (F_Flag_MF).State = S_Structural_Valid)
                 and then (Cursors (F_Fragment_Offset).State = S_Valid
                   or Cursors (F_Fragment_Offset).State = S_Structural_Valid)
                 and then (Cursors (F_TTL).State = S_Valid
                   or Cursors (F_TTL).State = S_Structural_Valid)
                 and then (Cursors (F_Protocol).State = S_Valid
                   or Cursors (F_Protocol).State = S_Structural_Valid)
                 and then (Cursors (F_Header_Checksum).State = S_Valid
                   or Cursors (F_Header_Checksum).State = S_Structural_Valid)
                 and then (Cursors (F_Source).State = S_Valid
                   or Cursors (F_Source).State = S_Structural_Valid)
                 and then (Cursors (F_Destination).State = S_Valid
                   or Cursors (F_Destination).State = S_Structural_Valid)
                 and then (Cursors (F_Options).State = S_Valid
                   or Cursors (F_Options).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) = 5
                   or RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) > 5)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Base'Size
                 and then (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1) = Header_Checksum'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address'Size
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address'Size,
           when F_Payload | F_Final =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Total_Length).State = S_Valid
                   or Cursors (F_Total_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Identification).State = S_Valid
                   or Cursors (F_Identification).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_R).State = S_Valid
                   or Cursors (F_Flag_R).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_DF).State = S_Valid
                   or Cursors (F_Flag_DF).State = S_Structural_Valid)
                 and then (Cursors (F_Flag_MF).State = S_Valid
                   or Cursors (F_Flag_MF).State = S_Structural_Valid)
                 and then (Cursors (F_Fragment_Offset).State = S_Valid
                   or Cursors (F_Fragment_Offset).State = S_Structural_Valid)
                 and then (Cursors (F_TTL).State = S_Valid
                   or Cursors (F_TTL).State = S_Structural_Valid)
                 and then (Cursors (F_Protocol).State = S_Valid
                   or Cursors (F_Protocol).State = S_Structural_Valid)
                 and then (Cursors (F_Header_Checksum).State = S_Valid
                   or Cursors (F_Header_Checksum).State = S_Structural_Valid)
                 and then (Cursors (F_Source).State = S_Valid
                   or Cursors (F_Source).State = S_Structural_Valid)
                 and then (Cursors (F_Destination).State = S_Valid
                   or Cursors (F_Destination).State = S_Structural_Valid)
                 and then (Cursors (F_Payload).State = S_Valid
                   or Cursors (F_Payload).State = S_Structural_Valid)
                 and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) = 5
                   or RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) > 5)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Base'Size
                 and then (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1) = Header_Checksum'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address'Size
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address'Size));

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

end RFLX.IPv4.Generic_Packet;

with RFLX.Types;
use type RFLX.Types.Integer_Address;
with RFLX.Message_Sequence;

generic
   with package Options_Sequence is new Message_Sequence (<>);
package RFLX.IPv4.Generic_Packet with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   type All_Field_Type is (F_Initial, F_Version, F_IHL, F_DSCP, F_ECN, F_Total_Length, F_Identification, F_Flag_R, F_Flag_DF, F_Flag_MF, F_Fragment_Offset, F_TTL, F_Protocol, F_Header_Checksum, F_Source, F_Destination, F_Options, F_Payload, F_Final);

   subtype Field_Type is All_Field_Type range F_Version .. F_Payload;

   type Context_Type (Buffer_First, Buffer_Last : RFLX.Types.Index_Type := RFLX.Types.Index_Type'First; First, Last : RFLX.Types.Bit_Index_Type := RFLX.Types.Bit_Index_Type'First; Buffer_Address : RFLX.Types.Integer_Address := 0) is private with
     Default_Initial_Condition =>
       False;

   function Create return Context_Type;

   procedure Initialize (Context : out Context_Type; Buffer : in out RFLX.Types.Bytes_Ptr) with
     Pre =>
       not Context'Constrained
          and then Buffer /= null
          and then Buffer'Last <= RFLX.Types.Index_Type'Last / 2,
     Post =>
       Valid_Context (Context)
          and then Has_Buffer (Context)
          and then Context.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and then Context.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and then Buffer = null;

   procedure Initialize (Context : out Context_Type; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index_Type) with
     Pre =>
       not Context'Constrained
          and then Buffer /= null
          and then RFLX.Types.Byte_Index (First) >= Buffer'First
          and then RFLX.Types.Byte_Index (Last) <= Buffer'Last
          and then First <= Last
          and then Last <= RFLX.Types.Bit_Index_Type'Last / 2,
     Post =>
       Valid_Context (Context)
          and then Buffer = null
          and then Has_Buffer (Context)
          and then Context.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and then Context.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and then Context.Buffer_Address = RFLX.Types.Bytes_Address (Buffer)'Old
          and then Context.First = First
          and then Context.Last = Last;

   procedure Take_Buffer (Context : in out Context_Type; Buffer : out RFLX.Types.Bytes_Ptr) with
     Pre =>
       Valid_Context (Context)
          and then Has_Buffer (Context),
     Post =>
       Valid_Context (Context)
          and then not Has_Buffer (Context)
          and then Buffer /= null
          and then Context.Buffer_First = Buffer'First
          and then Context.Buffer_Last = Buffer'Last
          and then Context.Buffer_Address = RFLX.Types.Bytes_Address (Buffer)
          and then Context.Buffer_First = Context.Buffer_First'Old
          and then Context.Buffer_Last = Context.Buffer_Last'Old
          and then Context.Buffer_Address = Context.Buffer_Address'Old
          and then Context.First = Context.First'Old
          and then Context.Last = Context.Last'Old
          and then Present (Context, F_Version) = Present (Context, F_Version)'Old
          and then Present (Context, F_IHL) = Present (Context, F_IHL)'Old
          and then Present (Context, F_DSCP) = Present (Context, F_DSCP)'Old
          and then Present (Context, F_ECN) = Present (Context, F_ECN)'Old
          and then Present (Context, F_Total_Length) = Present (Context, F_Total_Length)'Old
          and then Present (Context, F_Identification) = Present (Context, F_Identification)'Old
          and then Present (Context, F_Flag_R) = Present (Context, F_Flag_R)'Old
          and then Present (Context, F_Flag_DF) = Present (Context, F_Flag_DF)'Old
          and then Present (Context, F_Flag_MF) = Present (Context, F_Flag_MF)'Old
          and then Present (Context, F_Fragment_Offset) = Present (Context, F_Fragment_Offset)'Old
          and then Present (Context, F_TTL) = Present (Context, F_TTL)'Old
          and then Present (Context, F_Protocol) = Present (Context, F_Protocol)'Old
          and then Present (Context, F_Header_Checksum) = Present (Context, F_Header_Checksum)'Old
          and then Present (Context, F_Source) = Present (Context, F_Source)'Old
          and then Present (Context, F_Destination) = Present (Context, F_Destination)'Old
          and then Present (Context, F_Options) = Present (Context, F_Options)'Old
          and then Present (Context, F_Payload) = Present (Context, F_Payload)'Old;

   function Has_Buffer (Context : Context_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   procedure Field_Range (Context : Context_Type; Field : Field_Type; First : out RFLX.Types.Bit_Index_Type; Last : out RFLX.Types.Bit_Index_Type) with
     Pre =>
       Valid_Context (Context)
          and then Present (Context, Field),
     Post =>
       Present (Context, Field)
          and then Context.First <= First
          and then Context.Last >= Last
          and then First <= Last;

   function Index (Context : Context_Type) return RFLX.Types.Bit_Index_Type with
     Pre =>
       Valid_Context (Context),
     Post =>
       Index'Result >= Context.First
          and then Index'Result - Context.Last <= 1;

   procedure Verify (Context : in out Context_Type; Field : Field_Type) with
     Pre =>
       Valid_Context (Context),
     Post =>
       Valid_Context (Context)
          and then (if Field /= F_Version then (if Valid (Context, F_Version)'Old then Valid (Context, F_Version)))
          and then (if Field /= F_IHL then (if Valid (Context, F_IHL)'Old then Valid (Context, F_IHL)))
          and then (if Field /= F_DSCP then (if Valid (Context, F_DSCP)'Old then Valid (Context, F_DSCP)))
          and then (if Field /= F_ECN then (if Valid (Context, F_ECN)'Old then Valid (Context, F_ECN)))
          and then (if Field /= F_Total_Length then (if Valid (Context, F_Total_Length)'Old then Valid (Context, F_Total_Length)))
          and then (if Field /= F_Identification then (if Valid (Context, F_Identification)'Old then Valid (Context, F_Identification)))
          and then (if Field /= F_Flag_R then (if Valid (Context, F_Flag_R)'Old then Valid (Context, F_Flag_R)))
          and then (if Field /= F_Flag_DF then (if Valid (Context, F_Flag_DF)'Old then Valid (Context, F_Flag_DF)))
          and then (if Field /= F_Flag_MF then (if Valid (Context, F_Flag_MF)'Old then Valid (Context, F_Flag_MF)))
          and then (if Field /= F_Fragment_Offset then (if Valid (Context, F_Fragment_Offset)'Old then Valid (Context, F_Fragment_Offset)))
          and then (if Field /= F_TTL then (if Valid (Context, F_TTL)'Old then Valid (Context, F_TTL)))
          and then (if Field /= F_Protocol then (if Valid (Context, F_Protocol)'Old then Valid (Context, F_Protocol)))
          and then (if Field /= F_Header_Checksum then (if Valid (Context, F_Header_Checksum)'Old then Valid (Context, F_Header_Checksum)))
          and then (if Field /= F_Source then (if Valid (Context, F_Source)'Old then Valid (Context, F_Source)))
          and then (if Field /= F_Destination then (if Valid (Context, F_Destination)'Old then Valid (Context, F_Destination)))
          and then (if Field /= F_Options then (if Valid (Context, F_Options)'Old then Valid (Context, F_Options)))
          and then (if Field /= F_Payload then (if Valid (Context, F_Payload)'Old then Valid (Context, F_Payload)))
          and then Has_Buffer (Context) = Has_Buffer (Context)'Old
          and then Context.Buffer_First = Context.Buffer_First'Old
          and then Context.Buffer_Last = Context.Buffer_Last'Old
          and then Context.Buffer_Address = Context.Buffer_Address'Old
          and then Context.First = Context.First'Old
          and then Context.Last = Context.Last'Old;

   procedure Verify_Message (Context : in out Context_Type) with
     Pre =>
       Valid_Context (Context),
     Post =>
       Valid_Context (Context)
          and then Has_Buffer (Context) = Has_Buffer (Context)'Old
          and then Context.Buffer_First = Context.Buffer_First'Old
          and then Context.Buffer_Last = Context.Buffer_Last'Old
          and then Context.Buffer_Address = Context.Buffer_Address'Old
          and then Context.First = Context.First'Old
          and then Context.Last = Context.Last'Old;

   function Present (Context : Context_Type; Field : Field_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Structural_Valid (Context : Context_Type; Field : Field_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Valid (Context : Context_Type; Field : Field_Type) return Boolean with
     Pre =>
       Valid_Context (Context),
     Post =>
       (if Valid'Result then Present (Context, Field)
          and then Structural_Valid (Context, Field));

   function Incomplete (Context : Context_Type; Field : Field_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Structural_Valid_Message (Context : Context_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Valid_Message (Context : Context_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Incomplete_Message (Context : Context_Type) return Boolean with
     Pre =>
       Valid_Context (Context);

   function Get_Version (Context : Context_Type) return Version_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Version);

   function Get_IHL (Context : Context_Type) return IHL_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_IHL);

   function Get_DSCP (Context : Context_Type) return DCSP_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_DSCP);

   function Get_ECN (Context : Context_Type) return ECN_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_ECN);

   function Get_Total_Length (Context : Context_Type) return Total_Length_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Total_Length);

   function Get_Identification (Context : Context_Type) return Identification_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Identification);

   function Get_Flag_R (Context : Context_Type) return Flag_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Flag_R);

   function Get_Flag_DF (Context : Context_Type) return Flag_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Flag_DF);

   function Get_Flag_MF (Context : Context_Type) return Flag_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Flag_MF);

   function Get_Fragment_Offset (Context : Context_Type) return Fragment_Offset_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Fragment_Offset);

   function Get_TTL (Context : Context_Type) return TTL_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_TTL);

   function Get_Protocol (Context : Context_Type) return Protocol_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Protocol);

   function Get_Header_Checksum (Context : Context_Type) return Header_Checksum_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Header_Checksum);

   function Get_Source (Context : Context_Type) return Address_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Source);

   function Get_Destination (Context : Context_Type) return Address_Type with
     Pre =>
       Valid_Context (Context)
          and then Valid (Context, F_Destination);

   generic
      with procedure Process_Options (Options : RFLX.Types.Bytes);
   procedure Get_Options (Context : Context_Type) with
     Pre =>
       Valid_Context (Context)
          and then Has_Buffer (Context)
          and then Present (Context, F_Options);

   generic
      with procedure Process_Payload (Payload : RFLX.Types.Bytes);
   procedure Get_Payload (Context : Context_Type) with
     Pre =>
       Valid_Context (Context)
          and then Has_Buffer (Context)
          and then Present (Context, F_Payload);

   procedure Switch (Context : in out Context_Type; Sequence_Context : out Options_Sequence.Context_Type) with
     Pre =>
       Valid_Context (Context)
          and then not Context'Constrained
          and then not Sequence_Context'Constrained
          and then Has_Buffer (Context)
          and then Present (Context, F_Options),
     Post =>
       Valid_Context (Context)
          and then not Has_Buffer (Context)
          and then Present (Context, F_Options)
          and then Options_Sequence.Has_Buffer (Sequence_Context)
          and then Context.Buffer_First = Sequence_Context.Buffer_First
          and then Context.Buffer_Last = Sequence_Context.Buffer_Last
          and then Context.Buffer_Address = Sequence_Context.Buffer_Address;

   procedure Update (Context : in out Context_Type; Sequence_Context : in out Options_Sequence.Context_Type) with
     Pre =>
       Valid_Context (Context)
          and then not Has_Buffer (Context)
          and then Present (Context, F_Options)
          and then Options_Sequence.Has_Buffer (Sequence_Context)
          and then Context.Buffer_First = Sequence_Context.Buffer_First
          and then Context.Buffer_Last = Sequence_Context.Buffer_Last
          and then Context.Buffer_Address = Sequence_Context.Buffer_Address,
     Post =>
       Valid_Context (Context)
          and then Has_Buffer (Context)
          and then not Options_Sequence.Has_Buffer (Sequence_Context);

   function Valid_Context (Context : Context_Type) return Boolean;

private

   type State_Type is (S_Valid, S_Structural_Valid, S_Invalid, S_Preliminary, S_Incomplete);

   type Result_Type (Field : All_Field_Type := F_Initial) is
      record
         case Field is
            when F_Initial | F_Options | F_Payload | F_Final =>
               null;
            when F_Version =>
               Version_Value : Version_Type_Base;
            when F_IHL =>
               IHL_Value : IHL_Type_Base;
            when F_DSCP =>
               DSCP_Value : DCSP_Type;
            when F_ECN =>
               ECN_Value : ECN_Type;
            when F_Total_Length =>
               Total_Length_Value : Total_Length_Type_Base;
            when F_Identification =>
               Identification_Value : Identification_Type;
            when F_Flag_R =>
               Flag_R_Value : Flag_Type_Base;
            when F_Flag_DF =>
               Flag_DF_Value : Flag_Type_Base;
            when F_Flag_MF =>
               Flag_MF_Value : Flag_Type_Base;
            when F_Fragment_Offset =>
               Fragment_Offset_Value : Fragment_Offset_Type;
            when F_TTL =>
               TTL_Value : TTL_Type;
            when F_Protocol =>
               Protocol_Value : Protocol_Type_Base;
            when F_Header_Checksum =>
               Header_Checksum_Value : Header_Checksum_Type;
            when F_Source =>
               Source_Value : Address_Type;
            when F_Destination =>
               Destination_Value : Address_Type;
         end case;
      end record;

   function Valid_Type (Value : Result_Type) return Boolean is
     ((case Value.Field is
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

   type Cursor_Type (State : State_Type := S_Invalid) is
      record
         case State is
            when S_Valid | S_Structural_Valid | S_Preliminary =>
               First : RFLX.Types.Bit_Index_Type;
               Last : RFLX.Types.Bit_Length_Type;
               Value : Result_Type;
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if State = S_Valid
          or State = S_Structural_Valid then Valid_Type (Value));

   type Cursors_Type is array (Field_Type) of Cursor_Type;

   function Valid_Context (Buffer_First, Buffer_Last : RFLX.Types.Index_Type; First, Last : RFLX.Types.Bit_Index_Type; Buffer_Address : RFLX.Types.Integer_Address; Buffer : RFLX.Types.Bytes_Ptr; Index : RFLX.Types.Bit_Index_Type; Field : All_Field_Type; Cursors : Cursors_Type) return Boolean is
     ((if Buffer /= null then Buffer'First = Buffer_First
        and then Buffer'Last = Buffer_Last
        and then RFLX.Types.Bytes_Address (Buffer) = Buffer_Address)
      and then RFLX.Types.Byte_Index (First) >= Buffer_First
      and then RFLX.Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= RFLX.Types.Bit_Index_Type'Last / 2
      and then Index >= First
      and then Index - Last <= 1
      and then (for all F in Field_Type'First .. Field_Type'Last =>
        (if Cursors (F).State = S_Valid
        or Cursors (F).State = S_Structural_Valid then Cursors (F).First >= First
        and then Cursors (F).Last <= Last
        and then Cursors (F).First <= (Cursors (F).Last + 1)
        and then Cursors (F).Value.Field = F))
      and then (case Field is
           when F_Initial =>
              True,
           when F_Version =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size,
           when F_IHL =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size,
           when F_DSCP =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size,
           when F_ECN =>
              (Cursors (F_Version).State = S_Valid
                   or Cursors (F_Version).State = S_Structural_Valid)
                 and then (Cursors (F_IHL).State = S_Valid
                   or Cursors (F_IHL).State = S_Structural_Valid)
                 and then (Cursors (F_DSCP).State = S_Valid
                   or Cursors (F_DSCP).State = S_Structural_Valid)
                 and then (Cursors (F_ECN).State = S_Valid
                   or Cursors (F_ECN).State = S_Structural_Valid)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Type_Base'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Type_Base'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Type_Base'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset_Type'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset_Type'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL_Type'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset_Type'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL_Type'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Type_Base'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset_Type'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL_Type'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Type_Base'Size
                 and then (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1) = Header_Checksum_Type'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset_Type'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL_Type'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Type_Base'Size
                 and then (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1) = Header_Checksum_Type'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address_Type'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) = 5
                   or RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) > 5)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset_Type'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL_Type'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Type_Base'Size
                 and then (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1) = Header_Checksum_Type'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address_Type'Size
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address_Type'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) = 5
                   or RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) > 5)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset_Type'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL_Type'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Type_Base'Size
                 and then (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1) = Header_Checksum_Type'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address_Type'Size
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address_Type'Size,
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
                 and then RFLX.Types.Bit_Length_Type (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) * 4
                 and then Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
                 and then (RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) = 5
                   or RFLX.Types.Bit_Length_Type (Cursors (F_IHL).Value.IHL_Value) > 5)
                 and then (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Type_Base'Size
                 and then (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Type_Base'Size
                 and then (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP_Type'Size
                 and then (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN_Type'Size
                 and then (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Type_Base'Size
                 and then (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification_Type'Size
                 and then (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Type_Base'Size
                 and then (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset_Type'Size
                 and then (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL_Type'Size
                 and then (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Type_Base'Size
                 and then (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1) = Header_Checksum_Type'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address_Type'Size
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address_Type'Size));

   type Context_Type (Buffer_First, Buffer_Last : RFLX.Types.Index_Type := RFLX.Types.Index_Type'First; First, Last : RFLX.Types.Bit_Index_Type := RFLX.Types.Bit_Index_Type'First; Buffer_Address : RFLX.Types.Integer_Address := 0) is
      record
         Buffer : RFLX.Types.Bytes_Ptr := null;
         Index : RFLX.Types.Bit_Index_Type := RFLX.Types.Bit_Index_Type'First;
         Field : All_Field_Type := F_Initial;
         Cursors : Cursors_Type := (others => (State => S_Invalid));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Buffer_First, Buffer_Last, First, Last, Buffer_Address, Buffer, Index, Field, Cursors);

   function Valid_Context (Context : Context_Type) return Boolean is
     (Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Buffer_Address, Context.Buffer, Context.Index, Context.Field, Context.Cursors));

end RFLX.IPv4.Generic_Packet;

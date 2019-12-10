with RFLX.Types;
use type RFLX.Types.Integer_Address;
with RFLX.Message_Sequence;

generic
   with package Options_Sequence is new Message_Sequence (<>);
package RFLX.IPv4.Generic_Packet with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   pragma Annotate (GNATprove, Terminating, Generic_Packet);

   type Virtual_Field is (F_Initial, F_Version, F_IHL, F_DSCP, F_ECN, F_Total_Length, F_Identification, F_Flag_R, F_Flag_DF, F_Flag_MF, F_Fragment_Offset, F_TTL, F_Protocol, F_Header_Checksum, F_Source, F_Destination, F_Options, F_Payload, F_Final);

   subtype Field is Virtual_Field range F_Version .. F_Payload;

   type Field_Cursors is private;

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       False;

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
          and then Ctx.First = RFLX.Types.First_Bit_Index (Ctx.Buffer_First)
          and then Available_Space (Ctx, F_Version) = (RFLX.Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
          and then Valid_Predecessor (Ctx, F_Version)
          and then Path_Condition (Ctx, F_Version)
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
          and then Ctx.First = First
          and then Ctx.Last = Last
          and then Available_Space (Ctx, F_Version) = (RFLX.Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
          and then Valid_Predecessor (Ctx, F_Version)
          and then Path_Condition (Ctx, F_Version);

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
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.First = Ctx.First'Old
          and then Ctx.Last = Ctx.Last'Old
          and then Cursors (Ctx) = Cursors (Ctx)'Old;

   function Has_Buffer (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Message_Last (Ctx : Context) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and then Structural_Valid_Message (Ctx);

   procedure Field_Range (Ctx : Context; Fld : Field; First : out RFLX.Types.Bit_Index; Last : out RFLX.Types.Bit_Index) with
     Pre =>
       Valid_Context (Ctx)
          and then Present (Ctx, Fld),
     Post =>
       Present (Ctx, Fld)
          and then Ctx.First <= First
          and then Ctx.Last >= Last
          and then First <= Last;

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and then Valid_Predecessor (Ctx, Fld);

   function Field_Condition (Ctx : Context; Value : Field_Dependent_Value) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and then Value.Fld in Field'Range
          and then Valid_Predecessor (Ctx, Value.Fld);

   function Field_Length (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and then Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and then Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Available_Space (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and then Valid_Predecessor (Ctx, Fld)
          and then Path_Condition (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
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
       (if Valid'Result then
           Structural_Valid (Ctx, Fld)
             and then Present (Ctx, Fld));

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
          and then Options_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Present (Ctx, F_Options)
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Cursors (Ctx) = Cursors (Ctx)'Old;

   procedure Update (Ctx : in out Context; Sequence_Context : in out Options_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Has_Buffer (Ctx)
          and then Options_Sequence.Has_Buffer (Sequence_Context)
          and then Ctx.Buffer_First = Sequence_Context.Buffer_First
          and then Ctx.Buffer_Last = Sequence_Context.Buffer_Last
          and then Present (Ctx, F_Options),
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then not Options_Sequence.Has_Buffer (Sequence_Context);

   function Valid_Context (Ctx : Context) return Boolean;

   function Cursors (Ctx : Context) return Field_Cursors with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Incomplete);

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
         Predecessor : Virtual_Field := F_Final;
         case State is
            when S_Valid | S_Structural_Valid =>
               First : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First;
               Last : RFLX.Types.Bit_Length := RFLX.Types.Bit_Length'First;
               Value : Field_Dependent_Value := (Fld => F_Final);
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if State = S_Valid
             or State = S_Structural_Valid then
           Valid_Value (Value));

   type Field_Cursors is array (Virtual_Field) of Field_Cursor;

   function Structural_Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid
      or Cursor.State = S_Structural_Valid);

   function Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid);

   function Invalid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Invalid
      or Cursor.State = S_Incomplete);

   function Valid_Context (Buffer_First, Buffer_Last : RFLX.Types.Index; First, Last : RFLX.Types.Bit_Index; Buffer : RFLX.Types.Bytes_Ptr; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then
         Buffer'First = Buffer_First
           and then Buffer'Last = Buffer_Last)
      and then RFLX.Types.Byte_Index (First) >= Buffer_First
      and then RFLX.Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= RFLX.Types.Bit_Index'Last / 2
      and then (for all F in Field'First .. Field'Last =>
        (if Structural_Valid (Cursors (F)) then
         Cursors (F).First >= First
           and then Cursors (F).Last <= Last
           and then Cursors (F).First <= (Cursors (F).Last + 1)
           and then Cursors (F).Value.Fld = F))
      and then ((if Structural_Valid (Cursors (F_IHL)) then
           (Valid (Cursors (F_Version))
               and then Cursors (F_IHL).Predecessor = F_Version))
        and then (if Structural_Valid (Cursors (F_DSCP)) then
           (Valid (Cursors (F_IHL))
               and then Cursors (F_DSCP).Predecessor = F_IHL))
        and then (if Structural_Valid (Cursors (F_ECN)) then
           (Valid (Cursors (F_DSCP))
               and then Cursors (F_ECN).Predecessor = F_DSCP))
        and then (if Structural_Valid (Cursors (F_Total_Length)) then
           (Valid (Cursors (F_ECN))
               and then Cursors (F_Total_Length).Predecessor = F_ECN))
        and then (if Structural_Valid (Cursors (F_Identification)) then
           (Valid (Cursors (F_Total_Length))
               and then Cursors (F_Identification).Predecessor = F_Total_Length
               and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4))
        and then (if Structural_Valid (Cursors (F_Flag_R)) then
           (Valid (Cursors (F_Identification))
               and then Cursors (F_Flag_R).Predecessor = F_Identification))
        and then (if Structural_Valid (Cursors (F_Flag_DF)) then
           (Valid (Cursors (F_Flag_R))
               and then Cursors (F_Flag_DF).Predecessor = F_Flag_R
               and then RFLX.Types.Bit_Length (Cursors (F_Flag_R).Value.Flag_R_Value) = RFLX.Types.Bit_Length (Convert (Flag_False))))
        and then (if Structural_Valid (Cursors (F_Flag_MF)) then
           (Valid (Cursors (F_Flag_DF))
               and then Cursors (F_Flag_MF).Predecessor = F_Flag_DF))
        and then (if Structural_Valid (Cursors (F_Fragment_Offset)) then
           (Valid (Cursors (F_Flag_MF))
               and then Cursors (F_Fragment_Offset).Predecessor = F_Flag_MF))
        and then (if Structural_Valid (Cursors (F_TTL)) then
           (Valid (Cursors (F_Fragment_Offset))
               and then Cursors (F_TTL).Predecessor = F_Fragment_Offset))
        and then (if Structural_Valid (Cursors (F_Protocol)) then
           (Valid (Cursors (F_TTL))
               and then Cursors (F_Protocol).Predecessor = F_TTL))
        and then (if Structural_Valid (Cursors (F_Header_Checksum)) then
           (Valid (Cursors (F_Protocol))
               and then Cursors (F_Header_Checksum).Predecessor = F_Protocol))
        and then (if Structural_Valid (Cursors (F_Source)) then
           (Valid (Cursors (F_Header_Checksum))
               and then Cursors (F_Source).Predecessor = F_Header_Checksum))
        and then (if Structural_Valid (Cursors (F_Destination)) then
           (Valid (Cursors (F_Source))
               and then Cursors (F_Destination).Predecessor = F_Source))
        and then (if Structural_Valid (Cursors (F_Options)) then
           (Valid (Cursors (F_Destination))
               and then Cursors (F_Options).Predecessor = F_Destination
               and then RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) > 5))
        and then (if Structural_Valid (Cursors (F_Payload)) then
           (Valid (Cursors (F_Destination))
               and then Cursors (F_Payload).Predecessor = F_Destination
               and then RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) = 5)
             or (Structural_Valid (Cursors (F_Options))
               and then Cursors (F_Payload).Predecessor = F_Options)))
      and then ((if Invalid (Cursors (F_Version)) then
           Invalid (Cursors (F_IHL)))
        and then (if Invalid (Cursors (F_IHL)) then
           Invalid (Cursors (F_DSCP)))
        and then (if Invalid (Cursors (F_DSCP)) then
           Invalid (Cursors (F_ECN)))
        and then (if Invalid (Cursors (F_ECN)) then
           Invalid (Cursors (F_Total_Length)))
        and then (if Invalid (Cursors (F_Total_Length)) then
           Invalid (Cursors (F_Identification)))
        and then (if Invalid (Cursors (F_Identification)) then
           Invalid (Cursors (F_Flag_R)))
        and then (if Invalid (Cursors (F_Flag_R)) then
           Invalid (Cursors (F_Flag_DF)))
        and then (if Invalid (Cursors (F_Flag_DF)) then
           Invalid (Cursors (F_Flag_MF)))
        and then (if Invalid (Cursors (F_Flag_MF)) then
           Invalid (Cursors (F_Fragment_Offset)))
        and then (if Invalid (Cursors (F_Fragment_Offset)) then
           Invalid (Cursors (F_TTL)))
        and then (if Invalid (Cursors (F_TTL)) then
           Invalid (Cursors (F_Protocol)))
        and then (if Invalid (Cursors (F_Protocol)) then
           Invalid (Cursors (F_Header_Checksum)))
        and then (if Invalid (Cursors (F_Header_Checksum)) then
           Invalid (Cursors (F_Source)))
        and then (if Invalid (Cursors (F_Source)) then
           Invalid (Cursors (F_Destination)))
        and then (if Invalid (Cursors (F_Destination)) then
           Invalid (Cursors (F_Options)))
        and then (if Invalid (Cursors (F_Destination))
             and then Invalid (Cursors (F_Options)) then
           Invalid (Cursors (F_Payload))))
      and then (if Structural_Valid (Cursors (F_Version)) then
         (Cursors (F_Version).Last - Cursors (F_Version).First + 1) = Version_Base'Size
           and then Cursors (F_Version).Predecessor = F_Initial
           and then Cursors (F_Version).First = First
           and then (if Structural_Valid (Cursors (F_IHL)) then
              (Cursors (F_IHL).Last - Cursors (F_IHL).First + 1) = IHL_Base'Size
                and then Cursors (F_IHL).Predecessor = F_Version
                and then Cursors (F_IHL).First = (Cursors (F_Version).Last + 1)
                and then (if Structural_Valid (Cursors (F_DSCP)) then
                   (Cursors (F_DSCP).Last - Cursors (F_DSCP).First + 1) = DCSP'Size
                     and then Cursors (F_DSCP).Predecessor = F_IHL
                     and then Cursors (F_DSCP).First = (Cursors (F_IHL).Last + 1)
                     and then (if Structural_Valid (Cursors (F_ECN)) then
                        (Cursors (F_ECN).Last - Cursors (F_ECN).First + 1) = ECN'Size
                          and then Cursors (F_ECN).Predecessor = F_DSCP
                          and then Cursors (F_ECN).First = (Cursors (F_DSCP).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Total_Length)) then
                             (Cursors (F_Total_Length).Last - Cursors (F_Total_Length).First + 1) = Total_Length_Base'Size
                               and then Cursors (F_Total_Length).Predecessor = F_ECN
                               and then Cursors (F_Total_Length).First = (Cursors (F_ECN).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Identification))
                                    and then RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 4 then
                                  (Cursors (F_Identification).Last - Cursors (F_Identification).First + 1) = Identification'Size
                                    and then Cursors (F_Identification).Predecessor = F_Total_Length
                                    and then Cursors (F_Identification).First = (Cursors (F_Total_Length).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Flag_R)) then
                                       (Cursors (F_Flag_R).Last - Cursors (F_Flag_R).First + 1) = Flag_Base'Size
                                         and then Cursors (F_Flag_R).Predecessor = F_Identification
                                         and then Cursors (F_Flag_R).First = (Cursors (F_Identification).Last + 1)
                                         and then (if Structural_Valid (Cursors (F_Flag_DF))
                                              and then RFLX.Types.Bit_Length (Cursors (F_Flag_R).Value.Flag_R_Value) = RFLX.Types.Bit_Length (Convert (Flag_False)) then
                                            (Cursors (F_Flag_DF).Last - Cursors (F_Flag_DF).First + 1) = Flag_Base'Size
                                              and then Cursors (F_Flag_DF).Predecessor = F_Flag_R
                                              and then Cursors (F_Flag_DF).First = (Cursors (F_Flag_R).Last + 1)
                                              and then (if Structural_Valid (Cursors (F_Flag_MF)) then
                                                 (Cursors (F_Flag_MF).Last - Cursors (F_Flag_MF).First + 1) = Flag_Base'Size
                                                   and then Cursors (F_Flag_MF).Predecessor = F_Flag_DF
                                                   and then Cursors (F_Flag_MF).First = (Cursors (F_Flag_DF).Last + 1)
                                                   and then (if Structural_Valid (Cursors (F_Fragment_Offset)) then
                                                      (Cursors (F_Fragment_Offset).Last - Cursors (F_Fragment_Offset).First + 1) = Fragment_Offset'Size
                                                        and then Cursors (F_Fragment_Offset).Predecessor = F_Flag_MF
                                                        and then Cursors (F_Fragment_Offset).First = (Cursors (F_Flag_MF).Last + 1)
                                                        and then (if Structural_Valid (Cursors (F_TTL)) then
                                                           (Cursors (F_TTL).Last - Cursors (F_TTL).First + 1) = TTL'Size
                                                             and then Cursors (F_TTL).Predecessor = F_Fragment_Offset
                                                             and then Cursors (F_TTL).First = (Cursors (F_Fragment_Offset).Last + 1)
                                                             and then (if Structural_Valid (Cursors (F_Protocol)) then
                                                                (Cursors (F_Protocol).Last - Cursors (F_Protocol).First + 1) = Protocol_Base'Size
                                                                  and then Cursors (F_Protocol).Predecessor = F_TTL
                                                                  and then Cursors (F_Protocol).First = (Cursors (F_TTL).Last + 1)
                                                                  and then (if Structural_Valid (Cursors (F_Header_Checksum)) then
                                                                     (Cursors (F_Header_Checksum).Last - Cursors (F_Header_Checksum).First + 1) = Header_Checksum'Size
                                                                       and then Cursors (F_Header_Checksum).Predecessor = F_Protocol
                                                                       and then Cursors (F_Header_Checksum).First = (Cursors (F_Protocol).Last + 1)
                                                                       and then (if Structural_Valid (Cursors (F_Source)) then
                                                                          (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address'Size
                                                                            and then Cursors (F_Source).Predecessor = F_Header_Checksum
                                                                            and then Cursors (F_Source).First = (Cursors (F_Header_Checksum).Last + 1)
                                                                            and then (if Structural_Valid (Cursors (F_Destination)) then
                                                                               (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address'Size
                                                                                 and then Cursors (F_Destination).Predecessor = F_Source
                                                                                 and then Cursors (F_Destination).First = (Cursors (F_Source).Last + 1)
                                                                                 and then (if Structural_Valid (Cursors (F_Payload))
                                                                                      and then RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) = 5 then
                                                                                    (Cursors (F_Payload).Last - Cursors (F_Payload).First + 1) = (RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) * 8 - 160)
                                                                                      and then Cursors (F_Payload).Predecessor = F_Destination
                                                                                      and then Cursors (F_Payload).First = (Cursors (F_Destination).Last + 1))
                                                                                 and then (if Structural_Valid (Cursors (F_Options))
                                                                                      and then RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) > 5 then
                                                                                    (Cursors (F_Options).Last - Cursors (F_Options).First + 1) = (RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * 32 - 160)
                                                                                      and then Cursors (F_Options).Predecessor = F_Destination
                                                                                      and then Cursors (F_Options).First = (Cursors (F_Destination).Last + 1)
                                                                                      and then (if Structural_Valid (Cursors (F_Payload)) then
                                                                                         (Cursors (F_Payload).Last - Cursors (F_Payload).First + 1) = (RFLX.Types.Bit_Length (Cursors (F_Total_Length).Value.Total_Length_Value) * 8 + RFLX.Types.Bit_Length (Cursors (F_IHL).Value.IHL_Value) * (-32))
                                                                                           and then Cursors (F_Payload).Predecessor = F_Options
                                                                                           and then Cursors (F_Payload).First = (Cursors (F_Options).Last + 1)))))))))))))))))));

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First) is
      record
         Buffer : RFLX.Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Buffer_First, Buffer_Last, First, Last, Buffer, Cursors);

   function Valid_Context (Ctx : Context) return Boolean is
     (Valid_Context (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last, Ctx.Buffer, Ctx.Cursors));

   function Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.IPv4.Generic_Packet;

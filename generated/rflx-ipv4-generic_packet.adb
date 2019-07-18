package body RFLX.IPv4.Generic_Packet with
  SPARK_Mode
is

   function Create return Context_Type is
     ((RFLX.Types.Index_Type'First, RFLX.Types.Index_Type'First, RFLX.Types.Bit_Index_Type'First, RFLX.Types.Bit_Index_Type'First, 0, null, RFLX.Types.Bit_Index_Type'First, F_Initial, (others => (State => S_Invalid))));

   procedure Initialize (Context : out Context_Type; Buffer : in out RFLX.Types.Bytes_Ptr) is
   begin
      Initialize (Context, Buffer, RFLX.Types.First_Bit_Index (Buffer'First), RFLX.Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Context : out Context_Type; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index_Type) is
      Buffer_First : constant RFLX.Types.Index_Type := Buffer'First;
      Buffer_Last : constant RFLX.Types.Index_Type := Buffer'Last;
      Buffer_Address : constant RFLX.Types.Integer_Address := RFLX.Types.Bytes_Address (Buffer);
   begin
      Context := (Buffer_First, Buffer_Last, First, Last, Buffer_Address, Buffer, First, F_Initial, (others => (State => S_Invalid)));
      Buffer := null;
   end Initialize;

   procedure Take_Buffer (Context : in out Context_Type; Buffer : out RFLX.Types.Bytes_Ptr) is
   begin
      Buffer := Context.Buffer;
      Context.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Context : Context_Type) return Boolean is
     (Context.Buffer /= null);

   procedure Field_Range (Context : Context_Type; Field : Field_Type; First : out RFLX.Types.Bit_Index_Type; Last : out RFLX.Types.Bit_Index_Type) is
   begin
      First := Context.Cursors (Field).First;
      Last := Context.Cursors (Field).Last;
   end Field_Range;

   function Index (Context : Context_Type) return RFLX.Types.Bit_Index_Type is
     (Context.Index);

   function Preliminary_Valid (Context : Context_Type; Field : Field_Type) return Boolean is
     ((Context.Cursors (Field).State = S_Valid
        or Context.Cursors (Field).State = S_Structural_Valid
        or Context.Cursors (Field).State = S_Preliminary)
      and then Context.Cursors (Field).Value.Field = Field);

   function Preliminary_Valid_Predecessors (Context : Context_Type; Field : All_Field_Type) return Boolean is
     ((case Field is
         when F_Initial | F_Version =>
            True,
         when F_IHL =>
            Preliminary_Valid (Context, F_Version),
         when F_DSCP =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL),
         when F_ECN =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP),
         when F_Total_Length =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN),
         when F_Identification =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length),
         when F_Flag_R =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length)
               and then Preliminary_Valid (Context, F_Identification),
         when F_Flag_DF =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length)
               and then Preliminary_Valid (Context, F_Identification)
               and then Preliminary_Valid (Context, F_Flag_R),
         when F_Flag_MF =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length)
               and then Preliminary_Valid (Context, F_Identification)
               and then Preliminary_Valid (Context, F_Flag_R)
               and then Preliminary_Valid (Context, F_Flag_DF),
         when F_Fragment_Offset =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length)
               and then Preliminary_Valid (Context, F_Identification)
               and then Preliminary_Valid (Context, F_Flag_R)
               and then Preliminary_Valid (Context, F_Flag_DF)
               and then Preliminary_Valid (Context, F_Flag_MF),
         when F_TTL =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length)
               and then Preliminary_Valid (Context, F_Identification)
               and then Preliminary_Valid (Context, F_Flag_R)
               and then Preliminary_Valid (Context, F_Flag_DF)
               and then Preliminary_Valid (Context, F_Flag_MF)
               and then Preliminary_Valid (Context, F_Fragment_Offset),
         when F_Protocol =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length)
               and then Preliminary_Valid (Context, F_Identification)
               and then Preliminary_Valid (Context, F_Flag_R)
               and then Preliminary_Valid (Context, F_Flag_DF)
               and then Preliminary_Valid (Context, F_Flag_MF)
               and then Preliminary_Valid (Context, F_Fragment_Offset)
               and then Preliminary_Valid (Context, F_TTL),
         when F_Header_Checksum =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length)
               and then Preliminary_Valid (Context, F_Identification)
               and then Preliminary_Valid (Context, F_Flag_R)
               and then Preliminary_Valid (Context, F_Flag_DF)
               and then Preliminary_Valid (Context, F_Flag_MF)
               and then Preliminary_Valid (Context, F_Fragment_Offset)
               and then Preliminary_Valid (Context, F_TTL)
               and then Preliminary_Valid (Context, F_Protocol),
         when F_Source =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length)
               and then Preliminary_Valid (Context, F_Identification)
               and then Preliminary_Valid (Context, F_Flag_R)
               and then Preliminary_Valid (Context, F_Flag_DF)
               and then Preliminary_Valid (Context, F_Flag_MF)
               and then Preliminary_Valid (Context, F_Fragment_Offset)
               and then Preliminary_Valid (Context, F_TTL)
               and then Preliminary_Valid (Context, F_Protocol)
               and then Preliminary_Valid (Context, F_Header_Checksum),
         when F_Destination =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length)
               and then Preliminary_Valid (Context, F_Identification)
               and then Preliminary_Valid (Context, F_Flag_R)
               and then Preliminary_Valid (Context, F_Flag_DF)
               and then Preliminary_Valid (Context, F_Flag_MF)
               and then Preliminary_Valid (Context, F_Fragment_Offset)
               and then Preliminary_Valid (Context, F_TTL)
               and then Preliminary_Valid (Context, F_Protocol)
               and then Preliminary_Valid (Context, F_Header_Checksum)
               and then Preliminary_Valid (Context, F_Source),
         when F_Options | F_Payload =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length)
               and then Preliminary_Valid (Context, F_Identification)
               and then Preliminary_Valid (Context, F_Flag_R)
               and then Preliminary_Valid (Context, F_Flag_DF)
               and then Preliminary_Valid (Context, F_Flag_MF)
               and then Preliminary_Valid (Context, F_Fragment_Offset)
               and then Preliminary_Valid (Context, F_TTL)
               and then Preliminary_Valid (Context, F_Protocol)
               and then Preliminary_Valid (Context, F_Header_Checksum)
               and then Preliminary_Valid (Context, F_Source)
               and then Preliminary_Valid (Context, F_Destination),
         when F_Final =>
            Preliminary_Valid (Context, F_Version)
               and then Preliminary_Valid (Context, F_IHL)
               and then Preliminary_Valid (Context, F_DSCP)
               and then Preliminary_Valid (Context, F_ECN)
               and then Preliminary_Valid (Context, F_Total_Length)
               and then Preliminary_Valid (Context, F_Identification)
               and then Preliminary_Valid (Context, F_Flag_R)
               and then Preliminary_Valid (Context, F_Flag_DF)
               and then Preliminary_Valid (Context, F_Flag_MF)
               and then Preliminary_Valid (Context, F_Fragment_Offset)
               and then Preliminary_Valid (Context, F_TTL)
               and then Preliminary_Valid (Context, F_Protocol)
               and then Preliminary_Valid (Context, F_Header_Checksum)
               and then Preliminary_Valid (Context, F_Source)
               and then Preliminary_Valid (Context, F_Destination)
               and then Preliminary_Valid (Context, F_Payload)));

   function Valid_Predecessors (Context : Context_Type; Field : Field_Type) return Boolean is
     ((case Field is
         when F_Version =>
            True,
         when F_IHL =>
            Present (Context, F_Version),
         when F_DSCP =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL),
         when F_ECN =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP),
         when F_Total_Length =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN),
         when F_Identification =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN)
               and then Present (Context, F_Total_Length),
         when F_Flag_R =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN)
               and then Present (Context, F_Total_Length)
               and then Present (Context, F_Identification),
         when F_Flag_DF =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN)
               and then Present (Context, F_Total_Length)
               and then Present (Context, F_Identification)
               and then Present (Context, F_Flag_R),
         when F_Flag_MF =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN)
               and then Present (Context, F_Total_Length)
               and then Present (Context, F_Identification)
               and then Present (Context, F_Flag_R)
               and then Present (Context, F_Flag_DF),
         when F_Fragment_Offset =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN)
               and then Present (Context, F_Total_Length)
               and then Present (Context, F_Identification)
               and then Present (Context, F_Flag_R)
               and then Present (Context, F_Flag_DF)
               and then Present (Context, F_Flag_MF),
         when F_TTL =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN)
               and then Present (Context, F_Total_Length)
               and then Present (Context, F_Identification)
               and then Present (Context, F_Flag_R)
               and then Present (Context, F_Flag_DF)
               and then Present (Context, F_Flag_MF)
               and then Present (Context, F_Fragment_Offset),
         when F_Protocol =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN)
               and then Present (Context, F_Total_Length)
               and then Present (Context, F_Identification)
               and then Present (Context, F_Flag_R)
               and then Present (Context, F_Flag_DF)
               and then Present (Context, F_Flag_MF)
               and then Present (Context, F_Fragment_Offset)
               and then Present (Context, F_TTL),
         when F_Header_Checksum =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN)
               and then Present (Context, F_Total_Length)
               and then Present (Context, F_Identification)
               and then Present (Context, F_Flag_R)
               and then Present (Context, F_Flag_DF)
               and then Present (Context, F_Flag_MF)
               and then Present (Context, F_Fragment_Offset)
               and then Present (Context, F_TTL)
               and then Present (Context, F_Protocol),
         when F_Source =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN)
               and then Present (Context, F_Total_Length)
               and then Present (Context, F_Identification)
               and then Present (Context, F_Flag_R)
               and then Present (Context, F_Flag_DF)
               and then Present (Context, F_Flag_MF)
               and then Present (Context, F_Fragment_Offset)
               and then Present (Context, F_TTL)
               and then Present (Context, F_Protocol)
               and then Present (Context, F_Header_Checksum),
         when F_Destination =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN)
               and then Present (Context, F_Total_Length)
               and then Present (Context, F_Identification)
               and then Present (Context, F_Flag_R)
               and then Present (Context, F_Flag_DF)
               and then Present (Context, F_Flag_MF)
               and then Present (Context, F_Fragment_Offset)
               and then Present (Context, F_TTL)
               and then Present (Context, F_Protocol)
               and then Present (Context, F_Header_Checksum)
               and then Present (Context, F_Source),
         when F_Options | F_Payload =>
            Present (Context, F_Version)
               and then Present (Context, F_IHL)
               and then Present (Context, F_DSCP)
               and then Present (Context, F_ECN)
               and then Present (Context, F_Total_Length)
               and then Present (Context, F_Identification)
               and then Present (Context, F_Flag_R)
               and then Present (Context, F_Flag_DF)
               and then Present (Context, F_Flag_MF)
               and then Present (Context, F_Fragment_Offset)
               and then Present (Context, F_TTL)
               and then Present (Context, F_Protocol)
               and then Present (Context, F_Header_Checksum)
               and then Present (Context, F_Source)
               and then Present (Context, F_Destination)))
    with
     Post =>
       (if Valid_Predecessors'Result then Preliminary_Valid_Predecessors (Context, Field));

   function Valid_Target (Source_Field, Target_Field : All_Field_Type) return Boolean is
     ((case Source_Field is
         when F_Initial =>
            Target_Field = F_Version,
         when F_Version =>
            Target_Field = F_IHL,
         when F_IHL =>
            Target_Field = F_DSCP,
         when F_DSCP =>
            Target_Field = F_ECN,
         when F_ECN =>
            Target_Field = F_Total_Length,
         when F_Total_Length =>
            Target_Field = F_Identification,
         when F_Identification =>
            Target_Field = F_Flag_R,
         when F_Flag_R =>
            Target_Field = F_Flag_DF,
         when F_Flag_DF =>
            Target_Field = F_Flag_MF,
         when F_Flag_MF =>
            Target_Field = F_Fragment_Offset,
         when F_Fragment_Offset =>
            Target_Field = F_TTL,
         when F_TTL =>
            Target_Field = F_Protocol,
         when F_Protocol =>
            Target_Field = F_Header_Checksum,
         when F_Header_Checksum =>
            Target_Field = F_Source,
         when F_Source =>
            Target_Field = F_Destination,
         when F_Destination =>
            Target_Field = F_Payload
               or Target_Field = F_Options,
         when F_Options =>
            Target_Field = F_Payload,
         when F_Payload =>
            Target_Field = F_Final,
         when F_Final =>
            False));

   function Composite_Field (Field : Field_Type) return Boolean is
     ((case Field is
         when F_Version | F_IHL | F_DSCP | F_ECN | F_Total_Length | F_Identification | F_Flag_R | F_Flag_DF | F_Flag_MF | F_Fragment_Offset | F_TTL | F_Protocol | F_Header_Checksum | F_Source | F_Destination =>
            False,
         when F_Options | F_Payload =>
            True));

   function Field_Condition (Context : Context_Type; Source_Field, Target_Field : All_Field_Type) return Boolean is
     ((case Source_Field is
         when F_Initial =>
            (case Target_Field is
                  when F_Version =>
                     True,
                  when others =>
                     False),
         when F_Version =>
            (case Target_Field is
                  when F_IHL =>
                     True,
                  when others =>
                     False),
         when F_IHL =>
            (case Target_Field is
                  when F_DSCP =>
                     True,
                  when others =>
                     False),
         when F_DSCP =>
            (case Target_Field is
                  when F_ECN =>
                     True,
                  when others =>
                     False),
         when F_ECN =>
            (case Target_Field is
                  when F_Total_Length =>
                     True,
                  when others =>
                     False),
         when F_Total_Length =>
            (case Target_Field is
                  when F_Identification =>
                     RFLX.Types.Bit_Length_Type (Context.Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Context.Cursors (F_IHL).Value.IHL_Value) * 4,
                  when others =>
                     False),
         when F_Identification =>
            (case Target_Field is
                  when F_Flag_R =>
                     True,
                  when others =>
                     False),
         when F_Flag_R =>
            (case Target_Field is
                  when F_Flag_DF =>
                     Context.Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False),
                  when others =>
                     False),
         when F_Flag_DF =>
            (case Target_Field is
                  when F_Flag_MF =>
                     True,
                  when others =>
                     False),
         when F_Flag_MF =>
            (case Target_Field is
                  when F_Fragment_Offset =>
                     True,
                  when others =>
                     False),
         when F_Fragment_Offset =>
            (case Target_Field is
                  when F_TTL =>
                     True,
                  when others =>
                     False),
         when F_TTL =>
            (case Target_Field is
                  when F_Protocol =>
                     True,
                  when others =>
                     False),
         when F_Protocol =>
            (case Target_Field is
                  when F_Header_Checksum =>
                     True,
                  when others =>
                     False),
         when F_Header_Checksum =>
            (case Target_Field is
                  when F_Source =>
                     True,
                  when others =>
                     False),
         when F_Source =>
            (case Target_Field is
                  when F_Destination =>
                     True,
                  when others =>
                     False),
         when F_Destination =>
            (case Target_Field is
                  when F_Payload =>
                     RFLX.Types.Bit_Length_Type (Context.Cursors (F_IHL).Value.IHL_Value) = 5,
                  when F_Options =>
                     RFLX.Types.Bit_Length_Type (Context.Cursors (F_IHL).Value.IHL_Value) > 5,
                  when others =>
                     False),
         when F_Options =>
            (case Target_Field is
                  when F_Payload =>
                     True,
                  when others =>
                     False),
         when F_Payload =>
            (case Target_Field is
                  when F_Final =>
                     True,
                  when others =>
                     False),
         when F_Final =>
            False))
    with
     Pre =>
       Valid_Target (Source_Field, Target_Field)
          and then Preliminary_Valid_Predecessors (Context, Target_Field);

   function Field_Length (Context : Context_Type; Field : Field_Type) return RFLX.Types.Bit_Length_Type is
     ((case Context.Field is
         when F_Initial =>
            (case Field is
                  when F_Version =>
                     Version_Type_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Version =>
            (case Field is
                  when F_IHL =>
                     IHL_Type_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_IHL =>
            (case Field is
                  when F_DSCP =>
                     DCSP_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_DSCP =>
            (case Field is
                  when F_ECN =>
                     ECN_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_ECN =>
            (case Field is
                  when F_Total_Length =>
                     Total_Length_Type_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Total_Length =>
            (case Field is
                  when F_Identification =>
                     Identification_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Identification =>
            (case Field is
                  when F_Flag_R =>
                     Flag_Type_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Flag_R =>
            (case Field is
                  when F_Flag_DF =>
                     Flag_Type_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Flag_DF =>
            (case Field is
                  when F_Flag_MF =>
                     Flag_Type_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Flag_MF =>
            (case Field is
                  when F_Fragment_Offset =>
                     Fragment_Offset_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Fragment_Offset =>
            (case Field is
                  when F_TTL =>
                     TTL_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_TTL =>
            (case Field is
                  when F_Protocol =>
                     Protocol_Type_Base'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Protocol =>
            (case Field is
                  when F_Header_Checksum =>
                     Header_Checksum_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Header_Checksum =>
            (case Field is
                  when F_Source =>
                     Address_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Source =>
            (case Field is
                  when F_Destination =>
                     Address_Type'Size,
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Destination =>
            (case Field is
                  when F_Payload =>
                     (RFLX.Types.Bit_Length_Type (Context.Cursors (F_Total_Length).Value.Total_Length_Value) * 8 - 160),
                  when F_Options =>
                     (RFLX.Types.Bit_Length_Type (Context.Cursors (F_IHL).Value.IHL_Value) * 32 - 160),
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Options =>
            (case Field is
                  when F_Payload =>
                     (RFLX.Types.Bit_Length_Type (Context.Cursors (F_Total_Length).Value.Total_Length_Value) * 8 + RFLX.Types.Bit_Length_Type (Context.Cursors (F_IHL).Value.IHL_Value) * (-32)),
                  when others =>
                     RFLX.Types.Unreachable_Bit_Length_Type),
         when F_Payload | F_Final =>
            0))
    with
     Pre =>
       Valid_Target (Context.Field, Field)
          and then Valid_Predecessors (Context, Field)
          and then Field_Condition (Context, Context.Field, Field);

   function Field_First (Context : Context_Type; Field : Field_Type) return RFLX.Types.Bit_Index_Type is
     ((case Context.Field is
         when F_Initial | F_Version | F_IHL | F_DSCP | F_ECN | F_Total_Length | F_Identification | F_Flag_R | F_Flag_DF | F_Flag_MF | F_Fragment_Offset | F_TTL | F_Protocol | F_Header_Checksum | F_Source | F_Destination | F_Options | F_Payload | F_Final =>
            Context.Index))
    with
     Pre =>
       Valid_Target (Context.Field, Field)
          and then Valid_Predecessors (Context, Field)
          and then Field_Condition (Context, Context.Field, Field);

   function Field_Postcondition (Context : Context_Type; Field : Field_Type) return Boolean is
     ((case Field is
         when F_Version =>
            Field_Condition (Context, Field, F_IHL),
         when F_IHL =>
            Field_Condition (Context, Field, F_DSCP),
         when F_DSCP =>
            Field_Condition (Context, Field, F_ECN),
         when F_ECN =>
            Field_Condition (Context, Field, F_Total_Length),
         when F_Total_Length =>
            Field_Condition (Context, Field, F_Identification),
         when F_Identification =>
            Field_Condition (Context, Field, F_Flag_R),
         when F_Flag_R =>
            Field_Condition (Context, Field, F_Flag_DF),
         when F_Flag_DF =>
            Field_Condition (Context, Field, F_Flag_MF),
         when F_Flag_MF =>
            Field_Condition (Context, Field, F_Fragment_Offset),
         when F_Fragment_Offset =>
            Field_Condition (Context, Field, F_TTL),
         when F_TTL =>
            Field_Condition (Context, Field, F_Protocol),
         when F_Protocol =>
            Field_Condition (Context, Field, F_Header_Checksum),
         when F_Header_Checksum =>
            Field_Condition (Context, Field, F_Source),
         when F_Source =>
            Field_Condition (Context, Field, F_Destination),
         when F_Destination =>
            Field_Condition (Context, Field, F_Payload)
               or Field_Condition (Context, Field, F_Options),
         when F_Options =>
            Field_Condition (Context, Field, F_Payload),
         when F_Payload =>
            Field_Condition (Context, Field, F_Final)))
    with
     Pre =>
       Valid_Predecessors (Context, Field)
          and then Preliminary_Valid (Context, Field);

   function Valid_Context (Context : Context_Type; Field : Field_Type) return Boolean is
     (Valid_Target (Context.Field, Field)
      and then Valid_Predecessors (Context, Field)
      and then Context.Buffer /= null);

   function Sufficient_Buffer_Length (Context : Context_Type; Field : Field_Type) return Boolean is
     (Context.Buffer /= null
      and then Context.First <= RFLX.Types.Bit_Index_Type'Last / 2
      and then Field_First (Context, Field) <= RFLX.Types.Bit_Index_Type'Last / 2
      and then Field_Length (Context, Field) >= 0
      and then Field_Length (Context, Field) <= RFLX.Types.Bit_Length_Type'Last / 2
      and then (Field_First (Context, Field) + Field_Length (Context, Field)) <= RFLX.Types.Bit_Length_Type'Last / 2
      and then Context.First <= Field_First (Context, Field)
      and then Context.Last >= ((Field_First (Context, Field) + Field_Length (Context, Field))) - 1)
    with
     Pre =>
       Valid_Context (Context, Field)
          and then Field_Condition (Context, Context.Field, Field);

   function Get_Field_Value (Context : Context_Type; Field : Field_Type) return Result_Type with
     Pre =>
       Valid_Context (Context, Field)
          and then Field_Condition (Context, Context.Field, Field)
          and then Sufficient_Buffer_Length (Context, Field),
     Post =>
       Get_Field_Value'Result.Field = Field
   is
      First : constant RFLX.Types.Bit_Index_Type := Field_First (Context, Field);
      Length : constant RFLX.Types.Bit_Length_Type := Field_Length (Context, Field);
      function Buffer_First return RFLX.Types.Index_Type is
        (RFLX.Types.Byte_Index (First));
      function Buffer_Last return RFLX.Types.Index_Type is
        (RFLX.Types.Byte_Index ((First + Length - 1)))
       with
        Pre =>
          Length >= 1;
      function Offset return RFLX.Types.Offset_Type is
        (RFLX.Types.Offset_Type ((8 - ((First + Length - 1)) mod 8) mod 8));
   begin
      return ((case Field is
            when F_Version =>
               (Field => F_Version, Version_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_IHL =>
               (Field => F_IHL, IHL_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_DSCP =>
               (Field => F_DSCP, DSCP_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_ECN =>
               (Field => F_ECN, ECN_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Total_Length =>
               (Field => F_Total_Length, Total_Length_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Identification =>
               (Field => F_Identification, Identification_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Flag_R =>
               (Field => F_Flag_R, Flag_R_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Flag_DF =>
               (Field => F_Flag_DF, Flag_DF_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Flag_MF =>
               (Field => F_Flag_MF, Flag_MF_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Fragment_Offset =>
               (Field => F_Fragment_Offset, Fragment_Offset_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_TTL =>
               (Field => F_TTL, TTL_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Protocol =>
               (Field => F_Protocol, Protocol_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Header_Checksum =>
               (Field => F_Header_Checksum, Header_Checksum_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Source =>
               (Field => F_Source, Source_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Destination =>
               (Field => F_Destination, Destination_Value => Convert (Context.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Options =>
               (Field => F_Options),
            when F_Payload =>
               (Field => F_Payload)));
   end Get_Field_Value;

   procedure Verify (Context : in out Context_Type; Field : Field_Type) is
      First : RFLX.Types.Bit_Index_Type;
      Last : RFLX.Types.Bit_Length_Type;
      Value : Result_Type;
   begin
      if Valid_Context (Context, Field) then
         if Field_Condition (Context, Context.Field, Field) then
            if Sufficient_Buffer_Length (Context, Field) then
               First := Field_First (Context, Field);
               Last := ((First + Field_Length (Context, Field))) - 1;
               Value := Get_Field_Value (Context, Field);
               Context.Cursors (Field) := (State => S_Preliminary, First => First, Last => Last, Value => Value);
               if Valid_Type (Value)
                  and then Field_Postcondition (Context, Field) then
                  if Composite_Field (Field) then
                     Context.Cursors (Field) := (State => S_Structural_Valid, First => First, Last => Last, Value => Value);
                  else
                     Context.Cursors (Field) := (State => S_Valid, First => First, Last => Last, Value => Value);
                  end if;
                  Context.Index := (Last + 1);
                  Context.Field := Field;
               else
                  Context.Cursors (Field) := (State => S_Invalid);
               end if;
            else
               Context.Cursors (Field) := (State => S_Incomplete);
            end if;
         else
            Context.Cursors (Field) := (State => S_Invalid);
         end if;
      end if;
   end Verify;

   procedure Verify_Message (Context : in out Context_Type) is
   begin
      Verify (Context, F_Version);
      Verify (Context, F_IHL);
      Verify (Context, F_DSCP);
      Verify (Context, F_ECN);
      Verify (Context, F_Total_Length);
      Verify (Context, F_Identification);
      Verify (Context, F_Flag_R);
      Verify (Context, F_Flag_DF);
      Verify (Context, F_Flag_MF);
      Verify (Context, F_Fragment_Offset);
      Verify (Context, F_TTL);
      Verify (Context, F_Protocol);
      Verify (Context, F_Header_Checksum);
      Verify (Context, F_Source);
      Verify (Context, F_Destination);
      Verify (Context, F_Options);
      Verify (Context, F_Payload);
   end Verify_Message;

   function Present (Context : Context_Type; Field : Field_Type) return Boolean is
     ((Context.Cursors (Field).State = S_Valid
        or Context.Cursors (Field).State = S_Structural_Valid)
      and then Context.Cursors (Field).Value.Field = Field
      and then Context.Cursors (Field).First < (Context.Cursors (Field).Last + 1));

   function Structural_Valid (Context : Context_Type; Field : Field_Type) return Boolean is
     ((Context.Cursors (Field).State = S_Valid
        or Context.Cursors (Field).State = S_Structural_Valid));

   function Valid (Context : Context_Type; Field : Field_Type) return Boolean is
     (Context.Cursors (Field).State = S_Valid
      and then Context.Cursors (Field).Value.Field = Field
      and then Context.Cursors (Field).First < (Context.Cursors (Field).Last + 1));

   function Incomplete (Context : Context_Type; Field : Field_Type) return Boolean is
     (Context.Cursors (Field).State = S_Incomplete);

   function Structural_Valid_Message (Context : Context_Type) return Boolean is
     (Valid (Context, F_Version)
      and then Valid (Context, F_IHL)
      and then Valid (Context, F_DSCP)
      and then Valid (Context, F_ECN)
      and then Valid (Context, F_Total_Length)
      and then Valid (Context, F_Identification)
      and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Context.Cursors (F_IHL).Value.IHL_Value) * 4
      and then Valid (Context, F_Flag_R)
      and then Valid (Context, F_Flag_DF)
      and then Context.Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
      and then Valid (Context, F_Flag_MF)
      and then Valid (Context, F_Fragment_Offset)
      and then Valid (Context, F_TTL)
      and then Valid (Context, F_Protocol)
      and then Valid (Context, F_Header_Checksum)
      and then Valid (Context, F_Source)
      and then Valid (Context, F_Destination)
      and then ((Structural_Valid (Context, F_Payload)
          and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_IHL).Value.IHL_Value) = 5)
        or (Structural_Valid (Context, F_Options)
          and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_IHL).Value.IHL_Value) > 5
          and then Structural_Valid (Context, F_Payload))));

   function Valid_Message (Context : Context_Type) return Boolean is
     (Valid (Context, F_Version)
      and then Valid (Context, F_IHL)
      and then Valid (Context, F_DSCP)
      and then Valid (Context, F_ECN)
      and then Valid (Context, F_Total_Length)
      and then Valid (Context, F_Identification)
      and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_Total_Length).Value.Total_Length_Value) >= RFLX.Types.Bit_Length_Type (Context.Cursors (F_IHL).Value.IHL_Value) * 4
      and then Valid (Context, F_Flag_R)
      and then Valid (Context, F_Flag_DF)
      and then Context.Cursors (F_Flag_R).Value.Flag_R_Value = Convert (Flag_False)
      and then Valid (Context, F_Flag_MF)
      and then Valid (Context, F_Fragment_Offset)
      and then Valid (Context, F_TTL)
      and then Valid (Context, F_Protocol)
      and then Valid (Context, F_Header_Checksum)
      and then Valid (Context, F_Source)
      and then Valid (Context, F_Destination)
      and then ((Valid (Context, F_Payload)
          and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_IHL).Value.IHL_Value) = 5)
        or (Valid (Context, F_Options)
          and then RFLX.Types.Bit_Length_Type (Context.Cursors (F_IHL).Value.IHL_Value) > 5
          and then Valid (Context, F_Payload))));

   function Incomplete_Message (Context : Context_Type) return Boolean is
     (Incomplete (Context, F_Version)
      or Incomplete (Context, F_IHL)
      or Incomplete (Context, F_DSCP)
      or Incomplete (Context, F_ECN)
      or Incomplete (Context, F_Total_Length)
      or Incomplete (Context, F_Identification)
      or Incomplete (Context, F_Flag_R)
      or Incomplete (Context, F_Flag_DF)
      or Incomplete (Context, F_Flag_MF)
      or Incomplete (Context, F_Fragment_Offset)
      or Incomplete (Context, F_TTL)
      or Incomplete (Context, F_Protocol)
      or Incomplete (Context, F_Header_Checksum)
      or Incomplete (Context, F_Source)
      or Incomplete (Context, F_Destination)
      or Incomplete (Context, F_Options)
      or Incomplete (Context, F_Payload));

   function Get_Version (Context : Context_Type) return Version_Type is
     (Context.Cursors (F_Version).Value.Version_Value);

   function Get_IHL (Context : Context_Type) return IHL_Type is
     (Context.Cursors (F_IHL).Value.IHL_Value);

   function Get_DSCP (Context : Context_Type) return DCSP_Type is
     (Context.Cursors (F_DSCP).Value.DSCP_Value);

   function Get_ECN (Context : Context_Type) return ECN_Type is
     (Context.Cursors (F_ECN).Value.ECN_Value);

   function Get_Total_Length (Context : Context_Type) return Total_Length_Type is
     (Context.Cursors (F_Total_Length).Value.Total_Length_Value);

   function Get_Identification (Context : Context_Type) return Identification_Type is
     (Context.Cursors (F_Identification).Value.Identification_Value);

   function Get_Flag_R (Context : Context_Type) return Flag_Type is
     (Convert (Context.Cursors (F_Flag_R).Value.Flag_R_Value));

   function Get_Flag_DF (Context : Context_Type) return Flag_Type is
     (Convert (Context.Cursors (F_Flag_DF).Value.Flag_DF_Value));

   function Get_Flag_MF (Context : Context_Type) return Flag_Type is
     (Convert (Context.Cursors (F_Flag_MF).Value.Flag_MF_Value));

   function Get_Fragment_Offset (Context : Context_Type) return Fragment_Offset_Type is
     (Context.Cursors (F_Fragment_Offset).Value.Fragment_Offset_Value);

   function Get_TTL (Context : Context_Type) return TTL_Type is
     (Context.Cursors (F_TTL).Value.TTL_Value);

   function Get_Protocol (Context : Context_Type) return Protocol_Type is
     (Convert (Context.Cursors (F_Protocol).Value.Protocol_Value));

   function Get_Header_Checksum (Context : Context_Type) return Header_Checksum_Type is
     (Context.Cursors (F_Header_Checksum).Value.Header_Checksum_Value);

   function Get_Source (Context : Context_Type) return Address_Type is
     (Context.Cursors (F_Source).Value.Source_Value);

   function Get_Destination (Context : Context_Type) return Address_Type is
     (Context.Cursors (F_Destination).Value.Destination_Value);

   procedure Get_Options (Context : Context_Type) is
      First : constant RFLX.Types.Index_Type := RFLX.Types.Byte_Index (Context.Cursors (F_Options).First);
      Last : constant RFLX.Types.Index_Type := RFLX.Types.Byte_Index (Context.Cursors (F_Options).Last);
   begin
      Process_Options (Context.Buffer.all (First .. Last));
   end Get_Options;

   procedure Get_Payload (Context : Context_Type) is
      First : constant RFLX.Types.Index_Type := RFLX.Types.Byte_Index (Context.Cursors (F_Payload).First);
      Last : constant RFLX.Types.Index_Type := RFLX.Types.Byte_Index (Context.Cursors (F_Payload).Last);
   begin
      Process_Payload (Context.Buffer.all (First .. Last));
   end Get_Payload;

   procedure Switch (Context : in out Context_Type; Sequence_Context : out Options_Sequence.Context_Type) is
      Buffer : RFLX.Types.Bytes_Ptr;
   begin
      Take_Buffer (Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Options_Sequence.Initialize (Sequence_Context, Buffer, Context.Buffer_First, Context.Buffer_Last, Context.Cursors (F_Options).First, Context.Cursors (F_Options).Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch;

   procedure Update (Context : in out Context_Type; Sequence_Context : in out Options_Sequence.Context_Type) is
      Valid_Sequence : constant Boolean := Options_Sequence.Valid (Sequence_Context);
      Buffer : RFLX.Types.Bytes_Ptr;
   begin
      Options_Sequence.Take_Buffer (Sequence_Context, Buffer, Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last);
      Context.Buffer := Buffer;
      if Valid_Sequence then
         Context.Cursors (F_Options) := (State => S_Valid, First => Context.Cursors (F_Options).First, Last => Context.Cursors (F_Options).Last, Value => Context.Cursors (F_Options).Value);
      end if;
   end Update;

end RFLX.IPv4.Generic_Packet;

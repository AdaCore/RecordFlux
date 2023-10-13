pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.DCCP.Options;

package RFLX.DCCP.Packet with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Always_Return)
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

   type Virtual_Field is (F_Initial, F_Source_Port, F_Destination_Port, F_Data_Offset, F_CCVal, F_CsCov, F_Checksum, F_Res_3, F_Packet_Type, F_X, F_Res_8, F_Sequence_Number_Short, F_Sequence_Number_Long, F_Ack_Reserved_Short, F_Ack_Reserved_Long, F_Ack_Number_Short, F_Ack_Number_Long, F_Reset_Code, F_Service_Code, F_Data_1, F_Data_2, F_Data_3, F_Options, F_Data, F_Final);

   subtype Field is Virtual_Field range F_Source_Port .. F_Data;

   type Field_Cursor is private with
     Default_Initial_Condition =>
       False;

   type Field_Cursors is private with
     Default_Initial_Condition =>
       False;

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
       and RFLX.DCCP.Packet.Has_Buffer (Ctx),
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
       and RFLX.DCCP.Packet.Has_Buffer (Ctx)
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
       RFLX.DCCP.Packet.Has_Buffer (Ctx),
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
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Well_Formed_Message (Ctx)
       and then RFLX.DCCP.Packet.Byte_Size (Ctx) = Buffer'Length;

   function Read (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Well_Formed_Message (Ctx);

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
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Well_Formed_Message (Ctx)
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
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then Offset < RFLX.DCCP.Packet.Buffer_Length (Ctx)
       and then Pre (RFLX.DCCP.Packet.Buffer_Length (Ctx), Offset),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
       and Initialized (Ctx);

   function Has_Buffer (Ctx : Context) return Boolean;

   function Buffer_Length (Ctx : Context) return RFLX_Types.Length with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx);

   function Size (Ctx : Context) return RFLX_Types.Bit_Length with
     Post =>
       Size'Result rem RFLX_Types.Byte'Size = 0;

   function Byte_Size (Ctx : Context) return RFLX_Types.Length;

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Well_Formed_Message (Ctx);

   function Written_Last (Ctx : Context) return RFLX_Types.Bit_Length;

   procedure Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Well_Formed_Message (Ctx)
       and then Data'Length = RFLX.DCCP.Packet.Byte_Size (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Value (Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       RFLX.DCCP.Packet.Valid_Predecessor (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_Condition (Ctx : Context; Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Predecessor (Ctx, Fld)
       and then RFLX.DCCP.Packet.Valid_Value (Fld, Val)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, Fld)
       and then RFLX.DCCP.Packet.Sufficient_Space (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.DCCP.Packet.Valid_Next (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Options | F_Data =>
              Field_Size'Result rem RFLX_Types.Byte'Size = 0,
           when others =>
              True);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       RFLX.DCCP.Packet.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.DCCP.Packet.Valid_Next (Ctx, Fld)
       and then RFLX.DCCP.Packet.Sufficient_Space (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Options | F_Data =>
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
       RFLX.DCCP.Packet.Valid_Next (Ctx, Fld);

   function Sufficient_Space (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       RFLX.DCCP.Packet.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and RFLX.DCCP.Packet.Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx),
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
       RFLX.DCCP.Packet.Has_Buffer (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Incomplete_Message (Ctx : Context) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "precondition is always False");

   function Get_Source_Port (Ctx : Context) return RFLX.DCCP.Port_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Source_Port);

   function Get_Destination_Port (Ctx : Context) return RFLX.DCCP.Port_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Destination_Port);

   function Get_Data_Offset (Ctx : Context) return RFLX.DCCP.Data_Offset_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Data_Offset);

   function Get_CCVal (Ctx : Context) return RFLX.DCCP.CCVal_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_CCVal);

   function Get_CsCov (Ctx : Context) return RFLX.DCCP.Checksum_Coverage_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_CsCov);

   function Get_Checksum (Ctx : Context) return RFLX.DCCP.Checksum_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Checksum);

   function Get_Res_3 (Ctx : Context) return RFLX.DCCP.Reserved_3_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Res_3);

   function Get_Packet_Type (Ctx : Context) return RFLX.DCCP.Type_Field with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Packet_Type);

   function Get_X (Ctx : Context) return RFLX.DCCP.Ext_Seq_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_X);

   function Get_Res_8 (Ctx : Context) return RFLX.DCCP.Reserved_8_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Res_8);

   function Get_Sequence_Number_Short (Ctx : Context) return RFLX.DCCP.Sequence_Number_Short_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Sequence_Number_Short);

   function Get_Sequence_Number_Long (Ctx : Context) return RFLX.DCCP.Sequence_Number_Long_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Sequence_Number_Long);

   function Get_Ack_Reserved_Short (Ctx : Context) return RFLX.DCCP.Reserved_8_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Ack_Reserved_Short);

   function Get_Ack_Reserved_Long (Ctx : Context) return RFLX.DCCP.Reserved_16_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Ack_Reserved_Long);

   function Get_Ack_Number_Short (Ctx : Context) return RFLX.DCCP.Ack_Number_Short_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Ack_Number_Short);

   function Get_Ack_Number_Long (Ctx : Context) return RFLX.DCCP.Ack_Number_Long_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Ack_Number_Long);

   function Get_Reset_Code (Ctx : Context) return RFLX.DCCP.Reset_Code_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Reset_Code);

   function Get_Service_Code (Ctx : Context) return RFLX.DCCP.Service_Code_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Service_Code);

   function Get_Data_1 (Ctx : Context) return RFLX.DCCP.Data_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Data_1);

   function Get_Data_2 (Ctx : Context) return RFLX.DCCP.Data_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Data_2);

   function Get_Data_3 (Ctx : Context) return RFLX.DCCP.Data_Type with
     Pre =>
       RFLX.DCCP.Packet.Valid (Ctx, RFLX.DCCP.Packet.F_Data_3);

   pragma Warnings (On, "precondition is always False");

   function Get_Data (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Well_Formed (Ctx, RFLX.DCCP.Packet.F_Data)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Data),
     Post =>
       Get_Data'Result'Length = RFLX_Types.To_Length (Field_Size (Ctx, F_Data));

   procedure Get_Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Well_Formed (Ctx, RFLX.DCCP.Packet.F_Data)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Data)
       and then Data'Length = RFLX_Types.To_Length (RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Data)),
     Post =>
       Equal (Ctx, F_Data, Data);

   generic
      with procedure Process_Data (Data : RFLX_Types.Bytes);
   procedure Generic_Get_Data (Ctx : Context) with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and RFLX.DCCP.Packet.Present (Ctx, RFLX.DCCP.Packet.F_Data);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean with
     Pre =>
       RFLX.DCCP.Packet.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Set_Source_Port (Ctx : in out Context; Val : RFLX.DCCP.Port_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Source_Port)
       and then RFLX.DCCP.Valid_Port_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Source_Port) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Source_Port)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Source_Port, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Source_Port)
       and Get_Source_Port (Ctx) = Val
       and Invalid (Ctx, F_Destination_Port)
       and Invalid (Ctx, F_Data_Offset)
       and Invalid (Ctx, F_CCVal)
       and Invalid (Ctx, F_CsCov)
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Res_3)
       and Invalid (Ctx, F_Packet_Type)
       and Invalid (Ctx, F_X)
       and Invalid (Ctx, F_Res_8)
       and Invalid (Ctx, F_Sequence_Number_Short)
       and Invalid (Ctx, F_Sequence_Number_Long)
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Destination_Port) = F_Source_Port
            and Valid_Next (Ctx, F_Destination_Port))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Source_Port) = Predecessor (Ctx, F_Source_Port)'Old
       and Valid_Next (Ctx, F_Source_Port) = Valid_Next (Ctx, F_Source_Port)'Old
       and Field_First (Ctx, F_Source_Port) = Field_First (Ctx, F_Source_Port)'Old;

   procedure Set_Destination_Port (Ctx : in out Context; Val : RFLX.DCCP.Port_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Destination_Port)
       and then RFLX.DCCP.Valid_Port_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Destination_Port) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Destination_Port)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Destination_Port, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Destination_Port)
       and Get_Destination_Port (Ctx) = Val
       and Invalid (Ctx, F_Data_Offset)
       and Invalid (Ctx, F_CCVal)
       and Invalid (Ctx, F_CsCov)
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Res_3)
       and Invalid (Ctx, F_Packet_Type)
       and Invalid (Ctx, F_X)
       and Invalid (Ctx, F_Res_8)
       and Invalid (Ctx, F_Sequence_Number_Short)
       and Invalid (Ctx, F_Sequence_Number_Long)
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Data_Offset) = F_Destination_Port
            and Valid_Next (Ctx, F_Data_Offset))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Destination_Port) = Predecessor (Ctx, F_Destination_Port)'Old
       and Valid_Next (Ctx, F_Destination_Port) = Valid_Next (Ctx, F_Destination_Port)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Field_First (Ctx, F_Destination_Port) = Field_First (Ctx, F_Destination_Port)'Old
       and (for all F in Field range F_Source_Port .. F_Source_Port =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Data_Offset (Ctx : in out Context; Val : RFLX.DCCP.Data_Offset_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Data_Offset)
       and then RFLX.DCCP.Valid_Data_Offset_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Data_Offset) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Data_Offset)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Data_Offset, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Data_Offset)
       and Get_Data_Offset (Ctx) = Val
       and Invalid (Ctx, F_CCVal)
       and Invalid (Ctx, F_CsCov)
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Res_3)
       and Invalid (Ctx, F_Packet_Type)
       and Invalid (Ctx, F_X)
       and Invalid (Ctx, F_Res_8)
       and Invalid (Ctx, F_Sequence_Number_Short)
       and Invalid (Ctx, F_Sequence_Number_Long)
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_CCVal) = F_Data_Offset
            and Valid_Next (Ctx, F_CCVal))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data_Offset) = Predecessor (Ctx, F_Data_Offset)'Old
       and Valid_Next (Ctx, F_Data_Offset) = Valid_Next (Ctx, F_Data_Offset)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Field_First (Ctx, F_Data_Offset) = Field_First (Ctx, F_Data_Offset)'Old
       and (for all F in Field range F_Source_Port .. F_Destination_Port =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_CCVal (Ctx : in out Context; Val : RFLX.DCCP.CCVal_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_CCVal)
       and then RFLX.DCCP.Valid_CCVal_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_CCVal) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_CCVal)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_CCVal, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_CCVal)
       and Get_CCVal (Ctx) = Val
       and Invalid (Ctx, F_CsCov)
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Res_3)
       and Invalid (Ctx, F_Packet_Type)
       and Invalid (Ctx, F_X)
       and Invalid (Ctx, F_Res_8)
       and Invalid (Ctx, F_Sequence_Number_Short)
       and Invalid (Ctx, F_Sequence_Number_Long)
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_CsCov) = F_CCVal
            and Valid_Next (Ctx, F_CsCov))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_CCVal) = Predecessor (Ctx, F_CCVal)'Old
       and Valid_Next (Ctx, F_CCVal) = Valid_Next (Ctx, F_CCVal)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Field_First (Ctx, F_CCVal) = Field_First (Ctx, F_CCVal)'Old
       and (for all F in Field range F_Source_Port .. F_Data_Offset =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_CsCov (Ctx : in out Context; Val : RFLX.DCCP.Checksum_Coverage_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_CsCov)
       and then RFLX.DCCP.Valid_Checksum_Coverage_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_CsCov) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_CsCov)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_CsCov, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_CsCov)
       and Get_CsCov (Ctx) = Val
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Res_3)
       and Invalid (Ctx, F_Packet_Type)
       and Invalid (Ctx, F_X)
       and Invalid (Ctx, F_Res_8)
       and Invalid (Ctx, F_Sequence_Number_Short)
       and Invalid (Ctx, F_Sequence_Number_Long)
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Checksum) = F_CsCov
            and Valid_Next (Ctx, F_Checksum))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_CsCov) = Predecessor (Ctx, F_CsCov)'Old
       and Valid_Next (Ctx, F_CsCov) = Valid_Next (Ctx, F_CsCov)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Field_First (Ctx, F_CsCov) = Field_First (Ctx, F_CsCov)'Old
       and (for all F in Field range F_Source_Port .. F_CCVal =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Checksum (Ctx : in out Context; Val : RFLX.DCCP.Checksum_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Checksum)
       and then RFLX.DCCP.Valid_Checksum_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Checksum) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Checksum)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Checksum, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Checksum)
       and Get_Checksum (Ctx) = Val
       and Invalid (Ctx, F_Res_3)
       and Invalid (Ctx, F_Packet_Type)
       and Invalid (Ctx, F_X)
       and Invalid (Ctx, F_Res_8)
       and Invalid (Ctx, F_Sequence_Number_Short)
       and Invalid (Ctx, F_Sequence_Number_Long)
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Res_3) = F_Checksum
            and Valid_Next (Ctx, F_Res_3))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Checksum) = Predecessor (Ctx, F_Checksum)'Old
       and Valid_Next (Ctx, F_Checksum) = Valid_Next (Ctx, F_Checksum)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Field_First (Ctx, F_Checksum) = Field_First (Ctx, F_Checksum)'Old
       and (for all F in Field range F_Source_Port .. F_CsCov =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Res_3 (Ctx : in out Context; Val : RFLX.DCCP.Reserved_3_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Res_3)
       and then RFLX.DCCP.Valid_Reserved_3_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Res_3) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Res_3)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Res_3, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Res_3)
       and Get_Res_3 (Ctx) = Val
       and Invalid (Ctx, F_Packet_Type)
       and Invalid (Ctx, F_X)
       and Invalid (Ctx, F_Res_8)
       and Invalid (Ctx, F_Sequence_Number_Short)
       and Invalid (Ctx, F_Sequence_Number_Long)
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Packet_Type) = F_Res_3
            and Valid_Next (Ctx, F_Packet_Type))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Res_3) = Predecessor (Ctx, F_Res_3)'Old
       and Valid_Next (Ctx, F_Res_3) = Valid_Next (Ctx, F_Res_3)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Field_First (Ctx, F_Res_3) = Field_First (Ctx, F_Res_3)'Old
       and (for all F in Field range F_Source_Port .. F_Checksum =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Packet_Type (Ctx : in out Context; Val : RFLX.DCCP.Type_Field) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Packet_Type)
       and then RFLX.DCCP.Valid_Type_Field (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Packet_Type) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Packet_Type)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Packet_Type, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Packet_Type)
       and Get_Packet_Type (Ctx) = Val
       and Invalid (Ctx, F_X)
       and Invalid (Ctx, F_Res_8)
       and Invalid (Ctx, F_Sequence_Number_Short)
       and Invalid (Ctx, F_Sequence_Number_Long)
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_X) = F_Packet_Type
            and Valid_Next (Ctx, F_X))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Packet_Type) = Predecessor (Ctx, F_Packet_Type)'Old
       and Valid_Next (Ctx, F_Packet_Type) = Valid_Next (Ctx, F_Packet_Type)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Field_First (Ctx, F_Packet_Type) = Field_First (Ctx, F_Packet_Type)'Old
       and (for all F in Field range F_Source_Port .. F_Res_3 =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_X (Ctx : in out Context; Val : RFLX.DCCP.Ext_Seq_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_X)
       and then RFLX.DCCP.Valid_Ext_Seq_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_X) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_X)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_X, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_X)
       and Get_X (Ctx) = Val
       and Invalid (Ctx, F_Res_8)
       and Invalid (Ctx, F_Sequence_Number_Short)
       and Invalid (Ctx, F_Sequence_Number_Long)
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_X (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.EXTENDED))
            then
               Predecessor (Ctx, F_Res_8) = F_X
               and Valid_Next (Ctx, F_Res_8))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_X (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NOT_EXTENDED))
            then
               Predecessor (Ctx, F_Sequence_Number_Short) = F_X
               and Valid_Next (Ctx, F_Sequence_Number_Short))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_X) = Predecessor (Ctx, F_X)'Old
       and Valid_Next (Ctx, F_X) = Valid_Next (Ctx, F_X)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Field_First (Ctx, F_X) = Field_First (Ctx, F_X)'Old
       and (for all F in Field range F_Source_Port .. F_Packet_Type =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Res_8 (Ctx : in out Context; Val : RFLX.DCCP.Reserved_8_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Res_8)
       and then RFLX.DCCP.Valid_Reserved_8_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Res_8) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Res_8)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Res_8, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Res_8)
       and Get_Res_8 (Ctx) = Val
       and Invalid (Ctx, F_Sequence_Number_Short)
       and Invalid (Ctx, F_Sequence_Number_Long)
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Sequence_Number_Long) = F_Res_8
            and Valid_Next (Ctx, F_Sequence_Number_Long))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Res_8) = Predecessor (Ctx, F_Res_8)'Old
       and Valid_Next (Ctx, F_Res_8) = Valid_Next (Ctx, F_Res_8)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Field_First (Ctx, F_Res_8) = Field_First (Ctx, F_Res_8)'Old
       and (for all F in Field range F_Source_Port .. F_X =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Sequence_Number_Short (Ctx : in out Context; Val : RFLX.DCCP.Sequence_Number_Short_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Sequence_Number_Short)
       and then RFLX.DCCP.Valid_Sequence_Number_Short_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Sequence_Number_Short) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Sequence_Number_Short)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Sequence_Number_Short, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Sequence_Number_Short)
       and Get_Sequence_Number_Short (Ctx) = Val
       and Invalid (Ctx, F_Sequence_Number_Long)
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
            then
               Predecessor (Ctx, F_Ack_Reserved_Short) = F_Sequence_Number_Short
               and Valid_Next (Ctx, F_Ack_Reserved_Short))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Data) = F_Sequence_Number_Short
               and Valid_Next (Ctx, F_Data))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Options) = F_Sequence_Number_Short
               and Valid_Next (Ctx, F_Options))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Sequence_Number_Short) = Predecessor (Ctx, F_Sequence_Number_Short)'Old
       and Valid_Next (Ctx, F_Sequence_Number_Short) = Valid_Next (Ctx, F_Sequence_Number_Short)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Field_First (Ctx, F_Sequence_Number_Short) = Field_First (Ctx, F_Sequence_Number_Short)'Old
       and (for all F in Field range F_Source_Port .. F_Res_8 =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Sequence_Number_Long (Ctx : in out Context; Val : RFLX.DCCP.Sequence_Number_Long_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Sequence_Number_Long)
       and then RFLX.DCCP.Valid_Sequence_Number_Long_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Sequence_Number_Long) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Sequence_Number_Long)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Sequence_Number_Long, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Sequence_Number_Long)
       and Get_Sequence_Number_Long (Ctx) = Val
       and Invalid (Ctx, F_Ack_Reserved_Short)
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
            then
               Predecessor (Ctx, F_Ack_Reserved_Long) = F_Sequence_Number_Long
               and Valid_Next (Ctx, F_Ack_Reserved_Long))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Data) = F_Sequence_Number_Long
               and Valid_Next (Ctx, F_Data))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Options) = F_Sequence_Number_Long
               and Valid_Next (Ctx, F_Options))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
            then
               Predecessor (Ctx, F_Service_Code) = F_Sequence_Number_Long
               and Valid_Next (Ctx, F_Service_Code))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Sequence_Number_Long) = Predecessor (Ctx, F_Sequence_Number_Long)'Old
       and Valid_Next (Ctx, F_Sequence_Number_Long) = Valid_Next (Ctx, F_Sequence_Number_Long)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Get_Res_8 (Ctx) = Get_Res_8 (Ctx)'Old
       and Field_First (Ctx, F_Sequence_Number_Long) = Field_First (Ctx, F_Sequence_Number_Long)'Old
       and (for all F in Field range F_Source_Port .. F_Sequence_Number_Short =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Ack_Reserved_Short (Ctx : in out Context; Val : RFLX.DCCP.Reserved_8_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Ack_Reserved_Short)
       and then RFLX.DCCP.Valid_Reserved_8_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Ack_Reserved_Short) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Ack_Reserved_Short)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Ack_Reserved_Short, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Ack_Reserved_Short)
       and Get_Ack_Reserved_Short (Ctx) = Val
       and Invalid (Ctx, F_Ack_Reserved_Long)
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Ack_Number_Short) = F_Ack_Reserved_Short
            and Valid_Next (Ctx, F_Ack_Number_Short))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Ack_Reserved_Short) = Predecessor (Ctx, F_Ack_Reserved_Short)'Old
       and Valid_Next (Ctx, F_Ack_Reserved_Short) = Valid_Next (Ctx, F_Ack_Reserved_Short)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Get_Sequence_Number_Short (Ctx) = Get_Sequence_Number_Short (Ctx)'Old
       and Field_First (Ctx, F_Ack_Reserved_Short) = Field_First (Ctx, F_Ack_Reserved_Short)'Old
       and (for all F in Field range F_Source_Port .. F_Sequence_Number_Long =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Ack_Reserved_Long (Ctx : in out Context; Val : RFLX.DCCP.Reserved_16_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Ack_Reserved_Long)
       and then RFLX.DCCP.Valid_Reserved_16_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Ack_Reserved_Long) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Ack_Reserved_Long)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Ack_Reserved_Long, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Ack_Reserved_Long)
       and Get_Ack_Reserved_Long (Ctx) = Val
       and Invalid (Ctx, F_Ack_Number_Short)
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Ack_Number_Long) = F_Ack_Reserved_Long
            and Valid_Next (Ctx, F_Ack_Number_Long))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Ack_Reserved_Long) = Predecessor (Ctx, F_Ack_Reserved_Long)'Old
       and Valid_Next (Ctx, F_Ack_Reserved_Long) = Valid_Next (Ctx, F_Ack_Reserved_Long)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Get_Res_8 (Ctx) = Get_Res_8 (Ctx)'Old
       and Get_Sequence_Number_Long (Ctx) = Get_Sequence_Number_Long (Ctx)'Old
       and Field_First (Ctx, F_Ack_Reserved_Long) = Field_First (Ctx, F_Ack_Reserved_Long)'Old
       and (for all F in Field range F_Source_Port .. F_Ack_Reserved_Short =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Ack_Number_Short (Ctx : in out Context; Val : RFLX.DCCP.Ack_Number_Short_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Ack_Number_Short)
       and then RFLX.DCCP.Valid_Ack_Number_Short_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Ack_Number_Short) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Ack_Number_Short)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Ack_Number_Short, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Ack_Number_Short)
       and Get_Ack_Number_Short (Ctx) = Val
       and Invalid (Ctx, F_Ack_Number_Long)
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (if
               (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Data) = F_Ack_Number_Short
               and Valid_Next (Ctx, F_Data))
       and (if
               (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Options) = F_Ack_Number_Short
               and Valid_Next (Ctx, F_Options))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Ack_Number_Short) = Predecessor (Ctx, F_Ack_Number_Short)'Old
       and Valid_Next (Ctx, F_Ack_Number_Short) = Valid_Next (Ctx, F_Ack_Number_Short)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Get_Sequence_Number_Short (Ctx) = Get_Sequence_Number_Short (Ctx)'Old
       and Get_Ack_Reserved_Short (Ctx) = Get_Ack_Reserved_Short (Ctx)'Old
       and Field_First (Ctx, F_Ack_Number_Short) = Field_First (Ctx, F_Ack_Number_Short)'Old
       and (for all F in Field range F_Source_Port .. F_Ack_Reserved_Long =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Ack_Number_Long (Ctx : in out Context; Val : RFLX.DCCP.Ack_Number_Long_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Ack_Number_Long)
       and then RFLX.DCCP.Valid_Ack_Number_Long_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Ack_Number_Long) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Ack_Number_Long)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Ack_Number_Long, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Ack_Number_Long)
       and Get_Ack_Number_Long (Ctx) = Val
       and Invalid (Ctx, F_Reset_Code)
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (if
               (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Data) = F_Ack_Number_Long
               and Valid_Next (Ctx, F_Data))
       and (if
               (RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                or RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
               and RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Options) = F_Ack_Number_Long
               and Valid_Next (Ctx, F_Options))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESET))
            then
               Predecessor (Ctx, F_Reset_Code) = F_Ack_Number_Long
               and Valid_Next (Ctx, F_Reset_Code))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Packet_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESPONSE))
            then
               Predecessor (Ctx, F_Service_Code) = F_Ack_Number_Long
               and Valid_Next (Ctx, F_Service_Code))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Ack_Number_Long) = Predecessor (Ctx, F_Ack_Number_Long)'Old
       and Valid_Next (Ctx, F_Ack_Number_Long) = Valid_Next (Ctx, F_Ack_Number_Long)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Get_Res_8 (Ctx) = Get_Res_8 (Ctx)'Old
       and Get_Sequence_Number_Long (Ctx) = Get_Sequence_Number_Long (Ctx)'Old
       and Get_Ack_Reserved_Long (Ctx) = Get_Ack_Reserved_Long (Ctx)'Old
       and Field_First (Ctx, F_Ack_Number_Long) = Field_First (Ctx, F_Ack_Number_Long)'Old
       and (for all F in Field range F_Source_Port .. F_Ack_Number_Short =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Reset_Code (Ctx : in out Context; Val : RFLX.DCCP.Reset_Code_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Reset_Code)
       and then RFLX.DCCP.Valid_Reset_Code_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Reset_Code) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Reset_Code)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Reset_Code, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Reset_Code)
       and Get_Reset_Code (Ctx) = Val
       and Invalid (Ctx, F_Service_Code)
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Data_1) = F_Reset_Code
            and Valid_Next (Ctx, F_Data_1))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Reset_Code) = Predecessor (Ctx, F_Reset_Code)'Old
       and Valid_Next (Ctx, F_Reset_Code) = Valid_Next (Ctx, F_Reset_Code)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Get_Res_8 (Ctx) = Get_Res_8 (Ctx)'Old
       and Get_Sequence_Number_Long (Ctx) = Get_Sequence_Number_Long (Ctx)'Old
       and Get_Ack_Reserved_Long (Ctx) = Get_Ack_Reserved_Long (Ctx)'Old
       and Get_Ack_Number_Long (Ctx) = Get_Ack_Number_Long (Ctx)'Old
       and Field_First (Ctx, F_Reset_Code) = Field_First (Ctx, F_Reset_Code)'Old
       and (for all F in Field range F_Source_Port .. F_Ack_Number_Long =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Service_Code (Ctx : in out Context; Val : RFLX.DCCP.Service_Code_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Service_Code)
       and then RFLX.DCCP.Valid_Service_Code_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Service_Code) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Service_Code)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Service_Code, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Service_Code)
       and Get_Service_Code (Ctx) = Val
       and Invalid (Ctx, F_Data_1)
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (if
               RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Service_Code)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Data) = F_Service_Code
               and Valid_Next (Ctx, F_Data))
       and (if
               RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Service_Code)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Options) = F_Service_Code
               and Valid_Next (Ctx, F_Options))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Service_Code) = Predecessor (Ctx, F_Service_Code)'Old
       and Valid_Next (Ctx, F_Service_Code) = Valid_Next (Ctx, F_Service_Code)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Get_Res_8 (Ctx) = Get_Res_8 (Ctx)'Old
       and Get_Sequence_Number_Long (Ctx) = Get_Sequence_Number_Long (Ctx)'Old
       and Field_First (Ctx, F_Service_Code) = Field_First (Ctx, F_Service_Code)'Old
       and (for all F in Field range F_Source_Port .. F_Reset_Code =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Data_1 (Ctx : in out Context; Val : RFLX.DCCP.Data_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Data_1)
       and then RFLX.DCCP.Valid_Data_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Data_1) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Data_1)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Data_1, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Data_1)
       and Get_Data_1 (Ctx) = Val
       and Invalid (Ctx, F_Data_2)
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Data_2) = F_Data_1
            and Valid_Next (Ctx, F_Data_2))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data_1) = Predecessor (Ctx, F_Data_1)'Old
       and Valid_Next (Ctx, F_Data_1) = Valid_Next (Ctx, F_Data_1)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Get_Res_8 (Ctx) = Get_Res_8 (Ctx)'Old
       and Get_Sequence_Number_Long (Ctx) = Get_Sequence_Number_Long (Ctx)'Old
       and Get_Ack_Reserved_Long (Ctx) = Get_Ack_Reserved_Long (Ctx)'Old
       and Get_Ack_Number_Long (Ctx) = Get_Ack_Number_Long (Ctx)'Old
       and Get_Reset_Code (Ctx) = Get_Reset_Code (Ctx)'Old
       and Field_First (Ctx, F_Data_1) = Field_First (Ctx, F_Data_1)'Old
       and (for all F in Field range F_Source_Port .. F_Service_Code =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Data_2 (Ctx : in out Context; Val : RFLX.DCCP.Data_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Data_2)
       and then RFLX.DCCP.Valid_Data_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Data_2) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Data_2)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Data_2, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Data_2)
       and Get_Data_2 (Ctx) = Val
       and Invalid (Ctx, F_Data_3)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Data_3) = F_Data_2
            and Valid_Next (Ctx, F_Data_3))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data_2) = Predecessor (Ctx, F_Data_2)'Old
       and Valid_Next (Ctx, F_Data_2) = Valid_Next (Ctx, F_Data_2)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Get_Res_8 (Ctx) = Get_Res_8 (Ctx)'Old
       and Get_Sequence_Number_Long (Ctx) = Get_Sequence_Number_Long (Ctx)'Old
       and Get_Ack_Reserved_Long (Ctx) = Get_Ack_Reserved_Long (Ctx)'Old
       and Get_Ack_Number_Long (Ctx) = Get_Ack_Number_Long (Ctx)'Old
       and Get_Reset_Code (Ctx) = Get_Reset_Code (Ctx)'Old
       and Get_Data_1 (Ctx) = Get_Data_1 (Ctx)'Old
       and Field_First (Ctx, F_Data_2) = Field_First (Ctx, F_Data_2)'Old
       and (for all F in Field range F_Source_Port .. F_Data_1 =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Data_3 (Ctx : in out Context; Val : RFLX.DCCP.Data_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Data_3)
       and then RFLX.DCCP.Valid_Data_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Data_3) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Data_3)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Data_3, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Data_3)
       and Get_Data_3 (Ctx) = Val
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (if
               RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Data_3)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Data) = F_Data_3
               and Valid_Next (Ctx, F_Data))
       and (if
               RFLX_Types.Base_Integer (Get_Data_Offset (Ctx)) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Data_3)) - RFLX_Types.Base_Integer (Ctx.First) + 1
            then
               Predecessor (Ctx, F_Options) = F_Data_3
               and Valid_Next (Ctx, F_Options))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data_3) = Predecessor (Ctx, F_Data_3)'Old
       and Valid_Next (Ctx, F_Data_3) = Valid_Next (Ctx, F_Data_3)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Get_Res_8 (Ctx) = Get_Res_8 (Ctx)'Old
       and Get_Sequence_Number_Long (Ctx) = Get_Sequence_Number_Long (Ctx)'Old
       and Get_Ack_Reserved_Long (Ctx) = Get_Ack_Reserved_Long (Ctx)'Old
       and Get_Ack_Number_Long (Ctx) = Get_Ack_Number_Long (Ctx)'Old
       and Get_Reset_Code (Ctx) = Get_Reset_Code (Ctx)'Old
       and Get_Data_1 (Ctx) = Get_Data_1 (Ctx)'Old
       and Get_Data_2 (Ctx) = Get_Data_2 (Ctx)'Old
       and Field_First (Ctx, F_Data_3) = Field_First (Ctx, F_Data_3)'Old
       and (for all F in Field range F_Source_Port .. F_Data_2 =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   pragma Warnings (On, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Set_Data_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Data)
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Data) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Data)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Data, 0)
       and then RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Data) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Data)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Field_First (Ctx, F_Data) = Field_First (Ctx, F_Data)'Old;

   procedure Set_Options (Ctx : in out Context; Seq_Ctx : RFLX.DCCP.Options.Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Options)
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Options) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Options)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Options, 0)
       and then RFLX.DCCP.Packet.Valid_Length (Ctx, RFLX.DCCP.Packet.F_Options, RFLX.DCCP.Options.Byte_Size (Seq_Ctx))
       and then RFLX.DCCP.Options.Has_Buffer (Seq_Ctx)
       and then RFLX.DCCP.Options.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Data) = F_Options
            and Valid_Next (Ctx, F_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Field_First (Ctx, F_Options) = Field_First (Ctx, F_Options)'Old
       and (if Field_Size (Ctx, F_Options) > 0 then Present (Ctx, F_Options));

   procedure Initialize_Options (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Options)
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Options) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Options),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Options)
       and Invalid (Ctx, F_Data)
       and (Predecessor (Ctx, F_Data) = F_Options
            and Valid_Next (Ctx, F_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Field_First (Ctx, F_Options) = Field_First (Ctx, F_Options)'Old;

   procedure Initialize_Data (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Data)
       and then RFLX.DCCP.Packet.Valid_Length (Ctx, RFLX.DCCP.Packet.F_Data, Length)
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Data) >= RFLX_Types.To_Bit_Length (Length),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Data)
       and Field_Size (Ctx, F_Data) = RFLX_Types.To_Bit_Length (Length)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Field_First (Ctx, F_Data) = Field_First (Ctx, F_Data)'Old;

   procedure Set_Data (Ctx : in out Context; Data : RFLX_Types.Bytes) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Data)
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Data) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Data)
       and then RFLX.DCCP.Packet.Valid_Length (Ctx, RFLX.DCCP.Packet.F_Data, Data'Length)
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Data) >= Data'Length * RFLX_Types.Byte'Size
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Data, 0),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Data)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Field_First (Ctx, F_Data) = Field_First (Ctx, F_Data)'Old
       and Equal (Ctx, F_Data, Data);

   generic
      with procedure Process_Data (Data : out RFLX_Types.Bytes);
      with function Process_Data_Pre (Length : RFLX_Types.Length) return Boolean;
   procedure Generic_Set_Data (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Data)
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Data) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Data)
       and then RFLX.DCCP.Packet.Valid_Length (Ctx, RFLX.DCCP.Packet.F_Data, Length)
       and then RFLX_Types.To_Length (RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Data)) >= Length
       and then Process_Data_Pre (Length),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Data)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Data_Offset (Ctx) = Get_Data_Offset (Ctx)'Old
       and Get_CCVal (Ctx) = Get_CCVal (Ctx)'Old
       and Get_CsCov (Ctx) = Get_CsCov (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Get_Res_3 (Ctx) = Get_Res_3 (Ctx)'Old
       and Get_Packet_Type (Ctx) = Get_Packet_Type (Ctx)'Old
       and Get_X (Ctx) = Get_X (Ctx)'Old
       and Field_First (Ctx, F_Data) = Field_First (Ctx, F_Data)'Old;

   procedure Switch_To_Options (Ctx : in out Context; Seq_Ctx : out RFLX.DCCP.Options.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Options)
       and then RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Options) > 0
       and then RFLX.DCCP.Packet.Field_First (Ctx, RFLX.DCCP.Packet.F_Options) rem RFLX_Types.Byte'Size = 1
       and then RFLX.DCCP.Packet.Available_Space (Ctx, RFLX.DCCP.Packet.F_Options) >= RFLX.DCCP.Packet.Field_Size (Ctx, RFLX.DCCP.Packet.F_Options)
       and then RFLX.DCCP.Packet.Field_Condition (Ctx, RFLX.DCCP.Packet.F_Options, 0),
     Post =>
       not RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and RFLX.DCCP.Options.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_Options)
       and Seq_Ctx.Last = Field_Last (Ctx, F_Options)
       and RFLX.DCCP.Options.Valid (Seq_Ctx)
       and RFLX.DCCP.Options.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_Options)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Path_Condition (Ctx, F_Options) = Path_Condition (Ctx, F_Options)'Old
       and Field_Last (Ctx, F_Options) = Field_Last (Ctx, F_Options)'Old
       and (for all F in Field range F_Source_Port .. F_Data_3 =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F)),
     Contract_Cases =>
       (Well_Formed (Ctx, F_Options) =>
           (for all F in Field range F_Data .. F_Data =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F)),
        others =>
           (Predecessor (Ctx, F_Data) = F_Options
            and Valid_Next (Ctx, F_Data))
           and Invalid (Ctx, F_Data));

   function Complete_Options (Ctx : Context; Seq_Ctx : RFLX.DCCP.Options.Context) return Boolean with
     Pre =>
       RFLX.DCCP.Packet.Valid_Next (Ctx, RFLX.DCCP.Packet.F_Options);

   procedure Update_Options (Ctx : in out Context; Seq_Ctx : in out RFLX.DCCP.Options.Context) with
     Pre =>
       RFLX.DCCP.Packet.Present (Ctx, RFLX.DCCP.Packet.F_Options)
       and then not RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then RFLX.DCCP.Options.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_Options)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_Options),
     Post =>
       (if
           RFLX.DCCP.Packet.Complete_Options (Ctx, Seq_Ctx)
        then
           Present (Ctx, F_Options)
           and Context_Cursor (Ctx, F_Data) = Context_Cursor (Ctx, F_Data)'Old
        else
           Invalid (Ctx, F_Options)
           and Invalid (Ctx, F_Data))
       and Has_Buffer (Ctx)
       and not RFLX.DCCP.Options.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Seq_Ctx.First = Seq_Ctx.First'Old
       and Seq_Ctx.Last = Seq_Ctx.Last'Old
       and Field_First (Ctx, F_Options) = Field_First (Ctx, F_Options)'Old
       and Field_Size (Ctx, F_Options) = Field_Size (Ctx, F_Options)'Old
       and Context_Cursor (Ctx, F_Source_Port) = Context_Cursor (Ctx, F_Source_Port)'Old
       and Context_Cursor (Ctx, F_Destination_Port) = Context_Cursor (Ctx, F_Destination_Port)'Old
       and Context_Cursor (Ctx, F_Data_Offset) = Context_Cursor (Ctx, F_Data_Offset)'Old
       and Context_Cursor (Ctx, F_CCVal) = Context_Cursor (Ctx, F_CCVal)'Old
       and Context_Cursor (Ctx, F_CsCov) = Context_Cursor (Ctx, F_CsCov)'Old
       and Context_Cursor (Ctx, F_Checksum) = Context_Cursor (Ctx, F_Checksum)'Old
       and Context_Cursor (Ctx, F_Res_3) = Context_Cursor (Ctx, F_Res_3)'Old
       and Context_Cursor (Ctx, F_Packet_Type) = Context_Cursor (Ctx, F_Packet_Type)'Old
       and Context_Cursor (Ctx, F_X) = Context_Cursor (Ctx, F_X)'Old
       and Context_Cursor (Ctx, F_Res_8) = Context_Cursor (Ctx, F_Res_8)'Old
       and Context_Cursor (Ctx, F_Sequence_Number_Short) = Context_Cursor (Ctx, F_Sequence_Number_Short)'Old
       and Context_Cursor (Ctx, F_Sequence_Number_Long) = Context_Cursor (Ctx, F_Sequence_Number_Long)'Old
       and Context_Cursor (Ctx, F_Ack_Reserved_Short) = Context_Cursor (Ctx, F_Ack_Reserved_Short)'Old
       and Context_Cursor (Ctx, F_Ack_Reserved_Long) = Context_Cursor (Ctx, F_Ack_Reserved_Long)'Old
       and Context_Cursor (Ctx, F_Ack_Number_Short) = Context_Cursor (Ctx, F_Ack_Number_Short)'Old
       and Context_Cursor (Ctx, F_Ack_Number_Long) = Context_Cursor (Ctx, F_Ack_Number_Long)'Old
       and Context_Cursor (Ctx, F_Reset_Code) = Context_Cursor (Ctx, F_Reset_Code)'Old
       and Context_Cursor (Ctx, F_Service_Code) = Context_Cursor (Ctx, F_Service_Code)'Old
       and Context_Cursor (Ctx, F_Data_1) = Context_Cursor (Ctx, F_Data_1)'Old
       and Context_Cursor (Ctx, F_Data_2) = Context_Cursor (Ctx, F_Data_2)'Old
       and Context_Cursor (Ctx, F_Data_3) = Context_Cursor (Ctx, F_Data_3)'Old,
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

   type Field_Cursor (State : Cursor_State := S_Invalid) is
      record
         Predecessor : Virtual_Field := F_Final;
         case State is
            when S_Valid | S_Well_Formed =>
               First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First;
               Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First;
               Value : RFLX_Types.Base_Integer := 0;
            when S_Invalid | S_Incomplete =>
               null;
         end case;
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
                    Well_Formed (Cursors (F_Destination_Port))
                 then
                    (Valid (Cursors (F_Source_Port))
                     and then Cursors (F_Destination_Port).Predecessor = F_Source_Port))
                and then (if
                             Well_Formed (Cursors (F_Data_Offset))
                          then
                             (Valid (Cursors (F_Destination_Port))
                              and then Cursors (F_Data_Offset).Predecessor = F_Destination_Port))
                and then (if
                             Well_Formed (Cursors (F_CCVal))
                          then
                             (Valid (Cursors (F_Data_Offset))
                              and then Cursors (F_CCVal).Predecessor = F_Data_Offset))
                and then (if
                             Well_Formed (Cursors (F_CsCov))
                          then
                             (Valid (Cursors (F_CCVal))
                              and then Cursors (F_CsCov).Predecessor = F_CCVal))
                and then (if
                             Well_Formed (Cursors (F_Checksum))
                          then
                             (Valid (Cursors (F_CsCov))
                              and then Cursors (F_Checksum).Predecessor = F_CsCov))
                and then (if
                             Well_Formed (Cursors (F_Res_3))
                          then
                             (Valid (Cursors (F_Checksum))
                              and then Cursors (F_Res_3).Predecessor = F_Checksum))
                and then (if
                             Well_Formed (Cursors (F_Packet_Type))
                          then
                             (Valid (Cursors (F_Res_3))
                              and then Cursors (F_Packet_Type).Predecessor = F_Res_3))
                and then (if
                             Well_Formed (Cursors (F_X))
                          then
                             (Valid (Cursors (F_Packet_Type))
                              and then Cursors (F_X).Predecessor = F_Packet_Type))
                and then (if
                             Well_Formed (Cursors (F_Res_8))
                          then
                             (Valid (Cursors (F_X))
                              and then Cursors (F_Res_8).Predecessor = F_X
                              and then RFLX_Types.Base_Integer (Cursors (F_X).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.EXTENDED))))
                and then (if
                             Well_Formed (Cursors (F_Sequence_Number_Short))
                          then
                             (Valid (Cursors (F_X))
                              and then Cursors (F_Sequence_Number_Short).Predecessor = F_X
                              and then RFLX_Types.Base_Integer (Cursors (F_X).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NOT_EXTENDED))))
                and then (if
                             Well_Formed (Cursors (F_Sequence_Number_Long))
                          then
                             (Valid (Cursors (F_Res_8))
                              and then Cursors (F_Sequence_Number_Long).Predecessor = F_Res_8))
                and then (if
                             Well_Formed (Cursors (F_Ack_Reserved_Short))
                          then
                             (Valid (Cursors (F_Sequence_Number_Short))
                              and then Cursors (F_Ack_Reserved_Short).Predecessor = F_Sequence_Number_Short
                              and then (RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                        and RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST)))))
                and then (if
                             Well_Formed (Cursors (F_Ack_Reserved_Long))
                          then
                             (Valid (Cursors (F_Sequence_Number_Long))
                              and then Cursors (F_Ack_Reserved_Long).Predecessor = F_Sequence_Number_Long
                              and then (RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                        and RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST)))))
                and then (if
                             Well_Formed (Cursors (F_Ack_Number_Short))
                          then
                             (Valid (Cursors (F_Ack_Reserved_Short))
                              and then Cursors (F_Ack_Number_Short).Predecessor = F_Ack_Reserved_Short))
                and then (if
                             Well_Formed (Cursors (F_Ack_Number_Long))
                          then
                             (Valid (Cursors (F_Ack_Reserved_Long))
                              and then Cursors (F_Ack_Number_Long).Predecessor = F_Ack_Reserved_Long))
                and then (if
                             Well_Formed (Cursors (F_Reset_Code))
                          then
                             (Valid (Cursors (F_Ack_Number_Long))
                              and then Cursors (F_Reset_Code).Predecessor = F_Ack_Number_Long
                              and then RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESET))))
                and then (if
                             Well_Formed (Cursors (F_Service_Code))
                          then
                             (Valid (Cursors (F_Ack_Number_Long))
                              and then Cursors (F_Service_Code).Predecessor = F_Ack_Number_Long
                              and then RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESPONSE)))
                             or (Valid (Cursors (F_Sequence_Number_Long))
                                 and then Cursors (F_Service_Code).Predecessor = F_Sequence_Number_Long
                                 and then RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))))
                and then (if
                             Well_Formed (Cursors (F_Data_1))
                          then
                             (Valid (Cursors (F_Reset_Code))
                              and then Cursors (F_Data_1).Predecessor = F_Reset_Code))
                and then (if
                             Well_Formed (Cursors (F_Data_2))
                          then
                             (Valid (Cursors (F_Data_1))
                              and then Cursors (F_Data_2).Predecessor = F_Data_1))
                and then (if
                             Well_Formed (Cursors (F_Data_3))
                          then
                             (Valid (Cursors (F_Data_2))
                              and then Cursors (F_Data_3).Predecessor = F_Data_2))
                and then (if
                             Well_Formed (Cursors (F_Options))
                          then
                             (Valid (Cursors (F_Ack_Number_Long))
                              and then Cursors (F_Options).Predecessor = F_Ack_Number_Long
                              and then ((RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                                         or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                                         or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                         or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                         or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                         or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                        and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Base_Integer (First) + 1))
                             or (Valid (Cursors (F_Ack_Number_Short))
                                 and then Cursors (F_Options).Predecessor = F_Ack_Number_Short
                                 and then ((RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                           and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Base_Integer (First) + 1))
                             or (Valid (Cursors (F_Data_3))
                                 and then Cursors (F_Options).Predecessor = F_Data_3
                                 and then RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Data_3).Last) - RFLX_Types.Base_Integer (First) + 1)
                             or (Valid (Cursors (F_Sequence_Number_Long))
                                 and then Cursors (F_Options).Predecessor = F_Sequence_Number_Long
                                 and then (RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                           and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Base_Integer (First) + 1))
                             or (Valid (Cursors (F_Sequence_Number_Short))
                                 and then Cursors (F_Options).Predecessor = F_Sequence_Number_Short
                                 and then (RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                           and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Base_Integer (First) + 1))
                             or (Valid (Cursors (F_Service_Code))
                                 and then Cursors (F_Options).Predecessor = F_Service_Code
                                 and then RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Service_Code).Last) - RFLX_Types.Base_Integer (First) + 1))
                and then (if
                             Well_Formed (Cursors (F_Data))
                          then
                             (Valid (Cursors (F_Ack_Number_Long))
                              and then Cursors (F_Data).Predecessor = F_Ack_Number_Long
                              and then ((RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                                         or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                                         or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                         or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                         or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                         or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                        and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Base_Integer (First) + 1))
                             or (Valid (Cursors (F_Ack_Number_Short))
                                 and then Cursors (F_Data).Predecessor = F_Ack_Number_Short
                                 and then ((RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                           and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Base_Integer (First) + 1))
                             or (Valid (Cursors (F_Data_3))
                                 and then Cursors (F_Data).Predecessor = F_Data_3
                                 and then RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Data_3).Last) - RFLX_Types.Base_Integer (First) + 1)
                             or (Well_Formed (Cursors (F_Options))
                                 and then Cursors (F_Data).Predecessor = F_Options)
                             or (Valid (Cursors (F_Sequence_Number_Long))
                                 and then Cursors (F_Data).Predecessor = F_Sequence_Number_Long
                                 and then (RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                           and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Base_Integer (First) + 1))
                             or (Valid (Cursors (F_Sequence_Number_Short))
                                 and then Cursors (F_Data).Predecessor = F_Sequence_Number_Short
                                 and then (RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                           and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Base_Integer (First) + 1))
                             or (Valid (Cursors (F_Service_Code))
                                 and then Cursors (F_Data).Predecessor = F_Service_Code
                                 and then RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Service_Code).Last) - RFLX_Types.Base_Integer (First) + 1)))
      and then ((if Invalid (Cursors (F_Source_Port)) then Invalid (Cursors (F_Destination_Port)))
                and then (if Invalid (Cursors (F_Destination_Port)) then Invalid (Cursors (F_Data_Offset)))
                and then (if Invalid (Cursors (F_Data_Offset)) then Invalid (Cursors (F_CCVal)))
                and then (if Invalid (Cursors (F_CCVal)) then Invalid (Cursors (F_CsCov)))
                and then (if Invalid (Cursors (F_CsCov)) then Invalid (Cursors (F_Checksum)))
                and then (if Invalid (Cursors (F_Checksum)) then Invalid (Cursors (F_Res_3)))
                and then (if Invalid (Cursors (F_Res_3)) then Invalid (Cursors (F_Packet_Type)))
                and then (if Invalid (Cursors (F_Packet_Type)) then Invalid (Cursors (F_X)))
                and then (if Invalid (Cursors (F_X)) then Invalid (Cursors (F_Res_8)))
                and then (if Invalid (Cursors (F_X)) then Invalid (Cursors (F_Sequence_Number_Short)))
                and then (if Invalid (Cursors (F_Res_8)) then Invalid (Cursors (F_Sequence_Number_Long)))
                and then (if Invalid (Cursors (F_Sequence_Number_Short)) then Invalid (Cursors (F_Ack_Reserved_Short)))
                and then (if Invalid (Cursors (F_Sequence_Number_Long)) then Invalid (Cursors (F_Ack_Reserved_Long)))
                and then (if Invalid (Cursors (F_Ack_Reserved_Short)) then Invalid (Cursors (F_Ack_Number_Short)))
                and then (if Invalid (Cursors (F_Ack_Reserved_Long)) then Invalid (Cursors (F_Ack_Number_Long)))
                and then (if Invalid (Cursors (F_Ack_Number_Long)) then Invalid (Cursors (F_Reset_Code)))
                and then (if
                             Invalid (Cursors (F_Ack_Number_Long))
                             and then Invalid (Cursors (F_Sequence_Number_Long))
                          then
                             Invalid (Cursors (F_Service_Code)))
                and then (if Invalid (Cursors (F_Reset_Code)) then Invalid (Cursors (F_Data_1)))
                and then (if Invalid (Cursors (F_Data_1)) then Invalid (Cursors (F_Data_2)))
                and then (if Invalid (Cursors (F_Data_2)) then Invalid (Cursors (F_Data_3)))
                and then (if
                             Invalid (Cursors (F_Ack_Number_Long))
                             and then Invalid (Cursors (F_Ack_Number_Short))
                             and then Invalid (Cursors (F_Data_3))
                             and then Invalid (Cursors (F_Sequence_Number_Long))
                             and then Invalid (Cursors (F_Sequence_Number_Short))
                             and then Invalid (Cursors (F_Service_Code))
                          then
                             Invalid (Cursors (F_Options)))
                and then (if
                             Invalid (Cursors (F_Ack_Number_Long))
                             and then Invalid (Cursors (F_Ack_Number_Short))
                             and then Invalid (Cursors (F_Data_3))
                             and then Invalid (Cursors (F_Options))
                             and then Invalid (Cursors (F_Sequence_Number_Long))
                             and then Invalid (Cursors (F_Sequence_Number_Short))
                             and then Invalid (Cursors (F_Service_Code))
                          then
                             Invalid (Cursors (F_Data))))
      and then ((if
                    Well_Formed (Cursors (F_Source_Port))
                 then
                    (Cursors (F_Source_Port).Last - Cursors (F_Source_Port).First + 1 = 16
                     and then Cursors (F_Source_Port).Predecessor = F_Initial
                     and then Cursors (F_Source_Port).First = First))
                and then (if
                             Well_Formed (Cursors (F_Destination_Port))
                          then
                             (Cursors (F_Destination_Port).Last - Cursors (F_Destination_Port).First + 1 = 16
                              and then Cursors (F_Destination_Port).Predecessor = F_Source_Port
                              and then Cursors (F_Destination_Port).First = Cursors (F_Source_Port).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Data_Offset))
                          then
                             (Cursors (F_Data_Offset).Last - Cursors (F_Data_Offset).First + 1 = 8
                              and then Cursors (F_Data_Offset).Predecessor = F_Destination_Port
                              and then Cursors (F_Data_Offset).First = Cursors (F_Destination_Port).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_CCVal))
                          then
                             (Cursors (F_CCVal).Last - Cursors (F_CCVal).First + 1 = 4
                              and then Cursors (F_CCVal).Predecessor = F_Data_Offset
                              and then Cursors (F_CCVal).First = Cursors (F_Data_Offset).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_CsCov))
                          then
                             (Cursors (F_CsCov).Last - Cursors (F_CsCov).First + 1 = 4
                              and then Cursors (F_CsCov).Predecessor = F_CCVal
                              and then Cursors (F_CsCov).First = Cursors (F_CCVal).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Checksum))
                          then
                             (Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1 = 16
                              and then Cursors (F_Checksum).Predecessor = F_CsCov
                              and then Cursors (F_Checksum).First = Cursors (F_CsCov).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Res_3))
                          then
                             (Cursors (F_Res_3).Last - Cursors (F_Res_3).First + 1 = 3
                              and then Cursors (F_Res_3).Predecessor = F_Checksum
                              and then Cursors (F_Res_3).First = Cursors (F_Checksum).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Packet_Type))
                          then
                             (Cursors (F_Packet_Type).Last - Cursors (F_Packet_Type).First + 1 = 4
                              and then Cursors (F_Packet_Type).Predecessor = F_Res_3
                              and then Cursors (F_Packet_Type).First = Cursors (F_Res_3).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_X))
                          then
                             (Cursors (F_X).Last - Cursors (F_X).First + 1 = 1
                              and then Cursors (F_X).Predecessor = F_Packet_Type
                              and then Cursors (F_X).First = Cursors (F_Packet_Type).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Res_8))
                          then
                             (Cursors (F_Res_8).Last - Cursors (F_Res_8).First + 1 = 8
                              and then Cursors (F_Res_8).Predecessor = F_X
                              and then Cursors (F_Res_8).First = Cursors (F_X).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Sequence_Number_Short))
                          then
                             (Cursors (F_Sequence_Number_Short).Last - Cursors (F_Sequence_Number_Short).First + 1 = 24
                              and then Cursors (F_Sequence_Number_Short).Predecessor = F_X
                              and then Cursors (F_Sequence_Number_Short).First = Cursors (F_X).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Sequence_Number_Long))
                          then
                             (Cursors (F_Sequence_Number_Long).Last - Cursors (F_Sequence_Number_Long).First + 1 = 48
                              and then Cursors (F_Sequence_Number_Long).Predecessor = F_Res_8
                              and then Cursors (F_Sequence_Number_Long).First = Cursors (F_Res_8).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Ack_Reserved_Short))
                          then
                             (Cursors (F_Ack_Reserved_Short).Last - Cursors (F_Ack_Reserved_Short).First + 1 = 8
                              and then Cursors (F_Ack_Reserved_Short).Predecessor = F_Sequence_Number_Short
                              and then Cursors (F_Ack_Reserved_Short).First = Cursors (F_Sequence_Number_Short).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Ack_Reserved_Long))
                          then
                             (Cursors (F_Ack_Reserved_Long).Last - Cursors (F_Ack_Reserved_Long).First + 1 = 16
                              and then Cursors (F_Ack_Reserved_Long).Predecessor = F_Sequence_Number_Long
                              and then Cursors (F_Ack_Reserved_Long).First = Cursors (F_Sequence_Number_Long).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Ack_Number_Short))
                          then
                             (Cursors (F_Ack_Number_Short).Last - Cursors (F_Ack_Number_Short).First + 1 = 24
                              and then Cursors (F_Ack_Number_Short).Predecessor = F_Ack_Reserved_Short
                              and then Cursors (F_Ack_Number_Short).First = Cursors (F_Ack_Reserved_Short).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Ack_Number_Long))
                          then
                             (Cursors (F_Ack_Number_Long).Last - Cursors (F_Ack_Number_Long).First + 1 = 48
                              and then Cursors (F_Ack_Number_Long).Predecessor = F_Ack_Reserved_Long
                              and then Cursors (F_Ack_Number_Long).First = Cursors (F_Ack_Reserved_Long).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Reset_Code))
                          then
                             (Cursors (F_Reset_Code).Last - Cursors (F_Reset_Code).First + 1 = 8
                              and then Cursors (F_Reset_Code).Predecessor = F_Ack_Number_Long
                              and then Cursors (F_Reset_Code).First = Cursors (F_Ack_Number_Long).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Service_Code))
                          then
                             (if
                                 Well_Formed (Cursors (F_Ack_Number_Long))
                                 and then RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESPONSE))
                              then
                                 Cursors (F_Service_Code).Last - Cursors (F_Service_Code).First + 1 = 32
                                 and then Cursors (F_Service_Code).Predecessor = F_Ack_Number_Long
                                 and then Cursors (F_Service_Code).First = Cursors (F_Ack_Number_Long).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Sequence_Number_Long))
                                          and then RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST))
                                       then
                                          Cursors (F_Service_Code).Last - Cursors (F_Service_Code).First + 1 = 32
                                          and then Cursors (F_Service_Code).Predecessor = F_Sequence_Number_Long
                                          and then Cursors (F_Service_Code).First = Cursors (F_Sequence_Number_Long).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Data_1))
                          then
                             (Cursors (F_Data_1).Last - Cursors (F_Data_1).First + 1 = 8
                              and then Cursors (F_Data_1).Predecessor = F_Reset_Code
                              and then Cursors (F_Data_1).First = Cursors (F_Reset_Code).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Data_2))
                          then
                             (Cursors (F_Data_2).Last - Cursors (F_Data_2).First + 1 = 8
                              and then Cursors (F_Data_2).Predecessor = F_Data_1
                              and then Cursors (F_Data_2).First = Cursors (F_Data_1).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Data_3))
                          then
                             (Cursors (F_Data_3).Last - Cursors (F_Data_3).First + 1 = 8
                              and then Cursors (F_Data_3).Predecessor = F_Data_2
                              and then Cursors (F_Data_3).First = Cursors (F_Data_2).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Options))
                          then
                             (if
                                 Well_Formed (Cursors (F_Ack_Number_Long))
                                 and then ((RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                           and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Base_Integer (First) + 1)
                              then
                                 Cursors (F_Options).Last - Cursors (F_Options).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Cursors (F_Ack_Number_Long).Last)) + RFLX_Types.Bit_Length (First) - 1)
                                 and then Cursors (F_Options).Predecessor = F_Ack_Number_Long
                                 and then Cursors (F_Options).First = Cursors (F_Ack_Number_Long).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Ack_Number_Short))
                                          and then ((RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                                     or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                                     or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                                     or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                                    and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Base_Integer (First) + 1)
                                       then
                                          Cursors (F_Options).Last - Cursors (F_Options).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Cursors (F_Ack_Number_Short).Last)) + RFLX_Types.Bit_Length (First) - 1)
                                          and then Cursors (F_Options).Predecessor = F_Ack_Number_Short
                                          and then Cursors (F_Options).First = Cursors (F_Ack_Number_Short).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Data_3))
                                          and then RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Data_3).Last) - RFLX_Types.Base_Integer (First) + 1
                                       then
                                          Cursors (F_Options).Last - Cursors (F_Options).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Cursors (F_Data_3).Last)) + RFLX_Types.Bit_Length (First) - 1)
                                          and then Cursors (F_Options).Predecessor = F_Data_3
                                          and then Cursors (F_Options).First = Cursors (F_Data_3).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Sequence_Number_Long))
                                          and then (RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                                    and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Base_Integer (First) + 1)
                                       then
                                          Cursors (F_Options).Last - Cursors (F_Options).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Cursors (F_Sequence_Number_Long).Last)) + RFLX_Types.Bit_Length (First) - 1)
                                          and then Cursors (F_Options).Predecessor = F_Sequence_Number_Long
                                          and then Cursors (F_Options).First = Cursors (F_Sequence_Number_Long).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Sequence_Number_Short))
                                          and then (RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                                    and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Base_Integer (First) + 1)
                                       then
                                          Cursors (F_Options).Last - Cursors (F_Options).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Cursors (F_Sequence_Number_Short).Last)) + RFLX_Types.Bit_Length (First) - 1)
                                          and then Cursors (F_Options).Predecessor = F_Sequence_Number_Short
                                          and then Cursors (F_Options).First = Cursors (F_Sequence_Number_Short).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Service_Code))
                                          and then RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Cursors (F_Service_Code).Last) - RFLX_Types.Base_Integer (First) + 1
                                       then
                                          Cursors (F_Options).Last - Cursors (F_Options).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Cursors (F_Service_Code).Last)) + RFLX_Types.Bit_Length (First) - 1)
                                          and then Cursors (F_Options).Predecessor = F_Service_Code
                                          and then Cursors (F_Options).First = Cursors (F_Service_Code).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Data))
                          then
                             (if
                                 Well_Formed (Cursors (F_Ack_Number_Long))
                                 and then ((RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                            or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                           and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Base_Integer (First) + 1)
                              then
                                 Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Ack_Number_Long).Last)
                                 and then Cursors (F_Data).Predecessor = F_Ack_Number_Long
                                 and then Cursors (F_Data).First = Cursors (F_Ack_Number_Long).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Ack_Number_Short))
                                          and then ((RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                                                     or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                                                     or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                                                     or RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                                                    and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Base_Integer (First) + 1)
                                       then
                                          Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Ack_Number_Short).Last)
                                          and then Cursors (F_Data).Predecessor = F_Ack_Number_Short
                                          and then Cursors (F_Data).First = Cursors (F_Ack_Number_Short).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Data_3))
                                          and then RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Data_3).Last) - RFLX_Types.Base_Integer (First) + 1
                                       then
                                          Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Data_3).Last)
                                          and then Cursors (F_Data).Predecessor = F_Data_3
                                          and then Cursors (F_Data).First = Cursors (F_Data_3).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Options))
                                          and then True
                                       then
                                          Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Options).Last)
                                          and then Cursors (F_Data).Predecessor = F_Options
                                          and then Cursors (F_Data).First = Cursors (F_Options).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Sequence_Number_Long))
                                          and then (RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                                    and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Base_Integer (First) + 1)
                                       then
                                          Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Sequence_Number_Long).Last)
                                          and then Cursors (F_Data).Predecessor = F_Sequence_Number_Long
                                          and then Cursors (F_Data).First = Cursors (F_Sequence_Number_Long).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Sequence_Number_Short))
                                          and then (RFLX_Types.Base_Integer (Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                                                    and RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Base_Integer (First) + 1)
                                       then
                                          Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Sequence_Number_Short).Last)
                                          and then Cursors (F_Data).Predecessor = F_Sequence_Number_Short
                                          and then Cursors (F_Data).First = Cursors (F_Sequence_Number_Short).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Service_Code))
                                          and then RFLX_Types.Base_Integer (Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Cursors (F_Service_Code).Last) - RFLX_Types.Base_Integer (First) + 1
                                       then
                                          Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Service_Code).Last)
                                          and then Cursors (F_Data).Predecessor = F_Service_Code
                                          and then Cursors (F_Data).First = Cursors (F_Service_Code).Last + 1))))
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
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Verified_Last, Context.Written_Last, Context.Buffer, Context.Cursors);

   function Initialized (Ctx : Context) return Boolean is
     (Ctx.Verified_Last = Ctx.First - 1
      and then Valid_Next (Ctx, F_Source_Port)
      and then RFLX.DCCP.Packet.Field_First (Ctx, RFLX.DCCP.Packet.F_Source_Port) rem RFLX_Types.Byte'Size = 1
      and then Available_Space (Ctx, F_Source_Port) = Ctx.Last - Ctx.First + 1
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
          when F_Source_Port | F_Destination_Port =>
             RFLX.DCCP.Valid_Port_Type (Val),
          when F_Data_Offset =>
             RFLX.DCCP.Valid_Data_Offset_Type (Val),
          when F_CCVal =>
             RFLX.DCCP.Valid_CCVal_Type (Val),
          when F_CsCov =>
             RFLX.DCCP.Valid_Checksum_Coverage_Type (Val),
          when F_Checksum =>
             RFLX.DCCP.Valid_Checksum_Type (Val),
          when F_Res_3 =>
             RFLX.DCCP.Valid_Reserved_3_Type (Val),
          when F_Packet_Type =>
             RFLX.DCCP.Valid_Type_Field (Val),
          when F_X =>
             RFLX.DCCP.Valid_Ext_Seq_Type (Val),
          when F_Res_8 =>
             RFLX.DCCP.Valid_Reserved_8_Type (Val),
          when F_Sequence_Number_Short =>
             RFLX.DCCP.Valid_Sequence_Number_Short_Type (Val),
          when F_Sequence_Number_Long =>
             RFLX.DCCP.Valid_Sequence_Number_Long_Type (Val),
          when F_Ack_Reserved_Short =>
             RFLX.DCCP.Valid_Reserved_8_Type (Val),
          when F_Ack_Reserved_Long =>
             RFLX.DCCP.Valid_Reserved_16_Type (Val),
          when F_Ack_Number_Short =>
             RFLX.DCCP.Valid_Ack_Number_Short_Type (Val),
          when F_Ack_Number_Long =>
             RFLX.DCCP.Valid_Ack_Number_Long_Type (Val),
          when F_Reset_Code =>
             RFLX.DCCP.Valid_Reset_Code_Type (Val),
          when F_Service_Code =>
             RFLX.DCCP.Valid_Service_Code_Type (Val),
          when F_Data_1 | F_Data_2 | F_Data_3 =>
             RFLX.DCCP.Valid_Data_Type (Val),
          when F_Options | F_Data =>
             True));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial | F_Source_Port | F_Destination_Port | F_Data_Offset | F_CCVal | F_CsCov | F_Checksum | F_Res_3 | F_Packet_Type | F_Res_8 | F_Ack_Reserved_Short | F_Ack_Reserved_Long | F_Reset_Code | F_Data_1 | F_Data_2 | F_Options | F_Data | F_Final =>
             True,
          when F_X =>
             (case Fld is
                 when F_Res_8 =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_X).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.EXTENDED)),
                 when F_Sequence_Number_Short =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_X).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NOT_EXTENDED)),
                 when others =>
                    False),
          when F_Sequence_Number_Short =>
             (case Fld is
                 when F_Ack_Reserved_Short =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST)),
                 when F_Data =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when F_Options =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when others =>
                    False),
          when F_Sequence_Number_Long =>
             (case Fld is
                 when F_Ack_Reserved_Long =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST)),
                 when F_Data =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when F_Options =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when F_Service_Code =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST)),
                 when others =>
                    False),
          when F_Ack_Number_Short =>
             (case Fld is
                 when F_Data =>
                    (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when F_Options =>
                    (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when others =>
                    False),
          when F_Ack_Number_Long =>
             (case Fld is
                 when F_Data =>
                    (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when F_Options =>
                    (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when F_Reset_Code =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESET)),
                 when F_Service_Code =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESPONSE)),
                 when others =>
                    False),
          when F_Service_Code =>
             (case Fld is
                 when F_Data =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Service_Code).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when F_Options =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Service_Code).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when others =>
                    False),
          when F_Data_3 =>
             (case Fld is
                 when F_Data =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_3).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when F_Options =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_3).Last) - RFLX_Types.Base_Integer (Ctx.First) + 1,
                 when others =>
                    False)));

   function Field_Condition (Ctx : Context; Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean is
     ((case Fld is
          when F_Source_Port | F_Destination_Port | F_Data_Offset | F_CCVal | F_CsCov | F_Checksum | F_Res_3 | F_Packet_Type =>
             True,
          when F_X =>
             Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.EXTENDED))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NOT_EXTENDED)),
          when F_Res_8 =>
             True,
          when F_Sequence_Number_Short =>
             (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
              and RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST)))
             or (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1)
             or (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1),
          when F_Sequence_Number_Long =>
             (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
              and RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST)))
             or (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1)
             or (RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Sequence_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1)
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_REQUEST)),
          when F_Ack_Reserved_Short | F_Ack_Reserved_Long =>
             True,
          when F_Ack_Number_Short =>
             ((RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
               or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
               or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
               or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
              and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1)
             or ((RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Short)) - RFLX_Types.Base_Integer (Ctx.First) + 1),
          when F_Ack_Number_Long =>
             ((RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
               or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
               or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
               or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
               or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
               or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
              and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1)
             or ((RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                  or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Ack_Number_Long)) - RFLX_Types.Base_Integer (Ctx.First) + 1)
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESET))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.DCCP_RESPONSE)),
          when F_Reset_Code =>
             True,
          when F_Service_Code =>
             RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Service_Code)) - RFLX_Types.Base_Integer (Ctx.First) + 1
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Service_Code)) - RFLX_Types.Base_Integer (Ctx.First) + 1,
          when F_Data_1 | F_Data_2 =>
             True,
          when F_Data_3 =>
             RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Base_Integer (Field_Last (Ctx, F_Data_3)) - RFLX_Types.Base_Integer (Ctx.First) + 1
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Base_Integer (Field_Last (Ctx, F_Data_3)) - RFLX_Types.Base_Integer (Ctx.First) + 1,
          when F_Options | F_Data =>
             True));

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     ((case Fld is
          when F_Source_Port | F_Destination_Port =>
             16,
          when F_Data_Offset =>
             8,
          when F_CCVal | F_CsCov =>
             4,
          when F_Checksum =>
             16,
          when F_Res_3 =>
             3,
          when F_Packet_Type =>
             4,
          when F_X =>
             1,
          when F_Res_8 =>
             8,
          when F_Sequence_Number_Short =>
             24,
          when F_Sequence_Number_Long =>
             48,
          when F_Ack_Reserved_Short =>
             8,
          when F_Ack_Reserved_Long =>
             16,
          when F_Ack_Number_Short =>
             24,
          when F_Ack_Number_Long =>
             48,
          when F_Reset_Code =>
             8,
          when F_Service_Code =>
             32,
          when F_Data_1 | F_Data_2 | F_Data_3 =>
             8,
          when F_Options =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Long
                 and then ((RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                           and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
              then
                 RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Long).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Short
                 and then ((RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                           and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
              then
                 RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Short).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Data_3
                 and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_3).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1
              then
                 RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_3).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Long
                 and then (RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                           and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
              then
                 RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Long).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Short
                 and then (RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                           and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
              then
                 RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Short).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Service_Code
                 and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 > RFLX_Types.Bit_Length (Ctx.Cursors (F_Service_Code).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1
              then
                 RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 + ((-RFLX_Types.Bit_Length (Ctx.Cursors (F_Service_Code).Last)) + RFLX_Types.Bit_Length (Ctx.First) - 1)
              else
                 RFLX_Types.Unreachable),
          when F_Data =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Long
                 and then ((RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                           and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
              then
                 RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Long).Last)
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Short
                 and then ((RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                            or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                           and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
              then
                 RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Short).Last)
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Data_3
                 and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_3).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1
              then
                 RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_3).Last)
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Options
              then
                 RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Options).Last)
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Long
                 and then (RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                           and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
              then
                 RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Long).Last)
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Short
                 and then (RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                           and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
              then
                 RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Short).Last)
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Service_Code
                 and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Service_Code).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1
              then
                 RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Service_Code).Last)
              else
                 RFLX_Types.Unreachable)));

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index is
     ((if Fld = F_Source_Port then Ctx.First else Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1));

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
          when F_Source_Port =>
             Ctx.Cursors (Fld).Predecessor = F_Initial,
          when F_Destination_Port =>
             (Valid (Ctx.Cursors (F_Source_Port))
              and Ctx.Cursors (Fld).Predecessor = F_Source_Port),
          when F_Data_Offset =>
             (Valid (Ctx.Cursors (F_Destination_Port))
              and Ctx.Cursors (Fld).Predecessor = F_Destination_Port),
          when F_CCVal =>
             (Valid (Ctx.Cursors (F_Data_Offset))
              and Ctx.Cursors (Fld).Predecessor = F_Data_Offset),
          when F_CsCov =>
             (Valid (Ctx.Cursors (F_CCVal))
              and Ctx.Cursors (Fld).Predecessor = F_CCVal),
          when F_Checksum =>
             (Valid (Ctx.Cursors (F_CsCov))
              and Ctx.Cursors (Fld).Predecessor = F_CsCov),
          when F_Res_3 =>
             (Valid (Ctx.Cursors (F_Checksum))
              and Ctx.Cursors (Fld).Predecessor = F_Checksum),
          when F_Packet_Type =>
             (Valid (Ctx.Cursors (F_Res_3))
              and Ctx.Cursors (Fld).Predecessor = F_Res_3),
          when F_X =>
             (Valid (Ctx.Cursors (F_Packet_Type))
              and Ctx.Cursors (Fld).Predecessor = F_Packet_Type),
          when F_Res_8 | F_Sequence_Number_Short =>
             (Valid (Ctx.Cursors (F_X))
              and Ctx.Cursors (Fld).Predecessor = F_X),
          when F_Sequence_Number_Long =>
             (Valid (Ctx.Cursors (F_Res_8))
              and Ctx.Cursors (Fld).Predecessor = F_Res_8),
          when F_Ack_Reserved_Short =>
             (Valid (Ctx.Cursors (F_Sequence_Number_Short))
              and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Short),
          when F_Ack_Reserved_Long =>
             (Valid (Ctx.Cursors (F_Sequence_Number_Long))
              and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Long),
          when F_Ack_Number_Short =>
             (Valid (Ctx.Cursors (F_Ack_Reserved_Short))
              and Ctx.Cursors (Fld).Predecessor = F_Ack_Reserved_Short),
          when F_Ack_Number_Long =>
             (Valid (Ctx.Cursors (F_Ack_Reserved_Long))
              and Ctx.Cursors (Fld).Predecessor = F_Ack_Reserved_Long),
          when F_Reset_Code =>
             (Valid (Ctx.Cursors (F_Ack_Number_Long))
              and Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Long),
          when F_Service_Code =>
             (Valid (Ctx.Cursors (F_Ack_Number_Long))
              and Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Long)
             or (Valid (Ctx.Cursors (F_Sequence_Number_Long))
                 and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Long),
          when F_Data_1 =>
             (Valid (Ctx.Cursors (F_Reset_Code))
              and Ctx.Cursors (Fld).Predecessor = F_Reset_Code),
          when F_Data_2 =>
             (Valid (Ctx.Cursors (F_Data_1))
              and Ctx.Cursors (Fld).Predecessor = F_Data_1),
          when F_Data_3 =>
             (Valid (Ctx.Cursors (F_Data_2))
              and Ctx.Cursors (Fld).Predecessor = F_Data_2),
          when F_Options =>
             (Valid (Ctx.Cursors (F_Ack_Number_Long))
              and Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Long)
             or (Valid (Ctx.Cursors (F_Ack_Number_Short))
                 and Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Short)
             or (Valid (Ctx.Cursors (F_Data_3))
                 and Ctx.Cursors (Fld).Predecessor = F_Data_3)
             or (Valid (Ctx.Cursors (F_Sequence_Number_Long))
                 and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Long)
             or (Valid (Ctx.Cursors (F_Sequence_Number_Short))
                 and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Short)
             or (Valid (Ctx.Cursors (F_Service_Code))
                 and Ctx.Cursors (Fld).Predecessor = F_Service_Code),
          when F_Data =>
             (Valid (Ctx.Cursors (F_Ack_Number_Long))
              and Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Long)
             or (Valid (Ctx.Cursors (F_Ack_Number_Short))
                 and Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Short)
             or (Valid (Ctx.Cursors (F_Data_3))
                 and Ctx.Cursors (Fld).Predecessor = F_Data_3)
             or (Well_Formed (Ctx.Cursors (F_Options))
                 and Ctx.Cursors (Fld).Predecessor = F_Options)
             or (Valid (Ctx.Cursors (F_Sequence_Number_Long))
                 and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Long)
             or (Valid (Ctx.Cursors (F_Sequence_Number_Short))
                 and Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Short)
             or (Valid (Ctx.Cursors (F_Service_Code))
                 and Ctx.Cursors (Fld).Predecessor = F_Service_Code),
          when F_Final =>
             (Well_Formed (Ctx.Cursors (F_Data))
              and Ctx.Cursors (Fld).Predecessor = F_Data)));

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
     (Well_Formed (Ctx, F_Data));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Data));

   function Incomplete_Message (Ctx : Context) return Boolean is
     ((for some F in Field =>
          Incomplete (Ctx, F)));

   function Get_Source_Port (Ctx : Context) return RFLX.DCCP.Port_Type is
     (To_Actual (Ctx.Cursors (F_Source_Port).Value));

   function Get_Destination_Port (Ctx : Context) return RFLX.DCCP.Port_Type is
     (To_Actual (Ctx.Cursors (F_Destination_Port).Value));

   function Get_Data_Offset (Ctx : Context) return RFLX.DCCP.Data_Offset_Type is
     (To_Actual (Ctx.Cursors (F_Data_Offset).Value));

   function Get_CCVal (Ctx : Context) return RFLX.DCCP.CCVal_Type is
     (To_Actual (Ctx.Cursors (F_CCVal).Value));

   function Get_CsCov (Ctx : Context) return RFLX.DCCP.Checksum_Coverage_Type is
     (To_Actual (Ctx.Cursors (F_CsCov).Value));

   function Get_Checksum (Ctx : Context) return RFLX.DCCP.Checksum_Type is
     (To_Actual (Ctx.Cursors (F_Checksum).Value));

   function Get_Res_3 (Ctx : Context) return RFLX.DCCP.Reserved_3_Type is
     (To_Actual (Ctx.Cursors (F_Res_3).Value));

   function Get_Packet_Type (Ctx : Context) return RFLX.DCCP.Type_Field is
     (To_Actual (Ctx.Cursors (F_Packet_Type).Value));

   function Get_X (Ctx : Context) return RFLX.DCCP.Ext_Seq_Type is
     (To_Actual (Ctx.Cursors (F_X).Value));

   function Get_Res_8 (Ctx : Context) return RFLX.DCCP.Reserved_8_Type is
     (To_Actual (Ctx.Cursors (F_Res_8).Value));

   function Get_Sequence_Number_Short (Ctx : Context) return RFLX.DCCP.Sequence_Number_Short_Type is
     (To_Actual (Ctx.Cursors (F_Sequence_Number_Short).Value));

   function Get_Sequence_Number_Long (Ctx : Context) return RFLX.DCCP.Sequence_Number_Long_Type is
     (To_Actual (Ctx.Cursors (F_Sequence_Number_Long).Value));

   function Get_Ack_Reserved_Short (Ctx : Context) return RFLX.DCCP.Reserved_8_Type is
     (To_Actual (Ctx.Cursors (F_Ack_Reserved_Short).Value));

   function Get_Ack_Reserved_Long (Ctx : Context) return RFLX.DCCP.Reserved_16_Type is
     (To_Actual (Ctx.Cursors (F_Ack_Reserved_Long).Value));

   function Get_Ack_Number_Short (Ctx : Context) return RFLX.DCCP.Ack_Number_Short_Type is
     (To_Actual (Ctx.Cursors (F_Ack_Number_Short).Value));

   function Get_Ack_Number_Long (Ctx : Context) return RFLX.DCCP.Ack_Number_Long_Type is
     (To_Actual (Ctx.Cursors (F_Ack_Number_Long).Value));

   function Get_Reset_Code (Ctx : Context) return RFLX.DCCP.Reset_Code_Type is
     (To_Actual (Ctx.Cursors (F_Reset_Code).Value));

   function Get_Service_Code (Ctx : Context) return RFLX.DCCP.Service_Code_Type is
     (To_Actual (Ctx.Cursors (F_Service_Code).Value));

   function Get_Data_1 (Ctx : Context) return RFLX.DCCP.Data_Type is
     (To_Actual (Ctx.Cursors (F_Data_1).Value));

   function Get_Data_2 (Ctx : Context) return RFLX.DCCP.Data_Type is
     (To_Actual (Ctx.Cursors (F_Data_2).Value));

   function Get_Data_3 (Ctx : Context) return RFLX.DCCP.Data_Type is
     (To_Actual (Ctx.Cursors (F_Data_3).Value));

   function Valid_Size (Ctx : Context; Fld : Field; Size : RFLX_Types.Bit_Length) return Boolean is
     ((if
          Fld = F_Data
          and then Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Long
          and then ((RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_SYNCACK))
                     or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_SYNC))
                     or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                     or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                     or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                     or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                    and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Long).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
       then
          Size <= Available_Space (Ctx, Fld)
       elsif
          Fld = F_Data
          and then Ctx.Cursors (Fld).Predecessor = F_Ack_Number_Short
          and then ((RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSEREQ))
                     or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_CLOSE))
                     or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_ACK))
                     or RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA_ACK)))
                    and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Ack_Number_Short).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
       then
          Size <= Available_Space (Ctx, Fld)
       elsif
          Fld = F_Data
          and then Ctx.Cursors (Fld).Predecessor = F_Data_3
          and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_3).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1
       then
          Size <= Available_Space (Ctx, Fld)
       elsif
          Fld = F_Data
          and then Ctx.Cursors (Fld).Predecessor = F_Options
       then
          Size <= Available_Space (Ctx, Fld)
       elsif
          Fld = F_Data
          and then Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Long
          and then (RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                    and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Long).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
       then
          Size <= Available_Space (Ctx, Fld)
       elsif
          Fld = F_Data
          and then Ctx.Cursors (Fld).Predecessor = F_Sequence_Number_Short
          and then (RFLX_Types.Bit_Length (Ctx.Cursors (F_Packet_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.DCCP_DATA))
                    and RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Sequence_Number_Short).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1)
       then
          Size <= Available_Space (Ctx, Fld)
       elsif
          Fld = F_Data
          and then Ctx.Cursors (Fld).Predecessor = F_Service_Code
          and then RFLX_Types.Bit_Length (Ctx.Cursors (F_Data_Offset).Value) * 32 = RFLX_Types.Bit_Length (Ctx.Cursors (F_Service_Code).Last) - RFLX_Types.Bit_Length (Ctx.First) + 1
       then
          Size <= Available_Space (Ctx, Fld)
       else
          Size = Field_Size (Ctx, Fld)))
    with
     Pre =>
       RFLX.DCCP.Packet.Valid_Next (Ctx, Fld);

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean is
     (Valid_Size (Ctx, Fld, RFLX_Types.To_Bit_Length (Length)));

   function Complete_Options (Ctx : Context; Seq_Ctx : RFLX.DCCP.Options.Context) return Boolean is
     (RFLX.DCCP.Options.Valid (Seq_Ctx)
      and RFLX.DCCP.Options.Size (Seq_Ctx) = Field_Size (Ctx, F_Options));

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

   function Context_Cursors_Index (Cursors : Field_Cursors; Fld : Field) return Field_Cursor is
     (Cursors (Fld));

end RFLX.DCCP.Packet;

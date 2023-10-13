pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.DCCP.Option with
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

   type Virtual_Field is (F_Initial, F_Option_Type, F_Option_Length, F_Loss_Event_Rate, F_NDP_Count_Opt, F_Option_Feature, F_Receive_Rate, F_Timestamp_Echo_Opt, F_Timestamp_Option, F_Option_Value, F_Elapsed_Time_Opt, F_Final);

   subtype Field is Virtual_Field range F_Option_Type .. F_Elapsed_Time_Opt;

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
       and RFLX.DCCP.Option.Has_Buffer (Ctx),
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
       and RFLX.DCCP.Option.Has_Buffer (Ctx)
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
       RFLX.DCCP.Option.Has_Buffer (Ctx),
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
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Well_Formed_Message (Ctx)
       and then RFLX.DCCP.Option.Byte_Size (Ctx) = Buffer'Length;

   function Read (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Well_Formed_Message (Ctx);

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
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Well_Formed_Message (Ctx)
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
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then Offset < RFLX.DCCP.Option.Buffer_Length (Ctx)
       and then Pre (RFLX.DCCP.Option.Buffer_Length (Ctx), Offset),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
       and Initialized (Ctx);

   function Has_Buffer (Ctx : Context) return Boolean;

   function Buffer_Length (Ctx : Context) return RFLX_Types.Length with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx);

   function Size (Ctx : Context) return RFLX_Types.Bit_Length with
     Post =>
       Size'Result rem RFLX_Types.Byte'Size = 0;

   function Byte_Size (Ctx : Context) return RFLX_Types.Length;

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Well_Formed_Message (Ctx);

   function Written_Last (Ctx : Context) return RFLX_Types.Bit_Length;

   procedure Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Well_Formed_Message (Ctx)
       and then Data'Length = RFLX.DCCP.Option.Byte_Size (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Value (Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       RFLX.DCCP.Option.Valid_Predecessor (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_Condition (Ctx : Context; Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Predecessor (Ctx, Fld)
       and then RFLX.DCCP.Option.Valid_Value (Fld, Val)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, Fld)
       and then RFLX.DCCP.Option.Sufficient_Space (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.DCCP.Option.Valid_Next (Ctx, Fld),
     Post =>
       (case Fld is
           when F_NDP_Count_Opt | F_Option_Value | F_Elapsed_Time_Opt =>
              Field_Size'Result rem RFLX_Types.Byte'Size = 0,
           when others =>
              True);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       RFLX.DCCP.Option.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length with
     Pre =>
       RFLX.DCCP.Option.Valid_Next (Ctx, Fld)
       and then RFLX.DCCP.Option.Sufficient_Space (Ctx, Fld),
     Post =>
       (case Fld is
           when F_NDP_Count_Opt | F_Option_Value | F_Elapsed_Time_Opt =>
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
       RFLX.DCCP.Option.Valid_Next (Ctx, Fld);

   function Sufficient_Space (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       RFLX.DCCP.Option.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and RFLX.DCCP.Option.Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx),
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
       RFLX.DCCP.Option.Has_Buffer (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Incomplete_Message (Ctx : Context) return Boolean with
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "precondition is always False");

   function Get_Option_Type (Ctx : Context) return RFLX.DCCP.Opt_Type with
     Pre =>
       RFLX.DCCP.Option.Valid (Ctx, RFLX.DCCP.Option.F_Option_Type);

   function Get_Option_Length (Ctx : Context) return RFLX.DCCP.Option_Length_Type with
     Pre =>
       RFLX.DCCP.Option.Valid (Ctx, RFLX.DCCP.Option.F_Option_Length);

   function Get_Loss_Event_Rate (Ctx : Context) return RFLX.DCCP.Loss_Rate_Type with
     Pre =>
       RFLX.DCCP.Option.Valid (Ctx, RFLX.DCCP.Option.F_Loss_Event_Rate);

   function Get_Option_Feature (Ctx : Context) return RFLX.DCCP.Option_Feature_Type with
     Pre =>
       RFLX.DCCP.Option.Valid (Ctx, RFLX.DCCP.Option.F_Option_Feature);

   function Get_Receive_Rate (Ctx : Context) return RFLX.DCCP.Receive_Rate_Type with
     Pre =>
       RFLX.DCCP.Option.Valid (Ctx, RFLX.DCCP.Option.F_Receive_Rate);

   function Get_Timestamp_Echo_Opt (Ctx : Context) return RFLX.DCCP.Timestamp_Echo_Option_Type with
     Pre =>
       RFLX.DCCP.Option.Valid (Ctx, RFLX.DCCP.Option.F_Timestamp_Echo_Opt);

   function Get_Timestamp_Option (Ctx : Context) return RFLX.DCCP.Timestamp_Option_Type with
     Pre =>
       RFLX.DCCP.Option.Valid (Ctx, RFLX.DCCP.Option.F_Timestamp_Option);

   pragma Warnings (On, "precondition is always False");

   function Get_NDP_Count_Opt (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Well_Formed (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt),
     Post =>
       Get_NDP_Count_Opt'Result'Length = RFLX_Types.To_Length (Field_Size (Ctx, F_NDP_Count_Opt));

   function Get_Option_Value (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Well_Formed (Ctx, RFLX.DCCP.Option.F_Option_Value)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Option_Value),
     Post =>
       Get_Option_Value'Result'Length = RFLX_Types.To_Length (Field_Size (Ctx, F_Option_Value));

   function Get_Elapsed_Time_Opt (Ctx : Context) return RFLX_Types.Bytes with
     Ghost,
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Well_Formed (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt),
     Post =>
       Get_Elapsed_Time_Opt'Result'Length = RFLX_Types.To_Length (Field_Size (Ctx, F_Elapsed_Time_Opt));

   procedure Get_NDP_Count_Opt (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Well_Formed (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)
       and then Data'Length = RFLX_Types.To_Length (RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)),
     Post =>
       Equal (Ctx, F_NDP_Count_Opt, Data);

   procedure Get_Option_Value (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Well_Formed (Ctx, RFLX.DCCP.Option.F_Option_Value)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Option_Value)
       and then Data'Length = RFLX_Types.To_Length (RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Option_Value)),
     Post =>
       Equal (Ctx, F_Option_Value, Data);

   procedure Get_Elapsed_Time_Opt (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Well_Formed (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)
       and then Data'Length = RFLX_Types.To_Length (RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)),
     Post =>
       Equal (Ctx, F_Elapsed_Time_Opt, Data);

   generic
      with procedure Process_NDP_Count_Opt (NDP_Count_Opt : RFLX_Types.Bytes);
   procedure Generic_Get_NDP_Count_Opt (Ctx : Context) with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and RFLX.DCCP.Option.Present (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt);

   generic
      with procedure Process_Option_Value (Option_Value : RFLX_Types.Bytes);
   procedure Generic_Get_Option_Value (Ctx : Context) with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and RFLX.DCCP.Option.Present (Ctx, RFLX.DCCP.Option.F_Option_Value);

   generic
      with procedure Process_Elapsed_Time_Opt (Elapsed_Time_Opt : RFLX_Types.Bytes);
   procedure Generic_Get_Elapsed_Time_Opt (Ctx : Context) with
     Pre =>
       RFLX.DCCP.Option.Has_Buffer (Ctx)
       and RFLX.DCCP.Option.Present (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt);

   pragma Warnings (Off, "postcondition does not mention function result");

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean with
     Pre =>
       RFLX.DCCP.Option.Valid_Next (Ctx, Fld),
     Post =>
       True;

   pragma Warnings (On, "postcondition does not mention function result");

   pragma Warnings (Off, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Set_Option_Type (Ctx : in out Context; Val : RFLX.DCCP.Opt_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Option_Type)
       and then RFLX.DCCP.Valid_Opt_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Option_Type) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Option_Type)
       and then RFLX.DCCP.Option.Field_Condition (Ctx, RFLX.DCCP.Option.F_Option_Type, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Option_Type)
       and Get_Option_Type (Ctx) = Val
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Type))
       and Invalid (Ctx, F_Option_Length)
       and Invalid (Ctx, F_Loss_Event_Rate)
       and Invalid (Ctx, F_NDP_Count_Opt)
       and Invalid (Ctx, F_Option_Feature)
       and Invalid (Ctx, F_Receive_Rate)
       and Invalid (Ctx, F_Timestamp_Echo_Opt)
       and Invalid (Ctx, F_Timestamp_Option)
       and Invalid (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.SLOW_RECEIVER))
               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.PADDING))
               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.MANDATORY))
            then
               Predecessor (Ctx, F_Option_Length) = F_Option_Type
               and Valid_Next (Ctx, F_Option_Length))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Type) = Predecessor (Ctx, F_Option_Type)'Old
       and Valid_Next (Ctx, F_Option_Type) = Valid_Next (Ctx, F_Option_Type)'Old
       and Field_First (Ctx, F_Option_Type) = Field_First (Ctx, F_Option_Type)'Old;

   procedure Set_Option_Length (Ctx : in out Context; Val : RFLX.DCCP.Option_Length_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Option_Length)
       and then RFLX.DCCP.Valid_Option_Length_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Option_Length) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Option_Length)
       and then RFLX.DCCP.Option.Field_Condition (Ctx, RFLX.DCCP.Option.F_Option_Length, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Option_Length)
       and Get_Option_Length (Ctx) = Val
       and Invalid (Ctx, F_Loss_Event_Rate)
       and Invalid (Ctx, F_NDP_Count_Opt)
       and Invalid (Ctx, F_Option_Feature)
       and Invalid (Ctx, F_Receive_Rate)
       and Invalid (Ctx, F_Timestamp_Echo_Opt)
       and Invalid (Ctx, F_Timestamp_Option)
       and Invalid (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and (if
               Get_Option_Length (Ctx) >= 4
               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.ELAPSED_TIME))
            then
               Predecessor (Ctx, F_Elapsed_Time_Opt) = F_Option_Length
               and Valid_Next (Ctx, F_Elapsed_Time_Opt))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_LOSS_EVT_RATE))
            then
               Predecessor (Ctx, F_Loss_Event_Rate) = F_Option_Length
               and Valid_Next (Ctx, F_Loss_Event_Rate))
       and (if
               Get_Option_Length (Ctx) >= 3
               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NDP_COUNT))
            then
               Predecessor (Ctx, F_NDP_Count_Opt) = F_Option_Length
               and Valid_Next (Ctx, F_NDP_Count_Opt))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_R))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_L))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_L))
               or RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_R))
            then
               Predecessor (Ctx, F_Option_Feature) = F_Option_Length
               and Valid_Next (Ctx, F_Option_Feature))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_RCV_RATE))
            then
               Predecessor (Ctx, F_Receive_Rate) = F_Option_Length
               and Valid_Next (Ctx, F_Receive_Rate))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))
            then
               Predecessor (Ctx, F_Timestamp_Echo_Opt) = F_Option_Length
               and Valid_Next (Ctx, F_Timestamp_Echo_Opt))
       and (if
               RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP))
            then
               Predecessor (Ctx, F_Timestamp_Option) = F_Option_Length
               and Valid_Next (Ctx, F_Timestamp_Option))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Length) = Predecessor (Ctx, F_Option_Length)'Old
       and Valid_Next (Ctx, F_Option_Length) = Valid_Next (Ctx, F_Option_Length)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Field_First (Ctx, F_Option_Length) = Field_First (Ctx, F_Option_Length)'Old
       and (for all F in Field range F_Option_Type .. F_Option_Type =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Loss_Event_Rate (Ctx : in out Context; Val : RFLX.DCCP.Loss_Rate_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Loss_Event_Rate)
       and then RFLX.DCCP.Valid_Loss_Rate_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Loss_Event_Rate) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Loss_Event_Rate)
       and then RFLX.DCCP.Option.Field_Condition (Ctx, RFLX.DCCP.Option.F_Loss_Event_Rate, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Loss_Event_Rate)
       and Get_Loss_Event_Rate (Ctx) = Val
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Loss_Event_Rate))
       and Invalid (Ctx, F_NDP_Count_Opt)
       and Invalid (Ctx, F_Option_Feature)
       and Invalid (Ctx, F_Receive_Rate)
       and Invalid (Ctx, F_Timestamp_Echo_Opt)
       and Invalid (Ctx, F_Timestamp_Option)
       and Invalid (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Loss_Event_Rate) = Predecessor (Ctx, F_Loss_Event_Rate)'Old
       and Valid_Next (Ctx, F_Loss_Event_Rate) = Valid_Next (Ctx, F_Loss_Event_Rate)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Loss_Event_Rate) = Field_First (Ctx, F_Loss_Event_Rate)'Old
       and (for all F in Field range F_Option_Type .. F_Option_Length =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Option_Feature (Ctx : in out Context; Val : RFLX.DCCP.Option_Feature_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Option_Feature)
       and then RFLX.DCCP.Valid_Option_Feature_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Option_Feature) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Option_Feature)
       and then RFLX.DCCP.Option.Field_Condition (Ctx, RFLX.DCCP.Option.F_Option_Feature, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Option_Feature)
       and Get_Option_Feature (Ctx) = Val
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Feature))
       and Invalid (Ctx, F_Receive_Rate)
       and Invalid (Ctx, F_Timestamp_Echo_Opt)
       and Invalid (Ctx, F_Timestamp_Option)
       and Invalid (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and (Predecessor (Ctx, F_Option_Value) = F_Option_Feature
            and Valid_Next (Ctx, F_Option_Value))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Feature) = Predecessor (Ctx, F_Option_Feature)'Old
       and Valid_Next (Ctx, F_Option_Feature) = Valid_Next (Ctx, F_Option_Feature)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Option_Feature) = Field_First (Ctx, F_Option_Feature)'Old
       and (for all F in Field range F_Option_Type .. F_NDP_Count_Opt =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Receive_Rate (Ctx : in out Context; Val : RFLX.DCCP.Receive_Rate_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Receive_Rate)
       and then RFLX.DCCP.Valid_Receive_Rate_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Receive_Rate) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Receive_Rate)
       and then RFLX.DCCP.Option.Field_Condition (Ctx, RFLX.DCCP.Option.F_Receive_Rate, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Receive_Rate)
       and Get_Receive_Rate (Ctx) = Val
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Receive_Rate))
       and Invalid (Ctx, F_Timestamp_Echo_Opt)
       and Invalid (Ctx, F_Timestamp_Option)
       and Invalid (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Receive_Rate) = Predecessor (Ctx, F_Receive_Rate)'Old
       and Valid_Next (Ctx, F_Receive_Rate) = Valid_Next (Ctx, F_Receive_Rate)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Receive_Rate) = Field_First (Ctx, F_Receive_Rate)'Old
       and (for all F in Field range F_Option_Type .. F_Option_Feature =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Timestamp_Echo_Opt (Ctx : in out Context; Val : RFLX.DCCP.Timestamp_Echo_Option_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Timestamp_Echo_Opt)
       and then RFLX.DCCP.Valid_Timestamp_Echo_Option_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Timestamp_Echo_Opt) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Timestamp_Echo_Opt)
       and then RFLX.DCCP.Option.Field_Condition (Ctx, RFLX.DCCP.Option.F_Timestamp_Echo_Opt, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Timestamp_Echo_Opt)
       and Get_Timestamp_Echo_Opt (Ctx) = Val
       and Invalid (Ctx, F_Timestamp_Option)
       and Invalid (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and (if
               Get_Option_Length (Ctx) >= 8
               and RFLX_Types.Base_Integer (To_Base_Integer (Get_Option_Type (Ctx))) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))
            then
               Predecessor (Ctx, F_Elapsed_Time_Opt) = F_Timestamp_Echo_Opt
               and Valid_Next (Ctx, F_Elapsed_Time_Opt))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Timestamp_Echo_Opt) = Predecessor (Ctx, F_Timestamp_Echo_Opt)'Old
       and Valid_Next (Ctx, F_Timestamp_Echo_Opt) = Valid_Next (Ctx, F_Timestamp_Echo_Opt)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Timestamp_Echo_Opt) = Field_First (Ctx, F_Timestamp_Echo_Opt)'Old
       and (for all F in Field range F_Option_Type .. F_Receive_Rate =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   procedure Set_Timestamp_Option (Ctx : in out Context; Val : RFLX.DCCP.Timestamp_Option_Type) with
     Inline_Always,
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Timestamp_Option)
       and then RFLX.DCCP.Valid_Timestamp_Option_Type (RFLX.DCCP.To_Base_Integer (Val))
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Timestamp_Option) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Timestamp_Option)
       and then RFLX.DCCP.Option.Field_Condition (Ctx, RFLX.DCCP.Option.F_Timestamp_Option, RFLX.DCCP.To_Base_Integer (Val)),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Timestamp_Option)
       and Get_Timestamp_Option (Ctx) = Val
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Timestamp_Option))
       and Invalid (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Timestamp_Option) = Predecessor (Ctx, F_Timestamp_Option)'Old
       and Valid_Next (Ctx, F_Timestamp_Option) = Valid_Next (Ctx, F_Timestamp_Option)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Timestamp_Option) = Field_First (Ctx, F_Timestamp_Option)'Old
       and (for all F in Field range F_Option_Type .. F_Timestamp_Echo_Opt =>
               Context_Cursors_Index (Context_Cursors (Ctx), F) = Context_Cursors_Index (Context_Cursors (Ctx)'Old, F));

   pragma Warnings (On, "aspect ""*"" not enforced on inlined subprogram ""*""");

   procedure Initialize_NDP_Count_Opt (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_NDP_Count_Opt)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_NDP_Count_Opt))
       and Invalid (Ctx, F_Option_Feature)
       and Invalid (Ctx, F_Receive_Rate)
       and Invalid (Ctx, F_Timestamp_Echo_Opt)
       and Invalid (Ctx, F_Timestamp_Option)
       and Invalid (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_NDP_Count_Opt) = Predecessor (Ctx, F_NDP_Count_Opt)'Old
       and Valid_Next (Ctx, F_NDP_Count_Opt) = Valid_Next (Ctx, F_NDP_Count_Opt)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_NDP_Count_Opt) = Field_First (Ctx, F_NDP_Count_Opt)'Old;

   procedure Initialize_Option_Value (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Option_Value)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Option_Value) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Option_Value),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Option_Value)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Value))
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Value) = Predecessor (Ctx, F_Option_Value)'Old
       and Valid_Next (Ctx, F_Option_Value) = Valid_Next (Ctx, F_Option_Value)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Get_Option_Feature (Ctx) = Get_Option_Feature (Ctx)'Old
       and Field_First (Ctx, F_Option_Value) = Field_First (Ctx, F_Option_Value)'Old;

   procedure Initialize_Elapsed_Time_Opt (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Elapsed_Time_Opt)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Elapsed_Time_Opt))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Elapsed_Time_Opt) = Predecessor (Ctx, F_Elapsed_Time_Opt)'Old
       and Valid_Next (Ctx, F_Elapsed_Time_Opt) = Valid_Next (Ctx, F_Elapsed_Time_Opt)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Elapsed_Time_Opt) = Field_First (Ctx, F_Elapsed_Time_Opt)'Old;

   procedure Set_NDP_Count_Opt (Ctx : in out Context; Data : RFLX_Types.Bytes) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)
       and then RFLX.DCCP.Option.Valid_Length (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt, Data'Length)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt) >= Data'Length * RFLX_Types.Byte'Size
       and then RFLX.DCCP.Option.Field_Condition (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt, 0),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_NDP_Count_Opt)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_NDP_Count_Opt))
       and Invalid (Ctx, F_Option_Feature)
       and Invalid (Ctx, F_Receive_Rate)
       and Invalid (Ctx, F_Timestamp_Echo_Opt)
       and Invalid (Ctx, F_Timestamp_Option)
       and Invalid (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_NDP_Count_Opt) = Predecessor (Ctx, F_NDP_Count_Opt)'Old
       and Valid_Next (Ctx, F_NDP_Count_Opt) = Valid_Next (Ctx, F_NDP_Count_Opt)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_NDP_Count_Opt) = Field_First (Ctx, F_NDP_Count_Opt)'Old
       and Equal (Ctx, F_NDP_Count_Opt, Data);

   procedure Set_Option_Value (Ctx : in out Context; Data : RFLX_Types.Bytes) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Option_Value)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Option_Value) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Option_Value)
       and then RFLX.DCCP.Option.Valid_Length (Ctx, RFLX.DCCP.Option.F_Option_Value, Data'Length)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Option_Value) >= Data'Length * RFLX_Types.Byte'Size
       and then RFLX.DCCP.Option.Field_Condition (Ctx, RFLX.DCCP.Option.F_Option_Value, 0),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Option_Value)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Value))
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Value) = Predecessor (Ctx, F_Option_Value)'Old
       and Valid_Next (Ctx, F_Option_Value) = Valid_Next (Ctx, F_Option_Value)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Get_Option_Feature (Ctx) = Get_Option_Feature (Ctx)'Old
       and Field_First (Ctx, F_Option_Value) = Field_First (Ctx, F_Option_Value)'Old
       and Equal (Ctx, F_Option_Value, Data);

   procedure Set_Elapsed_Time_Opt (Ctx : in out Context; Data : RFLX_Types.Bytes) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)
       and then RFLX.DCCP.Option.Valid_Length (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt, Data'Length)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt) >= Data'Length * RFLX_Types.Byte'Size
       and then RFLX.DCCP.Option.Field_Condition (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt, 0),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Elapsed_Time_Opt)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Elapsed_Time_Opt))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Elapsed_Time_Opt) = Predecessor (Ctx, F_Elapsed_Time_Opt)'Old
       and Valid_Next (Ctx, F_Elapsed_Time_Opt) = Valid_Next (Ctx, F_Elapsed_Time_Opt)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Elapsed_Time_Opt) = Field_First (Ctx, F_Elapsed_Time_Opt)'Old
       and Equal (Ctx, F_Elapsed_Time_Opt, Data);

   generic
      with procedure Process_NDP_Count_Opt (NDP_Count_Opt : out RFLX_Types.Bytes);
      with function Process_Data_Pre (Length : RFLX_Types.Length) return Boolean;
   procedure Generic_Set_NDP_Count_Opt (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)
       and then RFLX.DCCP.Option.Valid_Length (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt, Length)
       and then RFLX_Types.To_Length (RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_NDP_Count_Opt)) >= Length
       and then Process_Data_Pre (Length),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_NDP_Count_Opt)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_NDP_Count_Opt))
       and Invalid (Ctx, F_Option_Feature)
       and Invalid (Ctx, F_Receive_Rate)
       and Invalid (Ctx, F_Timestamp_Echo_Opt)
       and Invalid (Ctx, F_Timestamp_Option)
       and Invalid (Ctx, F_Option_Value)
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_NDP_Count_Opt) = Predecessor (Ctx, F_NDP_Count_Opt)'Old
       and Valid_Next (Ctx, F_NDP_Count_Opt) = Valid_Next (Ctx, F_NDP_Count_Opt)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_NDP_Count_Opt) = Field_First (Ctx, F_NDP_Count_Opt)'Old;

   generic
      with procedure Process_Option_Value (Option_Value : out RFLX_Types.Bytes);
      with function Process_Data_Pre (Length : RFLX_Types.Length) return Boolean;
   procedure Generic_Set_Option_Value (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Option_Value)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Option_Value) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Option_Value)
       and then RFLX.DCCP.Option.Valid_Length (Ctx, RFLX.DCCP.Option.F_Option_Value, Length)
       and then RFLX_Types.To_Length (RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Option_Value)) >= Length
       and then Process_Data_Pre (Length),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Option_Value)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Value))
       and Invalid (Ctx, F_Elapsed_Time_Opt)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Value) = Predecessor (Ctx, F_Option_Value)'Old
       and Valid_Next (Ctx, F_Option_Value) = Valid_Next (Ctx, F_Option_Value)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Get_Option_Feature (Ctx) = Get_Option_Feature (Ctx)'Old
       and Field_First (Ctx, F_Option_Value) = Field_First (Ctx, F_Option_Value)'Old;

   generic
      with procedure Process_Elapsed_Time_Opt (Elapsed_Time_Opt : out RFLX_Types.Bytes);
      with function Process_Data_Pre (Length : RFLX_Types.Length) return Boolean;
   procedure Generic_Set_Elapsed_Time_Opt (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Option.Has_Buffer (Ctx)
       and then RFLX.DCCP.Option.Valid_Next (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)
       and then RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt) >= RFLX.DCCP.Option.Field_Size (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)
       and then RFLX.DCCP.Option.Valid_Length (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt, Length)
       and then RFLX_Types.To_Length (RFLX.DCCP.Option.Available_Space (Ctx, RFLX.DCCP.Option.F_Elapsed_Time_Opt)) >= Length
       and then Process_Data_Pre (Length),
     Post =>
       Has_Buffer (Ctx)
       and Well_Formed (Ctx, F_Elapsed_Time_Opt)
       and (if Well_Formed_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Elapsed_Time_Opt))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Elapsed_Time_Opt) = Predecessor (Ctx, F_Elapsed_Time_Opt)'Old
       and Valid_Next (Ctx, F_Elapsed_Time_Opt) = Valid_Next (Ctx, F_Elapsed_Time_Opt)'Old
       and Get_Option_Type (Ctx) = Get_Option_Type (Ctx)'Old
       and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and Field_First (Ctx, F_Elapsed_Time_Opt) = Field_First (Ctx, F_Elapsed_Time_Opt)'Old;

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
                    Well_Formed (Cursors (F_Option_Length))
                 then
                    (Valid (Cursors (F_Option_Type))
                     and then Cursors (F_Option_Length).Predecessor = F_Option_Type
                     and then (RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.SLOW_RECEIVER))
                               and RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.PADDING))
                               and RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.MANDATORY)))))
                and then (if
                             Well_Formed (Cursors (F_Loss_Event_Rate))
                          then
                             (Valid (Cursors (F_Option_Length))
                              and then Cursors (F_Loss_Event_Rate).Predecessor = F_Option_Length
                              and then RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_LOSS_EVT_RATE))))
                and then (if
                             Well_Formed (Cursors (F_NDP_Count_Opt))
                          then
                             (Valid (Cursors (F_Option_Length))
                              and then Cursors (F_NDP_Count_Opt).Predecessor = F_Option_Length
                              and then (Cursors (F_Option_Length).Value >= 3
                                        and RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NDP_COUNT)))))
                and then (if
                             Well_Formed (Cursors (F_Option_Feature))
                          then
                             (Valid (Cursors (F_Option_Length))
                              and then Cursors (F_Option_Feature).Predecessor = F_Option_Length
                              and then (RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_R))
                                        or RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_L))
                                        or RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_L))
                                        or RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_R)))))
                and then (if
                             Well_Formed (Cursors (F_Receive_Rate))
                          then
                             (Valid (Cursors (F_Option_Length))
                              and then Cursors (F_Receive_Rate).Predecessor = F_Option_Length
                              and then RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_RCV_RATE))))
                and then (if
                             Well_Formed (Cursors (F_Timestamp_Echo_Opt))
                          then
                             (Valid (Cursors (F_Option_Length))
                              and then Cursors (F_Timestamp_Echo_Opt).Predecessor = F_Option_Length
                              and then RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))))
                and then (if
                             Well_Formed (Cursors (F_Timestamp_Option))
                          then
                             (Valid (Cursors (F_Option_Length))
                              and then Cursors (F_Timestamp_Option).Predecessor = F_Option_Length
                              and then RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP))))
                and then (if
                             Well_Formed (Cursors (F_Option_Value))
                          then
                             (Valid (Cursors (F_Option_Feature))
                              and then Cursors (F_Option_Value).Predecessor = F_Option_Feature))
                and then (if
                             Well_Formed (Cursors (F_Elapsed_Time_Opt))
                          then
                             (Valid (Cursors (F_Option_Length))
                              and then Cursors (F_Elapsed_Time_Opt).Predecessor = F_Option_Length
                              and then (Cursors (F_Option_Length).Value >= 4
                                        and RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.ELAPSED_TIME))))
                             or (Valid (Cursors (F_Timestamp_Echo_Opt))
                                 and then Cursors (F_Elapsed_Time_Opt).Predecessor = F_Timestamp_Echo_Opt
                                 and then (Cursors (F_Option_Length).Value >= 8
                                           and RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))))))
      and then ((if Invalid (Cursors (F_Option_Type)) then Invalid (Cursors (F_Option_Length)))
                and then (if Invalid (Cursors (F_Option_Length)) then Invalid (Cursors (F_Loss_Event_Rate)))
                and then (if Invalid (Cursors (F_Option_Length)) then Invalid (Cursors (F_NDP_Count_Opt)))
                and then (if Invalid (Cursors (F_Option_Length)) then Invalid (Cursors (F_Option_Feature)))
                and then (if Invalid (Cursors (F_Option_Length)) then Invalid (Cursors (F_Receive_Rate)))
                and then (if Invalid (Cursors (F_Option_Length)) then Invalid (Cursors (F_Timestamp_Echo_Opt)))
                and then (if Invalid (Cursors (F_Option_Length)) then Invalid (Cursors (F_Timestamp_Option)))
                and then (if Invalid (Cursors (F_Option_Feature)) then Invalid (Cursors (F_Option_Value)))
                and then (if
                             Invalid (Cursors (F_Option_Length))
                             and then Invalid (Cursors (F_Timestamp_Echo_Opt))
                          then
                             Invalid (Cursors (F_Elapsed_Time_Opt))))
      and then ((if
                    Well_Formed (Cursors (F_Option_Type))
                 then
                    (Cursors (F_Option_Type).Last - Cursors (F_Option_Type).First + 1 = 8
                     and then Cursors (F_Option_Type).Predecessor = F_Initial
                     and then Cursors (F_Option_Type).First = First))
                and then (if
                             Well_Formed (Cursors (F_Option_Length))
                          then
                             (Cursors (F_Option_Length).Last - Cursors (F_Option_Length).First + 1 = 8
                              and then Cursors (F_Option_Length).Predecessor = F_Option_Type
                              and then Cursors (F_Option_Length).First = Cursors (F_Option_Type).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Loss_Event_Rate))
                          then
                             (Cursors (F_Loss_Event_Rate).Last - Cursors (F_Loss_Event_Rate).First + 1 = 32
                              and then Cursors (F_Loss_Event_Rate).Predecessor = F_Option_Length
                              and then Cursors (F_Loss_Event_Rate).First = Cursors (F_Option_Length).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_NDP_Count_Opt))
                          then
                             (Cursors (F_NDP_Count_Opt).Last - Cursors (F_NDP_Count_Opt).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Option_Length).Value) * 8 - 16
                              and then Cursors (F_NDP_Count_Opt).Predecessor = F_Option_Length
                              and then Cursors (F_NDP_Count_Opt).First = Cursors (F_Option_Length).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Option_Feature))
                          then
                             (Cursors (F_Option_Feature).Last - Cursors (F_Option_Feature).First + 1 = 8
                              and then Cursors (F_Option_Feature).Predecessor = F_Option_Length
                              and then Cursors (F_Option_Feature).First = Cursors (F_Option_Length).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Receive_Rate))
                          then
                             (Cursors (F_Receive_Rate).Last - Cursors (F_Receive_Rate).First + 1 = 32
                              and then Cursors (F_Receive_Rate).Predecessor = F_Option_Length
                              and then Cursors (F_Receive_Rate).First = Cursors (F_Option_Length).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Timestamp_Echo_Opt))
                          then
                             (Cursors (F_Timestamp_Echo_Opt).Last - Cursors (F_Timestamp_Echo_Opt).First + 1 = 32
                              and then Cursors (F_Timestamp_Echo_Opt).Predecessor = F_Option_Length
                              and then Cursors (F_Timestamp_Echo_Opt).First = Cursors (F_Option_Length).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Timestamp_Option))
                          then
                             (Cursors (F_Timestamp_Option).Last - Cursors (F_Timestamp_Option).First + 1 = 32
                              and then Cursors (F_Timestamp_Option).Predecessor = F_Option_Length
                              and then Cursors (F_Timestamp_Option).First = Cursors (F_Option_Length).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Option_Value))
                          then
                             (Cursors (F_Option_Value).Last - Cursors (F_Option_Value).First + 1 = 8
                              and then Cursors (F_Option_Value).Predecessor = F_Option_Feature
                              and then Cursors (F_Option_Value).First = Cursors (F_Option_Feature).Last + 1))
                and then (if
                             Well_Formed (Cursors (F_Elapsed_Time_Opt))
                          then
                             (if
                                 Well_Formed (Cursors (F_Option_Length))
                                 and then (Cursors (F_Option_Length).Value >= 4
                                           and RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.ELAPSED_TIME)))
                              then
                                 Cursors (F_Elapsed_Time_Opt).Last - Cursors (F_Elapsed_Time_Opt).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Option_Length).Value) * 8 - 16
                                 and then Cursors (F_Elapsed_Time_Opt).Predecessor = F_Option_Length
                                 and then Cursors (F_Elapsed_Time_Opt).First = Cursors (F_Option_Length).Last + 1)
                             and then (if
                                          Well_Formed (Cursors (F_Timestamp_Echo_Opt))
                                          and then (Cursors (F_Option_Length).Value >= 8
                                                    and RFLX_Types.Base_Integer (Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO)))
                                       then
                                          Cursors (F_Elapsed_Time_Opt).Last - Cursors (F_Elapsed_Time_Opt).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Option_Length).Value) * 8 - 48
                                          and then Cursors (F_Elapsed_Time_Opt).Predecessor = F_Timestamp_Echo_Opt
                                          and then Cursors (F_Elapsed_Time_Opt).First = Cursors (F_Timestamp_Echo_Opt).Last + 1))))
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
      and then Valid_Next (Ctx, F_Option_Type)
      and then RFLX.DCCP.Option.Field_First (Ctx, RFLX.DCCP.Option.F_Option_Type) rem RFLX_Types.Byte'Size = 1
      and then Available_Space (Ctx, F_Option_Type) = Ctx.Last - Ctx.First + 1
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
          when F_Option_Type =>
             RFLX.DCCP.Valid_Opt_Type (Val),
          when F_Option_Length =>
             RFLX.DCCP.Valid_Option_Length_Type (Val),
          when F_Loss_Event_Rate =>
             RFLX.DCCP.Valid_Loss_Rate_Type (Val),
          when F_NDP_Count_Opt =>
             True,
          when F_Option_Feature =>
             RFLX.DCCP.Valid_Option_Feature_Type (Val),
          when F_Receive_Rate =>
             RFLX.DCCP.Valid_Receive_Rate_Type (Val),
          when F_Timestamp_Echo_Opt =>
             RFLX.DCCP.Valid_Timestamp_Echo_Option_Type (Val),
          when F_Timestamp_Option =>
             RFLX.DCCP.Valid_Timestamp_Option_Type (Val),
          when F_Option_Value | F_Elapsed_Time_Opt =>
             True));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial | F_Loss_Event_Rate | F_NDP_Count_Opt | F_Option_Feature | F_Receive_Rate | F_Timestamp_Option | F_Option_Value | F_Elapsed_Time_Opt | F_Final =>
             True,
          when F_Option_Type =>
             RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.SLOW_RECEIVER))
             and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.PADDING))
             and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.MANDATORY)),
          when F_Option_Length =>
             (case Fld is
                 when F_Elapsed_Time_Opt =>
                    Ctx.Cursors (F_Option_Length).Value >= 4
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.ELAPSED_TIME)),
                 when F_Loss_Event_Rate =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_LOSS_EVT_RATE)),
                 when F_NDP_Count_Opt =>
                    Ctx.Cursors (F_Option_Length).Value >= 3
                    and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NDP_COUNT)),
                 when F_Option_Feature =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_R))
                    or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_L))
                    or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_L))
                    or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_R)),
                 when F_Receive_Rate =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_RCV_RATE)),
                 when F_Timestamp_Echo_Opt =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO)),
                 when F_Timestamp_Option =>
                    RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP)),
                 when others =>
                    False),
          when F_Timestamp_Echo_Opt =>
             Ctx.Cursors (F_Option_Length).Value >= 8
             and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))));

   function Field_Condition (Ctx : Context; Fld : Field; Val : RFLX_Types.Base_Integer) return Boolean is
     ((case Fld is
          when F_Option_Type =>
             (Val /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.SLOW_RECEIVER))
              and Val /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.PADDING))
              and Val /= RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.MANDATORY)))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.SLOW_RECEIVER))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.PADDING))
             or Val = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.MANDATORY)),
          when F_Option_Length =>
             (Val >= 4
              and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.ELAPSED_TIME)))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_LOSS_EVT_RATE))
             or (Val >= 3
                 and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.NDP_COUNT)))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CCID3_RCV_RATE))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_R))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_L))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_L))
             or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CHANGE_R)),
          when F_Loss_Event_Rate | F_NDP_Count_Opt | F_Option_Feature | F_Receive_Rate =>
             True,
          when F_Timestamp_Echo_Opt =>
             Ctx.Cursors (F_Option_Length).Value >= 8
             and RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO)),
          when F_Timestamp_Option | F_Option_Value | F_Elapsed_Time_Opt =>
             True));

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     ((case Fld is
          when F_Option_Type | F_Option_Length =>
             8,
          when F_Loss_Event_Rate =>
             32,
          when F_NDP_Count_Opt =>
             RFLX_Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value) * 8 - 16,
          when F_Option_Feature =>
             8,
          when F_Receive_Rate | F_Timestamp_Echo_Opt | F_Timestamp_Option =>
             32,
          when F_Option_Value =>
             8,
          when F_Elapsed_Time_Opt =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Option_Length
                 and then (Ctx.Cursors (F_Option_Length).Value >= 4
                           and RFLX_Types.Bit_Length (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.ELAPSED_TIME)))
              then
                 RFLX_Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value) * 8 - 16
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Timestamp_Echo_Opt
                 and then (Ctx.Cursors (F_Option_Length).Value >= 8
                           and RFLX_Types.Bit_Length (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Bit_Length (To_Base_Integer (RFLX.DCCP.TIMESTAMP_ECHO)))
              then
                 RFLX_Types.Bit_Length (Ctx.Cursors (F_Option_Length).Value) * 8 - 48
              else
                 RFLX_Types.Unreachable)));

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index is
     ((if Fld = F_Option_Type then Ctx.First else Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1));

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
          when F_Option_Type =>
             Ctx.Cursors (Fld).Predecessor = F_Initial,
          when F_Option_Length =>
             (Valid (Ctx.Cursors (F_Option_Type))
              and Ctx.Cursors (Fld).Predecessor = F_Option_Type),
          when F_Loss_Event_Rate | F_NDP_Count_Opt | F_Option_Feature | F_Receive_Rate | F_Timestamp_Echo_Opt | F_Timestamp_Option =>
             (Valid (Ctx.Cursors (F_Option_Length))
              and Ctx.Cursors (Fld).Predecessor = F_Option_Length),
          when F_Option_Value =>
             (Valid (Ctx.Cursors (F_Option_Feature))
              and Ctx.Cursors (Fld).Predecessor = F_Option_Feature),
          when F_Elapsed_Time_Opt =>
             (Valid (Ctx.Cursors (F_Option_Length))
              and Ctx.Cursors (Fld).Predecessor = F_Option_Length)
             or (Valid (Ctx.Cursors (F_Timestamp_Echo_Opt))
                 and Ctx.Cursors (Fld).Predecessor = F_Timestamp_Echo_Opt),
          when F_Final =>
             (Well_Formed (Ctx.Cursors (F_Elapsed_Time_Opt))
              and Ctx.Cursors (Fld).Predecessor = F_Elapsed_Time_Opt)
             or (Valid (Ctx.Cursors (F_Loss_Event_Rate))
                 and Ctx.Cursors (Fld).Predecessor = F_Loss_Event_Rate)
             or (Well_Formed (Ctx.Cursors (F_NDP_Count_Opt))
                 and Ctx.Cursors (Fld).Predecessor = F_NDP_Count_Opt)
             or (Valid (Ctx.Cursors (F_Option_Feature))
                 and Ctx.Cursors (Fld).Predecessor = F_Option_Feature)
             or (Valid (Ctx.Cursors (F_Option_Type))
                 and Ctx.Cursors (Fld).Predecessor = F_Option_Type)
             or (Well_Formed (Ctx.Cursors (F_Option_Value))
                 and Ctx.Cursors (Fld).Predecessor = F_Option_Value)
             or (Valid (Ctx.Cursors (F_Receive_Rate))
                 and Ctx.Cursors (Fld).Predecessor = F_Receive_Rate)
             or (Valid (Ctx.Cursors (F_Timestamp_Option))
                 and Ctx.Cursors (Fld).Predecessor = F_Timestamp_Option)));

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
     (Well_Formed (Ctx, F_Elapsed_Time_Opt)
      or Valid (Ctx, F_Loss_Event_Rate)
      or Well_Formed (Ctx, F_NDP_Count_Opt)
      or (Valid (Ctx, F_Option_Feature)
          and then ((RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_L))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_R)))
                    and (RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Feature).Value) < RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.FEATURE_RESERVED))
                         or Ctx.Cursors (F_Option_Feature).Value > 255)))
      or (Valid (Ctx, F_Option_Type)
          and then (RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.SLOW_RECEIVER))
                    or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.PADDING))
                    or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.MANDATORY))))
      or Well_Formed (Ctx, F_Option_Value)
      or Valid (Ctx, F_Receive_Rate)
      or Valid (Ctx, F_Timestamp_Option));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Elapsed_Time_Opt)
      or Valid (Ctx, F_Loss_Event_Rate)
      or Valid (Ctx, F_NDP_Count_Opt)
      or (Valid (Ctx, F_Option_Feature)
          and then ((RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_L))
                     or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.CONFIRM_R)))
                    and (RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Feature).Value) < RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.FEATURE_RESERVED))
                         or Ctx.Cursors (F_Option_Feature).Value > 255)))
      or (Valid (Ctx, F_Option_Type)
          and then (RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.SLOW_RECEIVER))
                    or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.PADDING))
                    or RFLX_Types.Base_Integer (Ctx.Cursors (F_Option_Type).Value) = RFLX_Types.Base_Integer (To_Base_Integer (RFLX.DCCP.MANDATORY))))
      or Valid (Ctx, F_Option_Value)
      or Valid (Ctx, F_Receive_Rate)
      or Valid (Ctx, F_Timestamp_Option));

   function Incomplete_Message (Ctx : Context) return Boolean is
     ((for some F in Field =>
          Incomplete (Ctx, F)));

   function Get_Option_Type (Ctx : Context) return RFLX.DCCP.Opt_Type is
     (To_Actual (Ctx.Cursors (F_Option_Type).Value));

   function Get_Option_Length (Ctx : Context) return RFLX.DCCP.Option_Length_Type is
     (To_Actual (Ctx.Cursors (F_Option_Length).Value));

   function Get_Loss_Event_Rate (Ctx : Context) return RFLX.DCCP.Loss_Rate_Type is
     (To_Actual (Ctx.Cursors (F_Loss_Event_Rate).Value));

   function Get_Option_Feature (Ctx : Context) return RFLX.DCCP.Option_Feature_Type is
     (To_Actual (Ctx.Cursors (F_Option_Feature).Value));

   function Get_Receive_Rate (Ctx : Context) return RFLX.DCCP.Receive_Rate_Type is
     (To_Actual (Ctx.Cursors (F_Receive_Rate).Value));

   function Get_Timestamp_Echo_Opt (Ctx : Context) return RFLX.DCCP.Timestamp_Echo_Option_Type is
     (To_Actual (Ctx.Cursors (F_Timestamp_Echo_Opt).Value));

   function Get_Timestamp_Option (Ctx : Context) return RFLX.DCCP.Timestamp_Option_Type is
     (To_Actual (Ctx.Cursors (F_Timestamp_Option).Value));

   function Valid_Size (Ctx : Context; Fld : Field; Size : RFLX_Types.Bit_Length) return Boolean is
     (Size = Field_Size (Ctx, Fld))
    with
     Pre =>
       RFLX.DCCP.Option.Valid_Next (Ctx, Fld);

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean is
     (Valid_Size (Ctx, Fld, RFLX_Types.To_Bit_Length (Length)));

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

   function Context_Cursors_Index (Cursors : Field_Cursors; Fld : Field) return Field_Cursor is
     (Cursors (Fld));

end RFLX.DCCP.Option;

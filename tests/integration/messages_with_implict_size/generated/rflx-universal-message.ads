pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.Universal.Option_Types;
with RFLX.Universal.Options;
with RFLX.Universal.Values;

package RFLX.Universal.Message with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Warnings (Off, "use clause for type ""U64"" * has no effect");

   pragma Warnings (Off, """LENGTH"" is already use-visible through previous use_type_clause");

   use type RFLX_Types.Bytes;

   use type RFLX_Types.Bytes_Ptr;

   use type RFLX_Types.Length;

   use type RFLX_Types.Index;

   use type RFLX_Types.Bit_Index;

   use type RFLX_Types.U64;

   pragma Warnings (On, """LENGTH"" is already use-visible through previous use_type_clause");

   pragma Warnings (On, "use clause for type ""U64"" * has no effect");

   type Virtual_Field is (F_Initial, F_Message_Type, F_Length, F_Data, F_Option_Types, F_Options, F_Value, F_Values, F_Final);

   subtype Field is Virtual_Field range F_Message_Type .. F_Values;

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
       and First mod RFLX_Types.Byte'Size = 1
       and Last mod RFLX_Types.Byte'Size = 0;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Data | F_Option_Types | F_Options | F_Values | F_Final =>
               null;
            when F_Message_Type =>
               Message_Type_Value : RFLX.Universal.Message_Type_Base;
            when F_Length =>
               Length_Value : RFLX.Universal.Length_Base;
            when F_Value =>
               Value_Value : RFLX.Universal.Value;
         end case;
      end record;

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
       and then First mod RFLX_Types.Byte'Size = 1
       and then Last mod RFLX_Types.Byte'Size = 0
       and then (Written_Last = 0
                 or (Written_Last >= First - 1
                     and Written_Last <= Last))
       and then Written_Last mod RFLX_Types.Byte'Size = 0,
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

   function Initialized (Ctx : Context) return Boolean with
     Ghost;

   procedure Reset (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and Has_Buffer (Ctx),
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
       and Has_Buffer (Ctx)
       and RFLX_Types.To_Index (First) >= Ctx.Buffer_First
       and RFLX_Types.To_Index (Last) <= Ctx.Buffer_Last
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

   function Read (Ctx : Context) return RFLX_Types.Bytes with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid_Message (Ctx);

   pragma Warnings (Off, "formal parameter ""*"" is not referenced");

   function Always_Valid (Buffer : RFLX_Types.Bytes) return Boolean is
     (True)
    with
     Ghost;

   pragma Warnings (On, "formal parameter ""*"" is not referenced");

   generic
      with procedure Read (Buffer : RFLX_Types.Bytes);
      with function Pre (Buffer : RFLX_Types.Bytes) return Boolean is Always_Valid;
   procedure Generic_Read (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid_Message (Ctx)
       and then Pre (Read (Ctx));

   pragma Warnings (Off, "formal parameter ""*"" is not referenced");

   function Always_Valid (Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) return Boolean is
     (True)
    with
     Ghost;

   pragma Warnings (On, "formal parameter ""*"" is not referenced");

   generic
      with procedure Write (Buffer : out RFLX_Types.Bytes; Length : out RFLX_Types.Length; Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length);
      with function Pre (Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) return Boolean is Always_Valid;
   procedure Generic_Write (Ctx : in out Context; Offset : RFLX_Types.Length := 0) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Offset < Buffer_Length (Ctx)
       and then Pre (Buffer_Length (Ctx), Offset),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
       and Initialized (Ctx);

   function Has_Buffer (Ctx : Context) return Boolean;

   function Buffer_Length (Ctx : Context) return RFLX_Types.Length with
     Pre =>
       Has_Buffer (Ctx);

   function Size (Ctx : Context) return RFLX_Types.Bit_Length;

   function Byte_Size (Ctx : Context) return RFLX_Types.Length;

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid_Message (Ctx);

   function Written_Last (Ctx : Context) return RFLX_Types.Bit_Length;

   function Message_Data (Ctx : Context) return RFLX_Types.Bytes with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid_Message (Ctx),
     Post =>
       Message_Data'Result'Length = Byte_Size (Ctx);

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
       Valid_Next (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Data | F_Option_Types | F_Options | F_Values =>
              Field_Size'Result mod RFLX_Types.Byte'Size = 0,
           when others =>
              True);

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld)
       and then Available_Space (Ctx, Fld) >= Field_Size (Ctx, Fld),
     Post =>
       (case Fld is
           when F_Data | F_Option_Types | F_Options | F_Values =>
              Field_Last'Result mod RFLX_Types.Byte'Size = 0,
           when others =>
              True);

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
       (if Valid'Result then Structural_Valid (Ctx, Fld) and Present (Ctx, Fld));

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

   function Get_Message_Type (Ctx : Context) return RFLX.Universal.Message_Type with
     Pre =>
       Valid (Ctx, F_Message_Type);

   function Get_Length (Ctx : Context) return RFLX.Universal.Length with
     Pre =>
       Valid (Ctx, F_Length);

   function Get_Value (Ctx : Context) return RFLX.Universal.Value with
     Pre =>
       Valid (Ctx, F_Value);

   pragma Warnings (On, "precondition is always False");

   function Get_Data (Ctx : Context) return RFLX_Types.Bytes with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid (Ctx, F_Data)
       and then Valid_Next (Ctx, F_Data),
     Post =>
       Get_Data'Result'Length = RFLX_Types.To_Length (Field_Size (Ctx, F_Data));

   procedure Get_Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid (Ctx, F_Data)
       and then Valid_Next (Ctx, F_Data)
       and then Data'Length >= RFLX_Types.To_Length (Field_Size (Ctx, F_Data));

   generic
      with procedure Process_Data (Data : RFLX_Types.Bytes);
   procedure Generic_Get_Data (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and Present (Ctx, F_Data);

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean with
     Pre =>
       Valid_Next (Ctx, Fld);

   procedure Set_Message_Type (Ctx : in out Context; Val : RFLX.Universal.Message_Type) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Message_Type)
       and then Field_Condition (Ctx, (F_Message_Type, To_Base (Val)))
       and then True
       and then Available_Space (Ctx, F_Message_Type) >= Field_Size (Ctx, F_Message_Type),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Message_Type)
       and Get_Message_Type (Ctx) = Val
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Message_Type))
       and Invalid (Ctx, F_Length)
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Option_Types)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and (if
               RFLX_Types.U64 (To_Base (Get_Message_Type (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data))
            then
               Predecessor (Ctx, F_Data) = F_Message_Type
               and Valid_Next (Ctx, F_Data))
       and (if
               RFLX_Types.U64 (To_Base (Get_Message_Type (Ctx))) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
               and RFLX_Types.U64 (To_Base (Get_Message_Type (Ctx))) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
               and RFLX_Types.U64 (To_Base (Get_Message_Type (Ctx))) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data))
            then
               Predecessor (Ctx, F_Length) = F_Message_Type
               and Valid_Next (Ctx, F_Length))
       and (if
               RFLX_Types.U64 (To_Base (Get_Message_Type (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
            then
               Predecessor (Ctx, F_Options) = F_Message_Type
               and Valid_Next (Ctx, F_Options))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Message_Type) = Predecessor (Ctx, F_Message_Type)'Old
       and Valid_Next (Ctx, F_Message_Type) = Valid_Next (Ctx, F_Message_Type)'Old;

   procedure Set_Length (Ctx : in out Context; Val : RFLX.Universal.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Length)
       and then Field_Condition (Ctx, (F_Length, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Length) >= Field_Size (Ctx, F_Length),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Length)
       and Get_Length (Ctx) = Val
       and Invalid (Ctx, F_Data)
       and Invalid (Ctx, F_Option_Types)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and (if
               RFLX_Types.U64 (To_Base (Get_Message_Type (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Data))
            then
               Predecessor (Ctx, F_Data) = F_Length
               and Valid_Next (Ctx, F_Data))
       and (if
               RFLX_Types.U64 (To_Base (Get_Message_Type (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Option_Types))
            then
               Predecessor (Ctx, F_Option_Types) = F_Length
               and Valid_Next (Ctx, F_Option_Types))
       and (if
               RFLX_Types.U64 (To_Base (Get_Message_Type (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Options))
            then
               Predecessor (Ctx, F_Options) = F_Length
               and Valid_Next (Ctx, F_Options))
       and (if
               RFLX_Types.U64 (To_Base (Get_Message_Type (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Value))
               and RFLX_Types.U64 (Get_Length (Ctx)) = Universal.Value'Size / 8
            then
               Predecessor (Ctx, F_Value) = F_Length
               and Valid_Next (Ctx, F_Value))
       and (if
               RFLX_Types.U64 (To_Base (Get_Message_Type (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Values))
            then
               Predecessor (Ctx, F_Values) = F_Length
               and Valid_Next (Ctx, F_Values))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Length) = Predecessor (Ctx, F_Length)'Old
       and Valid_Next (Ctx, F_Length) = Valid_Next (Ctx, F_Length)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
       and Context_Cursor (Ctx, F_Message_Type) = Context_Cursor (Ctx, F_Message_Type)'Old;

   procedure Set_Value (Ctx : in out Context; Val : RFLX.Universal.Value) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Value)
       and then Field_Condition (Ctx, (F_Value, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Value) >= Field_Size (Ctx, F_Value),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Value)
       and Get_Value (Ctx) = Val
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Value))
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Value) = Predecessor (Ctx, F_Value)'Old
       and Valid_Next (Ctx, F_Value) = Valid_Next (Ctx, F_Value)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and Context_Cursor (Ctx, F_Message_Type) = Context_Cursor (Ctx, F_Message_Type)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Data) = Context_Cursor (Ctx, F_Data)'Old
       and Context_Cursor (Ctx, F_Option_Types) = Context_Cursor (Ctx, F_Option_Types)'Old
       and Context_Cursor (Ctx, F_Options) = Context_Cursor (Ctx, F_Options)'Old;

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
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Invalid (Ctx, F_Option_Types)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old;

   procedure Set_Option_Types_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Option_Types)
       and then Field_Condition (Ctx, (Fld => F_Option_Types))
       and then Available_Space (Ctx, F_Option_Types) >= Field_Size (Ctx, F_Option_Types)
       and then Field_First (Ctx, F_Option_Types) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Option_Types) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Option_Types) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Option_Types) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Option_Types)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Types))
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Types) = Predecessor (Ctx, F_Option_Types)'Old
       and Valid_Next (Ctx, F_Option_Types) = Valid_Next (Ctx, F_Option_Types)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old;

   procedure Set_Options_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Options)
       and then Field_Condition (Ctx, (Fld => F_Options))
       and then Available_Space (Ctx, F_Options) >= Field_Size (Ctx, F_Options)
       and then Field_First (Ctx, F_Options) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Options) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Options) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Options) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Options)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Options))
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old;

   procedure Set_Values_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Values)
       and then Field_Condition (Ctx, (Fld => F_Values))
       and then Available_Space (Ctx, F_Values) >= Field_Size (Ctx, F_Values)
       and then Field_First (Ctx, F_Values) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Values) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Values) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Values) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Values)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Values))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Values) = Predecessor (Ctx, F_Values)'Old
       and Valid_Next (Ctx, F_Values) = Valid_Next (Ctx, F_Values)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old;

   procedure Set_Option_Types (Ctx : in out Context; Seq_Ctx : Universal.Option_Types.Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Option_Types)
       and then Field_Condition (Ctx, (Fld => F_Option_Types))
       and then Available_Space (Ctx, F_Option_Types) >= Field_Size (Ctx, F_Option_Types)
       and then Field_First (Ctx, F_Option_Types) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Option_Types) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Option_Types) mod RFLX_Types.Byte'Size = 0
       and then Valid_Length (Ctx, F_Option_Types, Universal.Option_Types.Byte_Size (Seq_Ctx))
       and then Universal.Option_Types.Has_Buffer (Seq_Ctx)
       and then Universal.Option_Types.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Option_Types)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Types))
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Types) = Predecessor (Ctx, F_Option_Types)'Old
       and Valid_Next (Ctx, F_Option_Types) = Valid_Next (Ctx, F_Option_Types)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and (if Field_Size (Ctx, F_Option_Types) > 0 then Present (Ctx, F_Option_Types));

   procedure Set_Options (Ctx : in out Context; Seq_Ctx : Universal.Options.Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Options)
       and then Field_Condition (Ctx, (Fld => F_Options))
       and then Available_Space (Ctx, F_Options) >= Field_Size (Ctx, F_Options)
       and then Field_First (Ctx, F_Options) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Options) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Options) mod RFLX_Types.Byte'Size = 0
       and then Valid_Length (Ctx, F_Options, Universal.Options.Byte_Size (Seq_Ctx))
       and then Universal.Options.Has_Buffer (Seq_Ctx)
       and then Universal.Options.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Options)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Options))
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
       and (if Field_Size (Ctx, F_Options) > 0 then Present (Ctx, F_Options));

   procedure Set_Values (Ctx : in out Context; Seq_Ctx : Universal.Values.Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Values)
       and then Field_Condition (Ctx, (Fld => F_Values))
       and then Available_Space (Ctx, F_Values) >= Field_Size (Ctx, F_Values)
       and then Field_First (Ctx, F_Values) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Values) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Values) mod RFLX_Types.Byte'Size = 0
       and then Valid_Length (Ctx, F_Values, Universal.Values.Byte_Size (Seq_Ctx))
       and then Universal.Values.Has_Buffer (Seq_Ctx)
       and then Universal.Values.Valid (Seq_Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Values)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Values))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Values) = Predecessor (Ctx, F_Values)'Old
       and Valid_Next (Ctx, F_Values) = Valid_Next (Ctx, F_Values)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and (if Field_Size (Ctx, F_Values) > 0 then Present (Ctx, F_Values));

   procedure Initialize_Data (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Data)
       and then Valid_Length (Ctx, F_Data, Length)
       and then Available_Space (Ctx, F_Data) >= RFLX_Types.To_Bit_Length (Length)
       and then Field_First (Ctx, F_Data) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Data)
       and Field_Size (Ctx, F_Data) = RFLX_Types.To_Bit_Length (Length)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Invalid (Ctx, F_Option_Types)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old;

   procedure Initialize_Option_Types (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Option_Types)
       and then Available_Space (Ctx, F_Option_Types) >= Field_Size (Ctx, F_Option_Types)
       and then Field_First (Ctx, F_Option_Types) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Option_Types) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Option_Types) mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Option_Types)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Option_Types))
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Types) = Predecessor (Ctx, F_Option_Types)'Old
       and Valid_Next (Ctx, F_Option_Types) = Valid_Next (Ctx, F_Option_Types)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old;

   procedure Initialize_Options (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Options)
       and then Valid_Length (Ctx, F_Options, Length)
       and then Available_Space (Ctx, F_Options) >= RFLX_Types.To_Bit_Length (Length)
       and then Field_First (Ctx, F_Options) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Options) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Options) mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Options)
       and Field_Size (Ctx, F_Options) = RFLX_Types.To_Bit_Length (Length)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Options))
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Valid_Next (Ctx, F_Options) = Valid_Next (Ctx, F_Options)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old;

   procedure Initialize_Values (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Values)
       and then Available_Space (Ctx, F_Values) >= Field_Size (Ctx, F_Values)
       and then Field_First (Ctx, F_Values) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Values) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Values) mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Values)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Values))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Values) = Predecessor (Ctx, F_Values)'Old
       and Valid_Next (Ctx, F_Values) = Valid_Next (Ctx, F_Values)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old;

   procedure Set_Data (Ctx : in out Context; Data : RFLX_Types.Bytes) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Data)
       and then Available_Space (Ctx, F_Data) >= Field_Size (Ctx, F_Data)
       and then Field_First (Ctx, F_Data) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Data) mod RFLX_Types.Byte'Size = 0
       and then Valid_Length (Ctx, F_Data, Data'Length)
       and then Field_Condition (Ctx, (Fld => F_Data)),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Data)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Data))
       and Invalid (Ctx, F_Option_Types)
       and Invalid (Ctx, F_Options)
       and Invalid (Ctx, F_Value)
       and Invalid (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Data) = Predecessor (Ctx, F_Data)'Old
       and Valid_Next (Ctx, F_Data) = Valid_Next (Ctx, F_Data)'Old
       and Get_Message_Type (Ctx) = Get_Message_Type (Ctx)'Old;

   procedure Switch_To_Option_Types (Ctx : in out Context; Seq_Ctx : out Universal.Option_Types.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Option_Types)
       and then Field_Size (Ctx, F_Option_Types) > 0
       and then Field_First (Ctx, F_Option_Types) mod RFLX_Types.Byte'Size = 1
       and then Field_Condition (Ctx, (Fld => F_Option_Types))
       and then Available_Space (Ctx, F_Option_Types) >= Field_Size (Ctx, F_Option_Types),
     Post =>
       not Has_Buffer (Ctx)
       and Universal.Option_Types.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_Option_Types)
       and Seq_Ctx.Last = Field_Last (Ctx, F_Option_Types)
       and Universal.Option_Types.Valid (Seq_Ctx)
       and Universal.Option_Types.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_Option_Types)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Option_Types) = Predecessor (Ctx, F_Option_Types)'Old
       and Path_Condition (Ctx, F_Option_Types) = Path_Condition (Ctx, F_Option_Types)'Old
       and Field_Last (Ctx, F_Option_Types) = Field_Last (Ctx, F_Option_Types)'Old
       and Context_Cursor (Ctx, F_Message_Type) = Context_Cursor (Ctx, F_Message_Type)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Data) = Context_Cursor (Ctx, F_Data)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Option_Types) =>
           Context_Cursor (Ctx, F_Options) = Context_Cursor (Ctx, F_Options)'Old
           and Context_Cursor (Ctx, F_Value) = Context_Cursor (Ctx, F_Value)'Old
           and Context_Cursor (Ctx, F_Values) = Context_Cursor (Ctx, F_Values)'Old,
        others =>
           Invalid (Ctx, F_Options)
           and Invalid (Ctx, F_Value)
           and Invalid (Ctx, F_Values));

   procedure Switch_To_Options (Ctx : in out Context; Seq_Ctx : out Universal.Options.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Options)
       and then Field_Size (Ctx, F_Options) > 0
       and then Field_First (Ctx, F_Options) mod RFLX_Types.Byte'Size = 1
       and then Field_Condition (Ctx, (Fld => F_Options))
       and then Available_Space (Ctx, F_Options) >= Field_Size (Ctx, F_Options),
     Post =>
       not Has_Buffer (Ctx)
       and Universal.Options.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_Options)
       and Seq_Ctx.Last = Field_Last (Ctx, F_Options)
       and Universal.Options.Valid (Seq_Ctx)
       and Universal.Options.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_Options)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Options) = Predecessor (Ctx, F_Options)'Old
       and Path_Condition (Ctx, F_Options) = Path_Condition (Ctx, F_Options)'Old
       and Field_Last (Ctx, F_Options) = Field_Last (Ctx, F_Options)'Old
       and Context_Cursor (Ctx, F_Message_Type) = Context_Cursor (Ctx, F_Message_Type)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Data) = Context_Cursor (Ctx, F_Data)'Old
       and Context_Cursor (Ctx, F_Option_Types) = Context_Cursor (Ctx, F_Option_Types)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Options) =>
           Context_Cursor (Ctx, F_Value) = Context_Cursor (Ctx, F_Value)'Old
           and Context_Cursor (Ctx, F_Values) = Context_Cursor (Ctx, F_Values)'Old,
        others =>
           Invalid (Ctx, F_Value)
           and Invalid (Ctx, F_Values));

   procedure Switch_To_Values (Ctx : in out Context; Seq_Ctx : out Universal.Values.Context) with
     Pre =>
       not Ctx'Constrained
       and then not Seq_Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Values)
       and then Field_Size (Ctx, F_Values) > 0
       and then Field_First (Ctx, F_Values) mod RFLX_Types.Byte'Size = 1
       and then Field_Condition (Ctx, (Fld => F_Values))
       and then Available_Space (Ctx, F_Values) >= Field_Size (Ctx, F_Values),
     Post =>
       not Has_Buffer (Ctx)
       and Universal.Values.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and Seq_Ctx.First = Field_First (Ctx, F_Values)
       and Seq_Ctx.Last = Field_Last (Ctx, F_Values)
       and Universal.Values.Valid (Seq_Ctx)
       and Universal.Values.Sequence_Last (Seq_Ctx) = Seq_Ctx.First - 1
       and Present (Ctx, F_Values)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Values) = Predecessor (Ctx, F_Values)'Old
       and Path_Condition (Ctx, F_Values) = Path_Condition (Ctx, F_Values)'Old
       and Field_Last (Ctx, F_Values) = Field_Last (Ctx, F_Values)'Old
       and Context_Cursor (Ctx, F_Message_Type) = Context_Cursor (Ctx, F_Message_Type)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Data) = Context_Cursor (Ctx, F_Data)'Old
       and Context_Cursor (Ctx, F_Option_Types) = Context_Cursor (Ctx, F_Option_Types)'Old
       and Context_Cursor (Ctx, F_Options) = Context_Cursor (Ctx, F_Options)'Old
       and Context_Cursor (Ctx, F_Value) = Context_Cursor (Ctx, F_Value)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Values) =>
           True,
        others =>
           True);

   function Complete_Option_Types (Ctx : Context; Seq_Ctx : Universal.Option_Types.Context) return Boolean with
     Pre =>
       Valid_Next (Ctx, F_Option_Types);

   function Complete_Options (Ctx : Context; Seq_Ctx : Universal.Options.Context) return Boolean with
     Pre =>
       Valid_Next (Ctx, F_Options);

   function Complete_Values (Ctx : Context; Seq_Ctx : Universal.Values.Context) return Boolean with
     Pre =>
       Valid_Next (Ctx, F_Values);

   procedure Update_Option_Types (Ctx : in out Context; Seq_Ctx : in out Universal.Option_Types.Context) with
     Pre =>
       Present (Ctx, F_Option_Types)
       and then Complete_Option_Types (Ctx, Seq_Ctx)
       and then not Has_Buffer (Ctx)
       and then Universal.Option_Types.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_Option_Types)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_Option_Types),
     Post =>
       Present (Ctx, F_Option_Types)
       and Has_Buffer (Ctx)
       and not Universal.Option_Types.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Seq_Ctx.First = Seq_Ctx.First'Old
       and Seq_Ctx.Last = Seq_Ctx.Last'Old
       and Field_First (Ctx, F_Option_Types) = Field_First (Ctx, F_Option_Types)'Old
       and Field_Size (Ctx, F_Option_Types) = Field_Size (Ctx, F_Option_Types)'Old
       and Context_Cursor (Ctx, F_Message_Type) = Context_Cursor (Ctx, F_Message_Type)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Data) = Context_Cursor (Ctx, F_Data)'Old
       and Context_Cursor (Ctx, F_Options) = Context_Cursor (Ctx, F_Options)'Old
       and Context_Cursor (Ctx, F_Value) = Context_Cursor (Ctx, F_Value)'Old
       and Context_Cursor (Ctx, F_Values) = Context_Cursor (Ctx, F_Values)'Old,
     Depends =>
       (Ctx => (Ctx, Seq_Ctx), Seq_Ctx => Seq_Ctx);

   procedure Update_Options (Ctx : in out Context; Seq_Ctx : in out Universal.Options.Context) with
     Pre =>
       Present (Ctx, F_Options)
       and then Complete_Options (Ctx, Seq_Ctx)
       and then not Has_Buffer (Ctx)
       and then Universal.Options.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_Options)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_Options),
     Post =>
       Present (Ctx, F_Options)
       and Has_Buffer (Ctx)
       and not Universal.Options.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Seq_Ctx.First = Seq_Ctx.First'Old
       and Seq_Ctx.Last = Seq_Ctx.Last'Old
       and Field_First (Ctx, F_Options) = Field_First (Ctx, F_Options)'Old
       and Field_Size (Ctx, F_Options) = Field_Size (Ctx, F_Options)'Old
       and Context_Cursor (Ctx, F_Message_Type) = Context_Cursor (Ctx, F_Message_Type)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Data) = Context_Cursor (Ctx, F_Data)'Old
       and Context_Cursor (Ctx, F_Option_Types) = Context_Cursor (Ctx, F_Option_Types)'Old
       and Context_Cursor (Ctx, F_Value) = Context_Cursor (Ctx, F_Value)'Old
       and Context_Cursor (Ctx, F_Values) = Context_Cursor (Ctx, F_Values)'Old,
     Depends =>
       (Ctx => (Ctx, Seq_Ctx), Seq_Ctx => Seq_Ctx);

   procedure Update_Values (Ctx : in out Context; Seq_Ctx : in out Universal.Values.Context) with
     Pre =>
       Present (Ctx, F_Values)
       and then Complete_Values (Ctx, Seq_Ctx)
       and then not Has_Buffer (Ctx)
       and then Universal.Values.Has_Buffer (Seq_Ctx)
       and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
       and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
       and then Seq_Ctx.First = Field_First (Ctx, F_Values)
       and then Seq_Ctx.Last = Field_Last (Ctx, F_Values),
     Post =>
       Present (Ctx, F_Values)
       and Has_Buffer (Ctx)
       and not Universal.Values.Has_Buffer (Seq_Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Seq_Ctx.First = Seq_Ctx.First'Old
       and Seq_Ctx.Last = Seq_Ctx.Last'Old
       and Field_First (Ctx, F_Values) = Field_First (Ctx, F_Values)'Old
       and Field_Size (Ctx, F_Values) = Field_Size (Ctx, F_Values)'Old
       and Context_Cursor (Ctx, F_Message_Type) = Context_Cursor (Ctx, F_Message_Type)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old
       and Context_Cursor (Ctx, F_Data) = Context_Cursor (Ctx, F_Data)'Old
       and Context_Cursor (Ctx, F_Option_Types) = Context_Cursor (Ctx, F_Option_Types)'Old
       and Context_Cursor (Ctx, F_Options) = Context_Cursor (Ctx, F_Options)'Old
       and Context_Cursor (Ctx, F_Value) = Context_Cursor (Ctx, F_Value)'Old,
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
          when F_Message_Type =>
             Valid (Val.Message_Type_Value),
          when F_Length =>
             Valid (Val.Length_Value),
          when F_Data | F_Option_Types | F_Options =>
             True,
          when F_Value =>
             Valid (Val.Value_Value),
          when F_Values =>
             True,
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
       (if State = S_Valid or State = S_Structural_Valid then Valid_Value (Field_Cursor.Value));

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

   function Valid_Context (Buffer_First, Buffer_Last : RFLX_Types.Index; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length; Verified_Last : RFLX_Types.Bit_Length; Written_Last : RFLX_Types.Bit_Length; Buffer : RFLX_Types.Bytes_Ptr; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then Buffer'First = Buffer_First and Buffer'Last = Buffer_Last)
      and then (RFLX_Types.To_Index (First) >= Buffer_First
                and RFLX_Types.To_Index (Last) <= Buffer_Last
                and Buffer_Last < RFLX_Types.Index'Last
                and First <= Last + 1
                and Last < RFLX_Types.Bit_Index'Last
                and First mod RFLX_Types.Byte'Size = 1
                and Last mod RFLX_Types.Byte'Size = 0)
      and then First - 1 <= Verified_Last
      and then First - 1 <= Written_Last
      and then Verified_Last <= Written_Last
      and then Written_Last <= Last
      and then First mod RFLX_Types.Byte'Size = 1
      and then Last mod RFLX_Types.Byte'Size = 0
      and then Verified_Last mod RFLX_Types.Byte'Size = 0
      and then Written_Last mod RFLX_Types.Byte'Size = 0
      and then (for all F in Field'First .. Field'Last =>
                   (if
                       Structural_Valid (Cursors (F))
                    then
                       Cursors (F).First >= First
                       and Cursors (F).Last <= Verified_Last
                       and Cursors (F).First <= Cursors (F).Last + 1
                       and Cursors (F).Value.Fld = F))
      and then ((if
                    Structural_Valid (Cursors (F_Length))
                 then
                    (Valid (Cursors (F_Message_Type))
                     and then Cursors (F_Length).Predecessor = F_Message_Type
                     and then (RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
                               and RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
                               and RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data)))))
                and then (if
                             Structural_Valid (Cursors (F_Data))
                          then
                             (Valid (Cursors (F_Length))
                              and then Cursors (F_Data).Predecessor = F_Length
                              and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Data)))
                             or (Valid (Cursors (F_Message_Type))
                                 and then Cursors (F_Data).Predecessor = F_Message_Type
                                 and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data))))
                and then (if
                             Structural_Valid (Cursors (F_Option_Types))
                          then
                             (Valid (Cursors (F_Length))
                              and then Cursors (F_Option_Types).Predecessor = F_Length
                              and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Option_Types))))
                and then (if
                             Structural_Valid (Cursors (F_Options))
                          then
                             (Valid (Cursors (F_Length))
                              and then Cursors (F_Options).Predecessor = F_Length
                              and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Options)))
                             or (Valid (Cursors (F_Message_Type))
                                 and then Cursors (F_Options).Predecessor = F_Message_Type
                                 and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))))
                and then (if
                             Structural_Valid (Cursors (F_Value))
                          then
                             (Valid (Cursors (F_Length))
                              and then Cursors (F_Value).Predecessor = F_Length
                              and then (RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Value))
                                        and RFLX_Types.U64 (Cursors (F_Length).Value.Length_Value) = Universal.Value'Size / 8)))
                and then (if
                             Structural_Valid (Cursors (F_Values))
                          then
                             (Valid (Cursors (F_Length))
                              and then Cursors (F_Values).Predecessor = F_Length
                              and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Values)))))
      and then ((if Invalid (Cursors (F_Message_Type)) then Invalid (Cursors (F_Length)))
                and then (if
                             Invalid (Cursors (F_Length))
                             and then Invalid (Cursors (F_Message_Type))
                          then
                             Invalid (Cursors (F_Data)))
                and then (if Invalid (Cursors (F_Length)) then Invalid (Cursors (F_Option_Types)))
                and then (if
                             Invalid (Cursors (F_Length))
                             and then Invalid (Cursors (F_Message_Type))
                          then
                             Invalid (Cursors (F_Options)))
                and then (if Invalid (Cursors (F_Length)) then Invalid (Cursors (F_Value)))
                and then (if Invalid (Cursors (F_Length)) then Invalid (Cursors (F_Values))))
      and then (if
                   Structural_Valid (Cursors (F_Message_Type))
                then
                   Cursors (F_Message_Type).Last - Cursors (F_Message_Type).First + 1 = RFLX.Universal.Message_Type_Base'Size
                   and then Cursors (F_Message_Type).Predecessor = F_Initial
                   and then Cursors (F_Message_Type).First = First
                   and then (if
                                Structural_Valid (Cursors (F_Data))
                                and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data))
                             then
                                Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Message_Type).Last)
                                and then Cursors (F_Data).Predecessor = F_Message_Type
                                and then Cursors (F_Data).First = Cursors (F_Message_Type).Last + 1)
                   and then (if
                                Structural_Valid (Cursors (F_Length))
                                and then (RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
                                          and RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
                                          and RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data)))
                             then
                                Cursors (F_Length).Last - Cursors (F_Length).First + 1 = RFLX.Universal.Length_Base'Size
                                and then Cursors (F_Length).Predecessor = F_Message_Type
                                and then Cursors (F_Length).First = Cursors (F_Message_Type).Last + 1
                                and then (if
                                             Structural_Valid (Cursors (F_Data))
                                             and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Data))
                                          then
                                             Cursors (F_Data).Last - Cursors (F_Data).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Length).Value.Length_Value) * 8
                                             and then Cursors (F_Data).Predecessor = F_Length
                                             and then Cursors (F_Data).First = Cursors (F_Length).Last + 1)
                                and then (if
                                             Structural_Valid (Cursors (F_Option_Types))
                                             and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Option_Types))
                                          then
                                             Cursors (F_Option_Types).Last - Cursors (F_Option_Types).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Length).Value.Length_Value) * 8
                                             and then Cursors (F_Option_Types).Predecessor = F_Length
                                             and then Cursors (F_Option_Types).First = Cursors (F_Length).Last + 1)
                                and then (if
                                             Structural_Valid (Cursors (F_Options))
                                             and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Options))
                                          then
                                             Cursors (F_Options).Last - Cursors (F_Options).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Length).Value.Length_Value) * 8
                                             and then Cursors (F_Options).Predecessor = F_Length
                                             and then Cursors (F_Options).First = Cursors (F_Length).Last + 1)
                                and then (if
                                             Structural_Valid (Cursors (F_Value))
                                             and then (RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Value))
                                                       and RFLX_Types.U64 (Cursors (F_Length).Value.Length_Value) = Universal.Value'Size / 8)
                                          then
                                             Cursors (F_Value).Last - Cursors (F_Value).First + 1 = RFLX.Universal.Value'Size
                                             and then Cursors (F_Value).Predecessor = F_Length
                                             and then Cursors (F_Value).First = Cursors (F_Length).Last + 1)
                                and then (if
                                             Structural_Valid (Cursors (F_Values))
                                             and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Values))
                                          then
                                             Cursors (F_Values).Last - Cursors (F_Values).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Length).Value.Length_Value) * 8
                                             and then Cursors (F_Values).Predecessor = F_Length
                                             and then Cursors (F_Values).First = Cursors (F_Length).Last + 1))
                   and then (if
                                Structural_Valid (Cursors (F_Options))
                                and then RFLX_Types.U64 (Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
                             then
                                Cursors (F_Options).Last - Cursors (F_Options).First + 1 = RFLX_Types.Bit_Length (Written_Last) - RFLX_Types.Bit_Length (Cursors (F_Message_Type).Last)
                                and then Cursors (F_Options).Predecessor = F_Message_Type
                                and then Cursors (F_Options).First = Cursors (F_Message_Type).Last + 1)));

   pragma Warnings (On, """Buffer"" is not modified, could be of access constant type");

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
      and then Valid_Next (Ctx, F_Message_Type)
      and then Field_First (Ctx, F_Message_Type) mod RFLX_Types.Byte'Size = 1
      and then Available_Space (Ctx, F_Message_Type) = Ctx.Last - Ctx.First + 1
      and then Invalid (Ctx, F_Message_Type)
      and then Invalid (Ctx, F_Length)
      and then Invalid (Ctx, F_Data)
      and then Invalid (Ctx, F_Option_Types)
      and then Invalid (Ctx, F_Options)
      and then Invalid (Ctx, F_Value)
      and then Invalid (Ctx, F_Values));

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Buffer_Length (Ctx : Context) return RFLX_Types.Length is
     (Ctx.Buffer'Length);

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Verified_Last);

   function Written_Last (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Written_Last);

   function Message_Data (Ctx : Context) return RFLX_Types.Bytes is
     (Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Verified_Last)));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Message_Type =>
                    True,
                 when others =>
                    False),
          when F_Message_Type =>
             (case Fld is
                 when F_Data =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data)),
                 when F_Length =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
                    and RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
                    and RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data)),
                 when F_Options =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options)),
                 when others =>
                    False),
          when F_Length =>
             (case Fld is
                 when F_Data =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Data)),
                 when F_Option_Types =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Option_Types)),
                 when F_Options =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Options)),
                 when F_Value =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Value))
                    and RFLX_Types.U64 (Ctx.Cursors (F_Length).Value.Length_Value) = Universal.Value'Size / 8,
                 when F_Values =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Values)),
                 when others =>
                    False),
          when F_Data | F_Option_Types | F_Options | F_Value | F_Values | F_Final =>
             False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
          when F_Initial =>
             True,
          when F_Message_Type =>
             RFLX_Types.U64 (Val.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data))
             or RFLX_Types.U64 (Val.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
             or (RFLX_Types.U64 (Val.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
                 and RFLX_Types.U64 (Val.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
                 and RFLX_Types.U64 (Val.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data)))
             or RFLX_Types.U64 (Val.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options)),
          when F_Length =>
             RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Data))
             or RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Option_Types))
             or RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Options))
             or (RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Value))
                 and RFLX_Types.U64 (Val.Length_Value) = Universal.Value'Size / 8)
             or RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Values)),
          when F_Data | F_Option_Types | F_Options | F_Value | F_Values =>
             True,
          when F_Final =>
             False));

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Message_Type =>
                    RFLX.Universal.Message_Type_Base'Size,
                 when others =>
                    raise Program_Error),
          when F_Message_Type =>
             (case Fld is
                 when F_Data =>
                    RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Message_Type).Last),
                 when F_Length =>
                    RFLX.Universal.Length_Base'Size,
                 when F_Options =>
                    RFLX_Types.Bit_Length (Ctx.Written_Last) - RFLX_Types.Bit_Length (Ctx.Cursors (F_Message_Type).Last),
                 when others =>
                    raise Program_Error),
          when F_Length =>
             (case Fld is
                 when F_Data | F_Option_Types | F_Options =>
                    RFLX_Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8,
                 when F_Value =>
                    RFLX.Universal.Value'Size,
                 when F_Values =>
                    RFLX_Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8,
                 when others =>
                    raise Program_Error),
          when F_Data | F_Option_Types | F_Options | F_Value | F_Values | F_Final =>
             0));

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index is
     ((case Fld is
          when F_Message_Type =>
             Ctx.First,
          when F_Length =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Message_Type
                 and then (RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
                           and RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
                           and RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data)))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Data =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Length
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Data))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Message_Type
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Option_Types =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Length
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Option_Types))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Options =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Length
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Options))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              elsif
                 Ctx.Cursors (Fld).Predecessor = F_Message_Type
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Value =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Length
                 and then (RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Value))
                           and RFLX_Types.U64 (Ctx.Cursors (F_Length).Value.Length_Value) = Universal.Value'Size / 8)
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Values =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Length
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Values))
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
          when F_Message_Type =>
             Ctx.Cursors (Fld).Predecessor = F_Initial,
          when F_Length =>
             (Valid (Ctx.Cursors (F_Message_Type))
              and Ctx.Cursors (Fld).Predecessor = F_Message_Type),
          when F_Data =>
             (Valid (Ctx.Cursors (F_Length))
              and Ctx.Cursors (Fld).Predecessor = F_Length)
             or (Valid (Ctx.Cursors (F_Message_Type))
                 and Ctx.Cursors (Fld).Predecessor = F_Message_Type),
          when F_Option_Types =>
             (Valid (Ctx.Cursors (F_Length))
              and Ctx.Cursors (Fld).Predecessor = F_Length),
          when F_Options =>
             (Valid (Ctx.Cursors (F_Length))
              and Ctx.Cursors (Fld).Predecessor = F_Length)
             or (Valid (Ctx.Cursors (F_Message_Type))
                 and Ctx.Cursors (Fld).Predecessor = F_Message_Type),
          when F_Value | F_Values =>
             (Valid (Ctx.Cursors (F_Length))
              and Ctx.Cursors (Fld).Predecessor = F_Length),
          when F_Final =>
             (Structural_Valid (Ctx.Cursors (F_Data))
              and Ctx.Cursors (Fld).Predecessor = F_Data)
             or (Valid (Ctx.Cursors (F_Message_Type))
                 and Ctx.Cursors (Fld).Predecessor = F_Message_Type)
             or (Structural_Valid (Ctx.Cursors (F_Option_Types))
                 and Ctx.Cursors (Fld).Predecessor = F_Option_Types)
             or (Structural_Valid (Ctx.Cursors (F_Options))
                 and Ctx.Cursors (Fld).Predecessor = F_Options)
             or (Valid (Ctx.Cursors (F_Value))
                 and Ctx.Cursors (Fld).Predecessor = F_Value)
             or (Structural_Valid (Ctx.Cursors (F_Values))
                 and Ctx.Cursors (Fld).Predecessor = F_Values)));

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
     (Valid (Ctx, F_Message_Type)
      and then ((Structural_Valid (Ctx, F_Data)
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data)))
                or RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
                or (Valid (Ctx, F_Length)
                    and then (RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
                              and RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
                              and RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data)))
                    and then ((Structural_Valid (Ctx, F_Data)
                               and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Data)))
                              or (Structural_Valid (Ctx, F_Option_Types)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Option_Types)))
                              or (Structural_Valid (Ctx, F_Options)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Options)))
                              or (Valid (Ctx, F_Value)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Value))
                                            and RFLX_Types.U64 (Ctx.Cursors (F_Length).Value.Length_Value) = Universal.Value'Size / 8))
                              or (Structural_Valid (Ctx, F_Values)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Values)))))
                or (Structural_Valid (Ctx, F_Options)
                    and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options)))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Message_Type)
      and then ((Valid (Ctx, F_Data)
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data)))
                or RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
                or (Valid (Ctx, F_Length)
                    and then (RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options))
                              and RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Null))
                              and RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) /= RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Data)))
                    and then ((Valid (Ctx, F_Data)
                               and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Data)))
                              or (Valid (Ctx, F_Option_Types)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Option_Types)))
                              or (Valid (Ctx, F_Options)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Options)))
                              or (Valid (Ctx, F_Value)
                                  and then (RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Value))
                                            and RFLX_Types.U64 (Ctx.Cursors (F_Length).Value.Length_Value) = Universal.Value'Size / 8))
                              or (Valid (Ctx, F_Values)
                                  and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Values)))))
                or (Valid (Ctx, F_Options)
                    and then RFLX_Types.U64 (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value) = RFLX_Types.U64 (To_Base (RFLX.Universal.MT_Unconstrained_Options)))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Message_Type)
      or Incomplete (Ctx, F_Length)
      or Incomplete (Ctx, F_Data)
      or Incomplete (Ctx, F_Option_Types)
      or Incomplete (Ctx, F_Options)
      or Incomplete (Ctx, F_Value)
      or Incomplete (Ctx, F_Values));

   function Get_Message_Type (Ctx : Context) return RFLX.Universal.Message_Type is
     (To_Actual (Ctx.Cursors (F_Message_Type).Value.Message_Type_Value));

   function Get_Length (Ctx : Context) return RFLX.Universal.Length is
     (To_Actual (Ctx.Cursors (F_Length).Value.Length_Value));

   function Get_Value (Ctx : Context) return RFLX.Universal.Value is
     (To_Actual (Ctx.Cursors (F_Value).Value.Value_Value));

   function Valid_Length (Ctx : Context; Fld : Field; Length : RFLX_Types.Length) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Message_Type =>
                    Length = RFLX_Types.To_Length (Field_Size (Ctx, Fld)),
                 when others =>
                    raise Program_Error),
          when F_Message_Type =>
             (case Fld is
                 when F_Data =>
                    Length <= RFLX_Types.To_Length (Available_Space (Ctx, Fld)),
                 when F_Length =>
                    Length = RFLX_Types.To_Length (Field_Size (Ctx, Fld)),
                 when F_Options =>
                    Length <= RFLX_Types.To_Length (Available_Space (Ctx, Fld)),
                 when others =>
                    raise Program_Error),
          when F_Length =>
             (case Fld is
                 when F_Data | F_Option_Types | F_Options | F_Value | F_Values =>
                    Length = RFLX_Types.To_Length (Field_Size (Ctx, Fld)),
                 when others =>
                    raise Program_Error),
          when F_Data | F_Option_Types | F_Options | F_Value | F_Values | F_Final =>
             raise Program_Error));

   function Complete_Option_Types (Ctx : Context; Seq_Ctx : Universal.Option_Types.Context) return Boolean is
     (Universal.Option_Types.Valid (Seq_Ctx)
      and Universal.Option_Types.Size (Seq_Ctx) = Field_Size (Ctx, F_Option_Types));

   function Complete_Options (Ctx : Context; Seq_Ctx : Universal.Options.Context) return Boolean is
     (Universal.Options.Valid (Seq_Ctx)
      and Universal.Options.Size (Seq_Ctx) = Field_Size (Ctx, F_Options));

   function Complete_Values (Ctx : Context; Seq_Ctx : Universal.Values.Context) return Boolean is
     (Universal.Values.Valid (Seq_Ctx)
      and Universal.Values.Size (Seq_Ctx) = Field_Size (Ctx, F_Values));

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.Universal.Message;

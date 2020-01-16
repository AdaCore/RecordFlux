with RFLX.Types;
use type RFLX.Types.Integer_Address;

generic
package RFLX.Ethernet.Generic_Frame with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_Frame);

   type Virtual_Field is (F_Initial, F_Destination, F_Source, F_Type_Length_TPID, F_TPID, F_TCI, F_Type_Length, F_Payload, F_Final);

   subtype Field is Virtual_Field range F_Destination .. F_Payload;

   type Field_Cursor is private with
     Default_Initial_Condition =>
       False;

   type Field_Cursors is array (Virtual_Field) of Field_Cursor;

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       False;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Payload | F_Final =>
               null;
            when F_Destination =>
               Destination_Value : Address;
            when F_Source =>
               Source_Value : Address;
            when F_Type_Length_TPID =>
               Type_Length_TPID_Value : Type_Length_Base;
            when F_TPID =>
               TPID_Value : TPID_Base;
            when F_TCI =>
               TCI_Value : TCI;
            when F_Type_Length =>
               Type_Length_Value : Type_Length_Base;
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
          and Has_Buffer (Ctx)
          and Buffer = null
          and Ctx.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and Ctx.First = RFLX.Types.First_Bit_Index (Ctx.Buffer_First)
          and Initialized (Ctx);

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
          and Buffer = null
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and Ctx.First = First
          and Ctx.Last = Last
          and Initialized (Ctx);

   function Initialized (Ctx : Context) return Boolean with
     Ghost;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX.Types.Bytes_Ptr) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx),
     Post =>
       Valid_Context (Ctx)
          and not Has_Buffer (Ctx)
          and Buffer /= null
          and Ctx.Buffer_First = Buffer'First
          and Ctx.Buffer_Last = Buffer'Last
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old
          and Cursors (Ctx) = Cursors (Ctx)'Old;

   function Has_Buffer (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Message_Last (Ctx : Context) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and Structural_Valid_Message (Ctx);

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Predecessor (Ctx, Fld);

   function Field_Condition (Ctx : Context; Value : Field_Dependent_Value; Length : RFLX.Types.Bit_Length := 0) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Value.Fld in Field'Range
          and Valid_Predecessor (Ctx, Value.Fld);

   function Field_Length (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Available_Space (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old;

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
             and Present (Ctx, Fld));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Invalid (Ctx : Context; Fld : Field) return Boolean with
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

   function Get_Destination (Ctx : Context) return Address with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Destination);

   function Get_Source (Ctx : Context) return Address with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Source);

   function Get_Type_Length_TPID (Ctx : Context) return Type_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Type_Length_TPID);

   function Get_TPID (Ctx : Context) return TPID with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_TPID);

   function Get_TCI (Ctx : Context) return TCI with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_TCI);

   function Get_Type_Length (Ctx : Context) return Type_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Type_Length);

   generic
      with procedure Process_Payload (Payload : RFLX.Types.Bytes);
   procedure Get_Payload (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Present (Ctx, F_Payload);

   procedure Set_Destination (Ctx : in out Context; Value : Address) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Destination)
          and then Field_Last (Ctx, F_Destination) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Destination, Value))
          and then Valid (Value)
          and then Available_Space (Ctx, F_Destination) >= Field_Length (Ctx, F_Destination),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Destination)
          and Get_Destination (Ctx) = Value
          and Invalid (Ctx, F_Source)
          and Invalid (Ctx, F_Type_Length_TPID)
          and Invalid (Ctx, F_TPID)
          and Invalid (Ctx, F_TCI)
          and Invalid (Ctx, F_Type_Length)
          and Invalid (Ctx, F_Payload)
          and (Predecessor (Ctx, F_Source) = F_Destination
            and Valid_Next (Ctx, F_Source))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Destination) = Predecessor (Ctx, F_Destination)'Old
          and Valid_Next (Ctx, F_Destination) = Valid_Next (Ctx, F_Destination)'Old;

   procedure Set_Source (Ctx : in out Context; Value : Address) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Source)
          and then Field_Last (Ctx, F_Source) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Source, Value))
          and then Valid (Value)
          and then Available_Space (Ctx, F_Source) >= Field_Length (Ctx, F_Source),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Source)
          and Get_Source (Ctx) = Value
          and Invalid (Ctx, F_Type_Length_TPID)
          and Invalid (Ctx, F_TPID)
          and Invalid (Ctx, F_TCI)
          and Invalid (Ctx, F_Type_Length)
          and Invalid (Ctx, F_Payload)
          and (Predecessor (Ctx, F_Type_Length_TPID) = F_Source
            and Valid_Next (Ctx, F_Type_Length_TPID))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Source) = Predecessor (Ctx, F_Source)'Old
          and Valid_Next (Ctx, F_Source) = Valid_Next (Ctx, F_Source)'Old
          and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
          and Cursor (Ctx, F_Destination) = Cursor (Ctx, F_Destination)'Old;

   procedure Set_Type_Length_TPID (Ctx : in out Context; Value : Type_Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Type_Length_TPID)
          and then Field_Last (Ctx, F_Type_Length_TPID) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Type_Length_TPID, Value))
          and then Valid (Value)
          and then Available_Space (Ctx, F_Type_Length_TPID) >= Field_Length (Ctx, F_Type_Length_TPID),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Type_Length_TPID)
          and Get_Type_Length_TPID (Ctx) = Value
          and Invalid (Ctx, F_TPID)
          and Invalid (Ctx, F_TCI)
          and Invalid (Ctx, F_Type_Length)
          and Invalid (Ctx, F_Payload)
          and (if RFLX.Types.Bit_Length (Get_Type_Length_TPID (Ctx)) = 16#8100# then
             Predecessor (Ctx, F_TPID) = F_Type_Length_TPID
               and Valid_Next (Ctx, F_TPID))
          and (if RFLX.Types.Bit_Length (Get_Type_Length_TPID (Ctx)) /= 16#8100# then
             Predecessor (Ctx, F_Type_Length) = F_Type_Length_TPID
               and Valid_Next (Ctx, F_Type_Length))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Type_Length_TPID) = Predecessor (Ctx, F_Type_Length_TPID)'Old
          and Valid_Next (Ctx, F_Type_Length_TPID) = Valid_Next (Ctx, F_Type_Length_TPID)'Old
          and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
          and Get_Source (Ctx) = Get_Source (Ctx)'Old
          and Cursor (Ctx, F_Destination) = Cursor (Ctx, F_Destination)'Old
          and Cursor (Ctx, F_Source) = Cursor (Ctx, F_Source)'Old;

   procedure Set_TPID (Ctx : in out Context; Value : TPID) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_TPID)
          and then Field_Last (Ctx, F_TPID) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_TPID, Value))
          and then Valid (Value)
          and then Available_Space (Ctx, F_TPID) >= Field_Length (Ctx, F_TPID),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_TPID)
          and Get_TPID (Ctx) = Value
          and Invalid (Ctx, F_TCI)
          and Invalid (Ctx, F_Type_Length)
          and Invalid (Ctx, F_Payload)
          and (Predecessor (Ctx, F_TCI) = F_TPID
            and Valid_Next (Ctx, F_TCI))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_TPID) = Predecessor (Ctx, F_TPID)'Old
          and Valid_Next (Ctx, F_TPID) = Valid_Next (Ctx, F_TPID)'Old
          and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
          and Get_Source (Ctx) = Get_Source (Ctx)'Old
          and Get_Type_Length_TPID (Ctx) = Get_Type_Length_TPID (Ctx)'Old
          and Cursor (Ctx, F_Destination) = Cursor (Ctx, F_Destination)'Old
          and Cursor (Ctx, F_Source) = Cursor (Ctx, F_Source)'Old
          and Cursor (Ctx, F_Type_Length_TPID) = Cursor (Ctx, F_Type_Length_TPID)'Old;

   procedure Set_TCI (Ctx : in out Context; Value : TCI) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_TCI)
          and then Field_Last (Ctx, F_TCI) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_TCI, Value))
          and then Valid (Value)
          and then Available_Space (Ctx, F_TCI) >= Field_Length (Ctx, F_TCI),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_TCI)
          and Get_TCI (Ctx) = Value
          and Invalid (Ctx, F_Type_Length)
          and Invalid (Ctx, F_Payload)
          and (Predecessor (Ctx, F_Type_Length) = F_TCI
            and Valid_Next (Ctx, F_Type_Length))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_TCI) = Predecessor (Ctx, F_TCI)'Old
          and Valid_Next (Ctx, F_TCI) = Valid_Next (Ctx, F_TCI)'Old
          and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
          and Get_Source (Ctx) = Get_Source (Ctx)'Old
          and Get_Type_Length_TPID (Ctx) = Get_Type_Length_TPID (Ctx)'Old
          and Get_TPID (Ctx) = Get_TPID (Ctx)'Old
          and Cursor (Ctx, F_Destination) = Cursor (Ctx, F_Destination)'Old
          and Cursor (Ctx, F_Source) = Cursor (Ctx, F_Source)'Old
          and Cursor (Ctx, F_Type_Length_TPID) = Cursor (Ctx, F_Type_Length_TPID)'Old
          and Cursor (Ctx, F_TPID) = Cursor (Ctx, F_TPID)'Old;

   procedure Set_Type_Length (Ctx : in out Context; Value : Type_Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Type_Length)
          and then Field_Last (Ctx, F_Type_Length) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Type_Length, Value))
          and then Valid (Value)
          and then Available_Space (Ctx, F_Type_Length) >= Field_Length (Ctx, F_Type_Length),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Type_Length)
          and Get_Type_Length (Ctx) = Value
          and Invalid (Ctx, F_Payload)
          and (if RFLX.Types.Bit_Length (Get_Type_Length (Ctx)) <= 1500 then
             Predecessor (Ctx, F_Payload) = F_Type_Length
               and Valid_Next (Ctx, F_Payload))
          and (if RFLX.Types.Bit_Length (Get_Type_Length (Ctx)) >= 1536 then
             Predecessor (Ctx, F_Payload) = F_Type_Length
               and Valid_Next (Ctx, F_Payload))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Type_Length) = Predecessor (Ctx, F_Type_Length)'Old
          and Valid_Next (Ctx, F_Type_Length) = Valid_Next (Ctx, F_Type_Length)'Old
          and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
          and Get_Source (Ctx) = Get_Source (Ctx)'Old
          and Get_Type_Length_TPID (Ctx) = Get_Type_Length_TPID (Ctx)'Old
          and Cursor (Ctx, F_Destination) = Cursor (Ctx, F_Destination)'Old
          and Cursor (Ctx, F_Source) = Cursor (Ctx, F_Source)'Old
          and Cursor (Ctx, F_Type_Length_TPID) = Cursor (Ctx, F_Type_Length_TPID)'Old
          and Cursor (Ctx, F_TPID) = Cursor (Ctx, F_TPID)'Old
          and Cursor (Ctx, F_TCI) = Cursor (Ctx, F_TCI)'Old;

   generic
      with procedure Process_Payload (Payload : out RFLX.Types.Bytes);
   procedure Set_Payload (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Payload)
          and then Field_Last (Ctx, F_Payload) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Payload), Field_Length (Ctx, F_Payload))
          and then Available_Space (Ctx, F_Payload) >= Field_Length (Ctx, F_Payload),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
          and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
          and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
          and Get_Source (Ctx) = Get_Source (Ctx)'Old
          and Get_Type_Length_TPID (Ctx) = Get_Type_Length_TPID (Ctx)'Old
          and Get_Type_Length (Ctx) = Get_Type_Length (Ctx)'Old
          and Structural_Valid (Ctx, F_Payload);

   generic
      with procedure Process_Payload (Payload : out RFLX.Types.Bytes);
   procedure Set_Bounded_Payload (Ctx : in out Context; Length : RFLX.Types.Bit_Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Payload)
          and then Field_Last (Ctx, F_Payload) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Payload), Length)
          and then Available_Space (Ctx, F_Payload) >= Length
          and then (Field_First (Ctx, F_Payload) + Length) <= RFLX.Types.Bit_Index'Last / 2
          and then ((Valid (Ctx, F_Type_Length)
              and Get_Type_Length (Ctx) >= 1536)),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
          and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
          and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
          and Get_Source (Ctx) = Get_Source (Ctx)'Old
          and Get_Type_Length_TPID (Ctx) = Get_Type_Length_TPID (Ctx)'Old
          and Get_Type_Length (Ctx) = Get_Type_Length (Ctx)'Old
          and Structural_Valid (Ctx, F_Payload);

   procedure Initialize_Payload (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Payload)
          and then Field_Last (Ctx, F_Payload) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Payload), Field_Length (Ctx, F_Payload))
          and then Available_Space (Ctx, F_Payload) >= Field_Length (Ctx, F_Payload),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
          and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
          and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
          and Get_Source (Ctx) = Get_Source (Ctx)'Old
          and Get_Type_Length_TPID (Ctx) = Get_Type_Length_TPID (Ctx)'Old
          and Get_Type_Length (Ctx) = Get_Type_Length (Ctx)'Old
          and Structural_Valid (Ctx, F_Payload);

   procedure Initialize_Bounded_Payload (Ctx : in out Context; Length : RFLX.Types.Bit_Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Payload)
          and then Field_Last (Ctx, F_Payload) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Payload), Length)
          and then Available_Space (Ctx, F_Payload) >= Length
          and then (Field_First (Ctx, F_Payload) + Length) <= RFLX.Types.Bit_Index'Last / 2
          and then ((Valid (Ctx, F_Type_Length)
              and Get_Type_Length (Ctx) >= 1536)),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
          and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
          and Get_Destination (Ctx) = Get_Destination (Ctx)'Old
          and Get_Source (Ctx) = Get_Source (Ctx)'Old
          and Get_Type_Length_TPID (Ctx) = Get_Type_Length_TPID (Ctx)'Old
          and Get_Type_Length (Ctx) = Get_Type_Length (Ctx)'Old
          and Structural_Valid (Ctx, F_Payload);

   function Valid_Context (Ctx : Context) return Boolean with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Cursor (Ctx : Context; Fld : Field) return Field_Cursor with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Cursors (Ctx : Context) return Field_Cursors with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Incomplete);

   function Valid_Value (Value : Field_Dependent_Value) return Boolean is
     ((case Value.Fld is
         when F_Destination =>
            Valid (Value.Destination_Value),
         when F_Source =>
            Valid (Value.Source_Value),
         when F_Type_Length_TPID =>
            Valid (Value.Type_Length_TPID_Value),
         when F_TPID =>
            Valid (Value.TPID_Value),
         when F_TCI =>
            Valid (Value.TCI_Value),
         when F_Type_Length =>
            Valid (Value.Type_Length_Value),
         when F_Payload =>
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

   function Structural_Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid
      or Cursor.State = S_Structural_Valid);

   function Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid);

   function Invalid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Invalid
      or Cursor.State = S_Incomplete);

   function Valid_Context (Buffer_First, Buffer_Last : RFLX.Types.Index; First, Last : RFLX.Types.Bit_Index; Buffer : access constant RFLX.Types.Bytes; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then
         Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last)
      and then RFLX.Types.Byte_Index (First) >= Buffer_First
      and then RFLX.Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= RFLX.Types.Bit_Index'Last / 2
      and then (for all F in Field'First .. Field'Last =>
        (if Structural_Valid (Cursors (F)) then
         Cursors (F).First >= First
           and Cursors (F).Last <= Last
           and Cursors (F).First <= (Cursors (F).Last + 1)
           and Cursors (F).Value.Fld = F))
      and then ((if Structural_Valid (Cursors (F_Source)) then
           (Valid (Cursors (F_Destination))
               and then Cursors (F_Source).Predecessor = F_Destination))
        and then (if Structural_Valid (Cursors (F_Type_Length_TPID)) then
           (Valid (Cursors (F_Source))
               and then Cursors (F_Type_Length_TPID).Predecessor = F_Source))
        and then (if Structural_Valid (Cursors (F_TPID)) then
           (Valid (Cursors (F_Type_Length_TPID))
               and then Cursors (F_TPID).Predecessor = F_Type_Length_TPID
               and then RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 16#8100#))
        and then (if Structural_Valid (Cursors (F_TCI)) then
           (Valid (Cursors (F_TPID))
               and then Cursors (F_TCI).Predecessor = F_TPID))
        and then (if Structural_Valid (Cursors (F_Type_Length)) then
           (Valid (Cursors (F_Type_Length_TPID))
               and then Cursors (F_Type_Length).Predecessor = F_Type_Length_TPID
               and then RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 16#8100#)
             or (Valid (Cursors (F_TCI))
               and then Cursors (F_Type_Length).Predecessor = F_TCI))
        and then (if Structural_Valid (Cursors (F_Payload)) then
           (Valid (Cursors (F_Type_Length))
               and then Cursors (F_Payload).Predecessor = F_Type_Length
               and then RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500)
             or (Valid (Cursors (F_Type_Length))
               and then Cursors (F_Payload).Predecessor = F_Type_Length
               and then RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536)))
      and then ((if Invalid (Cursors (F_Destination)) then
           Invalid (Cursors (F_Source)))
        and then (if Invalid (Cursors (F_Source)) then
           Invalid (Cursors (F_Type_Length_TPID)))
        and then (if Invalid (Cursors (F_Type_Length_TPID)) then
           Invalid (Cursors (F_TPID)))
        and then (if Invalid (Cursors (F_TPID)) then
           Invalid (Cursors (F_TCI)))
        and then (if Invalid (Cursors (F_Type_Length_TPID))
             and then Invalid (Cursors (F_TCI)) then
           Invalid (Cursors (F_Type_Length)))
        and then (if Invalid (Cursors (F_Type_Length)) then
           Invalid (Cursors (F_Payload))))
      and then (if Structural_Valid (Cursors (F_Destination)) then
         (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Ethernet.Address'Size
           and then Cursors (F_Destination).Predecessor = F_Initial
           and then Cursors (F_Destination).First = First
           and then (if Structural_Valid (Cursors (F_Source)) then
              (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Ethernet.Address'Size
                and then Cursors (F_Source).Predecessor = F_Destination
                and then Cursors (F_Source).First = (Cursors (F_Destination).Last + 1)
                and then (if Structural_Valid (Cursors (F_Type_Length_TPID)) then
                   (Cursors (F_Type_Length_TPID).Last - Cursors (F_Type_Length_TPID).First + 1) = Ethernet.Type_Length_Base'Size
                     and then Cursors (F_Type_Length_TPID).Predecessor = F_Source
                     and then Cursors (F_Type_Length_TPID).First = (Cursors (F_Source).Last + 1)
                     and then (if Structural_Valid (Cursors (F_TPID))
                          and then RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 16#8100# then
                        (Cursors (F_TPID).Last - Cursors (F_TPID).First + 1) = Ethernet.TPID_Base'Size
                          and then Cursors (F_TPID).Predecessor = F_Type_Length_TPID
                          and then Cursors (F_TPID).First = Cursors (F_Type_Length_TPID).First
                          and then (if Structural_Valid (Cursors (F_TCI)) then
                             (Cursors (F_TCI).Last - Cursors (F_TCI).First + 1) = Ethernet.TCI'Size
                               and then Cursors (F_TCI).Predecessor = F_TPID
                               and then Cursors (F_TCI).First = (Cursors (F_TPID).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Type_Length)) then
                                  (Cursors (F_Type_Length).Last - Cursors (F_Type_Length).First + 1) = Ethernet.Type_Length_Base'Size
                                    and then Cursors (F_Type_Length).Predecessor = F_TCI
                                    and then Cursors (F_Type_Length).First = (Cursors (F_TCI).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Payload))
                                         and then RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500 then
                                       (Cursors (F_Payload).Last - Cursors (F_Payload).First + 1) = RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) * 8
                                         and then Cursors (F_Payload).Predecessor = F_Type_Length
                                         and then Cursors (F_Payload).First = (Cursors (F_Type_Length).Last + 1))
                                    and then (if Structural_Valid (Cursors (F_Payload))
                                         and then RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536 then
                                       (Cursors (F_Payload).Last - Cursors (F_Payload).First + 1) = (Last - Cursors (F_Type_Length).Last)
                                         and then Cursors (F_Payload).Predecessor = F_Type_Length
                                         and then Cursors (F_Payload).First = (Cursors (F_Type_Length).Last + 1)))))
                     and then (if Structural_Valid (Cursors (F_Type_Length))
                          and then RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 16#8100# then
                        (Cursors (F_Type_Length).Last - Cursors (F_Type_Length).First + 1) = Ethernet.Type_Length_Base'Size
                          and then Cursors (F_Type_Length).Predecessor = F_Type_Length_TPID
                          and then Cursors (F_Type_Length).First = Cursors (F_Type_Length_TPID).First
                          and then (if Structural_Valid (Cursors (F_Payload))
                               and then RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500 then
                             (Cursors (F_Payload).Last - Cursors (F_Payload).First + 1) = RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) * 8
                               and then Cursors (F_Payload).Predecessor = F_Type_Length
                               and then Cursors (F_Payload).First = (Cursors (F_Type_Length).Last + 1))
                          and then (if Structural_Valid (Cursors (F_Payload))
                               and then RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536 then
                             (Cursors (F_Payload).Last - Cursors (F_Payload).First + 1) = (Last - Cursors (F_Type_Length).Last)
                               and then Cursors (F_Payload).Predecessor = F_Type_Length
                               and then Cursors (F_Payload).First = (Cursors (F_Type_Length).Last + 1)))))));

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First) is
      record
         Buffer : RFLX.Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Buffer_First, Buffer_Last, First, Last, Buffer, Cursors);

   function Valid_Context (Ctx : Context) return Boolean is
     (Valid_Context (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last, Ctx.Buffer, Ctx.Cursors));

   function Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.Ethernet.Generic_Frame;

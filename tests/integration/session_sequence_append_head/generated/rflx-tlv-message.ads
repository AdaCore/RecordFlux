pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.TLV.Message with
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

   type Virtual_Field is (F_Initial, F_Tag, F_Length, F_Value, F_Final);

   subtype Field is Virtual_Field range F_Tag .. F_Value;

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
            when F_Initial | F_Value | F_Final =>
               null;
            when F_Tag =>
               Tag_Value : RFLX.TLV.Tag_Base;
            when F_Length =>
               Length_Value : RFLX.TLV.Length;
         end case;
      end record;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
       and then Buffer /= null
       and then Buffer'Length > 0
       and then Buffer'Last < RFLX_Types.Index'Last,
     Post =>
       Has_Buffer (Ctx)
       and Buffer = null
       and Ctx.Buffer_First = Buffer'First'Old
       and Ctx.Buffer_Last = Buffer'Last'Old
       and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
       and Ctx.Last = RFLX_Types.To_Last_Bit_Index (Ctx.Buffer_Last)
       and Initialized (Ctx),
     Depends =>
       (Ctx => Buffer, Buffer => null);

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) with
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
       and then Last mod RFLX_Types.Byte'Size = 0,
     Post =>
       Buffer = null
       and Has_Buffer (Ctx)
       and Ctx.Buffer_First = Buffer'First'Old
       and Ctx.Buffer_Last = Buffer'Last'Old
       and Ctx.First = First
       and Ctx.Last = Last
       and Initialized (Ctx),
     Depends =>
       (Ctx => (Buffer, First, Last), Buffer => null);

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

   generic
      with procedure Read (Buffer : RFLX_Types.Bytes);
   procedure Read (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid_Message (Ctx);

   generic
      with procedure Write (Buffer : out RFLX_Types.Bytes; Length : out RFLX_Types.Length);
   procedure Write (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and Has_Buffer (Ctx),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
       and Initialized (Ctx);

   function Has_Buffer (Ctx : Context) return Boolean;

   function Size (Ctx : Context) return RFLX_Types.Bit_Length;

   function Byte_Size (Ctx : Context) return RFLX_Types.Length;

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid_Message (Ctx);

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
       Valid_Next (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld)
       and then Available_Space (Ctx, Fld) >= Field_Size (Ctx, Fld);

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

   function Get_Tag (Ctx : Context) return RFLX.TLV.Tag with
     Pre =>
       Valid (Ctx, F_Tag);

   function Get_Length (Ctx : Context) return RFLX.TLV.Length with
     Pre =>
       Valid (Ctx, F_Length);

   pragma Warnings (On, "precondition is always False");

   function Get_Value (Ctx : Context) return RFLX_Types.Bytes with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid (Ctx, F_Value)
       and then Valid_Next (Ctx, F_Value),
     Post =>
       Get_Value'Result'Length = RFLX_Types.To_Length (Field_Size (Ctx, F_Value));

   procedure Get_Value (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid (Ctx, F_Value)
       and then Valid_Next (Ctx, F_Value)
       and then Data'Length >= RFLX_Types.To_Length (Field_Size (Ctx, F_Value));

   generic
      with procedure Process_Value (Value : RFLX_Types.Bytes);
   procedure Generic_Get_Value (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and Present (Ctx, F_Value);

   procedure Set_Tag (Ctx : in out Context; Val : RFLX.TLV.Tag) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Tag)
       and then Field_Condition (Ctx, (F_Tag, To_Base (Val)))
       and then True
       and then Available_Space (Ctx, F_Tag) >= Field_Size (Ctx, F_Tag),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Tag)
       and Get_Tag (Ctx) = Val
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Tag))
       and Invalid (Ctx, F_Length)
       and Invalid (Ctx, F_Value)
       and (if
               RFLX_Types.U64 (To_Base (Get_Tag (Ctx))) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Data))
            then
               Predecessor (Ctx, F_Length) = F_Tag
               and Valid_Next (Ctx, F_Length))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Tag) = Predecessor (Ctx, F_Tag)'Old
       and Valid_Next (Ctx, F_Tag) = Valid_Next (Ctx, F_Tag)'Old;

   procedure Set_Length (Ctx : in out Context; Val : RFLX.TLV.Length) with
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
       and Invalid (Ctx, F_Value)
       and (Predecessor (Ctx, F_Value) = F_Length
            and Valid_Next (Ctx, F_Value))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Length) = Predecessor (Ctx, F_Length)'Old
       and Valid_Next (Ctx, F_Length) = Valid_Next (Ctx, F_Length)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Context_Cursor (Ctx, F_Tag) = Context_Cursor (Ctx, F_Tag)'Old;

   procedure Set_Value_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Value)
       and then Field_Condition (Ctx, (Fld => F_Value))
       and then Available_Space (Ctx, F_Value) >= Field_Size (Ctx, F_Value)
       and then Field_First (Ctx, F_Value) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Value) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Value) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Value) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Value)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Value))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Value) = Predecessor (Ctx, F_Value)'Old
       and Valid_Next (Ctx, F_Value) = Valid_Next (Ctx, F_Value)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old;

   procedure Initialize_Value (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Value)
       and then Available_Space (Ctx, F_Value) >= Field_Size (Ctx, F_Value)
       and then Field_First (Ctx, F_Value) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Value) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Value) mod RFLX_Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Value)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Value))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Value) = Predecessor (Ctx, F_Value)'Old
       and Valid_Next (Ctx, F_Value) = Valid_Next (Ctx, F_Value)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old;

   procedure Set_Value (Ctx : in out Context; Data : RFLX_Types.Bytes) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Value)
       and then Available_Space (Ctx, F_Value) >= Field_Size (Ctx, F_Value)
       and then Field_First (Ctx, F_Value) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Value) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Value) mod RFLX_Types.Byte'Size = 0
       and then Data'Length = RFLX_Types.To_Length (Field_Size (Ctx, F_Value))
       and then Field_Condition (Ctx, (Fld => F_Value)),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Value)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Value))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Value) = Predecessor (Ctx, F_Value)'Old
       and Valid_Next (Ctx, F_Value) = Valid_Next (Ctx, F_Value)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old;

   generic
      with procedure Process_Value (Value : out RFLX_Types.Bytes);
      with function Valid_Length (Length : RFLX_Types.Length) return Boolean;
   procedure Generic_Set_Value (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Value)
       and then Available_Space (Ctx, F_Value) >= Field_Size (Ctx, F_Value)
       and then Field_First (Ctx, F_Value) mod RFLX_Types.Byte'Size = 1
       and then Field_Last (Ctx, F_Value) mod RFLX_Types.Byte'Size = 0
       and then Field_Size (Ctx, F_Value) mod RFLX_Types.Byte'Size = 0
       and then Valid_Length (RFLX_Types.Length (Field_Size (Ctx, F_Value) / RFLX_Types.Byte'Size)),
     Post =>
       Has_Buffer (Ctx)
       and Structural_Valid (Ctx, F_Value)
       and (if Structural_Valid_Message (Ctx) then Message_Last (Ctx) = Field_Last (Ctx, F_Value))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Predecessor (Ctx, F_Value) = Predecessor (Ctx, F_Value)'Old
       and Valid_Next (Ctx, F_Value) = Valid_Next (Ctx, F_Value)'Old
       and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old;

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
          when F_Tag =>
             Valid (Val.Tag_Value),
          when F_Length =>
             Valid (Val.Length_Value),
          when F_Value =>
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

   function Valid_Context (Buffer_First, Buffer_Last : RFLX_Types.Index; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length; Message_Last : RFLX_Types.Bit_Length; Buffer : RFLX_Types.Bytes_Ptr; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then Buffer'First = Buffer_First and Buffer'Last = Buffer_Last)
      and then (RFLX_Types.To_Index (First) >= Buffer_First
                and RFLX_Types.To_Index (Last) <= Buffer_Last
                and Buffer_Last < RFLX_Types.Index'Last
                and First <= Last + 1
                and Last < RFLX_Types.Bit_Index'Last
                and First mod RFLX_Types.Byte'Size = 1
                and Last mod RFLX_Types.Byte'Size = 0)
      and then First - 1 <= Message_Last
      and then Message_Last <= Last
      and then First mod RFLX_Types.Byte'Size = 1
      and then Last mod RFLX_Types.Byte'Size = 0
      and then Message_Last mod RFLX_Types.Byte'Size = 0
      and then (for all F in Field'First .. Field'Last =>
                   (if
                       Structural_Valid (Cursors (F))
                    then
                       Cursors (F).First >= First
                       and Cursors (F).Last <= Message_Last
                       and Cursors (F).First <= Cursors (F).Last + 1
                       and Cursors (F).Value.Fld = F))
      and then ((if
                    Structural_Valid (Cursors (F_Length))
                 then
                    (Valid (Cursors (F_Tag))
                     and then Cursors (F_Length).Predecessor = F_Tag
                     and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Data))))
                and then (if
                             Structural_Valid (Cursors (F_Value))
                          then
                             (Valid (Cursors (F_Length))
                              and then Cursors (F_Value).Predecessor = F_Length)))
      and then ((if Invalid (Cursors (F_Tag)) then Invalid (Cursors (F_Length)))
                and then (if Invalid (Cursors (F_Length)) then Invalid (Cursors (F_Value))))
      and then (if
                   Structural_Valid (Cursors (F_Tag))
                then
                   Cursors (F_Tag).Last - Cursors (F_Tag).First + 1 = RFLX.TLV.Tag_Base'Size
                   and then Cursors (F_Tag).Predecessor = F_Initial
                   and then Cursors (F_Tag).First = First
                   and then (if
                                Structural_Valid (Cursors (F_Length))
                                and then RFLX_Types.U64 (Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Data))
                             then
                                Cursors (F_Length).Last - Cursors (F_Length).First + 1 = RFLX.TLV.Length'Size
                                and then Cursors (F_Length).Predecessor = F_Tag
                                and then Cursors (F_Length).First = Cursors (F_Tag).Last + 1
                                and then (if
                                             Structural_Valid (Cursors (F_Value))
                                          then
                                             Cursors (F_Value).Last - Cursors (F_Value).First + 1 = RFLX_Types.Bit_Length (Cursors (F_Length).Value.Length_Value) * 8
                                             and then Cursors (F_Value).Predecessor = F_Length
                                             and then Cursors (F_Value).First = Cursors (F_Length).Last + 1))));

   pragma Warnings (On, """Buffer"" is not modified, could be of access constant type");

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First) is
      record
         Message_Last : RFLX_Types.Bit_Length := First - 1;
         Buffer : RFLX_Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Message_Last, Context.Buffer, Context.Cursors);

   function Initialized (Ctx : Context) return Boolean is
     (Ctx.Message_Last = Ctx.First - 1
      and then Valid_Next (Ctx, F_Tag)
      and then Field_First (Ctx, F_Tag) mod RFLX_Types.Byte'Size = 1
      and then Available_Space (Ctx, F_Tag) = Ctx.Last - Ctx.First + 1
      and then Invalid (Ctx, F_Tag)
      and then Invalid (Ctx, F_Length)
      and then Invalid (Ctx, F_Value));

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Message_Last);

   function Message_Data (Ctx : Context) return RFLX_Types.Bytes is
     (Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Message_Last)));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Tag =>
                    True,
                 when others =>
                    False),
          when F_Tag =>
             (case Fld is
                 when F_Length =>
                    RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Data)),
                 when others =>
                    False),
          when F_Length =>
             (case Fld is
                 when F_Value =>
                    True,
                 when others =>
                    False),
          when F_Value | F_Final =>
             False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
          when F_Initial =>
             True,
          when F_Tag =>
             RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Error))
             or RFLX_Types.U64 (Val.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Data)),
          when F_Length | F_Value =>
             True,
          when F_Final =>
             False));

   function Field_Size (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
          when F_Initial =>
             (case Fld is
                 when F_Tag =>
                    RFLX.TLV.Tag_Base'Size,
                 when others =>
                    raise Program_Error),
          when F_Tag =>
             (case Fld is
                 when F_Length =>
                    RFLX.TLV.Length'Size,
                 when others =>
                    raise Program_Error),
          when F_Length =>
             (case Fld is
                 when F_Value =>
                    RFLX_Types.Bit_Length (Ctx.Cursors (F_Length).Value.Length_Value) * 8,
                 when others =>
                    raise Program_Error),
          when F_Value | F_Final =>
             0));

   function Field_First (Ctx : Context; Fld : Field) return RFLX_Types.Bit_Index is
     ((case Fld is
          when F_Tag =>
             Ctx.First,
          when F_Length =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Tag
                 and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Data))
              then
                 Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1
              else
                 raise Program_Error),
          when F_Value =>
             (if
                 Ctx.Cursors (Fld).Predecessor = F_Length
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
          when F_Tag =>
             Ctx.Cursors (Fld).Predecessor = F_Initial,
          when F_Length =>
             (Valid (Ctx.Cursors (F_Tag))
              and Ctx.Cursors (Fld).Predecessor = F_Tag),
          when F_Value =>
             (Valid (Ctx.Cursors (F_Length))
              and Ctx.Cursors (Fld).Predecessor = F_Length),
          when F_Final =>
             (Valid (Ctx.Cursors (F_Tag))
              and Ctx.Cursors (Fld).Predecessor = F_Tag)
             or (Structural_Valid (Ctx.Cursors (F_Value))
                 and Ctx.Cursors (Fld).Predecessor = F_Value)));

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
     (Valid (Ctx, F_Tag)
      and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Error))
                or (Valid (Ctx, F_Length)
                    and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Data))
                    and then Structural_Valid (Ctx, F_Value))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Tag)
      and then (RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Error))
                or (Valid (Ctx, F_Length)
                    and then RFLX_Types.U64 (Ctx.Cursors (F_Tag).Value.Tag_Value) = RFLX_Types.U64 (To_Base (RFLX.TLV.Msg_Data))
                    and then Valid (Ctx, F_Value))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Tag)
      or Incomplete (Ctx, F_Length)
      or Incomplete (Ctx, F_Value));

   function Get_Tag (Ctx : Context) return RFLX.TLV.Tag is
     (To_Actual (Ctx.Cursors (F_Tag).Value.Tag_Value));

   function Get_Length (Ctx : Context) return RFLX.TLV.Length is
     (To_Actual (Ctx.Cursors (F_Length).Value.Length_Value));

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.TLV.Message;
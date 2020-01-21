with RFLX.Types;
use type RFLX.Types.Integer_Address;
with RFLX.Message_Sequence;

generic
   with package Inner_Messages_Sequence is new RFLX.Message_Sequence (<>);
package RFLX.Arrays.Generic_Messages_Message with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_Messages_Message);

   type Virtual_Field is (F_Initial, F_Length, F_Messages, F_Final);

   subtype Field is Virtual_Field range F_Length .. F_Messages;

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
            when F_Initial | F_Messages | F_Final =>
               null;
            when F_Length =>
               Length_Value : Arrays.Length;
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

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Val.Fld in Field'Range
          and Valid_Predecessor (Ctx, Val.Fld);

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

   function Get_Length (Ctx : Context) return Arrays.Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Length);

   generic
      with procedure Process_Messages (Messages : RFLX.Types.Bytes);
   procedure Get_Messages (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Present (Ctx, F_Messages);

   procedure Set_Length (Ctx : in out Context; Val : Arrays.Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Length)
          and then Field_Last (Ctx, F_Length) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Length, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Length) >= Field_Length (Ctx, F_Length),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Length)
          and Get_Length (Ctx) = Val
          and Invalid (Ctx, F_Messages)
          and (Predecessor (Ctx, F_Messages) = F_Length
            and Valid_Next (Ctx, F_Messages))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Length) = Predecessor (Ctx, F_Length)'Old
          and Valid_Next (Ctx, F_Length) = Valid_Next (Ctx, F_Length)'Old;

   procedure Switch_To_Messages (Ctx : in out Context; Seq_Ctx : out Inner_Messages_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then not Seq_Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Messages)
          and then Field_Length (Ctx, F_Messages) > 0
          and then Field_Last (Ctx, F_Messages) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Messages))
          and then Available_Space (Ctx, F_Messages) >= Field_Length (Ctx, F_Messages),
     Post =>
       Valid_Context (Ctx)
          and not Has_Buffer (Ctx)
          and Inner_Messages_Sequence.Has_Buffer (Seq_Ctx)
          and Ctx.Buffer_First = Seq_Ctx.Buffer_First
          and Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
          and Seq_Ctx.First = Field_First (Ctx, F_Messages)
          and Seq_Ctx.Last = Field_Last (Ctx, F_Messages)
          and Inner_Messages_Sequence.Index (Seq_Ctx) = Seq_Ctx.First
          and Present (Ctx, F_Messages)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Messages) = Predecessor (Ctx, F_Messages)'Old
          and Path_Condition (Ctx, F_Messages) = Path_Condition (Ctx, F_Messages)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old,
     Contract_Cases =>
       (Structural_Valid (Ctx, F_Messages) =>
           True,
        others =>
           True);

   procedure Update_Messages (Ctx : in out Context; Seq_Ctx : in out Inner_Messages_Sequence.Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Present (Ctx, F_Messages)
          and then not Has_Buffer (Ctx)
          and then Inner_Messages_Sequence.Has_Buffer (Seq_Ctx)
          and then Ctx.Buffer_First = Seq_Ctx.Buffer_First
          and then Ctx.Buffer_Last = Seq_Ctx.Buffer_Last
          and then Seq_Ctx.First = Field_First (Ctx, F_Messages)
          and then Seq_Ctx.Last = Field_Last (Ctx, F_Messages),
     Post =>
       Valid_Context (Ctx)
          and Present (Ctx, F_Messages)
          and Has_Buffer (Ctx)
          and not Inner_Messages_Sequence.Has_Buffer (Seq_Ctx)
          and Seq_Ctx.First = Field_First (Ctx, F_Messages)
          and Seq_Ctx.Last = Field_Last (Ctx, F_Messages)
          and Seq_Ctx.First = Seq_Ctx.First'Old
          and Seq_Ctx.Last = Seq_Ctx.Last'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Field_First (Ctx, F_Messages) = Field_First (Ctx, F_Messages)'Old
          and Field_Length (Ctx, F_Messages) = Field_Length (Ctx, F_Messages)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old;

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

   function Valid_Value (Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
         when F_Length =>
            Valid (Val.Length_Value),
         when F_Messages =>
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
           Valid_Value (Field_Cursor.Value));

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
      and then ((if Structural_Valid (Cursors (F_Messages)) then
           (Valid (Cursors (F_Length))
               and then Cursors (F_Messages).Predecessor = F_Length)))
      and then ((if Invalid (Cursors (F_Length)) then
           Invalid (Cursors (F_Messages))))
      and then (if Structural_Valid (Cursors (F_Length)) then
         (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = Arrays.Length'Size
           and then Cursors (F_Length).Predecessor = F_Initial
           and then Cursors (F_Length).First = First
           and then (if Structural_Valid (Cursors (F_Messages)) then
              (Cursors (F_Messages).Last - Cursors (F_Messages).First + 1) = RFLX.Types.Bit_Length (Cursors (F_Length).Value.Length_Value) * 8
                and then Cursors (F_Messages).Predecessor = F_Length
                and then Cursors (F_Messages).First = (Cursors (F_Length).Last + 1))));

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

end RFLX.Arrays.Generic_Messages_Message;

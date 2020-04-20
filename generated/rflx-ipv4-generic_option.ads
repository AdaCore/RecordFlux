with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Builtin_Types.Conversions;
use RFLX.RFLX_Builtin_Types.Conversions;
with RFLX.RFLX_Generic_Types;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
package RFLX.IPv4.Generic_Option with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_Option);

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Length, Types.Bit_Index, Types.Bit_Length;

   type Virtual_Field is (F_Initial, F_Copied, F_Option_Class, F_Option_Number, F_Option_Length, F_Option_Data, F_Final);

   subtype Field is Virtual_Field range F_Copied .. F_Option_Data;

   type Field_Cursor is private with
     Default_Initial_Condition =>
       False;

   type Field_Cursors is array (Virtual_Field) of Field_Cursor;

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       False;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Option_Data | F_Final =>
               null;
            when F_Copied =>
               Copied_Value : RFLX.RFLX_Builtin_Types.Boolean_Base;
            when F_Option_Class =>
               Option_Class_Value : RFLX.IPv4.Option_Class_Base;
            when F_Option_Number =>
               Option_Number_Value : RFLX.IPv4.Option_Number;
            when F_Option_Length =>
               Option_Length_Value : RFLX.IPv4.Option_Length_Base;
         end case;
      end record;

   function Create return Context;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then Buffer'Last <= Types.Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Buffer = null
          and Ctx.Buffer_First = Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = Types.Bytes_Last (Buffer)'Old
          and Ctx.First = Types.First_Bit_Index (Ctx.Buffer_First)
          and Initialized (Ctx);

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then Types.Byte_Index (First) >= Buffer'First
          and then Types.Byte_Index (Last) <= Buffer'Last
          and then First <= Last
          and then Last <= Types.Bit_Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and Buffer = null
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = Types.Bytes_Last (Buffer)'Old
          and Ctx.First = First
          and Ctx.Last = Last
          and Initialized (Ctx);

   function Initialized (Ctx : Context) return Boolean with
     Ghost;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) with
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

   function Message_Last (Ctx : Context) return Types.Bit_Index with
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

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return Types.Bit_Index with
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

   function Available_Space (Ctx : Context; Fld : Field) return Types.Bit_Length with
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

   function Get_Copied (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Copied);

   function Get_Option_Class (Ctx : Context) return RFLX.IPv4.Option_Class with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Option_Class);

   function Get_Option_Number (Ctx : Context) return RFLX.IPv4.Option_Number with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Option_Number);

   function Get_Option_Length (Ctx : Context) return RFLX.IPv4.Option_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Option_Length);

   generic
      with procedure Process_Option_Data (Option_Data : Types.Bytes);
   procedure Get_Option_Data (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Present (Ctx, F_Option_Data);

   procedure Set_Copied (Ctx : in out Context; Val : Boolean) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Copied)
          and then Field_Last (Ctx, F_Copied) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Copied, To_Base (Val)))
          and then True
          and then Available_Space (Ctx, F_Copied) >= Field_Length (Ctx, F_Copied),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Copied)
          and Get_Copied (Ctx) = Val
          and Invalid (Ctx, F_Option_Class)
          and Invalid (Ctx, F_Option_Number)
          and Invalid (Ctx, F_Option_Length)
          and Invalid (Ctx, F_Option_Data)
          and (Predecessor (Ctx, F_Option_Class) = F_Copied
            and Valid_Next (Ctx, F_Option_Class))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Copied) = Predecessor (Ctx, F_Copied)'Old
          and Valid_Next (Ctx, F_Copied) = Valid_Next (Ctx, F_Copied)'Old;

   procedure Set_Option_Class (Ctx : in out Context; Val : RFLX.IPv4.Option_Class) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Option_Class)
          and then Field_Last (Ctx, F_Option_Class) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Option_Class, To_Base (Val)))
          and then True
          and then Available_Space (Ctx, F_Option_Class) >= Field_Length (Ctx, F_Option_Class),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Option_Class)
          and Get_Option_Class (Ctx) = Val
          and Invalid (Ctx, F_Option_Number)
          and Invalid (Ctx, F_Option_Length)
          and Invalid (Ctx, F_Option_Data)
          and (Predecessor (Ctx, F_Option_Number) = F_Option_Class
            and Valid_Next (Ctx, F_Option_Number))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Option_Class) = Predecessor (Ctx, F_Option_Class)'Old
          and Valid_Next (Ctx, F_Option_Class) = Valid_Next (Ctx, F_Option_Class)'Old
          and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
          and Cursor (Ctx, F_Copied) = Cursor (Ctx, F_Copied)'Old;

   procedure Set_Option_Number (Ctx : in out Context; Val : RFLX.IPv4.Option_Number) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Option_Number)
          and then Field_Last (Ctx, F_Option_Number) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Option_Number, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Option_Number) >= Field_Length (Ctx, F_Option_Number),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Option_Number)
          and Get_Option_Number (Ctx) = Val
          and Invalid (Ctx, F_Option_Length)
          and Invalid (Ctx, F_Option_Data)
          and (if Types.Bit_Length (Get_Option_Number (Ctx)) > 1 then
             Predecessor (Ctx, F_Option_Length) = F_Option_Number
               and Valid_Next (Ctx, F_Option_Length))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Option_Number) = Predecessor (Ctx, F_Option_Number)'Old
          and Valid_Next (Ctx, F_Option_Number) = Valid_Next (Ctx, F_Option_Number)'Old
          and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
          and Get_Option_Class (Ctx) = Get_Option_Class (Ctx)'Old
          and Cursor (Ctx, F_Copied) = Cursor (Ctx, F_Copied)'Old
          and Cursor (Ctx, F_Option_Class) = Cursor (Ctx, F_Option_Class)'Old;

   procedure Set_Option_Length (Ctx : in out Context; Val : RFLX.IPv4.Option_Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Option_Length)
          and then Field_Last (Ctx, F_Option_Length) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Option_Length, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Option_Length) >= Field_Length (Ctx, F_Option_Length),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Option_Length)
          and Get_Option_Length (Ctx) = Val
          and Invalid (Ctx, F_Option_Data)
          and (if (Types.Bit_Length (To_Base (Get_Option_Class (Ctx))) = Types.Bit_Length (To_Base (Debugging_And_Measurement))
                 and Types.Bit_Length (Get_Option_Number (Ctx)) = 4)
               or (Types.Bit_Length (To_Base (Get_Option_Class (Ctx))) = Types.Bit_Length (To_Base (Control))
                 and (Types.Bit_Length (Get_Option_Number (Ctx)) = 9
                   or Types.Bit_Length (Get_Option_Number (Ctx)) = 3
                   or Types.Bit_Length (Get_Option_Number (Ctx)) = 7))
               or (Types.Bit_Length (Get_Option_Length (Ctx)) = 11
                 and Types.Bit_Length (To_Base (Get_Option_Class (Ctx))) = Types.Bit_Length (To_Base (Control))
                 and Types.Bit_Length (Get_Option_Number (Ctx)) = 2)
               or (Types.Bit_Length (Get_Option_Length (Ctx)) = 4
                 and Types.Bit_Length (To_Base (Get_Option_Class (Ctx))) = Types.Bit_Length (To_Base (Control))
                 and Types.Bit_Length (Get_Option_Number (Ctx)) = 8) then
             Predecessor (Ctx, F_Option_Data) = F_Option_Length
               and Valid_Next (Ctx, F_Option_Data))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Option_Length) = Predecessor (Ctx, F_Option_Length)'Old
          and Valid_Next (Ctx, F_Option_Length) = Valid_Next (Ctx, F_Option_Length)'Old
          and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
          and Get_Option_Class (Ctx) = Get_Option_Class (Ctx)'Old
          and Get_Option_Number (Ctx) = Get_Option_Number (Ctx)'Old
          and Cursor (Ctx, F_Copied) = Cursor (Ctx, F_Copied)'Old
          and Cursor (Ctx, F_Option_Class) = Cursor (Ctx, F_Option_Class)'Old
          and Cursor (Ctx, F_Option_Number) = Cursor (Ctx, F_Option_Number)'Old;

   generic
      with procedure Process_Option_Data (Option_Data : out Types.Bytes);
   procedure Set_Option_Data (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Option_Data)
          and then Field_Last (Ctx, F_Option_Data) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Option_Data))
          and then Available_Space (Ctx, F_Option_Data) >= Field_Length (Ctx, F_Option_Data),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Option_Data) = Predecessor (Ctx, F_Option_Data)'Old
          and Valid_Next (Ctx, F_Option_Data) = Valid_Next (Ctx, F_Option_Data)'Old
          and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
          and Get_Option_Class (Ctx) = Get_Option_Class (Ctx)'Old
          and Get_Option_Number (Ctx) = Get_Option_Number (Ctx)'Old
          and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
          and Structural_Valid (Ctx, F_Option_Data);

   procedure Initialize_Option_Data (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Option_Data)
          and then Field_Last (Ctx, F_Option_Data) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Option_Data))
          and then Available_Space (Ctx, F_Option_Data) >= Field_Length (Ctx, F_Option_Data),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Option_Data) = Predecessor (Ctx, F_Option_Data)'Old
          and Valid_Next (Ctx, F_Option_Data) = Valid_Next (Ctx, F_Option_Data)'Old
          and Get_Copied (Ctx) = Get_Copied (Ctx)'Old
          and Get_Option_Class (Ctx) = Get_Option_Class (Ctx)'Old
          and Get_Option_Number (Ctx) = Get_Option_Number (Ctx)'Old
          and Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
          and Structural_Valid (Ctx, F_Option_Data);

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
         when F_Copied =>
            Valid (Val.Copied_Value),
         when F_Option_Class =>
            Valid (Val.Option_Class_Value),
         when F_Option_Number =>
            Valid (Val.Option_Number_Value),
         when F_Option_Length =>
            Valid (Val.Option_Length_Value),
         when F_Option_Data =>
            True,
         when F_Initial | F_Final =>
            False));

   type Field_Cursor (State : Cursor_State := S_Invalid) is
      record
         Predecessor : Virtual_Field := F_Final;
         case State is
            when S_Valid | S_Structural_Valid =>
               First : Types.Bit_Index := Types.Bit_Index'First;
               Last : Types.Bit_Length := Types.Bit_Length'First;
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

   pragma Warnings (Off, """Buffer"" is not modified, could be of access constant type");

   function Valid_Context (Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index; Buffer : Types.Bytes_Ptr; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then
         Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last)
      and then Types.Byte_Index (First) >= Buffer_First
      and then Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= Types.Bit_Index'Last / 2
      and then (for all F in Field'First .. Field'Last =>
        (if Structural_Valid (Cursors (F)) then
         Cursors (F).First >= First
           and Cursors (F).Last <= Last
           and Cursors (F).First <= (Cursors (F).Last + 1)
           and Cursors (F).Value.Fld = F))
      and then ((if Structural_Valid (Cursors (F_Option_Class)) then
           (Valid (Cursors (F_Copied))
               and then Cursors (F_Option_Class).Predecessor = F_Copied))
        and then (if Structural_Valid (Cursors (F_Option_Number)) then
           (Valid (Cursors (F_Option_Class))
               and then Cursors (F_Option_Number).Predecessor = F_Option_Class))
        and then (if Structural_Valid (Cursors (F_Option_Length)) then
           (Valid (Cursors (F_Option_Number))
               and then Cursors (F_Option_Length).Predecessor = F_Option_Number
               and then Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) > 1))
        and then (if Structural_Valid (Cursors (F_Option_Data)) then
           (Valid (Cursors (F_Option_Length))
               and then Cursors (F_Option_Data).Predecessor = F_Option_Length
               and then ((Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Debugging_And_Measurement))
                   and Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                 or (Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                   and (Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                     or Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                     or Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                 or (Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                   and Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                   and Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                 or (Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                   and Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                   and Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 8)))))
      and then ((if Invalid (Cursors (F_Copied)) then
           Invalid (Cursors (F_Option_Class)))
        and then (if Invalid (Cursors (F_Option_Class)) then
           Invalid (Cursors (F_Option_Number)))
        and then (if Invalid (Cursors (F_Option_Number)) then
           Invalid (Cursors (F_Option_Length)))
        and then (if Invalid (Cursors (F_Option_Length)) then
           Invalid (Cursors (F_Option_Data))))
      and then (if Structural_Valid (Cursors (F_Copied)) then
         (Cursors (F_Copied).Last - Cursors (F_Copied).First + 1) = RFLX.RFLX_Builtin_Types.Boolean_Base'Size
           and then Cursors (F_Copied).Predecessor = F_Initial
           and then Cursors (F_Copied).First = First
           and then (if Structural_Valid (Cursors (F_Option_Class)) then
              (Cursors (F_Option_Class).Last - Cursors (F_Option_Class).First + 1) = RFLX.IPv4.Option_Class_Base'Size
                and then Cursors (F_Option_Class).Predecessor = F_Copied
                and then Cursors (F_Option_Class).First = (Cursors (F_Copied).Last + 1)
                and then (if Structural_Valid (Cursors (F_Option_Number)) then
                   (Cursors (F_Option_Number).Last - Cursors (F_Option_Number).First + 1) = RFLX.IPv4.Option_Number'Size
                     and then Cursors (F_Option_Number).Predecessor = F_Option_Class
                     and then Cursors (F_Option_Number).First = (Cursors (F_Option_Class).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Option_Length))
                          and then Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) > 1 then
                        (Cursors (F_Option_Length).Last - Cursors (F_Option_Length).First + 1) = RFLX.IPv4.Option_Length_Base'Size
                          and then Cursors (F_Option_Length).Predecessor = F_Option_Number
                          and then Cursors (F_Option_Length).First = (Cursors (F_Option_Number).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Option_Data))
                               and then ((Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Debugging_And_Measurement))
                                   and Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 4)
                                 or (Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                   and (Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 9
                                     or Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 3
                                     or Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 7))
                                 or (Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 11
                                   and Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                   and Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 2)
                                 or (Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) = 4
                                   and Types.Bit_Length (Cursors (F_Option_Class).Value.Option_Class_Value) = Types.Bit_Length (To_Base (Control))
                                   and Types.Bit_Length (Cursors (F_Option_Number).Value.Option_Number_Value) = 8)) then
                             (Cursors (F_Option_Data).Last - Cursors (F_Option_Data).First + 1) = ((Types.Bit_Length (Cursors (F_Option_Length).Value.Option_Length_Value) - 2)) * 8
                               and then Cursors (F_Option_Data).Predecessor = F_Option_Length
                               and then Cursors (F_Option_Data).First = (Cursors (F_Option_Length).Last + 1)))))));

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is
      record
         Buffer : Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Buffer, Context.Cursors);

   function Valid_Context (Ctx : Context) return Boolean is
     (Valid_Context (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last, Ctx.Buffer, Ctx.Cursors));

   function Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.IPv4.Generic_Option;

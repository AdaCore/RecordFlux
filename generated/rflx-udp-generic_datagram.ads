with RFLX.Types;
use type RFLX.Types.Integer_Address;

generic
package RFLX.UDP.Generic_Datagram with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   pragma Annotate (GNATprove, Terminating, Generic_Datagram);

   type Virtual_Field is (F_Initial, F_Source_Port, F_Destination_Port, F_Length, F_Checksum, F_Payload, F_Final);

   subtype Field is Virtual_Field range F_Source_Port .. F_Payload;

   type Field_Cursors is private;

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       False;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Payload | F_Final =>
               null;
            when F_Source_Port =>
               Source_Port_Value : Port;
            when F_Destination_Port =>
               Destination_Port_Value : Port;
            when F_Length =>
               Length_Value : Length_Base;
            when F_Checksum =>
               Checksum_Value : Checksum;
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
          and then Available_Space (Ctx, F_Source_Port) = (RFLX.Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
          and then Valid_Predecessor (Ctx, F_Source_Port)
          and then Path_Condition (Ctx, F_Source_Port)
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
          and then Available_Space (Ctx, F_Source_Port) = (RFLX.Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
          and then Valid_Predecessor (Ctx, F_Source_Port)
          and then Path_Condition (Ctx, F_Source_Port);

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

   function Get_Source_Port (Ctx : Context) return Port with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Source_Port);

   function Get_Destination_Port (Ctx : Context) return Port with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Destination_Port);

   function Get_Length (Ctx : Context) return Length with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Length);

   function Get_Checksum (Ctx : Context) return Checksum with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Checksum);

   generic
      with procedure Process_Payload (Payload : RFLX.Types.Bytes);
   procedure Get_Payload (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Payload);

   function Valid_Context (Ctx : Context) return Boolean;

   function Cursors (Ctx : Context) return Field_Cursors with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Incomplete);

   function Valid_Value (Value : Field_Dependent_Value) return Boolean is
     ((case Value.Fld is
         when F_Source_Port =>
            Valid (Value.Source_Port_Value),
         when F_Destination_Port =>
            Valid (Value.Destination_Port_Value),
         when F_Length =>
            Valid (Value.Length_Value),
         when F_Checksum =>
            Valid (Value.Checksum_Value),
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
      and then ((if Structural_Valid (Cursors (F_Destination_Port)) then
           (Valid (Cursors (F_Source_Port))
               and then Cursors (F_Destination_Port).Predecessor = F_Source_Port))
        and then (if Structural_Valid (Cursors (F_Length)) then
           (Valid (Cursors (F_Destination_Port))
               and then Cursors (F_Length).Predecessor = F_Destination_Port))
        and then (if Structural_Valid (Cursors (F_Checksum)) then
           (Valid (Cursors (F_Length))
               and then Cursors (F_Checksum).Predecessor = F_Length))
        and then (if Structural_Valid (Cursors (F_Payload)) then
           (Valid (Cursors (F_Checksum))
               and then Cursors (F_Payload).Predecessor = F_Checksum)))
      and then ((if Invalid (Cursors (F_Source_Port)) then
           Invalid (Cursors (F_Destination_Port)))
        and then (if Invalid (Cursors (F_Destination_Port)) then
           Invalid (Cursors (F_Length)))
        and then (if Invalid (Cursors (F_Length)) then
           Invalid (Cursors (F_Checksum)))
        and then (if Invalid (Cursors (F_Checksum)) then
           Invalid (Cursors (F_Payload))))
      and then (if Structural_Valid (Cursors (F_Source_Port)) then
         (Cursors (F_Source_Port).Last - Cursors (F_Source_Port).First + 1) = Port'Size
           and then Cursors (F_Source_Port).Predecessor = F_Initial
           and then Cursors (F_Source_Port).First = First
           and then (if Structural_Valid (Cursors (F_Destination_Port)) then
              (Cursors (F_Destination_Port).Last - Cursors (F_Destination_Port).First + 1) = Port'Size
                and then Cursors (F_Destination_Port).Predecessor = F_Source_Port
                and then Cursors (F_Destination_Port).First = (Cursors (F_Source_Port).Last + 1)
                and then (if Structural_Valid (Cursors (F_Length)) then
                   (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = Length_Base'Size
                     and then Cursors (F_Length).Predecessor = F_Destination_Port
                     and then Cursors (F_Length).First = (Cursors (F_Destination_Port).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Checksum)) then
                        (Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1) = Checksum'Size
                          and then Cursors (F_Checksum).Predecessor = F_Length
                          and then Cursors (F_Checksum).First = (Cursors (F_Length).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Payload)) then
                             (Cursors (F_Payload).Last - Cursors (F_Payload).First + 1) = ((RFLX.Types.Bit_Length (Cursors (F_Length).Value.Length_Value) - 8)) * 8
                               and then Cursors (F_Payload).Predecessor = F_Checksum
                               and then Cursors (F_Payload).First = (Cursors (F_Checksum).Last + 1)))))));

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

end RFLX.UDP.Generic_Datagram;

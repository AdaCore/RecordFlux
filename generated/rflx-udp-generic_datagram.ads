pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
with RFLX.RFLX_Generic_Types;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
package RFLX.UDP.Generic_Datagram with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Warnings (Off, "use clause for type ""U64"" * has no effect");

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Bit_Index, Types.U64;

   pragma Warnings (On, "use clause for type ""U64"" * has no effect");

   type Virtual_Field is (F_Initial, F_Source_Port, F_Destination_Port, F_Length, F_Checksum, F_Payload, F_Final);

   subtype Field is Virtual_Field range F_Source_Port .. F_Payload;

   type Field_Cursor is private with
     Default_Initial_Condition =>
       False;

   type Field_Cursors is private with
     Default_Initial_Condition =>
       False;

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       Types.Byte_Index (First) >= Buffer_First
       and Types.Byte_Index (Last) <= Buffer_Last
       and First <= Last
       and Last <= Types.Bit_Index'Last / 2;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Payload | F_Final =>
               null;
            when F_Source_Port =>
               Source_Port_Value : RFLX.UDP.Port;
            when F_Destination_Port =>
               Destination_Port_Value : RFLX.UDP.Port;
            when F_Length =>
               Length_Value : RFLX.UDP.Length_Base;
            when F_Checksum =>
               Checksum_Value : RFLX.UDP.Checksum;
         end case;
      end record;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
       and then Buffer /= null
       and then Buffer'Length > 0
       and then Buffer'Last <= Types.Index'Last / 2,
     Post =>
       Has_Buffer (Ctx)
       and Buffer = null
       and Ctx.Buffer_First = Buffer'First'Old
       and Ctx.Buffer_Last = Buffer'Last'Old
       and Ctx.First = Types.First_Bit_Index (Ctx.Buffer_First)
       and Initialized (Ctx),
     Depends =>
       (Ctx => Buffer, Buffer => null);

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

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) with
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

   function Has_Buffer (Ctx : Context) return Boolean;

   function Message_Last (Ctx : Context) return Types.Bit_Index with
     Pre =>
       Has_Buffer (Ctx)
       and then Structural_Valid_Message (Ctx);

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Predecessor (Ctx, Fld);

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean with
     Pre =>
       Has_Buffer (Ctx)
       and Val.Fld in Field'Range
       and Valid_Predecessor (Ctx, Val.Fld);

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field;

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean;

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean;

   function Available_Space (Ctx : Context; Fld : Field) return Types.Bit_Length with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : Types.Bytes) return Boolean with
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
       (if
           Valid'Result
        then
           Structural_Valid (Ctx, Fld)
           and Present (Ctx, Fld));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean;

   function Invalid (Ctx : Context; Fld : Field) return Boolean;

   function Structural_Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Has_Buffer (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Has_Buffer (Ctx);

   function Incomplete_Message (Ctx : Context) return Boolean;

   function Get_Source_Port (Ctx : Context) return RFLX.UDP.Port with
     Pre =>
       Valid (Ctx, F_Source_Port);

   function Get_Destination_Port (Ctx : Context) return RFLX.UDP.Port with
     Pre =>
       Valid (Ctx, F_Destination_Port);

   function Get_Length (Ctx : Context) return RFLX.UDP.Length with
     Pre =>
       Valid (Ctx, F_Length);

   function Get_Checksum (Ctx : Context) return RFLX.UDP.Checksum with
     Pre =>
       Valid (Ctx, F_Checksum);

   generic
      with procedure Process_Payload (Payload : Types.Bytes);
   procedure Get_Payload (Ctx : Context) with
     Pre =>
       Has_Buffer (Ctx)
       and Present (Ctx, F_Payload);

   procedure Set_Source_Port (Ctx : in out Context; Val : RFLX.UDP.Port) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Source_Port)
       and then Field_Last (Ctx, F_Source_Port) <= Types.Bit_Index'Last / 2
       and then Field_Condition (Ctx, (F_Source_Port, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Source_Port) >= Field_Length (Ctx, F_Source_Port),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Source_Port)
       and Get_Source_Port (Ctx) = Val
       and Invalid (Ctx, F_Destination_Port)
       and Invalid (Ctx, F_Length)
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Destination_Port) = F_Source_Port
            and Valid_Next (Ctx, F_Destination_Port))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Predecessor (Ctx, F_Source_Port) = Predecessor (Ctx, F_Source_Port)'Old
       and Valid_Next (Ctx, F_Source_Port) = Valid_Next (Ctx, F_Source_Port)'Old;

   procedure Set_Destination_Port (Ctx : in out Context; Val : RFLX.UDP.Port) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Destination_Port)
       and then Field_Last (Ctx, F_Destination_Port) <= Types.Bit_Index'Last / 2
       and then Field_Condition (Ctx, (F_Destination_Port, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Destination_Port) >= Field_Length (Ctx, F_Destination_Port),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Destination_Port)
       and Get_Destination_Port (Ctx) = Val
       and Invalid (Ctx, F_Length)
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Length) = F_Destination_Port
            and Valid_Next (Ctx, F_Length))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Predecessor (Ctx, F_Destination_Port) = Predecessor (Ctx, F_Destination_Port)'Old
       and Valid_Next (Ctx, F_Destination_Port) = Valid_Next (Ctx, F_Destination_Port)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Context_Cursor (Ctx, F_Source_Port) = Context_Cursor (Ctx, F_Source_Port)'Old;

   procedure Set_Length (Ctx : in out Context; Val : RFLX.UDP.Length) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Length)
       and then Field_Last (Ctx, F_Length) <= Types.Bit_Index'Last / 2
       and then Field_Condition (Ctx, (F_Length, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Length) >= Field_Length (Ctx, F_Length),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Length)
       and Get_Length (Ctx) = Val
       and Invalid (Ctx, F_Checksum)
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Checksum) = F_Length
            and Valid_Next (Ctx, F_Checksum))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Predecessor (Ctx, F_Length) = Predecessor (Ctx, F_Length)'Old
       and Valid_Next (Ctx, F_Length) = Valid_Next (Ctx, F_Length)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Context_Cursor (Ctx, F_Source_Port) = Context_Cursor (Ctx, F_Source_Port)'Old
       and Context_Cursor (Ctx, F_Destination_Port) = Context_Cursor (Ctx, F_Destination_Port)'Old;

   procedure Set_Checksum (Ctx : in out Context; Val : RFLX.UDP.Checksum) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Checksum)
       and then Field_Last (Ctx, F_Checksum) <= Types.Bit_Index'Last / 2
       and then Field_Condition (Ctx, (F_Checksum, To_Base (Val)))
       and then Valid (To_Base (Val))
       and then Available_Space (Ctx, F_Checksum) >= Field_Length (Ctx, F_Checksum),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, F_Checksum)
       and Get_Checksum (Ctx) = Val
       and Invalid (Ctx, F_Payload)
       and (Predecessor (Ctx, F_Payload) = F_Checksum
            and Valid_Next (Ctx, F_Payload))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Predecessor (Ctx, F_Checksum) = Predecessor (Ctx, F_Checksum)'Old
       and Valid_Next (Ctx, F_Checksum) = Valid_Next (Ctx, F_Checksum)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and Context_Cursor (Ctx, F_Source_Port) = Context_Cursor (Ctx, F_Source_Port)'Old
       and Context_Cursor (Ctx, F_Destination_Port) = Context_Cursor (Ctx, F_Destination_Port)'Old
       and Context_Cursor (Ctx, F_Length) = Context_Cursor (Ctx, F_Length)'Old;

   procedure Set_Payload_Empty (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Payload)
       and then Field_Last (Ctx, F_Payload) <= Types.Bit_Index'Last / 2
       and then Field_Condition (Ctx, (Fld => F_Payload))
       and then Available_Space (Ctx, F_Payload) >= Field_Length (Ctx, F_Payload)
       and then Field_First (Ctx, F_Payload) mod Types.Byte'Size = 1
       and then Field_Length (Ctx, F_Payload) mod Types.Byte'Size = 0
       and then Field_Length (Ctx, F_Payload) = 0,
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Structural_Valid (Ctx, F_Payload);

   generic
      with procedure Process_Payload (Payload : out Types.Bytes);
      with function Valid_Length (Length : Types.Length) return Boolean;
   procedure Set_Payload (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Payload)
       and then Field_Last (Ctx, F_Payload) <= Types.Bit_Index'Last / 2
       and then Field_Condition (Ctx, (Fld => F_Payload))
       and then Available_Space (Ctx, F_Payload) >= Field_Length (Ctx, F_Payload)
       and then Field_First (Ctx, F_Payload) mod Types.Byte'Size = 1
       and then Field_Length (Ctx, F_Payload) mod Types.Byte'Size = 0
       and then Valid_Length (Types.Length (Field_Length (Ctx, F_Payload) / Types.Byte'Size)),
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Structural_Valid (Ctx, F_Payload);

   procedure Initialize_Payload (Ctx : in out Context) with
     Pre =>
       not Ctx'Constrained
       and then Has_Buffer (Ctx)
       and then Valid_Next (Ctx, F_Payload)
       and then Field_Last (Ctx, F_Payload) <= Types.Bit_Index'Last / 2
       and then Field_Condition (Ctx, (Fld => F_Payload))
       and then Available_Space (Ctx, F_Payload) >= Field_Length (Ctx, F_Payload)
       and then Field_First (Ctx, F_Payload) mod Types.Byte'Size = 1
       and then Field_Length (Ctx, F_Payload) mod Types.Byte'Size = 0,
     Post =>
       Has_Buffer (Ctx)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
       and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
       and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
       and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
       and Get_Length (Ctx) = Get_Length (Ctx)'Old
       and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
       and Structural_Valid (Ctx, F_Payload);

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
          when F_Source_Port =>
             Valid (Val.Source_Port_Value),
          when F_Destination_Port =>
             Valid (Val.Destination_Port_Value),
          when F_Length =>
             Valid (Val.Length_Value),
          when F_Checksum =>
             Valid (Val.Checksum_Value),
          when F_Payload =>
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
       (if
           State = S_Valid
           or State = S_Structural_Valid
        then
           Valid_Value (Field_Cursor.Value));

   type Field_Cursors is array (Virtual_Field) of Field_Cursor;

   function Structural_Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid
      or Cursor.State = S_Structural_Valid);

   function Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid);

   function Invalid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Invalid
      or Cursor.State = S_Incomplete);

   function Valid_Context (Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index; Buffer : access constant Types.Bytes; Cursors : Field_Cursors) return Boolean is
     ((if
          Buffer /= null
       then
          Buffer'First = Buffer_First
          and Buffer'Last = Buffer_Last)
      and then (Types.Byte_Index (First) >= Buffer_First
                and Types.Byte_Index (Last) <= Buffer_Last
                and First <= Last
                and Last <= Types.Bit_Index'Last / 2)
      and then (for all F in Field'First .. Field'Last =>
                   (if
                       Structural_Valid (Cursors (F))
                    then
                       Cursors (F).First >= First
                       and Cursors (F).Last <= Last
                       and Cursors (F).First <= Cursors (F).Last + 1
                       and Cursors (F).Value.Fld = F))
      and then ((if
                    Structural_Valid (Cursors (F_Destination_Port))
                 then
                    (Valid (Cursors (F_Source_Port))
                     and then Cursors (F_Destination_Port).Predecessor = F_Source_Port))
                and then (if
                             Structural_Valid (Cursors (F_Length))
                          then
                             (Valid (Cursors (F_Destination_Port))
                              and then Cursors (F_Length).Predecessor = F_Destination_Port))
                and then (if
                             Structural_Valid (Cursors (F_Checksum))
                          then
                             (Valid (Cursors (F_Length))
                              and then Cursors (F_Checksum).Predecessor = F_Length))
                and then (if
                             Structural_Valid (Cursors (F_Payload))
                          then
                             (Valid (Cursors (F_Checksum))
                              and then Cursors (F_Payload).Predecessor = F_Checksum)))
      and then ((if
                    Invalid (Cursors (F_Source_Port))
                 then
                    Invalid (Cursors (F_Destination_Port)))
                and then (if
                             Invalid (Cursors (F_Destination_Port))
                          then
                             Invalid (Cursors (F_Length)))
                and then (if
                             Invalid (Cursors (F_Length))
                          then
                             Invalid (Cursors (F_Checksum)))
                and then (if
                             Invalid (Cursors (F_Checksum))
                          then
                             Invalid (Cursors (F_Payload))))
      and then (if
                   Structural_Valid (Cursors (F_Source_Port))
                then
                   Cursors (F_Source_Port).Last - Cursors (F_Source_Port).First + 1 = RFLX.UDP.Port'Size
                   and then Cursors (F_Source_Port).Predecessor = F_Initial
                   and then Cursors (F_Source_Port).First = First
                   and then (if
                                Structural_Valid (Cursors (F_Destination_Port))
                             then
                                Cursors (F_Destination_Port).Last - Cursors (F_Destination_Port).First + 1 = RFLX.UDP.Port'Size
                                and then Cursors (F_Destination_Port).Predecessor = F_Source_Port
                                and then Cursors (F_Destination_Port).First = Cursors (F_Source_Port).Last + 1
                                and then (if
                                             Structural_Valid (Cursors (F_Length))
                                          then
                                             Cursors (F_Length).Last - Cursors (F_Length).First + 1 = RFLX.UDP.Length_Base'Size
                                             and then Cursors (F_Length).Predecessor = F_Destination_Port
                                             and then Cursors (F_Length).First = Cursors (F_Destination_Port).Last + 1
                                             and then (if
                                                          Structural_Valid (Cursors (F_Checksum))
                                                       then
                                                          Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1 = RFLX.UDP.Checksum'Size
                                                          and then Cursors (F_Checksum).Predecessor = F_Length
                                                          and then Cursors (F_Checksum).First = Cursors (F_Length).Last + 1
                                                          and then (if
                                                                       Structural_Valid (Cursors (F_Payload))
                                                                    then
                                                                       Cursors (F_Payload).Last - Cursors (F_Payload).First + 1 = (Types.Bit_Length (Cursors (F_Length).Value.Length_Value) - 8) * 8
                                                                       and then Cursors (F_Payload).Predecessor = F_Checksum
                                                                       and then Cursors (F_Payload).First = Cursors (F_Checksum).Last + 1))))));

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is
      record
         Buffer : Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Buffer, Context.Cursors);

   function Context_Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Context_Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.UDP.Generic_Datagram;

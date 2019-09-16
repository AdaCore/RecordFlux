with RFLX.Types;
use type RFLX.Types.Integer_Address;

generic
package RFLX.Ethernet.Generic_Frame with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   type Virtual_Field is (F_Initial, F_Destination, F_Source, F_Type_Length_TPID, F_TPID, F_TCI, F_Type_Length, F_Payload, F_Final);

   subtype Field is Virtual_Field range F_Destination .. F_Payload;

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First; Buffer_Address : RFLX.Types.Integer_Address := 0) is private with
     Default_Initial_Condition =>
       False;

   function Create return Context;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Last <= RFLX.Types.Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Ctx.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and then Ctx.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and then Buffer = null;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
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
          and then Ctx.Buffer_Address = RFLX.Types.Bytes_Address (Buffer)'Old
          and then Ctx.First = First
          and then Ctx.Last = Last;

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
          and then Ctx.Buffer_Address = RFLX.Types.Bytes_Address (Buffer)
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.Buffer_Address = Ctx.Buffer_Address'Old
          and then Ctx.First = Ctx.First'Old
          and then Ctx.Last = Ctx.Last'Old
          and then Present (Ctx, F_Destination) = Present (Ctx, F_Destination)'Old
          and then Present (Ctx, F_Source) = Present (Ctx, F_Source)'Old
          and then Present (Ctx, F_Type_Length_TPID) = Present (Ctx, F_Type_Length_TPID)'Old
          and then Present (Ctx, F_TPID) = Present (Ctx, F_TPID)'Old
          and then Present (Ctx, F_TCI) = Present (Ctx, F_TCI)'Old
          and then Present (Ctx, F_Type_Length) = Present (Ctx, F_Type_Length)'Old
          and then Present (Ctx, F_Payload) = Present (Ctx, F_Payload)'Old;

   function Has_Buffer (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   procedure Field_Range (Ctx : Context; Fld : Field; First : out RFLX.Types.Bit_Index; Last : out RFLX.Types.Bit_Index) with
     Pre =>
       Valid_Context (Ctx)
          and then Present (Ctx, Fld),
     Post =>
       Present (Ctx, Fld)
          and then Ctx.First <= First
          and then Ctx.Last >= Last
          and then First <= Last;

   function Index (Ctx : Context) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Index'Result >= Ctx.First
          and then Index'Result - Ctx.Last <= 1;

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and then (if Fld /= F_Destination then (if Valid (Ctx, F_Destination)'Old then Valid (Ctx, F_Destination)))
          and then (if Fld /= F_Source then (if Valid (Ctx, F_Source)'Old then Valid (Ctx, F_Source)))
          and then (if Fld /= F_Type_Length_TPID then (if Valid (Ctx, F_Type_Length_TPID)'Old then Valid (Ctx, F_Type_Length_TPID)))
          and then (if Fld /= F_TPID then (if Valid (Ctx, F_TPID)'Old then Valid (Ctx, F_TPID)))
          and then (if Fld /= F_TCI then (if Valid (Ctx, F_TCI)'Old then Valid (Ctx, F_TCI)))
          and then (if Fld /= F_Type_Length then (if Valid (Ctx, F_Type_Length)'Old then Valid (Ctx, F_Type_Length)))
          and then (if Fld /= F_Payload then (if Valid (Ctx, F_Payload)'Old then Valid (Ctx, F_Payload)))
          and then Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and then Ctx.Buffer_First = Ctx.Buffer_First'Old
          and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and then Ctx.Buffer_Address = Ctx.Buffer_Address'Old
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
          and then Ctx.Buffer_Address = Ctx.Buffer_Address'Old
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
       (if Valid'Result then Present (Ctx, Fld)
          and then Structural_Valid (Ctx, Fld));

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

   function Get_Destination (Ctx : Context) return Address with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Destination);

   function Get_Source (Ctx : Context) return Address with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Source);

   function Get_Type_Length_TPID (Ctx : Context) return Type_Length with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Type_Length_TPID);

   function Get_TPID (Ctx : Context) return TPID with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_TPID);

   function Get_TCI (Ctx : Context) return TCI with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_TCI);

   function Get_Type_Length (Ctx : Context) return Type_Length with
     Pre =>
       Valid_Context (Ctx)
          and then Valid (Ctx, F_Type_Length);

   generic
      with procedure Process_Payload (Payload : RFLX.Types.Bytes);
   procedure Get_Payload (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and then Has_Buffer (Ctx)
          and then Present (Ctx, F_Payload);

   function Valid_Context (Ctx : Context) return Boolean;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Preliminary, S_Incomplete);

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
         case State is
            when S_Valid | S_Structural_Valid | S_Preliminary =>
               First : RFLX.Types.Bit_Index;
               Last : RFLX.Types.Bit_Length;
               Value : Field_Dependent_Value;
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if State = S_Valid
          or State = S_Structural_Valid then Valid_Value (Value));

   type Field_Cursors is array (Field) of Field_Cursor;

   function Valid_Context (Buffer_First, Buffer_Last : RFLX.Types.Index; First, Last : RFLX.Types.Bit_Index; Buffer_Address : RFLX.Types.Integer_Address; Buffer : RFLX.Types.Bytes_Ptr; Index : RFLX.Types.Bit_Index; Fld : Virtual_Field; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then Buffer'First = Buffer_First
        and then Buffer'Last = Buffer_Last
        and then RFLX.Types.Bytes_Address (Buffer) = Buffer_Address)
      and then RFLX.Types.Byte_Index (First) >= Buffer_First
      and then RFLX.Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= RFLX.Types.Bit_Index'Last / 2
      and then Index >= First
      and then Index - Last <= 1
      and then (for all F in Field'First .. Field'Last =>
        (if Cursors (F).State = S_Valid
        or Cursors (F).State = S_Structural_Valid then Cursors (F).First >= First
        and then Cursors (F).Last <= Last
        and then Cursors (F).First <= (Cursors (F).Last + 1)
        and then Cursors (F).Value.Fld = F))
      and then (case Fld is
           when F_Initial =>
              True,
           when F_Destination =>
              (Cursors (F_Destination).State = S_Valid
                   or Cursors (F_Destination).State = S_Structural_Valid)
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address'Size,
           when F_Source =>
              (Cursors (F_Destination).State = S_Valid
                   or Cursors (F_Destination).State = S_Structural_Valid)
                 and then (Cursors (F_Source).State = S_Valid
                   or Cursors (F_Source).State = S_Structural_Valid)
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address'Size,
           when F_Type_Length_TPID =>
              (Cursors (F_Destination).State = S_Valid
                   or Cursors (F_Destination).State = S_Structural_Valid)
                 and then (Cursors (F_Source).State = S_Valid
                   or Cursors (F_Source).State = S_Structural_Valid)
                 and then (Cursors (F_Type_Length_TPID).State = S_Valid
                   or Cursors (F_Type_Length_TPID).State = S_Structural_Valid)
                 and then (RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 33024
                   or RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 33024)
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address'Size
                 and then (Cursors (F_Type_Length_TPID).Last - Cursors (F_Type_Length_TPID).First + 1) = Type_Length_Base'Size,
           when F_TPID =>
              (Cursors (F_Destination).State = S_Valid
                   or Cursors (F_Destination).State = S_Structural_Valid)
                 and then (Cursors (F_Source).State = S_Valid
                   or Cursors (F_Source).State = S_Structural_Valid)
                 and then (Cursors (F_Type_Length_TPID).State = S_Valid
                   or Cursors (F_Type_Length_TPID).State = S_Structural_Valid)
                 and then (Cursors (F_TPID).State = S_Valid
                   or Cursors (F_TPID).State = S_Structural_Valid)
                 and then (RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 33024
                   or RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 33024)
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address'Size
                 and then (Cursors (F_Type_Length_TPID).Last - Cursors (F_Type_Length_TPID).First + 1) = Type_Length_Base'Size
                 and then (Cursors (F_TPID).Last - Cursors (F_TPID).First + 1) = TPID_Base'Size,
           when F_TCI =>
              (Cursors (F_Destination).State = S_Valid
                   or Cursors (F_Destination).State = S_Structural_Valid)
                 and then (Cursors (F_Source).State = S_Valid
                   or Cursors (F_Source).State = S_Structural_Valid)
                 and then (Cursors (F_Type_Length_TPID).State = S_Valid
                   or Cursors (F_Type_Length_TPID).State = S_Structural_Valid)
                 and then (Cursors (F_TPID).State = S_Valid
                   or Cursors (F_TPID).State = S_Structural_Valid)
                 and then (Cursors (F_TCI).State = S_Valid
                   or Cursors (F_TCI).State = S_Structural_Valid)
                 and then (RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 33024
                   or RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 33024)
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address'Size
                 and then (Cursors (F_Type_Length_TPID).Last - Cursors (F_Type_Length_TPID).First + 1) = Type_Length_Base'Size
                 and then (Cursors (F_TPID).Last - Cursors (F_TPID).First + 1) = TPID_Base'Size
                 and then (Cursors (F_TCI).Last - Cursors (F_TCI).First + 1) = TCI'Size,
           when F_Type_Length =>
              (Cursors (F_Destination).State = S_Valid
                   or Cursors (F_Destination).State = S_Structural_Valid)
                 and then (Cursors (F_Source).State = S_Valid
                   or Cursors (F_Source).State = S_Structural_Valid)
                 and then (Cursors (F_Type_Length_TPID).State = S_Valid
                   or Cursors (F_Type_Length_TPID).State = S_Structural_Valid)
                 and then (Cursors (F_Type_Length).State = S_Valid
                   or Cursors (F_Type_Length).State = S_Structural_Valid)
                 and then (RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 33024
                   or RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 33024)
                 and then (RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
                   or RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536)
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address'Size
                 and then (Cursors (F_Type_Length_TPID).Last - Cursors (F_Type_Length_TPID).First + 1) = Type_Length_Base'Size
                 and then (Cursors (F_Type_Length).Last - Cursors (F_Type_Length).First + 1) = Type_Length_Base'Size,
           when F_Payload | F_Final =>
              (Cursors (F_Destination).State = S_Valid
                   or Cursors (F_Destination).State = S_Structural_Valid)
                 and then (Cursors (F_Source).State = S_Valid
                   or Cursors (F_Source).State = S_Structural_Valid)
                 and then (Cursors (F_Type_Length_TPID).State = S_Valid
                   or Cursors (F_Type_Length_TPID).State = S_Structural_Valid)
                 and then (Cursors (F_Type_Length).State = S_Valid
                   or Cursors (F_Type_Length).State = S_Structural_Valid)
                 and then (Cursors (F_Payload).State = S_Valid
                   or Cursors (F_Payload).State = S_Structural_Valid)
                 and then (RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) = 33024
                   or RFLX.Types.Bit_Length (Cursors (F_Type_Length_TPID).Value.Type_Length_TPID_Value) /= 33024)
                 and then (RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) <= 1500
                   or RFLX.Types.Bit_Length (Cursors (F_Type_Length).Value.Type_Length_Value) >= 1536)
                 and then (((Cursors (F_Payload).Last - Cursors (F_Payload).First + 1)) / 8 >= 46
                   and then ((Cursors (F_Payload).Last - Cursors (F_Payload).First + 1)) / 8 <= 1500)
                 and then (Cursors (F_Destination).Last - Cursors (F_Destination).First + 1) = Address'Size
                 and then (Cursors (F_Source).Last - Cursors (F_Source).First + 1) = Address'Size
                 and then (Cursors (F_Type_Length_TPID).Last - Cursors (F_Type_Length_TPID).First + 1) = Type_Length_Base'Size
                 and then (Cursors (F_Type_Length).Last - Cursors (F_Type_Length).First + 1) = Type_Length_Base'Size));

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First; Buffer_Address : RFLX.Types.Integer_Address := 0) is
      record
         Buffer : RFLX.Types.Bytes_Ptr := null;
         Index : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First;
         Fld : Virtual_Field := F_Initial;
         Cursors : Field_Cursors := (others => (State => S_Invalid));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Buffer_First, Buffer_Last, First, Last, Buffer_Address, Buffer, Index, Fld, Cursors);

   function Valid_Context (Ctx : Context) return Boolean is
     (Valid_Context (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last, Ctx.Buffer_Address, Ctx.Buffer, Ctx.Index, Ctx.Fld, Ctx.Cursors));

end RFLX.Ethernet.Generic_Frame;

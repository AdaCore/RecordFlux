pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, """Always_Terminates"" is not a valid aspect identifier");
with {prefix}RFLX_Types;

generic
   type Element_Type is private;
   Element_Size : Positive;
   with function Valid (Element : {prefix}RFLX_Types.Base_Integer) return Boolean;
   with function To_Actual (Element : {prefix}RFLX_Types.Base_Integer) return Element_Type;
   with function To_Base_Int (Element : Element_Type) return {prefix}RFLX_Types.Base_Integer;
package {prefix}RFLX_Scalar_Sequence with
  SPARK_Mode,
  Always_Terminates
is

   use type RFLX_Types.Bytes_Ptr;

   use type RFLX_Types.Index;

   pragma Warnings (Off, """LENGTH"" is already use-visible through previous use_type_clause");

   use type RFLX_Types.Length;

   pragma Warnings (On, """LENGTH"" is already use-visible through previous use_type_clause");

   use type RFLX_Types.Bit_Index;

   use type RFLX_Types.Base_Integer;

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First) is private with
     Default_Initial_Condition =>
       RFLX_Types.To_Index (First) >= Buffer_First
       and RFLX_Types.To_Index (Last) <= Buffer_Last
       and Buffer_Last < RFLX_Types.Index'Last
       and First <= Last + 1
       and Last <= RFLX_Types.Bit_Length'Last - 1
       and First mod RFLX_Types.Byte'Size = 1;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr) with
     Pre =>
       (not Ctx'Constrained
        and then Buffer /= null
        and then Buffer'Length > 0
        and then Buffer'Last < RFLX_Types.Index'Last),
     Post =>
       (Has_Buffer (Ctx)
        and Valid (Ctx)
        and Buffer = null
        and Ctx.Buffer_First = Buffer'First'Old
        and Ctx.Buffer_Last = Buffer'Last'Old
        and Ctx.First = RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First)
        and Ctx.Last = RFLX_Types.To_Last_Bit_Index (Ctx.Buffer_Last)
        and Sequence_Last (Ctx) = Ctx.First - 1),
     Depends =>
       (Ctx => Buffer, Buffer => null);

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) with
     Pre =>
       (not Ctx'Constrained
        and then Buffer /= null
        and then Buffer'Length > 0
        and then Buffer'Last < RFLX_Types.Index'Last
        and then RFLX_Types.To_Index (First) >= Buffer'First
        and then RFLX_Types.To_Index (Last) <= Buffer'Last
        and then First <= Last + 1
        and then Last <= RFLX_Types.Bit_Length'Last - 1
        and then First mod RFLX_Types.Byte'Size = 1),
     Post =>
       (Buffer = null
        and Has_Buffer (Ctx)
        and Valid (Ctx)
        and Ctx.Buffer_First = Buffer'First'Old
        and Ctx.Buffer_Last = Buffer'Last'Old
        and Ctx.First = First
        and Ctx.Last = Last
        and Sequence_Last (Ctx) = First - 1),
     Depends =>
       (Ctx => (Buffer, First, Last), Buffer => null);

   procedure Reset (Ctx : in out Context) with
     Pre =>
       Has_Buffer (Ctx),
     Post =>
       (Has_Buffer (Ctx)
        and Valid (Ctx)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old
        and Sequence_Last (Ctx) = Ctx.First - 1);

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX_Types.Bytes_Ptr) with
     Pre =>
       Has_Buffer (Ctx),
     Post =>
       (not Has_Buffer (Ctx)
        and Buffer /= null
        and Buffer'First = Ctx.Buffer_First
        and Buffer'Last = Ctx.Buffer_Last
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old
        and Valid (Ctx) = Valid (Ctx)'Old
        and Sequence_Last (Ctx) = Sequence_Last (Ctx)'Old),
     Depends =>
       (Ctx => Ctx, Buffer => Ctx);

   procedure Copy (Ctx : Context; Buffer : out RFLX_Types.Bytes) with
     Pre =>
       (Has_Buffer (Ctx)
        and Valid (Ctx)
        and Byte_Size (Ctx) = Buffer'Length);

   procedure Next (Ctx : in out Context) with
     Pre =>
       (Has_Buffer (Ctx)
        and then Has_Element (Ctx)),
     Post =>
       (Has_Buffer (Ctx)
        and Sequence_Last (Ctx) = Sequence_Last (Ctx)'Old + {prefix}RFLX_Types.Bit_Index (Element_Size)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old);

   function Has_Element (Ctx : Context) return Boolean;

   function Valid_Element (Ctx : Context) return Boolean;

   function Get_Element (Ctx : Context) return Element_Type with
     Pre =>
       Valid_Element (Ctx);

   function Head (Ctx : Context) return Element_Type with
     Pre =>
       (Valid (Ctx)
        and then Sequence_Last (Ctx) >= Ctx.First + {prefix}RFLX_Types.Bit_Index (Element_Size) - 1);

   procedure Append_Element (Ctx : in out Context; Value : Element_Type) with
     Pre =>
       (Has_Buffer (Ctx)
        and then Valid (Ctx)
        and then Valid (To_Base_Int (Value))
        and then (if Element_Size < 64 then To_Base_Int (Value) < 2**Element_Size)
        and then Available_Space (Ctx) >= {prefix}RFLX_Types.Bit_Index (Element_Size)),
     Post =>
       (Has_Buffer (Ctx)
        and Valid (Ctx)
        and Sequence_Last (Ctx) = Sequence_Last (Ctx)'Old + {prefix}RFLX_Types.Bit_Index (Element_Size)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old);

   function Valid (Ctx : Context) return Boolean;

   function Has_Buffer (Ctx : Context) return Boolean;

   function Available_Space (Ctx : Context) return RFLX_Types.Bit_Length;

   function Sequence_Last (Ctx : Context) return RFLX_Types.Bit_Length;

   function Size (Ctx : Context) return RFLX_Types.Bit_Length;

   function Byte_Size (Ctx : Context) return RFLX_Types.Length;

   procedure Data (Ctx : Context; Data : out RFLX_Types.Bytes) with
     Pre =>
       (Has_Buffer (Ctx)
        and then Valid (Ctx)
        and then Data'Length = Byte_Size (Ctx));

private

   -- Eng/RecordFlux/Workarounds#24
   pragma Warnings (Off, "use clause for package * has no effect");

   use {prefix}RFLX_Types;

   pragma Warnings (On, "use clause for package * has no effect");

   type Context_State is (S_Valid, S_Invalid);

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First) is
      record
         Sequence_Last : RFLX_Types.Bit_Length := First - 1;
         Buffer        : RFLX_Types.Bytes_Ptr := null;
         State         : Context_State := S_Valid;
         First_Element : {prefix}RFLX_Types.Base_Integer := {prefix}RFLX_Types.Base_Integer'First;
         Next_Element  : {prefix}RFLX_Types.Base_Integer := {prefix}RFLX_Types.Base_Integer'First;
      end record with
     Dynamic_Predicate =>
       ((if Buffer /= null then
          (Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last))
        and RFLX_Types.To_Index (First) >= Buffer_First
        and RFLX_Types.To_Index (Last) <= Buffer_Last
        and First mod RFLX_Types.Byte'Size = 1
        and Buffer_Last < RFLX_Types.Index'Last
        and First <= Last + 1
        and Last <= RFLX_Types.Bit_Length'Last - 1
        and Sequence_Last >= First - 1
        and Sequence_Last <= Last
        and (if Sequence_Last > First - 1 and State = S_Valid then Valid (First_Element)));

   function Has_Element (Ctx : Context) return Boolean is
     (Ctx.State = S_Valid and Ctx.Last - Ctx.Sequence_Last >= {prefix}RFLX_Types.Bit_Index (Element_Size));

   function Valid_Element (Ctx : Context) return Boolean is
     (Ctx.State = S_Valid and Valid (Ctx.Next_Element));

   function Valid (Ctx : Context) return Boolean is
     (Ctx.State = S_Valid);

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Available_Space (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Last - Ctx.Sequence_Last);

   function Sequence_Last (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Sequence_Last);

   function Size (Ctx : Context) return RFLX_Types.Bit_Length is
     (Ctx.Sequence_Last - Ctx.First + 1);

   function Byte_Size (Ctx : Context) return RFLX_Types.Length is
     (RFLX_Types.To_Length (Size (Ctx)));

end {prefix}RFLX_Scalar_Sequence;

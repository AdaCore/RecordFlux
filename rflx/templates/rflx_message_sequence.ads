pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, """Always_Terminates"" is not a valid aspect identifier");
with {prefix}RFLX_Types;

generic
   type Element_Context (Buffer_First, Buffer_Last : RFLX_Types.Index; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is private;
   with procedure Element_Initialize (Ctx : out Element_Context; Buffer : in out RFLX_Types.Bytes_Ptr; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length; Written_Last : RFLX_Types.Bit_Length := 0);
   with procedure Element_Take_Buffer (Ctx : in out Element_Context; Buffer : out RFLX_Types.Bytes_Ptr);
   with procedure Element_Copy (Ctx : Element_Context; Buffer : out RFLX_Types.Bytes);
   with function Element_Has_Buffer (Ctx : Element_Context) return Boolean;
   with function Element_Size (Ctx : Element_Context) return RFLX_Types.Bit_Length;
   with function Element_Last (Ctx : Element_Context) return RFLX_Types.Bit_Index;
   with function Element_Initialized (Ctx : Element_Context) return Boolean;
   with function Element_Valid_Message (Ctx : Element_Context) return Boolean;
package {prefix}RFLX_Message_Sequence with
  SPARK_Mode,
  Always_Terminates
is

   pragma Unevaluated_Use_Of_Old (Allow);

   pragma Warnings (Off, """LENGTH"" is already use-visible through previous use_type_clause");

   use type RFLX_Types.Bytes_Ptr, RFLX_Types.Index, RFLX_Types.Length, RFLX_Types.Bit_Index;

   pragma Warnings (On, """LENGTH"" is already use-visible through previous use_type_clause");

   type Context (Buffer_First, Buffer_Last : RFLX_Types.Index := RFLX_Types.Index'First; First : RFLX_Types.Bit_Index := RFLX_Types.Bit_Index'First; Last : RFLX_Types.Bit_Length := RFLX_Types.Bit_Length'First) is private with
     Default_Initial_Condition =>
       RFLX_Types.To_Index (First) >= Buffer_First
       and RFLX_Types.To_Index (Last) <= Buffer_Last
       and Buffer_Last < RFLX_Types.Index'Last
       and First <= Last + 1
       and Last <= RFLX_Types.Bit_Length'Last - 1
       and First mod RFLX_Types.Byte'Size = 1
       and Last mod RFLX_Types.Byte'Size = 0;

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
        and then First mod RFLX_Types.Byte'Size = 1
        and then Last mod RFLX_Types.Byte'Size = 0),
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

   function Has_Element (Ctx : Context) return Boolean;

   procedure Append_Element (Ctx : in out Context; Element_Ctx : Element_Context) with
     Pre =>
       (Has_Buffer (Ctx)
        and then Valid (Ctx)
        and then Element_Has_Buffer (Element_Ctx)
        and then Element_Valid_Message (Element_Ctx)
        and then Element_Size (Element_Ctx) > 0
        and then Available_Space (Ctx) >= Element_Size (Element_Ctx)),
     Post =>
       (Has_Buffer (Ctx)
        and Valid (Ctx)
        and Sequence_Last (Ctx) = Sequence_Last (Ctx)'Old + Element_Size (Element_Ctx)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old);

   procedure Switch (Ctx : in out Context; Element_Ctx : out Element_Context) with
     Pre =>
       (not Element_Ctx'Constrained
        and then Has_Buffer (Ctx)
        and then Has_Element (Ctx)
        and then Valid (Ctx)),
     Post =>
       (not Has_Buffer (Ctx)
        and Has_Element (Ctx)
        and Valid (Ctx)
        and Element_Has_Buffer (Element_Ctx)
        and Ctx.Buffer_First = Element_Ctx.Buffer_First
        and Ctx.Buffer_Last = Element_Ctx.Buffer_Last
        and Ctx.First <= Element_Ctx.First
        and Ctx.Last >= Element_Ctx.Last
        and Element_Ctx.First = Sequence_Last (Ctx) + 1
        and Element_Ctx.Last = Ctx.Last
        and Element_Initialized (Element_Ctx)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old
        and Sequence_Last (Ctx) = Sequence_Last (Ctx)'Old),
     Depends =>
       (Ctx => Ctx, Element_Ctx => Ctx);

   procedure Update (Ctx : in out Context; Element_Ctx : in out Element_Context) with
     Pre =>
       (not Has_Buffer (Ctx)
        and then Element_Has_Buffer (Element_Ctx)
        and then Has_Element (Ctx)
        and then Valid (Ctx)
        and then Ctx.Buffer_First = Element_Ctx.Buffer_First
        and then Ctx.Buffer_Last = Element_Ctx.Buffer_Last
        and then Ctx.First <= Element_Ctx.First
        and then Ctx.Last >= Element_Ctx.Last),
     Post =>
       (Has_Buffer (Ctx)
        and not Element_Has_Buffer (Element_Ctx)
        and (if Element_Valid_Message (Element_Ctx)'Old then Valid (Ctx))
        and Sequence_Last (Ctx) = RFLX_Types.Bit_Length'(if Element_Valid_Message (Element_Ctx) then Element_Last (Element_Ctx) else Sequence_Last (Ctx))'Old
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old),
     Contract_Cases =>
       (Element_Valid_Message (Element_Ctx) =>
          (Sequence_Last (Ctx) = Element_Last (Element_Ctx)'Old),
        others =>
          True),
     Depends =>
       (Ctx => (Ctx, Element_Ctx), Element_Ctx => Element_Ctx);

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
      end record with
     Dynamic_Predicate =>
       ((if Buffer /= null then
          (Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last))
        and RFLX_Types.To_Index (First) >= Buffer_First
        and RFLX_Types.To_Index (Last) <= Buffer_Last
        and Buffer_Last < RFLX_Types.Index'Last
        and First <= Last + 1
        and Last <= RFLX_Types.Bit_Length'Last - 1
        and First - 1 <= Sequence_Last
        and Sequence_Last <= Last
        and First mod RFLX_Types.Byte'Size = 1
        and Last mod RFLX_Types.Byte'Size = 0
        and Sequence_Last mod RFLX_Types.Byte'Size = 0);

   function Has_Element (Ctx : Context) return Boolean is
     (Ctx.State = S_Valid and Ctx.Sequence_Last < Ctx.Last);

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

end {prefix}RFLX_Message_Sequence;

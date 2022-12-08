pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.B.Session_Allocator;
with RFLX.RFLX_Types;
with RFLX.Universal;
with RFLX.Universal.Message;

package RFLX.B.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Index;

   use type RFLX.RFLX_Types.Length;

   type Channel is (C_Channel);

   type State is (S_Start, S_Final);

   type Private_Context is private;

   type Context is abstract tagged limited
      record
         P : Private_Context;
      end record;

   function Uninitialized (Ctx : Context'Class) return Boolean;

   function Initialized (Ctx : Context'Class) return Boolean;

   function Active (Ctx : Context'Class) return Boolean;

   procedure Initialize (Ctx : in out Context'Class) with
     Pre =>
       Uninitialized (Ctx),
     Post =>
       Initialized (Ctx)
       and Active (Ctx);

   procedure Finalize (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Uninitialized (Ctx)
       and not Active (Ctx);

   pragma Warnings (Off, "subprogram ""Tick"" has no effect");

   procedure Tick (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx);

   pragma Warnings (On, "subprogram ""Tick"" has no effect");

   function In_IO_State (Ctx : Context'Class) return Boolean;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx);

   pragma Warnings (On, "subprogram ""Run"" has no effect");

   function Next_State (Ctx : Context'Class) return State;

   function Needs_Data (Ctx : Context'Class; Chan : Channel) return Boolean with
     Pre =>
       Initialized (Ctx);

   function Write_Buffer_Size (Ctx : Context'Class; Chan : Channel) return RFLX_Types.Length with
     Pre =>
       Initialized (Ctx)
       and then Needs_Data (Ctx, Chan);

   procedure Write (Ctx : in out Context'Class; Chan : Channel; Buffer : RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) with
     Pre =>
       Initialized (Ctx)
       and then Needs_Data (Ctx, Chan)
       and then Buffer'Length > 0
       and then Offset <= RFLX_Types.Length'Last - Buffer'Length
       and then Buffer'Length + Offset <= Write_Buffer_Size (Ctx, Chan),
     Post =>
       Initialized (Ctx);

private

   type Private_Context is
      record
         Next_State : State := S_Start;
         M_Ctx : Universal.Message.Context;
         Slots : B.Session_Allocator.Slots;
         Memory : B.Session_Allocator.Memory;
      end record;

   function Uninitialized (Ctx : Context'Class) return Boolean is
     (not Universal.Message.Has_Buffer (Ctx.P.M_Ctx)
      and B.Session_Allocator.Uninitialized (Ctx.P.Slots));

   function Global_Initialized (Ctx : Context'Class) return Boolean is
     (Universal.Message.Has_Buffer (Ctx.P.M_Ctx)
      and then Ctx.P.M_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Ctx.P.M_Ctx.Buffer_Last = RFLX_Types.Index'First + 2047);

   function Initialized (Ctx : Context'Class) return Boolean is
     (Global_Initialized (Ctx)
      and then B.Session_Allocator.Global_Allocated (Ctx.P.Slots));

   function Active (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State /= S_Final);

   function Next_State (Ctx : Context'Class) return State is
     (Ctx.P.Next_State);

   function Needs_Data (Ctx : Context'Class; Chan : Channel) return Boolean is
     ((case Chan is
          when C_Channel =>
             (case Ctx.P.Next_State is
                 when S_Start =>
                    True,
                 when others =>
                    False)));

   function Write_Buffer_Size (Ctx : Context'Class; Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_Channel =>
             (case Ctx.P.Next_State is
                 when S_Start =>
                    Universal.Message.Buffer_Length (Ctx.P.M_Ctx),
                 when others =>
                    RFLX_Types.Unreachable)));

end RFLX.B.Session;
pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Test.Session_Allocator;
with RFLX.RFLX_Types;

package RFLX.Test.Session with
  SPARK_Mode
is

   type State is (S_Start, S_Check_Message, S_Check_Message_Sequence, S_Check_Scalar_Sequence, S_Error, S_Final);

   type Private_Context is private;

   type Context is abstract tagged limited
      record
         P : Private_Context;
      end record;

   procedure Check_Size (Ctx : in out Context; Size : RFLX.Test.Size; Data : RFLX_Types.Bytes; RFLX_Result : out Boolean) is abstract;

   function Uninitialized (Unused_Ctx : Context'Class) return Boolean;

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

   function In_IO_State (Unused_Ctx : Context'Class) return Boolean;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx);

   pragma Warnings (On, "subprogram ""Run"" has no effect");

   function Next_State (Ctx : Context'Class) return State;

private

   type Private_Context is
      record
         Next_State : State := S_Start;
         Slots : Test.Session_Allocator.Slots;
         Memory : Test.Session_Allocator.Memory;
      end record;

   function Uninitialized (Unused_Ctx : Context'Class) return Boolean is
     (True);

   function Initialized (Ctx : Context'Class) return Boolean is
     (Test.Session_Allocator.Global_Allocated (Ctx.P.Slots));

   function Active (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State /= S_Final);

   function Next_State (Ctx : Context'Class) return State is
     (Ctx.P.Next_State);

end RFLX.Test.Session;

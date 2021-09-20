pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.Universal;
with RFLX.Universal.Message;

generic
   with function Channel_Has_Data return Boolean;
   with procedure Channel_Read (Buffer : out RFLX_Types.Bytes; Length : out RFLX_Types.Length);
   with procedure Channel_Write (Buffer : RFLX_Types.Bytes);
package RFLX.Test.Session with
  SPARK_Mode,
  Initial_Condition =>
    Uninitialized
is

   pragma Unreferenced (Channel_Has_Data);

   function Uninitialized return Boolean;

   function Initialized return Boolean;

   function Active return Boolean;

   procedure Initialize with
     Post =>
       Initialized
       and Active;

   procedure Finalize with
     Pre =>
       Initialized,
     Post =>
       Uninitialized
       and not Active;

   pragma Warnings (Off, "subprogram ""Tick"" has no effect");

   procedure Tick with
     Pre =>
       Initialized,
     Post =>
       Initialized;

   pragma Warnings (On, "subprogram ""Tick"" has no effect");

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run with
     Post =>
       Uninitialized;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

private

   use type RFLX.RFLX_Types.Index;

   type Session_State is (S_Start, S_Reply, S_Terminated);

   State : Session_State := S_Start;

   Message_Ctx : Universal.Message.Context;

   function Uninitialized return Boolean is
     (not Universal.Message.Has_Buffer (Message_Ctx));

   function Initialized return Boolean is
     (Universal.Message.Has_Buffer (Message_Ctx)
      and then Message_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Message_Ctx.Buffer_Last = RFLX_Types.Index'First + 4095);

   function Active return Boolean is
     (State /= S_Terminated);

end RFLX.Test.Session;

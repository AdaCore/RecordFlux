pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.TLV;
with RFLX.TLV.Messages;
with RFLX.TLV.Tags;

generic
   with procedure Channel_Write (Buffer : RFLX_Types.Bytes);
package RFLX.Test.Session with
  SPARK_Mode,
  Initial_Condition =>
    Uninitialized
is

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

   Messages_Ctx : TLV.Messages.Context;

   Tags_Ctx : TLV.Tags.Context;

   function Uninitialized return Boolean is
     (not TLV.Messages.Has_Buffer (Messages_Ctx)
      and not TLV.Tags.Has_Buffer (Tags_Ctx));

   function Initialized return Boolean is
     (TLV.Messages.Has_Buffer (Messages_Ctx)
      and then Messages_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Messages_Ctx.Buffer_Last = RFLX_Types.Index'First + 4095
      and then TLV.Tags.Has_Buffer (Tags_Ctx)
      and then Tags_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Tags_Ctx.Buffer_Last = RFLX_Types.Index'First + 4095);

   function Active return Boolean is
     (State /= S_Terminated);

end RFLX.Test.Session;
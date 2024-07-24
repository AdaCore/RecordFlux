with RFLX.SPDM_Responder.Session.FSM;
with RFLX.RFLX_Types;
with RFLX.RFLX_Types.Operators;

package body Responder with
   SPARK_Mode,
   Refined_State => (Responder_State => (Buffer, Context))
is
   use RFLX.RFLX_Types;
   use RFLX.RFLX_Types.Operators;

   Buffer : Bytes (Index'First .. Index'First + 1279) := (others => 0);
   Context : RFLX.SPDM_Responder.Session.FSM.Context;

   function Uninitialized return Boolean is (RFLX.SPDM_Responder.Session.FSM.Uninitialized (Context));

   procedure Responder_Main with
      Pre => Uninitialized,
      Post => Uninitialized,
      Global => (In_Out => (Buffer, Context));

   procedure Main
   is
   begin
      if Uninitialized then
         Responder_Main;
      end if;
   end Main;

   procedure Responder_Main
   is
      package SR renames RFLX.SPDM_Responder.Session.FSM;
   begin
      loop
         --  Eng/RecordFlux/RecordFlux#1032
         --  Context.Plat_Initialize;
         pragma Loop_Invariant (SR.Uninitialized (Context));

         SR.Initialize (Context);

         while SR.Active (Context) loop
            pragma Loop_Invariant (SR.Initialized (Context));

            if SR.Has_Data (Context, SR.C_Transport) then
               declare
                  use type RFLX.RFLX_Types.Length;
                  BS : constant Length := SR.Read_Buffer_Size (Context, SR.C_Transport);
               begin
                  if Buffer'Length >= BS then
                     SR.Read
                        (Context,
                         SR.C_Transport,
                         Buffer (Buffer'First .. Buffer'First + BS - 1));
                     pragma Inspection_Point (Buffer);
                  end if;
               end;
            end if;

            if SR.Needs_Data (Context, SR.C_Transport) then
               declare
                  BS : constant Length := SR.Write_Buffer_Size (Context, SR.C_Transport);
               begin
                  SR.Write (Context, SR.C_Transport, Buffer (Buffer'First .. Buffer'First + BS - 1));
               end;
            end if;

            SR.Run (Context);
         end loop;

         SR.Finalize (Context);
      end loop;
   end Responder_Main;

end Responder;

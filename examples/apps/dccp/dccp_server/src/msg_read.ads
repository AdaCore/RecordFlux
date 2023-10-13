with RFLX.DCCP.Packet;

package Msg_Read with
   SPARK_Mode => On
is
   use RFLX;
   use type DCCP.Type_Field;

   procedure DCCP_REQUEST (Ctx : DCCP.Packet.Context) with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then DCCP.Packet.Well_Formed_Message (Ctx)
       and then DCCP.Packet.Get_Packet_Type (Ctx) = DCCP.DCCP_REQUEST;

   procedure DCCP_ACK (Ctx :  in out DCCP.Packet.Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then DCCP.Packet.Well_Formed_Message (Ctx)
       and then DCCP.Packet.Get_Packet_Type (Ctx) = DCCP.DCCP_ACK,
     Post =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx),
     Annotate =>
       (GNATprove, Might_Not_Return);

   procedure DCCP_DATA_ACK (Ctx : in out DCCP.Packet.Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then DCCP.Packet.Well_Formed_Message (Ctx)
       and then DCCP.Packet.Get_Packet_Type (Ctx) = DCCP.DCCP_DATA_ACK,
     Post =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx),
     Annotate =>
       (GNATprove, Might_Not_Return);

   procedure DCCP_DATA (Ctx : DCCP.Packet.Context) with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then DCCP.Packet.Well_Formed_Message (Ctx)
       and then DCCP.Packet.Get_Packet_Type (Ctx) = DCCP.DCCP_DATA,
     Annotate =>
       (GNATprove, Might_Not_Return);

   procedure DCCP_CLOSE (Ctx : DCCP.Packet.Context) with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then DCCP.Packet.Well_Formed_Message (Ctx)
       and then DCCP.Packet.Get_Packet_Type (Ctx) = DCCP.DCCP_CLOSE;

   procedure DCCP_RESET (Ctx : in out DCCP.Packet.Context) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then DCCP.Packet.Well_Formed_Message (Ctx)
       and then DCCP.Packet.Get_Packet_Type (Ctx) = DCCP.DCCP_RESET,
     Post =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx),
     Annotate =>
       (GNATprove, Might_Not_Return);

   procedure DCCP_RESPONSE (Ctx : DCCP.Packet.Context) with
     Pre =>
       RFLX.DCCP.Packet.Has_Buffer (Ctx)
       and then DCCP.Packet.Well_Formed_Message (Ctx)
       and then DCCP.Packet.Get_Packet_Type (Ctx) = DCCP.DCCP_RESPONSE;

end Msg_Read;

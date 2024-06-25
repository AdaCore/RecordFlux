------------------------------------------------------------------------------
--                                                                          --
--                         Generated by RecordFlux                          --
--                                                                          --
--                          Copyright (C) AdaCore                           --
--                                                                          --
--         SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception          --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Streams);
pragma Ada_2012;
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.TLV;
with RFLX.TLV.Message;

package RFLX.P.S with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Index;

   use type RFLX.RFLX_Types.Bit_Length;

   use type RFLX.RFLX_Types.Bytes_Ptr;

   use type RFLX.RFLX_Types.Length;

   type Channel is (C_X);

   type State is (S_A, S_B, S_Final);

   type External_Buffer is (B_M);

   type Private_Context is private;

   type Context is abstract tagged limited
      record
         P : Private_Context;
      end record;

   pragma Unevaluated_Use_Of_Old (Allow);

   function Uninitialized (Ctx : Context'Class) return Boolean;

   function Initialized (Ctx : Context'Class) return Boolean;

   function Active (Ctx : Context'Class) return Boolean;

   procedure Initialize (Ctx : in out Context'Class; M_Buffer : in out RFLX_Types.Bytes_Ptr) with
     Pre =>
       Uninitialized (Ctx)
       and then M_Buffer /= null
       and then M_Buffer'Length > 0
       and then M_Buffer'Last < RFLX_Types.Index'Last,
     Post =>
       Initialized (Ctx)
       and Active (Ctx)
       and Has_Buffer (Ctx, B_M)
       and M_Buffer = null;

   procedure Finalize (Ctx : in out Context'Class; M_Buffer : in out RFLX_Types.Bytes_Ptr) with
     Pre =>
       Initialized (Ctx)
       and then M_Buffer = null,
     Post =>
       Uninitialized (Ctx)
       and not Active (Ctx)
       and M_Buffer /= null;

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

   function Buffer_Accessible (Ctx : Context'Class; Unused_Ext_Buf : External_Buffer) return Boolean;

   function Channel_Accessible (Ctx : Context'Class; Chan : Channel) return Boolean;

   function Accessible_Buffer (Ctx : Context'Class; Chan : Channel) return External_Buffer with
     Pre =>
       Channel_Accessible (Ctx, Chan);

   function Has_Buffer (Ctx : Context'Class; Ext_Buf : External_Buffer) return Boolean;

   function Written_Last (Ctx : Context'Class; Ext_Buf : External_Buffer) return RFLX_Types.Bit_Length;

   procedure Add_Buffer (Ctx : in out Context'Class; Ext_Buf : External_Buffer; Buffer : in out RFLX_Types.Bytes_Ptr; Written_Last : RFLX_Types.Bit_Length) with
     Pre =>
       Buffer_Accessible (Ctx, Ext_Buf)
       and then not Has_Buffer (Ctx, Ext_Buf)
       and then Buffer /= null
       and then Buffer'Length > 0
       and then Buffer'Last < RFLX_Types.Index'Last
       and then (Written_Last = 0
                 or (Written_Last >= RFLX_Types.To_First_Bit_Index (Buffer'First) - 1
                     and Written_Last <= RFLX_Types.To_Last_Bit_Index (Buffer'Last)))
       and then Written_Last mod RFLX_Types.Byte'Size = 0,
     Post =>
       Buffer_Accessible (Ctx, Ext_Buf)
       and then Has_Buffer (Ctx, Ext_Buf)
       and then Buffer = null
       and then (if
                    Ext_Buf /= B_M
                 then
                    Has_Buffer (Ctx, B_M) = Has_Buffer (Ctx, B_M)'Old
                    and P.S.Written_Last (Ctx, B_M) = P.S.Written_Last (Ctx, B_M)'Old)
       and then Buffer_Accessible (Ctx, B_M) = Buffer_Accessible (Ctx, B_M)'Old
       and then Next_State (Ctx) = Next_State (Ctx)'Old;

   procedure Remove_Buffer (Ctx : in out Context'Class; Ext_Buf : External_Buffer; Buffer : out RFLX_Types.Bytes_Ptr) with
     Pre =>
       Buffer_Accessible (Ctx, Ext_Buf)
       and Has_Buffer (Ctx, Ext_Buf),
     Post =>
       Buffer_Accessible (Ctx, Ext_Buf)
       and then not Has_Buffer (Ctx, Ext_Buf)
       and then Buffer /= null
       and then Buffer'Length > 0
       and then Buffer'Last < RFLX_Types.Index'Last
       and then (Written_Last (Ctx, Ext_Buf) = 0
                 or (Written_Last (Ctx, Ext_Buf) >= RFLX_Types.To_First_Bit_Index (Buffer'First) - 1
                     and Written_Last (Ctx, Ext_Buf) <= RFLX_Types.To_Last_Bit_Index (Buffer'Last)))
       and then Written_Last (Ctx, Ext_Buf) mod RFLX_Types.Byte'Size = 0
       and then (if
                    Ext_Buf /= B_M
                 then
                    Has_Buffer (Ctx, B_M) = Has_Buffer (Ctx, B_M)'Old
                    and P.S.Written_Last (Ctx, B_M) = P.S.Written_Last (Ctx, B_M)'Old)
       and then Buffer_Accessible (Ctx, B_M) = Buffer_Accessible (Ctx, B_M)'Old
       and then Next_State (Ctx) = Next_State (Ctx)'Old;

   function Has_Data (Ctx : Context'Class; Chan : Channel) return Boolean with
     Pre =>
       Initialized (Ctx);

   function Read_Buffer_Size (Ctx : Context'Class; Chan : Channel) return RFLX_Types.Length with
     Pre =>
       Initialized (Ctx)
       and then Has_Data (Ctx, Chan);

   procedure Read (Ctx : Context'Class; Chan : Channel; Buffer : out RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) with
     Pre =>
       Initialized (Ctx)
       and then Has_Data (Ctx, Chan)
       and then Buffer'Length > 0
       and then Offset <= RFLX_Types.Length'Last - Buffer'Length
       and then Buffer'Length + Offset <= Read_Buffer_Size (Ctx, Chan),
     Post =>
       Initialized (Ctx)
       and then Next_State (Ctx) = Next_State (Ctx)'Old;

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
       Initialized (Ctx)
       and then Next_State (Ctx) = Next_State (Ctx)'Old;

private

   type Private_Context is
      record
         Next_State : State := S_A;
         M_Ctx : TLV.Message.Context;
      end record;

   function Uninitialized (Ctx : Context'Class) return Boolean is
     (not TLV.Message.Has_Buffer (Ctx.P.M_Ctx));

   function Global_Initialized (Ctx : Context'Class) return Boolean is
     (TLV.Message.Has_Buffer (Ctx.P.M_Ctx));

   function Initialized (Ctx : Context'Class) return Boolean is
     (Global_Initialized (Ctx));

   function Active (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State /= S_Final);

   function Next_State (Ctx : Context'Class) return State is
     (Ctx.P.Next_State);

   function Buffer_Accessible (Ctx : Context'Class; Unused_Ext_Buf : External_Buffer) return Boolean is
     ((for some C in Channel =>
          Channel_Accessible (Ctx, C)));

   function Channel_Accessible (Ctx : Context'Class; Chan : Channel) return Boolean is
     ((case Chan is
          when C_X =>
             (case Ctx.P.Next_State is
                 when S_A | S_B =>
                    True,
                 when others =>
                    False)));

   function Unreachable_External_Buffer return External_Buffer is
     (B_M)
    with
     Pre =>
       False;

   function Accessible_Buffer (Ctx : Context'Class; Chan : Channel) return External_Buffer is
     ((case Chan is
          when C_X =>
             (case Ctx.P.Next_State is
                 when S_A | S_B =>
                    B_M,
                 when others =>
                    Unreachable_External_Buffer)));

   function Has_Buffer (Ctx : Context'Class; Ext_Buf : External_Buffer) return Boolean is
     ((case Ext_Buf is
          when B_M =>
             TLV.Message.Has_Buffer (Ctx.P.M_Ctx)));

   function Written_Last (Ctx : Context'Class; Ext_Buf : External_Buffer) return RFLX_Types.Bit_Length is
     ((case Ext_Buf is
          when B_M =>
             TLV.Message.Written_Last (Ctx.P.M_Ctx)));

   function Has_Data (Ctx : Context'Class; Chan : Channel) return Boolean is
     ((case Chan is
          when C_X =>
             (case Ctx.P.Next_State is
                 when S_B =>
                    TLV.Message.Well_Formed_Message (Ctx.P.M_Ctx)
                    and TLV.Message.Byte_Size (Ctx.P.M_Ctx) > 0,
                 when others =>
                    False)));

   function Read_Buffer_Size (Ctx : Context'Class; Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_X =>
             (case Ctx.P.Next_State is
                 when S_B =>
                    TLV.Message.Byte_Size (Ctx.P.M_Ctx),
                 when others =>
                    RFLX_Types.Unreachable)));

   function Needs_Data (Ctx : Context'Class; Chan : Channel) return Boolean is
     ((case Chan is
          when C_X =>
             (case Ctx.P.Next_State is
                 when S_A =>
                    True,
                 when others =>
                    False)));

   function Write_Buffer_Size (Ctx : Context'Class; Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_X =>
             (case Ctx.P.Next_State is
                 when S_A =>
                    TLV.Message.Buffer_Length (Ctx.P.M_Ctx),
                 when others =>
                    RFLX_Types.Unreachable)));

end RFLX.P.S;
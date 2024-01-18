------------------------------------------------------------------------------
--                                                                          --
--                         Generated by RecordFlux                          --
--                                                                          --
--                        Copyright (C) AdaCore GmbH                        --
--                                                                          --
--                   SPDX-License-Identifier: Apache-2.0                    --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Message_Sequence;
with RFLX.TLV.Message;
pragma Warnings (Off, "unit ""*RFLX_Types"" is not referenced");
with RFLX.RFLX_Types;
pragma Warnings (On, "unit ""*RFLX_Types"" is not referenced");

package RFLX.TLV.Messages is new RFLX.RFLX_Message_Sequence (RFLX.TLV.Message.Context, RFLX.TLV.Message.Initialize, RFLX.TLV.Message.Take_Buffer, RFLX.TLV.Message.Copy, RFLX.TLV.Message.Has_Buffer, RFLX.TLV.Message.Size, RFLX.TLV.Message.Message_Last, RFLX.TLV.Message.Initialized, RFLX.TLV.Message.Well_Formed_Message);

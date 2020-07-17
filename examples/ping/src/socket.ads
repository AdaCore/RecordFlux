with Generic_Socket;
with RFLX.RFLX_Builtin_Types;

package Socket is new Generic_Socket (RFLX.RFLX_Builtin_Types.Byte,
                                      RFLX.RFLX_Builtin_Types.Index,
                                      RFLX.RFLX_Builtin_Types.Bytes);

with Generic_Socket;
with RFLX.RFLX_Builtin_Types;
with RFLX.IPv4;

package Socket is new Generic_Socket (RFLX.RFLX_Builtin_Types.Byte,
                                      RFLX.RFLX_Builtin_Types.Index,
                                      RFLX.RFLX_Builtin_Types.Bytes,
                                      RFLX.IPv4.Address);

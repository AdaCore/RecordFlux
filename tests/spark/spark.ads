with Ada.Unchecked_Deallocation;
with RFLX.RFLX_Builtin_Types;

package SPARK is

   procedure Free_Bytes_Ptr is new Ada.Unchecked_Deallocation (Object => RFLX.RFLX_Builtin_Types.Bytes,
                                                               Name => RFLX.RFLX_Builtin_Types.Bytes_Ptr);

end SPARK;

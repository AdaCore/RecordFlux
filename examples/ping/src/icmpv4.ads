with RFLX.IPv4;
with RFLX.RFLX_Builtin_Types;

package ICMPv4 with
   SPARK_Mode
is

   use type RFLX.RFLX_Builtin_Types.Bytes_Ptr;

   procedure Get_Address (Str   : String;
                          Addr  : out RFLX.IPv4.Address;
                          Valid : out Boolean);

   procedure Generate (Buf  : in out RFLX.RFLX_Builtin_Types.Bytes_Ptr;
                       Addr :        RFLX.IPv4.Address) with
      Pre => Buf /= null,
      Post => Buf /= null;

   procedure Print (Buf : in out RFLX.RFLX_Builtin_Types.Bytes_Ptr) with
      Pre => Buf /= null,
      Post => Buf /= null;

end ICMPv4;

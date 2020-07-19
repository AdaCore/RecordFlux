with SPARK.Heap;
with RFLX.IPv4;
with RFLX.RFLX_Builtin_Types;
with Ada.Text_IO;
with Ada.Real_Time;

package ICMPv4 with
   SPARK_Mode,
   Abstract_State => Ping_State,
   Initializes => Ping_State
is

   pragma Unevaluated_Use_Of_Old (Allow);

   use type RFLX.RFLX_Builtin_Types.Bytes_Ptr;
   use type RFLX.RFLX_Builtin_Types.Length;

   procedure Get_Address (Str   : String;
                          Addr  : out RFLX.IPv4.Address;
                          Valid : out Boolean) with
      Global => null;

   procedure Ping (Addr : String) with
      Global => (In_Out => (Ping_State,
                            Ada.Text_IO.File_System,
                            SPARK.Heap.Dynamic_Memory),
                 Input => Ada.Real_Time.Clock_Time);

   procedure Generate (Buf  : in out RFLX.RFLX_Builtin_Types.Bytes_Ptr;
                       Addr :        RFLX.IPv4.Address) with
      Pre => Buf /= null
      and then Buf'Length = 1024
      and then Buf'First = 1,
      Post => Buf /= null
      and then Buf'Length = Buf'Length'Old
      and then Buf'First = Buf'First'Old,
      Global => (In_Out => Ping_State);

   procedure Print (Buf : in out RFLX.RFLX_Builtin_Types.Bytes_Ptr) with
      Pre => Buf /= null
      and then Buf'Length = 1024
      and then Buf'First = 1,
      Post => Buf /= null
      and then Buf'Length = Buf'Length'Old
      and then Buf'First = Buf'First'Old,
      Global => (In_Out => Ada.Text_IO.File_System);

end ICMPv4;

pragma Style_Checks ("N3aAbCdefhiIklnOprStux");

package {prefix}RFLX_Builtin_Types with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Always_Return)
is

   type Length is new Natural;

   type Index is new Length range 1 .. Length'Last;

   type Byte is mod 2**8;

   type Bytes is array (Index range <>) of Byte;

   type Bytes_Ptr is access Bytes;

   type Bit_Length is range 0 .. Length'Last * 8;

   type Boolean_Base is mod 2;

end {prefix}RFLX_Builtin_Types;

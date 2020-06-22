pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package {prefix}RFLX_Utils with
  SPARK_Mode
is

   type U64 is mod 2**64 with
     Annotate => (GNATprove, No_Wrap_Around);

   function Right_Shift (Value : U64; Value_Size : Positive; Length : Natural) return U64 with
     Pre =>
       Value_Size <= U64'Size
       and then Length < U64'Size
       and then Value_Size >= Length
       and then Value_Size - Length in 0 .. U64'Size - 1
       and then (if Value_Size < U64'Size then Value < 2**Value_Size),
     Post =>
       Right_Shift'Result < 2**(Value_Size - Length);

   function Left_Shift (Value : U64; Value_Size : Positive; Length : Natural) return U64 with
     Pre =>
       Value_Size <= U64'Size
       and then Length < U64'Size
       and then Value_Size + Length in 1 .. U64'Size
       and then (if Value_Size < U64'Size then Value < U64 (2)**Value_Size),
     Post =>
       (if
          Value_Size + Length < U64'Size
        then
          Left_Shift'Result <= U64 (2)**(Value_Size + Length) - U64 (2)**Length
          and U64 (2)**(Value_Size + Length) >= U64 (2)**Length
        else
          Left_Shift'Result <= U64'Last - U64 (2)**Length + 1);

   function Mod_Pow2 (Value : U64; Exp : Natural) return U64 with
     Pre =>
       Exp < U64'Size,
     Post =>
       Mod_Pow2'Result < 2**Exp;

end {prefix}RFLX_Utils;

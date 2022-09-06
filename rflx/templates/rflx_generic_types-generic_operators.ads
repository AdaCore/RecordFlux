pragma Style_Checks ("N3aAbCdefhiIklnOprStux");

generic
package {prefix}RFLX_Generic_Types.Generic_Operators with
   SPARK_Mode,
   Annotate =>
     (GNATprove, Always_Return)
is

   function "+" (Left : Index; Right : Length) return Index is
      (Index (Length (Left) + Right))
   with
     Pre =>
       Length (Left) <= Length'Last - Right;

   function "-" (Left : Index; Right : Index) return Length is
      (Length (Left) - Length (Right))
   with
     Pre =>
       Length (Left) >= Length'First + Length (Right);

   function "-" (Left : Index; Right : Length) return Index is
      (Index (Length (Left) - Right))
   with
     Pre =>
       Right < Length'Last
       and then Length (Left) >= Length (Index'First) + Right;

end {prefix}RFLX_Generic_Types.Generic_Operators;

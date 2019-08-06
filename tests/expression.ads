with Types;
use type Types.Bytes, Types.Index_Type, Types.Length_Type;

package Expression
  with SPARK_Mode
is

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Types_Index_Type return Types.Index_Type is
      (Types.Index_Type'First)
     with
       Pre => False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Types_Length_Type return Types.Length_Type is
      (Types.Length_Type'First)
     with
       Pre => False;

   pragma Warnings (On, "precondition is statically false");

end Expression;

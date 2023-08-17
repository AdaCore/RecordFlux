pragma Warnings (Off, """Always_Terminates"" is not a valid aspect identifier");
with RFLX.RFLX_Builtin_Types;

package SPARK.Assertions with
  SPARK_Mode,
  Always_Terminates
is

   use type RFLX.RFLX_Builtin_Types.Bytes;

   pragma Warnings (Off, "postcondition does not check the outcome of calling ""Assert""");

   procedure Assert (Condition : Boolean;
                     Message   : String) with
     Post => Condition;

   function Assert (Condition : Boolean;
                    Message   : String) return Boolean with
     Post => (if Assert'Result then Condition);

   procedure Assert (Actual    : String;
                     Expected  : String;
                     Message   : String) with
     Post => Actual = Expected;

   procedure Assert (Actual   : RFLX.RFLX_Builtin_Types.Bytes;
                     Expected : RFLX.RFLX_Builtin_Types.Bytes;
                     Message  : String) with
     Post => Actual = Expected;

   pragma Warnings (On, "postcondition does not check the outcome of calling ""Assert""");

end SPARK.Assertions;

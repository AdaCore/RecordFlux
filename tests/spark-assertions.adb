with AUnit.Assertions;

package body SPARK.Assertions
  with SPARK_Mode => Off
is

   procedure Assert (Condition : Boolean;
                     Message   : String) is
   begin
      AUnit.Assertions.Assert (Condition, Message);
   end Assert;

   function Assert (Condition : Boolean;
                    Message   : String) return Boolean is
   begin
      return AUnit.Assertions.Assert (Condition, Message);
   end Assert;

   procedure Assert (Actual    : String;
                     Expected  : String;
                     Message   : String) is
   begin
      AUnit.Assertions.Assert (Actual, Expected, Message);
   end Assert;

end SPARK.Assertions;

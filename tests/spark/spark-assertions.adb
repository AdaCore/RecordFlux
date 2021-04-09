with AUnit.Assertions;

package body SPARK.Assertions with
  SPARK_Mode => Off
is

   procedure Assert (Condition : Boolean;
                     Message   : String)
   is
   begin
      AUnit.Assertions.Assert (Condition, Message);
   end Assert;

   function Assert (Condition : Boolean;
                    Message   : String) return Boolean
   is
   begin
      return AUnit.Assertions.Assert (Condition, Message);
   end Assert;

   procedure Assert (Actual    : String;
                     Expected  : String;
                     Message   : String)
   is
   begin
      AUnit.Assertions.Assert (Actual, Expected, Message);
   end Assert;

   procedure Assert (Actual   : RFLX.RFLX_Builtin_Types.Bytes;
                     Expected : RFLX.RFLX_Builtin_Types.Bytes;
                     Message  : String)
   is
      use type RFLX.RFLX_Builtin_Types.Index;
   begin
      if Actual'Length /= Expected'Length then
         Assert (False, Message & ": lengths different");
      end if;
      for I in RFLX.RFLX_Builtin_Types.Index range 1 .. Actual'Length loop
         Assert (Actual (Actual'First + I - 1)'Image,
                 Expected (Expected'First + I - 1)'Image,
                 Message & ": byte" & I'Image);
      end loop;
   end Assert;

end SPARK.Assertions;

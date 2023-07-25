pragma Style_Checks ("N3aAbCdefhiIklnOprStux");

with RFLX.RFLX_Types;
with RFLX.Universal;
with RFLX.Test.Definite_Message;
with RFLX.Test.Session;

package Session with
   SPARK_Mode,
   Elaborate_Body
is

   type Context is new RFLX.Test.Session.Context with null record;

   pragma Warnings (Off, """Ctx"" is not modified, could be IN");
   pragma Warnings (Off, "unused variable ""Ctx""");

   overriding
   procedure Get_Message_Type
      (Ctx    : in out Context;
       Result :    out RFLX.Universal.Option_Type);

   overriding
   procedure Create_Message
      (Ctx          : in out Context;
       Message_Type :        RFLX.Universal.Option_Type;
       Length       :        RFLX.Test.Length;
       Data         :        RFLX.RFLX_Types.Bytes;
       Result       :    out RFLX.Test.Definite_Message.Structure);

   overriding
   procedure Valid_Message
      (Ctx           : in out Context;
       Message_Type  :        RFLX.Universal.Option_Type;
       Strict        :        Boolean;
       Result        :    out RFLX.Test.Result);

   overriding
   procedure Byte_Size
      (Ctx           : in out Context;
       Result        :    out RFLX.Test.Length);

   pragma Warnings (On, "unused variable ""Ctx""");
   pragma Warnings (On, """Ctx"" is not modified, could be IN");

end Session;

pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

with RFLX.RFLX_Types;
with RFLX.Universal;
with RFLX.Fixed_Size.Simple_Message;

package Func with
   SPARK_Mode
is

   procedure Get_Message_Type (Result : out RFLX.Universal.Option_Type) with
     Pre =>
       not Result'Constrained;

   procedure Create_Message (Result : out RFLX.Fixed_Size.Simple_Message.Structure; Message_Type : RFLX.Universal.Option_Type; Data : RFLX.RFLX_Types.Bytes);

   procedure Valid_Message (Valid_Message : out Boolean; Message_Type : RFLX.Universal.Option_Type; Strict : Boolean);

end Func;
